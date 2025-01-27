import os

import flopy
import numpy as np
import pytest
from framework import TestFramework
from simulation import TestSimulation

paktest = "sfr"

ex = [
    "sfr_npt02a",
]

# temporal discretization
nper = 10
tdis_rc = []
for n in range(nper):
    tdis_rc.append((1.0, 1, 1.0))

# spatial discretization data
nlay, nrow, ncol = 1, 1, 1
delr, delc = 100.0, 100.0
top = 0.0
botm = -10.0
strt = 0.0

# sfr data
nreaches = 2
rlen = 50.0
rwid0 = 10.0
conversion_fact = 1.0
roughness = 0.001
rbth = 1.0
rhk = 0.0
slope = 0.001
ustrf = 1.0
ndv = 0
inflow = 1000.0

np_data = {}
for n in range(nper):
    rwid = float(n + 1) * rwid0
    np_data[n] = {
        "x": np.array([0.0, rwid], dtype=float),
        "h": np.array([0.0, 0.0], dtype=float),
    }

# depth as a function of flow for a wide cross-section
def flow_to_depth_wide(rwid, q):
    return ((q * roughness) / (conversion_fact * rwid * np.sqrt(slope))) ** 0.6


#
def build_model(idx, ws):

    # build MODFLOW 6 files
    name = ex[idx]
    sim = flopy.mf6.MFSimulation(
        sim_name=name,
        version="mf6",
        exe_name="mf6",
        sim_ws=ws,
    )
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(
        sim,
        time_units="seconds",
        nper=nper,
        perioddata=tdis_rc,
    )

    # create iterative model solution and register the gwf model with it
    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="ALL",
    )

    # create gwf model
    gwf = flopy.mf6.ModflowGwf(
        sim,
        modelname=name,
        save_flows=True,
    )

    dis = flopy.mf6.ModflowGwfdis(
        gwf,
        length_units="meters",
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
    )

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt)

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(gwf)

    # chd files
    # chd data
    spd = [
        [(0, 0, 0), 0.0],
    ]
    chd = flopy.mf6.modflow.ModflowGwfchd(
        gwf, stress_period_data=spd, pname="chd-1"
    )

    # sfr file
    packagedata = []
    for irch in range(nreaches):
        nconn = 1
        if 0 < irch < nreaches - 1:
            nconn += 1
        rp = [
            irch,
            "none",
            rlen,
            rwid,
            slope,
            top,
            rbth,
            rhk,
            roughness,
            nconn,
            ustrf,
            ndv,
        ]
        packagedata.append(rp)

    connectiondata = []
    for irch in range(nreaches):
        rc = [irch]
        if irch > 0:
            rc.append(irch - 1)
        if irch < nreaches - 1:
            rc.append(-(irch + 1))
        connectiondata.append(rc)

    # create cross-section files
    perioddata = {}
    for n in range(nper):
        sfr_tab = f"{name}.{n:02d}.sfr.tab"
        pname = f"sfrtab{n:02d}"
        stations = np_data[n]["x"] / rwid0
        depths = np_data[n]["h"]
        table = [[x, d] for x, d in zip(stations, depths)]
        t = flopy.mf6.ModflowUtlsfrtab(
            gwf,
            nrow=stations.shape[0],
            ncol=2,
            table=table,
            filename=sfr_tab,
            pname=pname,
        )
        t.write()
        spd = []
        if n == 0:
            spd.append((0, "inflow", inflow))
        spd.append((0, "cross_section", sfr_tab))
        spd.append((1, "cross_section", sfr_tab))
        perioddata[n] = spd

    budpth = f"{name}.{paktest}.cbc"
    sfr = flopy.mf6.ModflowGwfsfr(
        gwf,
        print_stage=True,
        print_flows=True,
        print_input=True,
        budget_filerecord=budpth,
        mover=True,
        nreaches=nreaches,
        packagedata=packagedata,
        connectiondata=connectiondata,
        perioddata=perioddata,
        pname="sfr-1",
    )
    fname = f"{name}.sfr.obs"
    sfr_obs = {
        f"{fname}.csv": [
            ("inflow", "ext-inflow", (0,)),
            ("outflow", "ext-outflow", (nreaches - 1,)),
            ("depth", "depth", (nreaches - 1,)),
            ("width", "wet-width", (nreaches - 1,)),
        ]
    }
    sfr.obs.initialize(
        filename=fname, digits=25, print_input=True, continuous=sfr_obs
    )

    # output control
    budpth = f"{name}.cbc"
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=budpth,
        printrecord=[
            ("BUDGET", "ALL"),
        ],
        saverecord=[
            ("BUDGET", "ALL"),
        ],
    )

    return sim, None


def eval_npointdepth(sim):
    name = sim.name
    print("evaluating n-point cross-section results..." f"({name})")

    obs_pth = os.path.join(sim.simpath, f"{name}.sfr.obs.csv")
    obs = flopy.utils.Mf6Obs(obs_pth).get_data()

    assert np.allclose(
        obs["INFLOW"], np.abs(obs["OUTFLOW"])
    ), "inflow not equal to outflow"

    d = flow_to_depth_wide(
        obs["WIDTH"],
        inflow,
    )

    assert np.allclose(
        obs["DEPTH"], d
    ), "sfr depth not equal to calculated depth"


@pytest.mark.parametrize(
    "name",
    ex,
)
def test_mf6model(name, function_tmpdir, targets):
    ws = str(function_tmpdir)
    test = TestFramework()
    test.build(build_model, 0, ws)
    test.run(
        TestSimulation(
            name=name,
            exe_dict=targets,
            exfunc=eval_npointdepth,
            idxsim=0,
        ),
        ws,
    )
