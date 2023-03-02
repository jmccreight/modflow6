"""

Simple 4 segment network with 5 vertices


zero-based diagram below
v1
o
 |
  |
   |  s0
    |
     |
      |
o------o------o------o
v0     v2     v3     v4
    s1     s2     s3

ia  ja
0   0 2
2   1 2
4   2 0 1 3
8   3 2
10

"""

import os

import flopy
import numpy as np
import pytest
from framework import TestFramework
from simulation import TestSimulation

ex = ["snf-disl01",]


def build_model(idx, dir):

    sim_ws = dir
    name = ex[idx]
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name="mf6", sim_ws=sim_ws,
        memory_print_option='all',
    )

    tdis = flopy.mf6.ModflowTdis(sim)
    ems = flopy.mf6.ModflowEms(sim)
    snf = flopy.mf6.ModflowSnf(sim, modelname=name, save_flows=True)

    vertices = [
        [0, 0., 0., 0.], 
        [1, 0., 1., 0.],
        [2, 1., 0., 0.],
        [3, 2., 0., 0.],
        [4, 3., 0., 0.],
    ]
    # icell1d fdc ncvert icvert
    cell2d = [
        [0, 0.5, 2, 1, 2],
        [1, 0.5, 2, 0, 2],
        [2, 0.5, 2, 2, 3],
        [3, 0.5, 2, 3, 4],
    ]

    nodes = len(cell2d)
    nvert = len(vertices)

    disl = flopy.mf6.ModflowSnfdisl(
        snf, 
        nodes=nodes, 
        nvert=nvert,
        segment_length=1000.0,
        tosegment=[2, 2, 3, -1],   # -1 gives 0 in one-based, which means outflow cell
        idomain=1, 
        vertices=vertices, 
        cell2d=cell2d,
    )
    
    # note: for specifying zero-based reach number, put reach number in tuple
    fname = f"{name}.mmr.obs.csv"
    mmr_obs = {
        fname: [
            ("OUTFLOW", "EXT-OUTFLOW", (nodes - 1,)),
        ],
        "digits": 10,
    }

    mmr = flopy.mf6.ModflowSnfmmr(
        snf, 
        observations=mmr_obs,
        print_flows=True,
        save_flows=True,
        iseg_order=list(range(nodes)),
        qoutflow0=0.0,
        k_coef=0.001, 
        x_coef=0.2
    )

    # output control
    oc = flopy.mf6.ModflowSnfoc(
        snf,
        budget_filerecord=f"{name}.bud",
        saverecord=[("BUDGET", "ALL"), ],
        printrecord=[("BUDGET", "ALL"), ],
    )

    flw_spd = {0: [[0, 1000.0], [1, 500.]]}
    flw = flopy.mf6.ModflowSnfflw(
        snf,
        print_input=True,
        print_flows=True,
        stress_period_data=flw_spd,
    )

    return sim, None


def eval_model(sim):
    print("evaluating model...")

    # read the observation output
    name = ex[sim.idxsim]
    fpth = os.path.join(sim.simpath, f"{name}.mmr.obs.csv")
    obsvals = np.genfromtxt(fpth, names=True, delimiter=",")

    return


@pytest.mark.parametrize(
    "idx, name",
    list(enumerate(ex)),
)
def test_mf6model(idx, name, function_tmpdir, targets):
    ws = str(function_tmpdir)
    test = TestFramework()
    test.build(build_model, idx, ws)
    test.run(
        TestSimulation(
            name=name, exe_dict=targets, exfunc=eval_model, idxsim=idx
        ),
        ws,
    )
