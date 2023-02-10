"""

Thomas routine example
500 miles
dx = 25 miles
sinusoidal inflow

"""

import os

import flopy
import numpy as np


def test_snf_thomas(function_tmpdir, targets):
    sim_ws = str(function_tmpdir)
    mf6 = targets["mf6"]
    name = "snf-thomas"
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name=mf6, sim_ws=sim_ws
    )
    nper = 10
    tdis_rc = [(1., 1, 1.0) for ispd in range(nper)]
    tdis = flopy.mf6.ModflowTdis(sim, nper=nper, perioddata=tdis_rc)
    ems = flopy.mf6.ModflowEms(sim)
    snf = flopy.mf6.ModflowSnf(sim, modelname=name)

    vertices = None
    cell2d = None
    nvert = None

    nodes = 20
    channel_length = 500. # miles
    segment_length = channel_length * 5280. / nodes
    tosegment = list(range(1, nodes))
    tosegment.append(-1)

    disl = flopy.mf6.ModflowSnfdisl(
        snf, 
        nodes=nodes, 
        nvert=nvert,
        segment_length=segment_length,
        tosegment=tosegment,   # -1 gives 0 in one-based, which means outflow cell
        idomain=1, 
        vertices=vertices, 
        cell2d=cell2d,
    )
    
    mmr = flopy.mf6.ModflowSnfmmr(
        snf, 
        print_flows=True,
        iseg_order=list(range(nodes)),
        mann_n=0.0297, 
        seg_depth=100., 
        seg_slope=1./5280., 
        x_coef=0.2
    )

    inflow = [125, 200, 125, 50, 50, 50, 50, 50, 50, 50]
    flw_spd = {ispd: [[0, inflow[ispd]]] for ispd in range(10)}
    flw = flopy.mf6.ModflowSnfflw(
        snf,
        print_input=True,
        print_flows=True,
        stress_period_data=flw_spd,
    )

    sim.write_simulation()

    mfsimnamtxt = f"""BEGIN options
END options

BEGIN timing
  TDIS6  {name}.tdis
END timing

BEGIN models
  snf6  {name}.nam  {name}
END models

BEGIN exchanges
END exchanges

BEGIN SOLUTIONGROUP 1
  EMS6 {name}.ems {name}
END SOLUTIONGROUP
"""
    fname = os.path.join(sim_ws, 'mfsim.nam')
    with open(fname, 'w') as f:
        f.write(mfsimnamtxt)


    success, buff = sim.run_simulation(silent=False)
    errmsg = f"model did not terminate successfully\n{buff}"
    assert success, errmsg

