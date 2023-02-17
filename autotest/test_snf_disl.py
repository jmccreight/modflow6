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


def test_disl_simple(function_tmpdir, targets):
    sim_ws = str(function_tmpdir)
    mf6 = targets["mf6"]
    name = "snf-disl01"
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name=mf6, sim_ws=sim_ws,
    )
    sim.name_file.memory_print_option = "all"
    tdis = flopy.mf6.ModflowTdis(sim)
    ems = flopy.mf6.ModflowEms(sim)
    snf = flopy.mf6.ModflowSnf(sim, modelname=name)

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
    
    mmr = flopy.mf6.ModflowSnfmmr(
        snf, 
        print_flows=True,
        iseg_order=list(range(nodes)),
        qoutflow0=0.,
        mann_n=0.04, 
        seg_depth=100., 
        seg_slope=0.0001, 
        x_coef=0.2
    )

    flw_spd = {0: [[0, 1000.0], [1, 500.]]}
    flw = flopy.mf6.ModflowSnfflw(
        snf,
        print_input=True,
        print_flows=True,
        stress_period_data=flw_spd,
    )

    sim.write_simulation()

    mfsimnamtxt = f"""BEGIN options
  MEMORY_PRINT_OPTION ALL
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

