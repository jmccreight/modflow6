import os

import flopy
import numpy as np


def test_disl_simple(function_tmpdir, targets):
    mf6 = targets["mf6"]
    name = "snf-disl01"
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name=mf6, sim_ws=str(function_tmpdir)
    )
    tdis = flopy.mf6.ModflowTdis(sim)
    ims = flopy.mf6.ModflowIms(sim, print_option="SUMMARY")
    snf = flopy.mf6.ModflowSnf(sim, modelname=name)

    vertices = [[0, .1, .1, 0.], [1, .5, .5, 0.]]
    # icell1d fdc ncvert icvert
    cell1d = [[0, 0.5, 2, 0, 1]]
    disl = flopy.mf6.ModflowSnfdisl(snf, nodes=1, nvert=2, 
        idomain=1, vertices=vertices, cell1d=cell1d)
    
    
    sim.write_simulation()
    success, buff = sim.run_simulation(silent=False)
    errmsg = f"model did not terminate successfully\n{buff}"
    assert success, errmsg

