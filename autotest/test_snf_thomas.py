"""

http://uon.sdsu.edu/the_thomas_problem_with_online_computation.html
Thomas routine example
500 miles
dx = 25 miles
sinusoidal inflow

"""

import os

import flopy
import numpy as np


def inflow_hydrograph(qpi, qb, time_base, times):
    """
    Generate an inflow hydrograph for the Thomas problem based on
    a sine wave.  Function assumes all input is in seconds.

    Parameters
    ----------
    qpi : float
        peak inflow in cfs/ft
    qpb : float
        base flow rate in cfs/ft
    time_base : float
        hydrograph time base, in seconds.  This is the
        time it takes for the inflow to return to the
        base rate of qb.
    times : ndarray
        times, in seconds, to calculate the inflow
    
    Returns
    ----------
    q_inflow : ndarray
        array inflow rates, in cfs/ft.  Has same size as input
        argument, times.

    """
    amplitude = (qpi - qb) / 2
    q_inflow = amplitude + qb + amplitude * np.sin( (2 * np.pi / time_base) * times  - .5 * np.pi)
    idx = times > time_base
    q_inflow[idx] = qb
    return q_inflow


def test_snf_thomas(function_tmpdir, targets):
    sim_ws = str(function_tmpdir)
    mf6 = targets["mf6"]
    name = "snf-thomas"
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name=mf6, sim_ws=sim_ws
    )
    nper = 20
    tdis_rc = [(1., 1, 1.0) for ispd in range(nper)]
    tdis = flopy.mf6.ModflowTdis(sim, nper=nper, perioddata=tdis_rc)
    ems = flopy.mf6.ModflowEms(sim)
    snf = flopy.mf6.ModflowSnf(sim, modelname=name)

    vertices = None
    cell2d = None
    nvert = None

    nodes = 20
    channel_length = 500. # miles
    channel_length = channel_length * 5280. / 3.2808 # meters
    segment_length = channel_length * 5280. / nodes
    tosegment = list(range(1, nodes))
    tosegment.append(-1)

    qpi = 200.
    qb = 50.
    hour_to_second = 60. * 60.
    days_to_second = 24. * 60. * 60.
    times = np.arange((nper + 1) * days_to_second, step=1 * days_to_second)
    q_inflow = inflow_hydrograph(200., 50., 192 * hour_to_second, times)

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
        qoutflow0=qb,
        mann_n=0.0297, 
        seg_depth=22.675, 
        seg_slope=1./5280., 
        x_coef=0.2
    )

    inflow = q_inflow[1:]
    flw_spd = {ispd: [[0, inflow[ispd]]] for ispd in range(nper)}
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

