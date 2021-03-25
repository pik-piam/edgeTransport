import carculator_truck as ct
import numpy as np
import pandas as pd

cip = ct.TruckInputParameters()

cip.static()

_, array = ct.fill_xarray_from_input_parameters(cip)

array = array.interp(
    year=np.array([2020, 2030, 2040, 2050]),
    kwargs={'fill_value': 'extrapolate'})


mc = "Regional delivery"
# all countries have the same ttw values, using DE
cm = ct.TruckModel(array, cycle=mc, country="DE")
    
cm.set_all()

df = cm.array.sel(parameter="TtW energy").to_dataframe(name="ttw_energy").reset_index().drop(columns=["parameter", "value"])

df["unit"] = "kJ/km"

df.to_csv("{}_truck_efficiencies.csv".format(mc))
