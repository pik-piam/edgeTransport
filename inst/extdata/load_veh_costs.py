import carculator_truck as ct
import carculator as cc
import numpy as np
import pandas as pd

cip = ct.TruckInputParameters()

cip.static()

_, array = ct.fill_xarray_from_input_parameters(cip)

array = array.interp(
    year=np.array([2020, 2030, 2040, 2050]),
    kwargs={'fill_value': 'extrapolate'})

cm = ct.TruckModel(array, cycle="Regional delivery" country="DE")

cm.set_all()

df = cm.array.sel(parameter="purchase cost").to_dataframe(name="purchase_cost").reset_index().drop(columns=["parameter", "value"])

df["unit"] = "€/veh."

df.to_csv("truck_costs.csv")


cip = cc.CarInputParameters()

cip.static()

_, array = cc.fill_xarray_from_input_parameters(cip)

array = array.interp(
    year=np.array([2020, 2030, 2040, 2050]),
    kwargs={'fill_value': 'extrapolate'})

cm = cc.CarModel(array)

cm.set_all()

df = cm.array.sel(parameter="purchase cost").to_dataframe(name="purchase_cost").reset_index().drop(columns=["parameter", "value"])

df["unit"] = "€/veh."

df.to_csv("car_costs.csv")
