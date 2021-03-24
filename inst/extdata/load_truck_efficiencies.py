import carculator_truck as ct
import numpy as np
import pandas as pd

cip = ct.TruckInputParameters()

cip.static()

_, array = ct.fill_xarray_from_input_parameters(cip)

array = array.interp(
    year=np.array([2020, 2030, 2040, 2050]),
    kwargs={'fill_value': 'extrapolate'})


rg2ctr = {
    'OAS': 'TH',
    'ENC': 'DK',
    'NES': 'TR',
    'MEA': 'EG',
    'SSA': 'KE',
    'LAM': 'BR',
    'REF': 'RU',
    'CAZ': 'CA',
    'EWN': 'NL',
    'ECS': 'HR',
    'CHA': 'CH',
    'ESC': 'IT',
    'ECE': 'PL',
    'FRA': 'FR',
    'DEU': 'DE',
    'UKI': 'UK',
    'NEN': 'NO',
    'IND': 'IN',
    'JPN': 'JP',
    'ESW': 'ES',
    'USA': 'US'
}

tab = []
for rec, ccode in rg2ctr.items():
    mc = "Regional delivery"
    cm = ct.TruckModel(array, cycle=mc, country="TH")
    
    cm.set_all()

    df = cm.array.sel(parameter="TtW efficiency").to_dataframe(name="efficiency").reset_index().drop(columns=["parameter", "value"])

    df["region"] = rec
    tab.append(df)

full = pd.concat(tab)
full.to_csv("{}_truck_efficiencies.csv".format(mc))
