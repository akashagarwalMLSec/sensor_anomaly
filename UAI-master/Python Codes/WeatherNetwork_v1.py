# -*- coding: utf-8 -*-
"""
Created on Mon May 14 13:07:05 2018

@author: moniy
"""
import xarray as xr
import numpy as np
import pandas as pd
from math import sin, cos, sqrt, atan2, radians
import math

ds = xr.open_dataset('C:\\Users\\moniy\\Desktop\\GelLab\\Data\\ontario_2001_2011_higherreso_12pmsteps.nc')
weekly_data = ds.resample(time='W').mean()

del ds

df = weekly_data.to_dataframe()
df.reset_index(inplace = True)

del weekly_data

## Convert longitude from 0 - 360 to -180 to 180
df['longitude_new'] = ((df['longitude']+180) % 360)-180
df.drop('longitude', axis=1, inplace=True)
df.rename(columns = {'longitude_new':'longitude'},inplace = True)

## Get unique lat and longitude values from the weather data
lat_long = df.groupby(['latitude','longitude']).size().reset_index().rename(columns={0:'count'})
lat_long.drop('count', axis=1, inplace=True)

## Get lat and long values from the insurance data
Nodes_insurance = pd.read_csv('C:\\Users\\moniy\\Desktop\\GelLab\\Data\\InsuranceNodes.csv')


## Cross join to find minimum distance
def df_crossjoin(df1, df2, **kwargs):
    df1['_tmpkey'] = 1
    df2['_tmpkey'] = 1

    res = pd.merge(df1, df2, on='_tmpkey', **kwargs).drop('_tmpkey', axis=1)
    res.index = pd.MultiIndex.from_product((df1.index, df2.index))

    df1.drop('_tmpkey', axis=1, inplace=True)
    df2.drop('_tmpkey', axis=1, inplace=True)

    return res

dfx = df_crossjoin(Nodes_insurance, lat_long)


del lat_long
del Nodes_insurance
del df



def haversine(p1, p2):
    R = 6371     # earth radius in km
    p1 = [math.radians(v) for v in p1]
    p2 = [math.radians(v) for v in p2]

    d_lat = p2[0] - p1[0]
    d_lng = p2[1] - p1[1]
    a = math.pow(math.sin(d_lat / 2), 2) + math.cos(p1[0]) * math.cos(p2[0]) * math.pow(math.sin(d_lng / 2), 2)
    c = 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))

    return R * c


dfx['dist'] = dfx.apply(lambda row: haversine((row['Latitude'], row['Longitude']), (row['latitude'], row['longitude'])),
                 axis=1)

min_distance = dfx.groupby(['Latitude','Longitude'])['dist'].min().reset_index()

required_coords = pd.merge(min_distance,dfx,  how='inner', on=['Latitude','Longitude','dist'])

del dfx

weather_data = pd.merge(required_coords,df,how = 'inner', on = ['latitude','longitude'])

del df

weather_data['total_precipitation']= weather_data['tp']*3600*24

weather_data.to_csv('C:\\Users\\moniy\\Desktop\\GelLab\\Data\\weather_data_weekly.csv', encoding='utf-8')

del min_distance
del required_coords
## Build the network

#cols_of_interest = ['latitude','longitude','time','total_precipitation']
cols = ['Postal Code','total_precipitation','time']
w_data = weather_data[cols]

#w_data = weather_data[cols_of_interest]
#w_data_noduprec = w_data.drop_duplicates()

network = w_data.pivot_table(values='total_precipitation', index='time', columns=['Postal Code'])
#network = w_data.pivot_table(values='total_precipitation', index='time', columns=['latitude','longitude'])


network.to_csv('C:\\Users\\moniy\\Desktop\\GelLab\\Data\\weather_network_wideformat_v3.csv', encoding='utf-8')
