# -*- coding: utf-8 -*-
"""
Created on Sun Feb 17 23:39:57 2019

@author: moniy
"""
import pandas as pd
from sklearn import cluster
from sklearn import metrics
import matplotlib.pyplot as plt
import plotly
plotly.tools.set_credentials_file(username='moniyuv', api_key='w2NBdksdtxziaW5GHpbG')
import plotly.plotly as py

Embeds = pd.read_csv("C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\TDAClusters.csv")
zipcodes = pd.read_csv("C:/Users/moniy/Desktop/GelLab/Climate Network/Data/CanadaZipCodes.csv")
latlong_aff = pd.merge(Embeds,zipcodes,how = 'left', left_on = 'PostalCode', right_on = 'Postal Code')


ClusterSize = Embeds.groupby(['ClusterNumber']).count()
ClusterSize[ClusterSize['PostalCode']>10]

Cluster = latlong_aff[latlong_aff['ClusterNumber'] == 1]
Cluster = latlong_aff[latlong_aff['ClusterNumber'] == 5]
Cluster = latlong_aff[latlong_aff['ClusterNumber'] == 6]
Cluster = latlong_aff[latlong_aff['ClusterNumber'] == 7]
Cluster = latlong_aff[latlong_aff['ClusterNumber'] == 8]

clusts = [1,5,6,7,8]
clusts_6 = [1,5,7,8]

Anomaly = latlong_aff[~latlong_aff['ClusterNumber'].isin(clusts)]
Clusters_10 = latlong_aff[latlong_aff['ClusterNumber'].isin(clusts_6)]

data = [ dict(
    lat = Clusters_10['Latitude'],
    lon = Clusters_10['Longitude'],
    text = Clusters_10['PostalCode'],
    marker = dict(
        color = Clusters_10['ClusterNumber'],
        colorscale='Jet',
        size = 7
    ),
    type = 'scattergeo'
)]

layout = dict(
    geo = dict(
        scope = 'north america',
        showland = True,
        landcolor = "rgb(200, 200, 200)",
        subunitcolor = "rgb(255, 255, 255)",
        countrycolor = "rgb(255, 255, 255)",
        showlakes = True,
        lakecolor = "rgb(135, 206, 250)",
        showsubunits = True,
        showcountries = True,
        resolution = 50,
        projection = dict(
            type = 'conic conformal'
        ),
        lonaxis = dict(
            showgrid = True,
            gridwidth = 0.25,
            range= [ -95.0, -80.0 ],
            dtick = 5
        ),
        lataxis = dict (
            showgrid = True,
            gridwidth = 0.5,
            range= [ 40.0, 50.0 ],
            dtick = 5
        )
    ),

)
fig = { 'data':data, 'layout':layout }
py.iplot(fig,filename = "TDAAllexceptbig")


