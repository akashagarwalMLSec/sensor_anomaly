# -*- coding: utf-8 -*-
"""
Created on Fri Jul  6 04:19:27 2018

@author: moniy
"""

import pandas as pd

import numpy as np
import networkx as nx
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import seaborn as sns
import plotly
plotly.tools.set_credentials_file(username='moniyuv', api_key='w2NBdksdtxziaW5GHpbG')
import pandas as pd
import plotly.plotly as py

# Load data
data = pd.read_csv('C:/Users/moniy/Desktop/GelLab/Data/edge_W_g.comb_fiveVr_0.01_v2.csv')

prelim = pd.read_csv('C:/Users/moniy/Desktop/GelLab/Climate Network/Codes/MATLAB Codes/plotprelim.csv')
prelim.rename(columns={prelim.columns[0]: "PostalCode" },inplace=True)

nodes = pd.read_csv('C:/Users/moniy/Desktop/GelLab/Data/NodesforPaper.csv')
zipcodes = pd.read_csv("C:/Users/moniy/Desktop/GelLab/Climate Network/Data/CanadaZipCodes.csv")

## Join lat long data to tsne
Nodes = pd.merge(prelim,zipcodes,how = 'left', left_on = 'PostalCode', right_on = 'Postal Code')
Nodes['Precip']= Nodes['colSums(precip_subset)']/520
#Edges = pd.read_csv('C:/Users/moniy/Desktop/GelLab/Data/EdgesforPaper.csv')

### Plot on Canadian map
scl = [ [0,"rgb(5, 10, 172)"],[0.35,"rgb(40, 60, 190)"],[0.5,"rgb(70, 100, 245)"],\
    [0.6,"rgb(90, 120, 245)"],[0.7,"rgb(106, 137, 247)"],[1,"rgb(220, 220, 220)"] ]

data = [ dict(
        type='scattergeo',
    lat = Nodes['Latitude'],
    lon = Nodes['Longitude'],
    text = Nodes['PostalCode'],
    marker = dict(
        color = Nodes['Precip'],
        colorscale='Hot',
        #reversescale=True,
        size = 7,
        colorbar=dict(
                title='Avg. Precipitation',
                lenmode='fraction',
                len=0.5
            ),
    ))]

layout = dict(
    geo = dict(
        scope = 'north america',
        showland = True,
        landcolor = "rgb(175, 175, 175)",
        subunitcolor = "rgb(0, 0, 0)",
        countrycolor = "rgb(0, 0, 0)",
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
py.iplot(fig,filename = "Precip")