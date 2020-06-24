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


# Load data
data = pd.read_csv('C:/Users/moniy/Desktop/GelLab/Data/edge_W_g.comb_fiveVr_0.01_v2.csv')

### Plot on Canadain map
import plotly
plotly.tools.set_credentials_file(username='moniyuv', api_key='w2NBdksdtxziaW5GHpbG')
import pandas as pd
import plotly.plotly as py

nodes = pd.read_csv('C:/Users/moniy/Desktop/GelLab/Data/NodesforPaper.csv')
zipcodes = pd.read_csv("C:/Users/moniy/Desktop/GelLab/Data/CanadaZipCodes.csv")

## Join lat long data to tsne
Nodes = pd.merge(nodes,zipcodes,how = 'left', left_on = 'Nodes', right_on = 'Postal Code')

Edges = pd.read_csv('C:/Users/moniy/Desktop/GelLab/Data/EdgesforPaper.csv')

cities = [ dict(
        type = 'scattergeo',
        lat = Nodes['Latitude'],
        lon = Nodes['Longitude'],
        hoverinfo = 'text',
        text = Nodes['Postal Code'],
        mode = 'markers',
        marker = dict(
            size=2,
            color='rgb(255, 0, 0)',
            line = dict(
                width=3,
                color='rgba(68, 68, 68, 0)'
            )
        ))]

edges_all = []
for i in range( len( Edges)):
    edges_all.append(
        dict(
            type = 'scattergeo',
            lon = [ Edges['From.Longitude'][i], Edges['Longitude'][i] ],
            lat = [ Edges['From.Latitude'][i], Edges['Latitude'][i] ],
            mode = 'lines',
            line = dict(
                width = 1,
                color = 'red',
            )#,
            #opacity = float(df_flight_paths['cnt'][i])/float(df_flight_paths['cnt'].max()),
        )
    )

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
            type = 'conic conformal'#,
            #rotation = dict(
             #   lon = -100
            #)
        ),
        lonaxis = dict(
            showgrid = True,
            gridwidth = 0.25,
            range= [ -92.0, -80.0 ],
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

fig = dict( data= edges_all + cities, layout=layout )
py.iplot( fig, filename='Network_v4')