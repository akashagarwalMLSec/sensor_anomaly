# -*- coding: utf-8 -*-
"""
Created on Fri May  4 00:20:10 2018

@author: moniy
"""

import csv
import copy
import json


data_file = "C:\\Users\\moniy\\Desktop\\GelLab\\Data\\EdgesWeather.csv"

map_data = []

# base JS format that Google Maps API uses
#
# var
# flightPlanCoordinates = [
#     {lat: 45.1953, lng: -76.1496},
#     {lat: 45.4131, lng: -74.9148}
# ];
# var
# flightPath = new
# google.maps.Polyline({
#     path: flightPlanCoordinates,
#     geodesic: true,
#     strokeColor: '#FF0000',
#     strokeOpacity: 1.0,
#     strokeWeight: 2
# });
base_line = {
    'path': [],
    'geodesic': True,
    'strokeColor': '#FF0000',
    'strokeOpacity': 1.0,
    'strokeWeight': 0.5,
}

count = 0
first_row = True

with open(data_file) as csvfile:
    mapreader = csv.reader(csvfile)

    for row in mapreader:
        # skip first row
        if first_row:
            first_row = False
            continue

        curr_row = copy.copy(base_line)
        curr_row['path'] = [
               {'lat': float(row[3]), 'lng': float(row[4])},
               {'lat': float(row[5]), 'lng': float(row[6])}
        ]
        map_data.append(curr_row)

f = open('converted_weather.json', 'w')
f.write(json.dumps(map_data))
f.close()