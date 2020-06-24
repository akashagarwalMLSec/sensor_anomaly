##############################################################################################
#        Title: Using Google Maps API to pull elevation of Ontario FSA Codes
#                       Created on: Tuesday,Jul 18 2018
#                           Author: Monisha Yuvaraj
##############################################################################################

import json
import urllib.request
import pandas as pd
import numpy as np

def elevation(lat, lng):
    apikey = "AIzaSyCThdFxx7VrNHVc6lRJC54CV6BkcrCSOak"
    url = "https://maps.googleapis.com/maps/api/elevation/json"
    request = urllib.request.urlopen(url+"?locations="+str(lat)+","+str(lng)+"&key="+apikey)
    results = json.load(request).get('results')
    elevation = results[0].get('elevation')
    return elevation


if __name__ == "__main__":

   zipcodes = pd.read_csv("C:/Users/moniy/Desktop/GelLab/Data/CanadaZipCodes.csv")
   Elevation = []
   for i in range(0,len(zipcodes['Latitude'])):
      Elevation.append(elevation(zipcodes['Latitude'][i],zipcodes['Longitude'][i]))

   np.savetxt("C:/Users/moniy/Desktop/GelLab/Data/Elevations.csv",Elevation)