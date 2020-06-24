#!/usr/bin/env python
from ecmwfapi import ECMWFDataServer
server = ECMWFDataServer()
server.retrieve({
    "class": "ei",
    "dataset": "interim",
    "date": "2015-01-01/to/2015-01-31",
    "expver": "1",
    "grid": "0.1/0.1",
    "levtype": "sfc",
    "param": "228.128",
    "step": "3/6/9/12",
    "stream": "oper",
    "time": "12:00:00",
    "area": "62/-100/42/-50", #N/S/W/E format of lattiude and longitude
    "type": "fc",
    "format": "netcdf",
    "target": "ontario_2015_jan.nc",
})



