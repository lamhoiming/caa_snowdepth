#!/usr/bin/env python
# coding: utf-8

# In[ ]:


"""
Translating read_caa_csv.R into Python
Lam Hoi Ming
02-07-2019

Data processing is done on the original csv file. Separated into files by station
"""
import os
import re
import pandas as pd
import matplotlib.pyplot as plt


def read_station():
    for i, station in enumerate(list_stn):
#         if idname.StationName.str.match(station).bool:
        ID = idname.index[idname.StationName == station].format()
        print(station)
        fdata[station] = pd.read_csv(ID[0] + '.csv', parse_dates = [0], infer_datetime_format=True, index_col = 'Date')


# READING SNOW DEPTH FROM EACH STATION
def read_snowdepth(data):
    for i, station in enumerate(list_stn):
        stn_title = station.replace(' ', '_')
        print(stn_title)
        stn_snow[station] = pd.DataFrame({'snow_depth':data[station]['Snowdepth']})

############ data input #################
#### set data input working directory: Where are your data?
Mdir = "d:/phd/caa/eccc/snowdepth/"   # Master/root dir
wdir = Mdir + "data/station/csv/" # specific working dir
os.chdir(wdir)



# Getting station ID
# Lat lon info are stored in idname
idname = pd.read_csv('ID_NAME.csv', encoding = "utf-8", sep = ',', index_col = 'StationID') # ISO-8859-1 for ASCII
idname.head()



# List of stations
list_stn = [
  # West of 100W, no 2016
  "CAPE PARRY ZUE", 
  "COPPERMINE YCO",
  "HOLMAN ISLAND YHI",
  "ISACHSEN (OLD ICE) IC1",   #no trend by month
  "ISACHSEN YIC",
  "LADY FRANKLIN POINT YUJ",
  "MOULD BAY YMD",
  "SACHS HARBOUR YSY", 
  # current stations up to 2016
  "ALERT LT1", 
    "ALERT YLT", 
    "CAMBRIDGE BAY YCB", 
    "CORAL HARBOUR YZS", 
    "EUREKA WEU", 
    "HALL BEACH YUX", 
    "IQALUIT YFB", 
    "RESOLUTE YRB", # with 2016 data
  # East of 100W, in hudson bay, no 2016
  "ARCTIC BAY YAB",
  "CAPE DORSET YTE",
  "CHESTERFIELD INLET YCS",
  "CHURCHILL YYQ",
  "CLYDE YCY",
  "GLADMAN POINT YUR",
  "INUKJUAK PH1",
  "INUKJUAK WPH",                 #no trend by month
  "IQALUIT YFB",              
  "KUUJJUAQ YVP",
  "KUUJJUARAPIK YGW",
  "MOOSONEE WZC",
  "POND INLET YIO",
  "QUAQTAQ HA1",
  "QUAQTAQ YHA",
  "SHEPHERD BAY YUS",              #no trend by month
  "SPENCE BAY YNC",              
  #inland stations for comparison
  "BAKER LAKE YBK",
  "INUVIK YEV"
  # Further southeast around Labrador sea/PEI?
]
print(list_stn)

#read all stations
fdata = {}
read_station()
# fdata['ALERT YLT']


# In[ ]:


# Month list from August
#sep_jul <- c("S", "O", "N", "D", "J", "F", "M", "A", "M", "J", "J")
aug_jul = ["Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul"]
A_J = ["A","S", "O", "N", "D", "J", "F", "M", "A", "M", "J", "J"]


#read snowdepth from each station
stn_snow = {}
read_snowdepth(fdata)



## After reading the snow depth vs date, now manipulate in different ways
#Function to plot time series for all stations
#def month_time_series(stn_data,save_directory):
    





