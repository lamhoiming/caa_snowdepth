#!/usr/bin/env python
# coding: utf-8
"""
Translating read_caa_csv.R into Python
Lam Hoi Ming
02-07-2019

Data processing is done on the original csv file. Separated into files by station
"""
import os
import pandas as pd
#import matplotlib.pyplot as plt

def read_station():
    for i, station in enumerate(list_station):
#         if idname.StationName.str.match(station).bool:
        ID = idname.index[idname.StationName == station].format()
        print(station)
        eccc_data[station] = pd.read_csv(ID[0] + '.csv', parse_dates = [0], infer_datetime_format=True, index_col = 'Date')

# station titles without space
def print_station_title():
    station_title = {station:station.replace(' ', '_') for i, station in enumerate(list_station)}
    return station_title

# READING SNOW DEPTH FROM EACH STATION
def read_snowdepth(data):
    for i, station in enumerate(list_station):
        station_snowdepth[station] = pd.DataFrame({'snow_depth':data[station]['Snowdepth'], 'ice_thickness':data[station]['Icethickness']})
#        station_snowdepth[station] = pd.DataFrame({'date':data[station]['Date'], 'snow_depth':data[station]['Snowdepth'], 'ice_thickness':data[station]['Icethickness']})
#   ***If not already datetime object, add this line:
#        station_snowdepth[station].index = station_snowdepth[station].index.strftime('%Y/%m/%d')
        station_snowdepth[station].index = pd.to_datetime(station_snowdepth[station].index, format="%Y%m%d")
#        station_snowdepth[station].set_index(station_snowdepth[station]['date'], inplace=True)
    return station_snowdepth

# Calculate month average
def average_snowdepth_by_month(data):
    for i, station in enumerate(list_station):
        station_snowdepth_monthly_mean[station] = data[station]['snow_depth'].resample('M').mean()
        station_snowdepth_monthly_std[station] = data[station]['snow_depth'].resample('M').std()
        station_snowdepth_monthly_mean[station].index = pd.to_datetime(station_snowdepth_monthly_mean[station].index, format="%Y%m")
        
    return station_snowdepth_monthly_mean

## Plot monthly time series
#def plot_month_time_series(data,save_directory):
#    os.chdir(save_directory)
#    for i, station in enumerate(list_station):
#        print(i)
#        plt.figure(i)
#        data[station].plot(style='-')
#        plt.xlabel('Year')
#        plt.ylabel('Snow depth (cm)')
#        plt.savefig(station_title[station]+'_month_time_series.pdf', dpi = 300, transparent = True)
#        plt.close(i)
##################

############ data input #################
#### set data input working directory: Where are your data?
Mdir = "d:/phd/caa/eccc/snowdepth/"   # Master/root dir
wdir = Mdir + "data/station/csv/" # specific working dir
os.chdir(wdir)


# Getting station ID
# Lat lon info are stored in idname
idname = pd.read_csv('ID_NAME.csv', encoding = "utf-8", sep = ',', index_col = 'StationID') # ISO-8859-1 for ASCII

# List of stations
list_station = [
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


# In[ ]:

station_title = print_station_title()

station_location = pd.DataFrame({'latitude':idname.latitude, 'longitude':idname.longitude})
station_location.to_csv('station_location.csv')
# In[ ]:


eccc_data = {}
read_station()


# In[ ]:


dir(eccc_data)
eccc_data.keys()


# In[ ]:


# Month list from August
#sep_jul <- c("S", "O", "N", "D", "J", "F", "M", "A", "M", "J", "J")
aug_jul = ["Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul"]
A_J = ["A","S", "O", "N", "D", "J", "F", "M", "A", "M", "J", "J"]


# In[ ]:


# READING SNOW DEPTH FROM EACH STATION



# In[ ]:

station_snowdepth = {}
read_snowdepth(eccc_data)


station_snowdepth_monthly_mean = {}
station_snowdepth_monthly_std = {}
average_snowdepth_by_month(station_snowdepth)


#plot_month_time_series(station_snowdepth_monthly_mean, Mdir+"output/ts_mth_avg/plots/")
## After reading the snow depth vs date, now manipulate in different ways
#Function to plot monthly time series for all stations


#

## In[ ]:
#
#
#svdir = Mdir + 'output/ts_mth_avg/plots/'
#month_time_series(,svdir)


# In[ ]:




