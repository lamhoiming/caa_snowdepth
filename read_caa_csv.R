# 2019-04-16
# Lam Hoi Ming
#read csv file of snow depth and ice thickness data from eccc stations

#set working directory
wdir <- "d:/phd/caa/data/station/csv/"
setwd(wdir)

idname <- read.csv(file = "_ID_NAME.csv", header = TRUE, sep = ",")
# List of stations in CAA with 2016 data
list_recent <- list("ALERT LT1", "ALERT YLT", "CAMBRIDGE BAY YCB", "CORAL HARBOUR YZS", "EUREKA WEU", "HALL BEACH YUX", "IQALUIT YFB", "RESOLUTE YRB")

i = 0
fdata <- vector("list")

# Read files with 2016 data
for (stn in list_recent)  {
    i = i +1
    n <- match(stn,idname[,2])
    fname <- paste(idname[n,1],".csv", sep = "") 
    message(i, " ", fname, " ", idname[n,2]) #Print out file to be read and its station name
    fdata[[i]] <- read.csv(fname, header = TRUE, sep = ",")
  }

