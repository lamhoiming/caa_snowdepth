# 2019-04-16
# Lam Hoi Ming
#read csv file of snow depth and ice thickness data from eccc stations

# install packages
require(graphics)
require(zoo)


#### set working directory
wdir <- "d:/phd/caa/data/station/csv/"
setwd(wdir)

#### Reading data
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


#### Analysis
# Time series for Alert YLT
ylt <- fdata[[2]]
ylt.date <- as.Date(ylt[,1])
ylt_snow <- zoo(ylt[,3],ylt.date)

# Average by month, separate years
ylt_mean_bymonth <- aggregate(ylt_snow, format(time(ylt_snow), "%m"), mean, na.rm = TRUE)
ylt_sd_bymonth <- aggregate(ylt_snow, format(time(ylt_snow), "%m"), sd, na.rm = TRUE)

# Rearrange to start from August to July
ylt_mean_bymonth_aug <- zoo(ylt_mean_bymonth,c(6:12,1:5))
                         
aug_jul <- as.Date(c("A", "S", "O", "N", "D", "J", "F", "M", "A", "M", "J", "J"), "%b")