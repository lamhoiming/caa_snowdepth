# 2019-04-16
# Lam Hoi Ming
#read csv file of snow depth and ice thickness data from eccc stations

# install packages
require(graphics)
require(zoo)
library(ggplot2)
library(Hmisc)
library(xts)
library(magrittr)

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
# # Time series for Alert YLT
# ylt <- fdata[[2]]
# ylt.date <- as.Date(ylt[,1])
# ylt_snow <- zoo(ylt[,3],ylt.date)
# 
# # Average by month, separate years
# ylt_mean_bymonth <- aggregate(ylt_snow, format(time(ylt_snow), "%m"), mean, na.rm = TRUE)
# ylt_sd_bymonth <- aggregate(ylt_snow, format(time(ylt_snow), "%m"), sd, na.rm = TRUE)
# 
# # Rearrange to start from August to July
# ylt_mean_bymonth_aug <- zoo(ylt_mean_bymonth,c(6:12,1:5))
# ylt_sd_bymonth_aug <- zoo(ylt_sd_bymonth, c(6:12,1:5))                         
# aug_jul <- c("A", "S", "O", "N", "D", "J", "F", "M", "A", "M", "J", "J")
# 
# # Plot monthly average starting from August
# plot(ylt_mean_bymonth_aug, xaxt = "n")
# errbar(xaxt = "n",  xlab = "Month", ylab = "Snow depth (cm)", ylim = c(0,60), type ="o", 1:12,ylt_mean_bymonth_aug,ylt_mean_bymonth_aug + ylt_sd_bymonth_aug, ylt_mean_bymonth_aug - ylt_sd_bymonth_aug)
# axis(side = 1, labels = aug_jul[1:12], at = 1:12)


##############################################
##############################################
# Time series for any
x = 4
title <- paste(list_recent[x], collapse = '')
stn <- fdata[[x]]
stn.date <- as.Date(stn[,1])
stn_snow <- zoo(stn[,3],stn.date)
#stn_snow <- xts(stn[,3],stn.date)

sep_jul <- c("S", "O", "N", "D", "J", "F", "M", "A", "M", "J", "J")

# Average by month, separate years
stn_mean_bymonth <- aggregate(stn_snow, format(time(stn_snow), "%m"), mean, na.rm = TRUE) #%>%
  coredata() # convert from zoo (time-ordered) to matrix/vector
stn_sd_bymonth <- aggregate(stn_snow, format(time(stn_snow), "%m"), sd, na.rm = TRUE) %>%
  coredata() # convert from zoo (time-ordered) to matrix/vector

# Rearrange to start from Sep to July; ignore August
# # if length(stn_mean_bymonth) == 12

tmp_mean_bymonth <- c(stn_mean_bymonth[-(1:7)],stn_mean_bymonth[1:7])
stn_mean_bymonth_sep <- data.frame(month = sep_jul, snow_depth = tmp_mean_bymonth[2:12])
rm(tmp_mean_bymonth)

tmp_sd_bymonth <- c(stn_sd_bymonth[-(1:7)],stn_sd_bymonth[1:7])
stn_sd_bymonth_sep <- data.frame(month = sep_jul, snow_depth = tmp_sd_bymonth[2:12])
rm(tmp_sd_bymonth)

# Plot monthly average starting from August
#pdf("test.pdf")
plot(stn_mean_bymonth_sep[,2], xaxt = "n")
errbar(1:11, stn_mean_bymonth_sep[,2],stn_mean_bymonth_sep[,2] + stn_sd_bymonth_sep[,2], stn_mean_bymonth_sep[,2] - stn_sd_bymonth_sep[,2],
       xaxt = "n",  xlab = "Month", ylab = "Snow depth (cm)", ylim = c(0,60), type ="o")
axis(side = 1, labels = sep_jul[1:11], at = 1:11)
title(main = title)
#dev.off()