# 2019-04-16
# Lam Hoi Ming
#read csv file of snow depth and ice thickness data from eccc stations

# install packages
require(graphics)
#require(zoo)
library(ggplot2)
library(Hmisc)
#library(xts)
library(magrittr)
library(hydroTSM)
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

# Define month sequence for plotting
#sep_jul <- c("S", "O", "N", "D", "J", "F", "M", "A", "M", "J", "J")
aug_jul = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul")
A_J <- c("A","S", "O", "N", "D", "J", "F", "M", "A", "M", "J", "J")

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
for (x in 1:8)
  {title <- paste(list_recent[x], collapse = '')
stn <- fdata[[x]]
stn.date <- as.Date(stn[,1])
#stn_snow <- zoo(stn[,3],stn.date)
stn_snow <- data.frame(date = stn.date, snow_depth = stn[,3])#, order.by = as.yearmon(stn.date))


# Average by month, separate years
#/# stn_mean_bymonth <- aggregate(stn_snow, format(time(stn_snow), "%m"), mean, na.rm = TRUE) %>%
#  coredata() # convert from zoo (time-ordered) to matrix/vector
stn_mean_bymonth <- monthlyfunction(stn_snow, FUN = mean, na.rm = TRUE) %>% data.frame()
stn_sd_bymonth <- monthlyfunction(stn_snow, FUN = sd, na.rm = TRUE) %>% data.frame()

if (length(stn_mean_bymonth) >= 11) {
stn_mean_bymonth_aug <- stn_mean_bymonth[aug_jul[2:12]] %>% as.numeric()
stn_sd_bymonth_aug <- stn_sd_bymonth[aug_jul[2:12]] %>% as.numeric()

} else if (length(stn_mean_bymonth) < 11) {
  paste(x, title, "Missing Sep!!") %>% print()
  print("set Aug and Sep to NA")
  stn_mean_bymonth_aug <- c('Sep' = Inf, stn_mean_bymonth[aug_jul[-(1:2)]]) %>% as.numeric()
  stn_sd_bymonth_aug <- c('Sep' = Inf, stn_sd_bymonth[aug_jul[-(1:2)]]) %>% as.numeric()
}
# Plot monthly averages
pdf(paste(title, '.pdf', collapse = ''))
png(paste(title, '.png', collapse = ''))
errbar(1:11, stn_mean_bymonth_aug, stn_mean_bymonth_aug + stn_sd_bymonth_aug, stn_mean_bymonth_aug - stn_sd_bymonth_aug,
       xaxt = "n",  xlab = "Month", ylab = "Snow depth (cm)", ylim = c(0,60), type = "o")
axis(side = 1, labels = A_J[2:12], at = 1:11)
title(main = title)
dev.off()
}



#stn_sd_bymonth <- aggregate(stn_snow, format(time(stn_snow), "%m"), sd, na.rm = TRUE) %>%
#  coredata() # convert from zoo (time-ordered) to matrix/vector

# Rearrange to start from Sep to July; ignore August
# # 
# tmp_mean_bymonth <- c(stn_mean_bymonth[-(1:7)],stn_mean_bymonth[1:7])
# tmp_sd_bymonth <- c(stn_sd_bymonth[-(1:7)],stn_sd_bymonth[1:7])
# 
# if (length(stn_mean_bymonth) == 12) {
#   stn_mean_bymonth_sep <- data.frame(month = sep_jul, snow_depth = tmp_mean_bymonth[2:12])
#   stn_sd_bymonth_sep <- data.frame(month = sep_jul, std_dev = tmp_sd_bymonth[2:12])
#   } else if () {
#   
# }
# rm(tmp_mean_bymonth)
# rm(tmp_sd_bymonth)

# Plot monthly average starting from August
#pdf("test.pdf")
# plot(stn_mean_bymonth_sep[,2], xaxt = "n")
# errbar(1:11, stn_mean_bymonth_sep[,2],stn_mean_bymonth_sep[,2] + stn_sd_bymonth_sep[,2], stn_mean_bymonth_sep[,2] - stn_sd_bymonth_sep[,2],
#        xaxt = "n",  xlab = "Month", ylab = "Snow depth (cm)", ylim = c(0,60), type ="o")
# axis(side = 1, labels = sep_jul[1:11], at = 1:11)
# title(main = title)
#dev.off()

#plot(stn_mean_bymonth_aug[1:12], xaxt = "n")

