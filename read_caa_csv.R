# 2019-04-16
# Lam Hoi Ming
#read csv file of snow depth and ice thickness data from eccc stations

# install packages
require(graphics)
#require(zoo)
library(ggplot2)
library(Hmisc) # for errbar()
library(xts)
library(magrittr) #for %>%
library(hydroTSM) #for monthlyfunction()
library(stringr)
library(dplyr)
library(lubridate)
#### set working directory
wdir <- "d:/phd/caa/data/station/csv/"
setwd(wdir)

#### Reading data
idname <- read.csv(file = "_ID_NAME.csv", header = TRUE, sep = ",")

# List of stations in CAA with 2016 data
list_stn <- list("CAPE PARRY ZUE", 
                 "COPPERMINE YCO",
                 "HOLMAN ISLAND YHI",
                 "ISACHSEN (OLD ICE) IC1",
                 "ISACHSEN YIC",
                 "LADY FRANKLIN POINT YUJ",
                 "MOULD BAY YMD",
                 "SACHS HARBOUR YSY", # West of 100W, no 2016
                 ####
                 "ALERT LT1", "ALERT YLT", "CAMBRIDGE BAY YCB", "CORAL HARBOUR YZS", "EUREKA WEU", "HALL BEACH YUX", "IQALUIT YFB", "RESOLUTE YRB", # with 2016 data
                 ####
                "ARCTIC BAY YAB",
"CAPE DORSET YTE",
"CHESTERFIELD INLET YCS",
                 "CHURCHILL YYQ",
                 "CLYDE YCY",
                 "GLADMAN POINT YUR",
                 "INUKJUAK PH1",
                 "INUKJUAK WPH",
                 "IQALUIT YFB",
                 "KUUJJUAQ YVP",
                 "KUUJJUARAPIK YGW",
                 "MOOSONEE WZC",
                 "POND INLET YIO",
                 "QUAQTAQ HA1",
                 "QUAQTAQ YHA",
                 "SHEPHERD BAY YUS",
                 "SPENCE BAY YNC" # East of 100W, in hudson bay, no 2016
)

i = 0
fdata <- vector("list")

# Read files 
for (s in list_stn)  {
    i = i +1
    n <- match(s,idname[,2]) 
    fname <- paste(idname[n,1],".csv", sep = "")
    message(i, " ", fname, " ", idname[n,2]) #Print out file to be read and its station name
    fdata[[i]] <- read.csv(fname, header = TRUE, sep = ",")
  }

# Define month sequence for plotting
#sep_jul <- c("S", "O", "N", "D", "J", "F", "M", "A", "M", "J", "J")
aug_jul = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul")
A_J <- c("A","S", "O", "N", "D", "J", "F", "M", "A", "M", "J", "J")


# construct Time series for all stations
for (x in 1:length(list_stn)) {
  stn_title <- gsub('([[:punct:]])|\\s+', '_', list_stn[x])
stn <- fdata[[x]]
stn.date <- as.Date(stn[,1]) #, format = "%Y-%m-%d")
#stn_snow <- zoo(stn[,3],stn.date)
stn_snow <- data.frame(date = stn.date, snow_depth = stn[,3])#, order.by = as.yearmon(stn.date))


# Average by month, separate years
#/# stn_mean_bymonth <- aggregate(stn_snow, format(time(stn_snow), "%m"), mean, na.rm = TRUE) %>%
#  coredata() # convert from zoo (time-ordered) to matrix/vector
stn_mean_bymonth <- monthlyfunction(stn_snow, FUN = mean, na.rm = TRUE) %>% data.frame()
stn_sd_bymonth <- monthlyfunction(stn_snow, FUN = sd, na.rm = TRUE) %>% data.frame()

if (all(aug_jul %in% colnames(stn_mean_bymonth)) == TRUE ) {
stn_mean_bymonth_aug <- stn_mean_bymonth[aug_jul[2:12]] %>% as.numeric()
stn_sd_bymonth_aug <- stn_sd_bymonth[aug_jul[2:12]] %>% as.numeric()

} else {
  missing <- setdiff(aug_jul, names(stn_mean_bymonth))
  # paste(x, stn_title, "Missing Sep!!") %>% print()
  # print("set Aug and Sep to NA")
  stn_mean_bymonth[missing] <- Inf
  stn_sd_bymonth[missing] <- Inf
  stn_mean_bymonth_aug <- stn_mean_bymonth[aug_jul[2:12]] %>% as.numeric()
  stn_sd_bymonth_aug <- stn_sd_bymonth[aug_jul[2:12]] %>% as.numeric()
} 
#########
# else if (length(stn_mean_bymonth) < 10) {
#   paste("!!!", x, stn_title, "Too few month, check") %>% print()
#   next()
# }

# Plot monthly averages
# svdir <- "d:/phd/caa/output/graphs/monthly_series"
# setwd(svdir)
# 
# pdf(paste(stn_title, '.pdf', sep = ''))
# png(paste(stn_title, '.png', sep = ''))
# errbar(1:11, stn_mean_bymonth_aug, stn_mean_bymonth_aug + stn_sd_bymonth_aug, stn_mean_bymonth_aug - stn_sd_bymonth_aug,
#        xaxt = "n",  xlab = "Month", ylab = "Snow depth (cm)", ylim = c(0,60), type = "o")
# axis(side = 1, labels = A_J[2:12], at = 1:11)
# title(main = stn_title)
# dev.off()
# dev.off()
########
# plot entire ts
svdir <- "d:/phd/caa/output/graphs/whole_ts"
setwd(svdir)
pdf(paste(stn_title, '_all_ts.pdf', sep = ''))
png(paste(stn_title, '_all_ts.png', sep = ''))
plot(stn_snow,  xlab = "Year", ylab = "Snow depth (cm)", ylim = c(0,100), type = "o")
title(main = stn_title)
dev.off()
dev.off()


}


########################
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
# title(main = stn_title)
#dev.off()

#plot(stn_mean_bymonth_aug[1:12], xaxt = "n")

