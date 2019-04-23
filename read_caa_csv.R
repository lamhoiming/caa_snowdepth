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
Mdir <- "d:/phd/caa/"
wdir <- paste(Mdir, "data/station/csv/", sep = "")
setwd(wdir)

#### Reading data
idname <- read.csv(file = "_ID_NAME.csv", header = TRUE, sep = ",")

# List of stations in CAA with 2016 data
list_stn <- list(
  # West of 100W, no 2016
                "CAPE PARRY ZUE", 
                 "COPPERMINE YCO",
                 "HOLMAN ISLAND YHI",
                 "ISACHSEN (OLD ICE) IC1",
                 "ISACHSEN YIC",
                 "LADY FRANKLIN POINT YUJ",
                 "MOULD BAY YMD",
                 "SACHS HARBOUR YSY", 
# current stations up to 2016
                 "ALERT LT1", "ALERT YLT", "CAMBRIDGE BAY YCB", "CORAL HARBOUR YZS", "EUREKA WEU", "HALL BEACH YUX", "IQALUIT YFB", "RESOLUTE YRB", # with 2016 data
# East of 100W, in hudson bay, no 2016
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
                 "SPENCE BAY YNC", 
#inland stations for comparison
                  "BAKER LAKE YBK",
                  "INUVIK YEV"
# Further southeast around Labrador sea/PEI?
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

####################
###### Switch ######
####################

do.monthtrend == TRUE
do.monthts == TRUE
do.monthseries == TRUE
do.entirets == TRUE




####################



# construct Time series for all stations
for (x in 1:length(list_stn)) {
  stn_title <- gsub('([[:punct:]])|\\s+', '_', list_stn[x])
stn <- fdata[[x]]
stn.date <- as.Date(stn[,1]) #, format = "%Y-%m-%d")
stn_snow <- data.frame(date = stn.date, snow_depth = stn[,3])#, order.by = as.yearmon(stn.date))

# ## Average by month in each year

if (do.monthtrend == TRUE) {
xts.stn_snow <- xts(stn[,3],stn.date)
stn_mean_byyearmonth <- apply.monthly(xts.stn_snow, mean)
stn_sd_byyearmonth <- apply.monthly(xts.stn_snow, sd)

index(stn_mean_byyearmonth) <- as.yearmon(index(stn_mean_byyearmonth))
index(stn_sd_byyearmonth) <- as.yearmon(index(stn_sd_byyearmonth))

#cycle(stn_mean_byyearmonth)


### Montly trend

# cnames <- unique(format(index(stn_mean_byyearmonth), "%Y"))
# df <- data.frame(matrix(nrow = 12, ncol = length(cnames)))
# colnames(df) <- cnames
# rownames(df) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
#                   "Aug", "Sep", "Oct", "Nov", "Dec")
rnames <- toupper(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                                     "Aug", "Sep", "Oct", "Nov", "Dec"))

svdir <- paste(Mdir, "output/graphs/trend/monthly/", sep = "")
setwd(svdir)
for (mth in 1:12) {  #monthly trend
  if (any(cycle(stn_mean_byyearmonth) == mth)){
    
    dfmthmean <- subset(stn_mean_byyearmonth, cycle(stn_mean_byyearmonth) == mth)
    dfmthsd <- subset(stn_sd_byyearmonth, cycle(stn_sd_byyearmonth) == mth)
    
    dfmth <- cbind("mean" = dfmthmean,"sd" = dfmthsd)
    write.csv(dfmth, file = paste(stn_title, '_', rnames[mth], '.csv', sep =''), row.names = format(index(dfmth), "%Y"), na = "NA")
    
    trd <- read.csv(file = paste(stn_title, '_', rnames[mth], '.csv', sep =''), col.names = c("yr", "mean", "sd"))
    lm.trd <- lm(trd$mean~trd$yr, na.action = 'na.omit')
    
    # stats.trd <- summary(lm.trd)
    # #if (is.na(coef(stats.trd))
    # slp_trd <- coef(stats.trd)['trd$yr','Estimate'] #getting slope
    # std.err_trd <- coef(stats.trd)['trd$yr','Std. Error'] #getting std error of regression
    # r.sq_trd <- stats.trd$adj.r.squared
    # # 
    # lgd.trd <- c(paste("Slope =", round(slp_trd, digit = 2), "±",
    #                     round(std.err_trd, digits = 2)), paste("R-square =", round(r.sq_trd, digits = 3)))
    # # 
    svdir <- paste(Mdir, "output/graphs/trend/monthly/", sep = "")
    setwd(svdir)
    pdf(paste(stn_title, '_', rnames[mth], '_month_trend.pdf', sep = ''))
    png(paste(stn_title, '_', rnames[mth], '_month_trend.png', sep = ''), width=800, height=567, units="px")
    
    errbar(trd$yr, trd$mean, trd$mean + trd$sd, trd$mean - trd$sd, ylim = c(0,100), las = 1, type = "o", xlab = "Year", ylab = "Snow depth (cm)")
    title(main = paste(stn_title, rnames[mth], sep = " "))
    #legend("topleft", legend = lgd.trd, bty = "n")
    #abline(lm.trd, col = "red")
    
    dev.off()
    dev.off()
} else {
    next
    }
    
  # plot(dec, main = stn_title, type = "o", ylim = c(-1,100), 
  #           grid.col = NA, yaxis.right = FALSE, axis = 2, las = 2, mar = c(6,4,4,2)) %>% print()
 

  }
}
#dev.off()
#dev.off()


#### Monthly ts
if (do.monthts == TRUE) {
svdir <- paste(Mdir, "output/graphs/whole_ts/monthly/", sep = "")
setwd(svdir)
pdf(paste(stn_title, '_month_ts.pdf', sep = ''))
png(paste(stn_title, '_month_ts.png', sep = ''), width=800, height=567, units="px")
plot(stn_mean_byyearmonth, main = stn_title, type = "l", ylim = c(-1,100), 
                    grid.col = NA, yaxis.right = FALSE, axis = 2, las = 2, mar = c(6,4,4,2)) %>% print()
# tcks = c (0, 20, 40, 60, 80)
# axis(side = 2, labels = tcks, at = tcks)
dev.off()
dev.off()
}


if (do.monthseries == TRUE) {
# Average by month from all years
#/# stn_mean_bymonth <- aggregate(stn_snow, format(time(stn_snow), "%m"), mean, na.rm = TRUE) %>%
#  coredata() # convert from zoo (time-ordered) to matrix/vector
stn_mean_bymonth <- monthlyfunction(stn_snow, FUN = mean, na.rm = TRUE) %>% data.frame()
stn_sd_bymonth <- monthlyfunction(stn_snow, FUN = sd, na.rm = TRUE) %>% data.frame()

# if (all(aug_jul %in% colnames(stn_mean_bymonth)) == TRUE ) {
# stn_mean_bymonth_aug <- stn_mean_bymonth[aug_jul[2:12]] %>% as.numeric()
# stn_sd_bymonth_aug <- stn_sd_bymonth[aug_jul[2:12]] %>% as.numeric()
# 
# } else {
  missing <- setdiff(aug_jul, names(stn_mean_bymonth))
  # paste(x, stn_title, "Missing Sep!!") %>% print()
  # print("set Aug and Sep to NA")
  stn_mean_bymonth[missing] <- NA
  stn_sd_bymonth[missing] <- NA
  stn_mean_bymonth_aug <- stn_mean_bymonth[aug_jul] %>% as.numeric()
  stn_sd_bymonth_aug <- stn_sd_bymonth[aug_jul] %>% as.numeric()
#} 

# Linear regression for accumulation season
lm.acc <- lm(stn_mean_bymonth_aug[2:10]~c(1:9), na.action = 'na.omit') #Linear regression for Sep to May
tbl.snw <- data.frame('month'= aug_jul,'snowdepth'= stn_mean_bymonth_aug, 'sd' = stn_sd_bymonth_aug)
stats.snw <- summary(lm.acc)
slp_sep_may <- coef(stats.snw)['c(1:9)','Estimate'] #getting slope
std.err_sep_may <- coef(stats.snw)['c(1:9)','Std. Error'] #getting std error of regression
r.sq <- stats.snw$adj.r.squared

svdir <- paste(Mdir, "output/tables/snowdepth/", sep = "")
setwd(svdir)
write.csv(tbl.snw, file = paste(stn_title, '.csv', sep =''), na = "NA")

svdir <- paste(Mdir, "output/tables/stats/", sep = "")
setwd(svdir)
capture.output(stats.snw, file = paste(stn_title, '_stats.txt', sep =''))

#############
##Plot monthly averages seasonal trend
svdir <- paste(Mdir, "output/graphs/monthly_series", sep = "")
setwd(svdir)

lgd.snw <- c(paste("Slope =", round(slp_sep_may, digit = 2), "±",
                   round(std.err_sep_may, digits = 2)), paste("R-square =", round(r.sq, digits = 3)))

pdf(paste(stn_title, '.pdf', sep = ''))
png(paste(stn_title, '.png', sep = ''), width=800, height=800, units="px")

errbar(1:11, stn_mean_bymonth_aug[2:12], stn_mean_bymonth_aug[2:12] + stn_sd_bymonth_aug[2:12], stn_mean_bymonth_aug[2:12] - stn_sd_bymonth_aug[2:12],
        xaxt = "n",  xlab = "Month", ylab = "Snow depth (cm)", ylim = c(0,60), type = "o")
# clip(1,9, -100,200)
# abline(lm.acc, col = "red")
legend("topleft", legend = lgd.snw, bty = "n")
axis(side = 1, labels = A_J[2:12], at = 1:11)
title(main = stn_title)


dev.off()
dev.off()
}

if (do.entirets == TRUE) {
# ########
# ##plot entire ts
# svdir <- paste(Mdir, "output/graphs/whole_ts/all/", sep = "")
# setwd(svdir)
# pdf(paste(stn_title, '_all_ts.pdf', sep = ''))
# png(paste(stn_title, '_all_ts.png', sep = ''), width=800, height=567, units="px")
# plot(stn_snow,  xlab = "Year", ylab = "Snow depth (cm)", ylim = c(0,100), type = "o")
# title(main = stn_title)
# dev.off()
# dev.off()
}

}


########################
### discard

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

