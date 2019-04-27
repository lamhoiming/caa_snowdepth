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


############ data input #################

#### set data input working directory
Mdir <- "d:/phd/caa/eccc/snowdepth/"
wdir <- paste(Mdir, "data/station/csv/", sep = "")
setwd(wdir)

#### Reading data
idname <- read.csv(file = "_ID_NAME.csv", header = TRUE, sep = ",") 
## Lat lon info are stored in idname!


# List of stations in CAA with 2016 data
list_stn <- list(
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
  "ALERT LT1", "ALERT YLT", "CAMBRIDGE BAY YCB", "CORAL HARBOUR YZS", "EUREKA WEU", "HALL BEACH YUX", "IQALUIT YFB", "RESOLUTE YRB", # with 2016 data
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


#############################################
#################### Switch #################
#############################################
do.entirets <- FALSE #TRUE #          #######
do.monthts <- FALSE #TRUE   #FALSE #  #######
do.monthseries <-  FALSE #TRUE #      #######
do.monthtrend <-   FALSE #    TRUE #  #######
do.stackedbar <- TRUE # FALSE #FALSE         #######
#############################################
#############################################

# construct Time series for all stations
for (x in 1:length(list_stn)) {
  message(x)
  stn_title <- gsub('([[:punct:]])|\\s+', '_', list_stn[x])
  stn <- fdata[[x]]
  stn.date <- as.Date(stn[,1]) #, format = "%Y-%m-%d")
  stn_snow <- data.frame(date = stn.date, snow_depth = stn[,3])#, order.by = as.yearmon(stn.date))
  
  
  #### Plot entire ts (no averaging) #################################
  if (do.entirets == TRUE) {
    
    svdir <- paste(Mdir, "output/ts_all_pts/plots/", sep = "")
    setwd(svdir)
    pdf(paste(stn_title, '_all_ts.pdf', sep = ''))
    png(paste(stn_title, '_all_ts.png', sep = ''), width=800, height=567, units="px")
    plot(stn_snow,  xlab = "Year", ylab = "Snow depth (cm)", ylim = c(0,100), type = "o")
    title(main = stn_title)
    dev.off()
    dev.off()
  }
  #####################################################################
  
  #### Monthly ts ################################
  if (do.monthts == TRUE) {
    xts.stn_snow <- xts(stn[,3],stn.date)
    stn_mean_byyearmonth <- apply.monthly(xts.stn_snow, mean)
    stn_sd_byyearmonth <- apply.monthly(xts.stn_snow, sd)
    
    index(stn_mean_byyearmonth) <- as.yearmon(index(stn_mean_byyearmonth))
    index(stn_sd_byyearmonth) <- as.yearmon(index(stn_sd_byyearmonth))
    
    svdir <- paste(Mdir, "output/ts_mth_avg/plots/", sep = "")
    setwd(svdir)
    pdf(paste(stn_title, '_month_ts.pdf', sep = ''))
    png(paste(stn_title, '_month_ts.png', sep = ''), width=800, height=567, units="px")
    plot(stn_mean_byyearmonth, main = stn_title, type = "l", ylim = c(-1,100), 
         grid.col = NA, yaxis.right = FALSE, axis = 2, las = 2, mar = c(6,4,4,2)) %>% print()
    # tcks = c (0, 20, 40, 60, 80)
    # axis(side = 2, labels = tcks, at = tcks)
    dev.off()
    dev.off()
    
    svdir <- paste(Mdir, "output/ts_mth_avg/tables/", sep = "")
    setwd(svdir)
    write.zoo(stn_mean_byyearmonth, file = paste(stn_title, '_ts_mth_avg.csv', sep =''), sep = ',', na = "NA")
    
  }
  ###############################################
  
  ##### Monthly series/Seasonal cycles ##################
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
    
    svdir <- paste(Mdir, "output/seasonal_cycle/tables/", sep = "")
    setwd(svdir)
    write.csv(tbl.snw, file = paste(stn_title, '_seasonal_cycle.csv', sep =''), na = "NA")
    
    svdir <- paste(Mdir, "output/seasonal_cycle/tables/stats/", sep = "")
    setwd(svdir)
    capture.output(stats.snw, file = paste(stn_title, '_seasonal_cycle_stats.txt', sep =''))
    
    # Plot monthly averages seasonal trend
    svdir <- paste(Mdir, "output/seasonal_cycle/plots/", sep = "")
    setwd(svdir)
    
    lgd.snw <- c(paste("Slope =", round(slp_sep_may, digit = 2), "±",
                       round(std.err_sep_may, digits = 2), "cm/month"), 
                 paste("R-square =", round(r.sq, digits = 3)),
                 ' ',
                 paste("start :", as.yearmon(stn.date)[1]),
                 paste(" end :", as.yearmon(stn.date)[length(stn.date)])
                 )
    
    pdf(paste(stn_title, 'seasonal_cycle.pdf', sep = ''))
    png(paste(stn_title, 'seasonal_cycle.png', sep = ''), width=1600, height=1600, units="px")
    
    errbar(1:11, stn_mean_bymonth_aug[2:12], stn_mean_bymonth_aug[2:12] + stn_sd_bymonth_aug[2:12], stn_mean_bymonth_aug[2:12] - stn_sd_bymonth_aug[2:12],
           xaxt = "n",  xlab = "", ylab = "", ylim = c(0,60), type = "o",
           cex.axis = 2, cex = 2, 
           mar = c(5, 9, 4, 2) + 0.1)
    # clip(1,9, -100,200)
    # abline(lm.acc, col = "red")
    legend("topleft", legend = lgd.snw, bty = "n", cex = 2.5)
    axis(side = 1, labels = A_J[2:12], at = 1:11, cex.axis = 2, tck = 0.02)
    mtext("Month", side = 1, cex = 2, line = 3.5)
    mtext("Snow depth (cm)", side = 2, cex = 1.7, line = 2.85)
    title(main = stn_title)
    
    
    dev.off()
    dev.off()
  }
  ####################################################################
  
  ### Montly trend ##########################################
  # Average by month in each year
  
  if (do.monthtrend == TRUE) {
    xts.stn_snow <- xts(stn[,3],stn.date)
    stn_mean_byyearmonth <- apply.monthly(xts.stn_snow, mean)
    stn_sd_byyearmonth <- apply.monthly(xts.stn_snow, sd)
    
    index(stn_mean_byyearmonth) <- as.yearmon(index(stn_mean_byyearmonth))
    index(stn_sd_byyearmonth) <- as.yearmon(index(stn_sd_byyearmonth))
    
    #cycle(stn_mean_byyearmonth)
    
    # cnames <- unique(format(index(stn_mean_byyearmonth), "%Y"))
    # df <- data.frame(matrix(nrow = 12, ncol = length(cnames)))
    # colnames(df) <- cnames
    # rownames(df) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
    #                   "Aug", "Sep", "Oct", "Nov", "Dec")
    rnames <- toupper(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                        "Aug", "Sep", "Oct", "Nov", "Dec"))
    
 
    
    stats.out <- data.frame()
    
    if (#x ==32
        x == 4 | x == 24 | x == 32
        # stn_title == "ISACHSEN_OLD_ICE_IC1" |   #no trend by month
        # stn_title == "INUKJUAK_WPH" |           #no trend by month
        # stn_title == "SHEPHERD_BAY_YUS"
        )        #notrend by month 
        {next}
    else{
      for (mth in 1:12) {  #monthly trend
        if (any(cycle(stn_mean_byyearmonth) == mth)){ #test for if data exist in that month
          
          dfmthmean <- subset(stn_mean_byyearmonth, cycle(stn_mean_byyearmonth) == mth)
          dfmthsd <- subset(stn_sd_byyearmonth, cycle(stn_sd_byyearmonth) == mth)
          if (length(dfmthmean) < 6) { # At least 7 years of data to plot trend
            next
          } 
          else {
            dfmth <- cbind("mean" = dfmthmean,"sd" = dfmthsd)
            
            svdir <- paste(Mdir, "output/trd_ind_mth_all_yr_avg/tables/", sep = "")
            setwd(svdir)
            write.csv(dfmth, file = paste(stn_title, '_', rnames[mth], '_all_yr_avg.csv', sep =''), 
                      row.names = format(index(dfmth), "%Y"), na = "NA")
            
            trd <- read.csv(file = paste(stn_title, '_', rnames[mth], '_all_yr_avg.csv', sep =''), 
                            col.names = c("yr", "mean", "sd"))
            lm.trd <- lm(trd$mean~trd$yr, na.action = 'na.omit')
            
            stats.trd <- summary(lm.trd)
            
            slp_trd <- coef(stats.trd)['trd$yr','Estimate'] #getting slope
            std.err_trd <- coef(stats.trd)['trd$yr','Std. Error'] #getting std error of regression
            r.sq_trd <- stats.trd$adj.r.squared
            
            tmp.stats.out <- data.frame("Trend" = slp_trd, "Std.err" = std.err_trd, 
                                        "R-sq" = r.sq_trd, row.names = rnames[mth])
            
            
            # # Plotting trend for each month with slope and r-sq as legends
            lgd.trd <- c(paste("Slope =", round(slp_trd, digit = 2), "±",
                               round(std.err_trd, digits = 2), "cm/year"), paste("R-square =", round(r.sq_trd, digits = 3)))
            
            svdir <- paste(Mdir, "output/trd_ind_mth_all_yr_avg/plots/", sep = "")
            setwd(svdir)
            pdf(paste(stn_title, '_', rnames[mth], '_all_yr_avg.pdf', sep = ''))
            png(paste(stn_title, '_', rnames[mth], '_all_yr_avg.png', sep = ''), 
                width = 1600, height = 1134, units="px")
            
            
            errbar(trd$yr, trd$mean, trd$mean + trd$sd, trd$mean - trd$sd, 
                   ylim = c(0,100), las = 1, xlab = "", ylab = "",
                   cex.axis = 1.8, cex = 2,
                   mar = c(5, 9, 4, 2) + 0.1)  #type = "o"
            
            title(main = paste(stn_title, rnames[mth], sep = " "))
            mtext("Year", side = 1, cex = 2, line = 3.5)
            mtext("Snow depth (cm)", side = 2, cex = 1.7, line = 2.7)
            legend("topright", legend = lgd.trd, bty = "n", cex = 2)
            abline(lm.trd, col = "red")
            
            dev.off()
            dev.off()
            
          }
        } 
        else {next} #skipping months without any data
        
        # plot(dec, main = stn_title, type = "o", ylim = c(-1,100), 
        #           grid.col = NA, yaxis.right = FALSE, axis = 2, las = 2, mar = c(6,4,4,2)) %>% print()
        
        stats.out <- rbind.data.frame(stats.out,tmp.stats.out)
      }
    }
    svdir <- paste(Mdir, "output/trd_vs_mth/tables/stats/", sep = "")
    setwd(svdir)
    #### Build up station trend by month
    missing <- setdiff(rnames, row.names(stats.out))
    stats.out[missing,] <- NA
    stats.out <- stats.out[match(toupper(aug_jul), rownames(stats.out)),]
    write.csv(stats.out, file = paste(stn_title, '_trd_vs_mth_stats.csv', sep = ''))
    
    # plot trend by month with SONDJFMAMJJ
    svdir <- paste(Mdir, "output/trd_vs_mth/plots", sep = "")
    setwd(svdir)
    pdf(paste(stn_title, '_trd_vs_mth.pdf', sep = ''))
    png(paste(stn_title, '_trd_vs_mth.png', sep = ''), 
        width = 1600, height = 1600, units="px")
    
    errbar(1:11, stats.out$Trend[2:12], 
           stats.out$Trend[2:12] + stats.out$Std.err[2:12], stats.out$Trend[2:12] - stats.out$Std.err[2:12],
           ylim = c(-3,3), xaxt = "n",
           xlab = '', ylab = '',
           cex = 2,
           las = 1, cex.axis = 2,
           mar = c(5, 9, 4, 2) + 0.1)
    
    title(main = paste(stn_title, sep = " "))
    axis(side = 1, labels = A_J[2:12], at = 1:11, cex.axis = 2, tck = 0.02)
    mtext("Month", side = 1, cex = 2, line = 3.5)
    mtext("Trend (cm/year)", side = 2, cex = 2, line = 2.5)
    lgd.startend = c(paste("start :", index(stn_mean_byyearmonth)[1]), 
                     paste(" end :", index(stn_mean_byyearmonth)[length(index(stn_mean_byyearmonth))]))
    legend("topright", legend = lgd.startend, bty = "n", cex = 2.5)
    abline(0,0)
    
    dev.off()
    dev.off()
    
  }
  
  ################################################
  
  ##### Stacked barcharts
  if (do.stackedbar == TRUE) {
    xts.stn_snow <- xts(stn[,3],stn.date)
    
  ##### functions
    endpoints.seasons <- function(x, on = "spring") {
      if (timeBased(x)) {
        NR <- length(x)
        x <- xts(NULL, order.by = x)
      }
      else NR <- NROW(x)
      if (!is.xts(x))
        x <- try.xts(x, error = "must be either xts-coercible or timeBased")
      
      posixltindex <- as.POSIXlt(.POSIXct(.index(x)), tz = indexTZ(x))$mon
      if (on == "winter") {
        tocal <- c(0, 1, 2)
      }
      else if (on == "spring") {
        tocal <- c(3, 4, 5)
      }
      else if (on == "summer") {
        tocal <- c(6, 7)
      }
      else if (on == "autumn") {
        tocal <- c(8, 9, 10, 11)
      }
      
      xi <- rep(0, NR)
      xi[posixltindex %in% tocal] <- 1
      if(xi[1] == 1) {
        ep <- as.integer(c(0, which(diff(xi) != 0)))
      }else {
        ep <- as.integer(which(diff(xi) != 0))
      }
      if(xi[NR] == 1) {
        ep[length(ep) + 1] <- NR
      }
      ep
    }#define seasonal months
    season.apply <- function(x, INDEX, FUN, ...)
    {
      x <- try.xts(x, error = FALSE)
      FUN <- match.fun(FUN)
      
      re <- sapply(1:(length(INDEX)/2), function(y) {
        FUN(x[(INDEX[y*2 - 1] + 1):(INDEX[y*2])], ...)
      })
      
      if (is.vector(re))
        re <- t(re)
      re <- t(re)
      if (is.null(colnames(re)) && NCOL(x) == NCOL(re))
        colnames(re) <- colnames(x)
      
      SINDEX <- INDEX[seq(2, length(INDEX), by = 2)]
      reclass(re, x[SINDEX])
    }
    apply.spring <- function(x, FUN, ...) {
      ep <- endpoints.seasons(x, "spring")
      season.apply(x, ep, FUN, ...)
    }
    apply.summer <- function(x, FUN, ...) {
      ep <- endpoints.seasons(x, "summer")
      season.apply(x, ep, FUN, ...)
    }
    apply.autumn <- function(x, FUN, ...) {
      ep <- endpoints.seasons(x, "autumn")
      season.apply(x, ep, FUN, ...)
    }
    apply.winter <- function(x, FUN, ...) {
      ep <- endpoints.seasons(x, "winter")
      season.apply(x, ep, FUN, ...)
    }
  ########
    
    mean.fall <- apply.autumn(xts.stn_snow, mean)
    mean.winter <- apply.winter(xts.stn_snow, mean)
    mean.spring <- apply.spring(xts.stn_snow, mean)
    
    index(mean.fall) <- as.yearmon(index(mean.fall))
    index(mean.winter) <- as.yearmon(index(mean.winter))
    index(mean.spring) <- as.yearmon(index(mean.spring))
    
    tmp.fall <- data.frame("Year" = format(index(mean.fall), "%Y"), "Fall - SOND" = mean.fall)
    tmp.winter <- data.frame("Year" = format(index(mean.winter), "%Y"), "Winter - JFM" = mean.winter)
    tmp.spring <- data.frame("Year" = format(index(mean.spring), "%Y"), "Spring - AMJ" = mean.spring)
    
    df.stacked <- merge(merge(tmp.fall, tmp.winter, by = "Year", all.x = TRUE, all.y = TRUE), 
                        tmp.spring, by = "Year", all.x = TRUE, all.y = TRUE)
    
    svdir <- paste(Mdir, "output/stacked_season_snowdepth/tables/", sep = "")
    setwd(svdir)
    write.csv(df.stacked, file = paste(stn_title, '_stacked_season.csv', sep =''), na = "NA")
    
    rn <- as.data.frame.integer(df.stacked$Year)
    df.stacked <- df.stacked[order(rn),]
    row.names(df.stacked) <- df.stacked[,1]
    df.stacked <- df.stacked[,-1]
    df.stacked[is.na(df.stacked)] <- 0 
 
    
    ytck <- seq(0, 1.5*max(t(as.matrix(df.stacked)), na.rm = TRUE), by = 5)
    ylim <- c(0, 1.5*max(t(as.matrix(df.stacked)), na.rm = TRUE))
    # ytck <- seq(0, 2.25*max(t(as.matrix(df.stacked)), na.rm = TRUE), by = 50)
    # ylim <- c(0, 2.25*max(t(as.matrix(df.stacked)), na.rm = TRUE))
    # 
    # svdir <- paste(Mdir, "output/stacked_season_snowdepth/plots/", sep = "")
    # setwd(svdir)
    # pdf(paste(stn_title, '_stacked_season.pdf', sep = ''))
    # png(paste(stn_title, '_stacked_season.png', sep = ''), width=1400, height=784, units="px")
    svdir <- paste(Mdir, "output/stacked_season_snowdepth/plots/beside/", sep = "")
    setwd(svdir)
    pdf(paste(stn_title, '_stacked_season_beside.pdf', sep = ''))
    png(paste(stn_title, '_stacked_season_beside.png', sep = ''), width=2400, height=800, units="px")
    
    mx <- t(as.matrix(df.stacked))
    bp <- barplot.default(mx, beside = TRUE,
                          ylim = ylim, yaxt = 'n', 
                          legend	= names(df.stacked),
                          args.legend = list('topright', bty = 'o', cex = 2), #x = ncol(mx), y = max(colSums(mx), na.rm = TRUE), bty = "n"),
                          cex.names = 1.5,
                          mar = c(5, 7, 4, 2) + 0.1)
    # bp <- barplot.default(mx, col = c('red', 'blue', 'springgreen2'), width = 0.1,
    #               beside = TRUE,
    #         border="black", space=0.8, ylim = ylim, yaxt = 'n', 
    # legend	= names(df.stacked),
    # args.legend = list('topright', bty = 'o', cex = 2), #x = ncol(mx), y = max(colSums(mx), na.rm = TRUE), bty = "n"),
    # cex.names = 1.1)
    title(main = paste(stn_title, sep = " "))
    axis(2, at = ytck, las = 2, line = -3.5, cex.axis = 2)
    mtext("Seasonally-averaged snow depth (cm)", side = 2, cex = 2.5, line = 1.5)
    axis(1, at = bp[seq(2,length(bp), by = 3)], labels = rep('', length(rownames(df.stacked))))
    mtext("Year", side = 1, cex = 2, line = 4)
    
    #colors()
    dev.off()
    dev.off()
  }
  
}





# end
########################
# # # ### discard
# # # 
# # # #stn_sd_bymonth <- aggregate(stn_snow, format(time(stn_snow), "%m"), sd, na.rm = TRUE) %>%
# # # #  coredata() # convert from zoo (time-ordered) to matrix/vector
# # # 
# # # # Rearrange to start from Sep to July; ignore August
# # # # #
# # # # tmp_mean_bymonth <- c(stn_mean_bymonth[-(1:7)],stn_mean_bymonth[1:7])
# # # # tmp_sd_bymonth <- c(stn_sd_bymonth[-(1:7)],stn_sd_bymonth[1:7])
# # # #
# # # if (length(stn_mean_bymonth) == 12) {
# # #   stn_mean_bymonth_sep <- data.frame(month = sep_jul, snow_depth = tmp_mean_bymonth[2:12])
# # #   stn_sd_bymonth_sep <- data.frame(month = sep_jul, std_dev = tmp_sd_bymonth[2:12])
# # #   } else if () {
# # #
# # # }
# # # rm(tmp_mean_bymonth)
# # # rm(tmp_sd_bymonth)
# # 
# # # Plot monthly average starting from August
# # #pdf("test.pdf")
# # # plot(stn_mean_bymonth_sep[,2], xaxt = "n")
# # # errbar(1:11, stn_mean_bymonth_sep[,2],stn_mean_bymonth_sep[,2] + stn_sd_bymonth_sep[,2], stn_mean_bymonth_sep[,2] - stn_sd_bymonth_sep[,2],
# # #        xaxt = "n",  xlab = "Month", ylab = "Snow depth (cm)", ylim = c(0,60), type ="o")
# # # axis(side = 1, labels = sep_jul[1:11], at = 1:11)
# # # title(main = stn_title)
# # #dev.off()
# # 
# # #plot(stn_mean_bymonth_aug[1:12], xaxt = "n")
# 

