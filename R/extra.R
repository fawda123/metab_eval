library(lubridate)
library(tidyverse)
library(SWMPr)
library(oce)
library(gridExtra)
devtools::load_all('M:/docs/EstuaryMetabolism')

######

# datetimestamp: POSIXct, referenced to time zone, no dst
# depth: depth bin (m), positive values only
# do_mgl: dissolved oxygen, mg/l
# sal: salinity, psu
# temp: in situ temp, C
# atemp: outside air temp, C 
# bp: outside pressure, mb
# wspd: outside wind speed, m/s
data(datall)

lat <- 30.4367
long <- -88.0117
tz <- 'America/Regina'

# get data frame of midpoints and width of each depth bin
depsd <- depsd(datall)

# get metab ests
datest <- datall %>% 
  rename(binmd = depth) %>% 
  group_by(binmd) %>% 
  nest %>% 
  left_join(., depsd, by = 'binmd') %>% 
  group_by(binmd, binwd) %>% 
  mutate(
    data = map(data, getwithin, binmid = binmd, binwid = binwd),
    data = map(data, metab_day, tz = tz, lat = lat, long = long)
  ) %>% 
  unnest %>% 
  ungroup %>% 
  getn2kv %>% 
  getdv



splts <- split(datest, datest$datetimestamp)

tmp <- splts[35:42]

# optimize avediff function within depth interval
# this finds the depth (as midpoint between bins) that maximizes the difference in averages of water density above/below
findmid <- function(profin) {
  
  binmd <- profin$binmd
  binln <- length(binmd)
  
  if(binln >= 2){
  
    ints <- diff(binmd) / 2
    ints <- binmd[-binln] + ints
    ints <- c(ints[1], ints[length(ints)])
      
    brk <- optimize(avediff, interval = ints, profin = profin)$minimum
    brk <- c(max(binmd[binmd < brk]), binmd[binmd > brk][1])
    brk <- brk[1] + diff(brk) / 2

  }
  
  return(brk)
        
}

# minimization function for difference of averages
avediff <- function(strt, profin){

  cts <- profin$binmd %>% 
    cut(breaks = c(-Inf, strt, Inf))

  ptsabv <- filter(profin, binmd < strt)
  ptsbel <- filter(profin, binmd >= strt)
  
  # estabv <- mean(ptsabv$sig)
  means <- lapply(split(profin$sig, cts), mean) %>% 
    unlist %>% 
    diff 
  
  mindiff <- -1 * means
  
  return(mindiff)
  
}

# RMSE of stick model as average density < strt, regression for density > strt
stkmod <- function(strt, profin, est = F) {
  
  # cut depth vector above/below strt depth
  cts <- profin$binmd %>%
    cut(breaks = c(-Inf, strt, Inf))

  # separate interpolated data by cts
  ptsabv <- filter(profin, binmd < strt)
  ptsbel <- filter(profin, binmd >= strt)

  # average density above strt
  aveabv <- mean(ptsabv$sig, na.rm = T)

  # format pts below for fixed origin model at strt and aveabv
  ptsbel <- mutate(ptsbel,
    binmd = binmd - strt, 
    sig = sig - aveabv
    )
  
  # fit model, get predictions, translate back to original scale
  lmbel <- lm(sig ~ 0 + binmd, ptsbel)
  lmbel <- predict(lmbel) + aveabv
  
  # return estimates if true
  if(est){
    
    xout <- c(strt, range(profin$bin)) %>% 
      sort
    yout <- c(rep(aveabv, 2), lmbel[length(lmbel)])
    out <- list(xout = xout, yout = yout)
    
    return(out)

  } 
  
  # otherwise return rmse of model fit  
  errabv <- ptsabv$sig - aveabv
  errbel <- ptsbel$sig + aveabv - lmbel
  out <- sum(errabv^2, errbel^2) %>%
    sqrt
  
  return(out)

}

# optimize stkmod function within depth interval
findstk <- function(profin, plot = F) {
  
  # get relevant columns, remove NA
  profin <- select(profin, binmd, sig) %>% 
    na.omit

  # do not evaluate if less than two obs  
  if(nrow(profin) < 2) return(NA)

  # linearly interpolate to avoid sample size issues
  xint <- range(profin$binmd)
  xint <- seq(xint[1], xint[2], length = 500)
  yint <- with(profin, approx(binmd, sig, xout = xint)$y)
  ints <- data.frame(binmd = xint, sig = yint)
  
  # interpolated depths and length
  binmd <- ints$binmd
  binln <- length(binmd)
  
  # interval ranges for optimizer
  rngs <- diff(binmd) / 2
  rngs <- binmd[-binln] + rngs
  rngs <- c(rngs[1], rngs[length(rngs)])
    
  # optimize stickmodel within the interval
  brk <- optimize(stkmod, interval = rngs, profin = ints)$minimum
  
  # return the optimum if no plot
  if(!plot) return(brk)
    
  # create plot
  ests <- stkmod(brk, ints, est = T)
  
  plot(sig ~ binmd, profin)
  with(ests, {
    segments(x0 = xout[1], x1 = xout[2], y0 = yout[1])
    segments(x0 = xout[2], x1 = xout[3], y0 = yout[2], y1 = yout[3])
    })
        
}  

library(parallel)
nc <- detectCores() - 1
cl <- makeCluster(nc)
clusterEvalQ(cl, library(dplyr))
clusterExport(cl, c('findstk', 'stkmod'))
ests <- parLapply(cl, splts, findstk)
stopCluster(cl)

strts <- do.call('rbind', ests) %>% 
  data.frame(pycno = .) %>% 
  rownames_to_column %>% 
  mutate(rowname = as.POSIXct(rowname, format = '%Y-%m-%d %H:%M:%S', tz = 'America/Regina')) %>% 
  rename(datetimestamp = rowname)

data(datall)
library(dplyr)
load(file = 'C:/Users/mbeck/Desktop/strts.RData')

strts <- mutate(strts, datetimestamp = datetimestamp - 1800)
datall <- left_join(datall, strts, by = 'datetimestamp')

subs <- as.POSIXct(c('2015-06-14 0:0', '2015-07-17 0:0'), tz = 'America/Regina') 
toplo <- filter(datall, datetimestamp <= subs[2] & datetimestamp >= subs[1])


ctd_time(na.omit(toplo), var = 'sal', lines = F, aggs = F, mix = T)



