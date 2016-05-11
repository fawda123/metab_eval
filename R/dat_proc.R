library(dplyr)
library(tidyr)
library(SWMPr)
library(purrr)
library(lubridate)

strReverse <- function(x)
        sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")

# format met
met <- read.table('C:/Users/mbeck/Desktop/middlebay_met_2015.txt', sep = ',', header = TRUE)

metfrm <- select(met, yeardata, jday, timedata, Air.Temperature, Barometric.Pressure, Wind.Speed, airtemp1Flag, bar_pressure1Flag, windspeed1Flag) %>% 
  filter(airtemp1Flag == 3 & bar_pressure1Flag == 3 & windspeed1Flag == 3) %>%  
  rename(
    year = yeardata, 
    atemp = Air.Temperature, 
    bp = Barometric.Pressure, 
    wspd = Wind.Speed
  ) %>% 
  mutate(
    timedata = strReverse(substr(strReverse(paste0('000', as.character(timedata))), 1, 4)),
    hours = as.numeric(substr(timedata, 1, 2)),
    mins = as.numeric(substr(timedata, 3, 4)), 
    datetimestamp = as.POSIXct(paste(year, jday, hours, mins), format = '%Y %j %H %M', tz = 'America/Regina'),
    wsdp = wspd * 0.51444, 
    bp = bp * 33.8639
  ) %>% 
  select(datetimestamp, atemp, bp, wspd) %>% 
  setstep(., 'datetimestamp', 60)

  
wq <- read.csv('C:/Users/mbeck/Desktop/Middle_BayHyd.csv', skip = 20) %>% 
  filter(Water.Height.Flag == 3 & Water.Temp.Flag == 3 & Salinity.Flag == 3 & DO.mg.L.Flag == 3) %>% 
  select(-Comments, -Dissolved.Oxygen.Percent...., -matches('Flag$')) %>% 
  rename( 
    depth = Water.Height..meter., 
    temp = Water.Tempreture..C.., 
    sal = Salinity..psu., 
    do_mgl = Dissolved.Oxygen.mg.L
  ) %>% 
  mutate(
    Time = strReverse(substr(strReverse(paste0('000', as.character(Time))), 1, 4)),
    hours = as.numeric(substr(Time, 1, 2)),
    mins = as.numeric(substr(Time, 3, 4)), 
    datetimestamp = as.POSIXct(paste(Year, Julian.Day, hours, mins), format = '%Y %j %H %M', tz = 'America/Regina'), 
    datetimestamp2 = round_date(datetimestamp, unit = 'hour')
  ) %>% 
  select(datetimestamp, datetimestamp2, depth, temp, sal, do_mgl)
    
dat_in <- wq
num_int <- 200
var <- 'do_mgl'

# first create new grid
uni_dts <- sort(unique(dat_in$depth))
dists <- unique(dat_in$datetimestamp)

new_grd <- expand.grid(
    approx(dists, n = num_int)$y, 
    approx(uni_dts, n = num_int)$y
    )
  
  # then interp
  int_val <- fields::interp.surface(
    obj = list(  
      x = dists, 
      y = uni_dts, 
      z = dat_in[, var(]), 
    loc = new_grd
    )
  out_mat <- cbind(new_grd, int_val)
  names(out_mat) <- c('Time', 'Depth', 'var')
  out_mat <- spread(out_mat, Depth, var)
  if(dat_out) return(do_mat)

ggplot(wq, aes(x = datetimestamp2, y = depth, col = temp)) +
  geom_tile() + 
  theme_bw()
