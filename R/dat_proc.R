library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)

strReverse <- function(x)
        sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")

######
# raw data processing

# format met
met <- read.table('ignore/middlebay_met_2015.txt', sep = ',', header = TRUE)

met_2015 <- select(met, yeardata, jday, timedata, Air.Temperature, Barometric.Pressure, Wind.Speed, airtemp1Flag, bar_pressure1Flag, windspeed1Flag) %>% 
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

# format wq  
wq_2015 <- read.csv('ignore/Middle_BayHyd.csv', skip = 20) %>% 
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
    datetimestamp = round_date(datetimestamp, unit = 'hour')
  ) %>% 
  select(datetimestamp, depth, temp, sal, do_mgl)

save(met_2015, file = 'data/met_2015.RData', compress = 'xz')
save(wq_2015, file = 'data/wq_2015.RData', compress = 'xz')
    
