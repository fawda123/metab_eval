library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(SWMPr)
library(oce)
devtools::load_all('M:/docs/EstuaryMetabolism')

strReverse <- function(x)
        sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")

# middle bay lighthouse metadata
lat <- 30.4367
long <- -88.0117
tz <- 'America/Regina'

######
# combine met and wq data

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
    wsdp = wspd * 0.51444, # knots to m/s
    bp = bp * 33.8639 # inches of hg to mb
  ) %>% 
  select(datetimestamp, atemp, bp, wspd) %>% 
  setstep(., 'datetimestamp', 60)

##
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
  select(datetimestamp, depth, temp, sal, do_mgl) %>% 
  mutate(depth = plyr::round_any(depth, 0.5)) %>% 
  filter(depth < 3.5)

# put zero depth to 0.5, rounding error
wq_2015$depth[wq_2015$depth == 0] <- 0.5

# long format by variable
wq_2015 <- gather(wq_2015, 'var', 'val', temp:do_mgl) %>% 
  group_by(datetimestamp, depth, var) %>% 
  summarise(val = mean(val, na.rm = T)) %>% 
  ungroup

# standardize time step
dts <- with(wq_2015, seq(min(datetimestamp), max(datetimestamp), by = 'hours'))
depth <- with(wq_2015, sort(unique(depth)))
vars <- with(wq_2015, sort(unique(var)))
grd <- expand.grid(dts, depth, vars) %>% 
  data.frame %>% 
  rename(
    datetimestamp = Var1, 
    depth = Var2, 
    var = Var3
  ) %>% 
  mutate(var = as.character(var)) %>% 
  arrange(datetimestamp, var, depth) 

wq_2015 <- left_join(grd, wq_2015, by = c('datetimestamp', 'depth', 'var'))

# smooth the time series, by variable and depth
wq_smth <- group_by(wq_2015, depth, var) %>% 
  nest %>% 
  mutate(
    valsm = map(data, function(x){
    
      # two hour, left-centered filter
      smth <- stats::filter(x$val, filter = rep(1, 4)/4, sides = 2, method = 'convolution')
      smth
    
    })
  ) %>% 
  unnest %>% 
  mutate(valsm = ifelse(is.na(valsm), val, valsm)) %>% 
  select(datetimestamp, var, depth, valsm) %>% 
  rename(val = valsm) %>% 
  spread(var, val)

##
# join smoothed wq data and weather
#
# datetimestamp: POSIXct, referenced to time zone, no dst
# depth: depth bin (m), positive values only
# do_mgl: dissolved oxygen, mg/l
# sal: salinity, psu
# temp: in situ temp, C
# atemp: outside air temp, C 
# bp: outside pressure, mb
# wspd: outside wind speed, m/s
datall <- left_join(wq_smth, met_2015, by = 'datetimestamp')

save(datall, file = 'data/datall.RData', compress = 'xz')

##
# create metab data, also for shiny app

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
  getdv %>% 
  getzmix

save(datest, file = 'data/datest.RData')
