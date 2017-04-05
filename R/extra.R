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


