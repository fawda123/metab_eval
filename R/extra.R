library(lubridate)
library(tidyverse)
library(SWMPr)
library(oce)
library(gridExtra)
library(purrr)
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

# expand
datall <- datprep(datall)

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
  getzmix(sigdiff = 2) %>% 
  getdsdvdz

tmp <- datest %>% 
  mutate(nepi = ddo - dz + dv + ds) %>% 
  group_by(metab_date, binmd, solar_period, day_hrs) %>% 
  summarise(
    nepi = mean(nepi, na.rm = T)
    ) %>% 
  ungroup(solar_period) %>% 
  spread(solar_period, nepi) %>% 
  mutate(
    gppi = day_hrs * (sunrise + sunset),
    ri = sunset * 24,
    nepi = gppi + ri
  )
  
toplo <- select(tmp, metab_date, binmd, ddo) 

ggplot(toplo, aes(x = metab_date, y = ddo)) + 
  geom_line() + 
  geom_point() + 
  facet_wrap(~binmd, ncol = 1)


# net ecosystem production per time step
nepi <- ddo - dz + dv + ds

# day
DOF_d <- mean(with(x[x$solar_period == 'sunrise', ], ddo * H), na.rm = T)
D_d <- mean(with(x[x$solar_period == 'sunrise', ], D), na.rm = T)
        
#night
DOF_n <- mean(with(x[x$solar_period == 'sunset', ], ddo * H), na.rm = T)
D_n <- mean(with(x[x$solar_period == 'sunset', ], D), na.rm = T)


# subs <- as.POSIXct(c('2015-05-28 0:0', '2015-06-09 0:0'), tz = 'America/Regina') 
# toplo <- filter(datest, datetimestamp <= subs[2] & datetimestamp >= subs[1]) %>% 
#   rename(depth = binmd)
# 
# ctd_time(na.omit(toplo), var = 'sig', lines = F, aggs = F, mix = T)

tmp <- datest[385:390, ] %>% 
  select(binmd, sig, n2, dv, dz, ds, do) %>% 
  gather('var', 'val', -binmd)
tmp2 <- datest[391:396, ] %>% 
  select(binmd, sig, n2, dv, dz, ds, do) %>% 
  gather('var', 'val', -binmd)
p1 <- ggplot(tmp, aes(x = factor(-binmd), y = val, group = var)) + 
  coord_flip() + 
  geom_point(size = 5) +
  facet_wrap(~var, ncol = 6, scales = 'free_x') + 
  theme_bw()
p2 <- ggplot(tmp2, aes(x = factor(-binmd), y = val, group = var)) + 
  coord_flip() + 
  geom_point(size = 5) +
  facet_wrap(~var, ncol = 6, scales = 'free_x') + 
  theme_bw()

library(gridExtra)
grid.arrange(p1, p2, ncol = 1)

