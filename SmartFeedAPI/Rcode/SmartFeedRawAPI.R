#SmartFeederAPI
# Libraries ----
library(rvest)
library(tidyverse)
library(data.table)
library(httr)
library(stringr)

# Parameters ----
un = 'sdsuwr' # account username
pw = 'Pasm605' # Account password
d = 'meas' # Data type: meas, secs, feed, rfid, cmds
fids = c('500196,500197,500198,500199,500200,500201') # Feeder ideas, separted by commas
st = '2022-03-27' # Start time in YYYY-MM-DD%hh:mm:ss format
et = '2022-03-30' # End time in same format
# ts # Optional
# zip = 1 # Set to 1 to download as csv

# Login ----
# First Authenticate to receive token:
req <- POST("https://portal.c-lockinc.com/api/login", body=list(user=un, pass=pw))
stop_for_status(req)
TOK <- trimws(content(req))

# Set up data URL ----
url = paste0('https://portal.c-lockinc.com/api/',
             'getraw?d=',d, # Specify vists or daily average
             '&fids=',fids, # Specify feeder ids
             '&st=',st, # Specify start date
             '&et=',et # Specify end date
)

# Download data ----
req = POST(url,body = list(token=TOK))
stop_for_status(req)

# Parse data ----
ct = str_split(content(req, as = 'text'), '\\n')[[1]]
dt = data.table(do.call('rbind', str_split(ct[3:length(ct)],",")))
colnames(dt) = c(str_split(ct[2],',')[[1]])

# Get variable names
urlnames = paste0('https://portal.c-lockinc.com/api/getraw?',
                  'd=vars',
                  '&fid=',fids
                  )
varurl = POST(urlnames, body = list(token = TOK))
varurl = str_split(content(varurl, as = 'text'), '\\n')[[1]]
varnames = data.table(do.call('rbind', str_split(varurl[3:length(varurl)],",")))
colnames(varnames) = c(str_split(varurl[2],',')[[1]])

fwrite(dt, file = 'Data/SmartFeedSec.csv')

# Get equations ----
# First Authenticate to receive token:
req <- POST("https://portal.c-lockinc.com/api/login", body=list(user=un, pass=pw))
stop_for_status(req)
TOK <- trimws(content(req))
# Download data
req = POST('https://portal.c-lockinc.com/api/getraw?d=equs&fid=500196',body = list(token=TOK))
stop_for_status(req)

# Parse data ----
eqct = str_split(content(req, as = 'text'), '\\n')[[1]]
eqdt = data.table(do.call('rbind', str_split(eqct[1:length(eqct)],",")))
colnames(eqdt) = c(str_split(eqct[2],',')[[1]])
eqdt


# Plot Raw Data ----
dt %>% 
  dplyr::filter(SystemID == '500196') %>% 
  mutate(DateTime = as.POSIXct(DateTime)) %>% 
  ggplot(aes(x = DateTime, y = S20))+
  geom_point()

dt[, DateTime := as.POSIXct(DateTime, format = '%Y-%m-%d %H:%M:%S', tz = 'MDT')]
dt[, Date := as.Date(DateTime)]

dt %>% 
  dplyr::filter(SystemID == '500196',
                DateTime > as.Date('2022-03-28'),
                DateTime < as.Date('2022-03-29')) %>% 
  ggplot(aes(x = DateTime, y = S20))+
  geom_point()

dt1 = dt[Date == as.Date('2022-03-28'),]
dt1[,FeedMass := (as.numeric(S20)-46000)*0.0022]
setorder(dt1, cols = 'DateTime')

dt1[,RA60 := frollmean(FeedMass, n = 60, align = 'center')]

dt1 %>% 
  ggplot(aes(x = DateTime, y = RA60))+
  geom_point()
