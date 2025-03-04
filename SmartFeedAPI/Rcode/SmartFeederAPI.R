#SmartFeederAPI
# Libraries ----
library(rvest)
library(ggplot2)
library(data.table)
library(httr)
library(stringr)

# Parameters ----
un = 'sdsuwr' # account username
pw = 'Pasm605' # Account password
d = 'visits' # Data type: "daily", "visits"
fids = c('500196,500197,500198,500199,500200,500201') # Feeder ideas, separted by commas
st = '2022-03-27' # Start time in YYYY-MM-DD%hh:mm:ss format
et = '2022-05-14' # End time in same format
# ts # Optional
# zip = 1 # Set to 1 to download as csv

# Login ----
# First Authenticate to receive token:
req <- POST("https://portal.c-lockinc.com/api/login", body=list(user=un, pass=pw))
stop_for_status(req)
TOK <- trimws(content(req))

# Set up URL ----
url = paste0('https://portal.c-lockinc.com/api/',
             'getintake?d=',d, # Specify vists or daily average
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
fwrite(dt, file = 'Data/SmartFeederAPIvisits.csv')
