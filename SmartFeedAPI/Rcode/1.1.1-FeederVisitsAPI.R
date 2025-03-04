# Loading libraries [install the libraries before if not already installed]
library(rvest)
library(httr)
library(stringr)
library(data.table)
library(fasttime)
# Set working directory
setwd(dir = '/Users/ira/Documents/SDSU/Projects/2023_API Paper/SmartFeedAPI/')

# Report Parameters -----
params= data.table::fread(input = 'Report_parameters/Report_params.csv')[]
un <- as.character(params[Parameter == 'USERNAME', c('Value')])
pw <- as.character(params[Parameter == 'PASSWORD', c('Value')])
assignments = data.frame(Assignment = unlist(strsplit(as.character(params[Parameter == 'Assignments', c('Value')]), split = ',')))
intake.type = ifelse(as.character(params[Parameter=='GET_VISITS', c('Value')])==1,'visits','daily')
equipDB= as.character(params[Parameter == 'EquipmentDB', c('Value')])
cattleDB = as.character(params[Parameter == 'CattleDB', c('Value')])

# Equipment assignments -----

equipment = fread(file = paste0(equipDB,'/SmartFeeders.csv'))
equipment[, Date_assign := as.Date(Date_assign)]
equipment = equipment[, .SD[which.max(Date_assign)], by = 'FeederID'] # Get the last deployment for each device
equipment[, Date_assign := NULL]
equipment = equipment[, .SD[Assignment %in% assignments$Assignment]]
equipIDs = unique(equipment$FeederID)
equipIDs = paste0(equipIDs, collapse = ',')

# define start and end dates -----
date.start <- as.Date(as.character(params[Parameter == 'Start', c('Value')]),format = '%m/%d/%Y', tz = 'UTC')
if(as.character(params[Parameter == 'Current_day', c('Value')]) == 1){
  date.end   <- as.Date(Sys.Date(), tz = 'UTC')
}else{
  date.end   <- as.POSIXct(as.character(params[Parameter == 'End', c('Value')]), format = '%m/%d/%Y',tz = 'UTC')
}

# Current dataset ----
if(file.exists('Data/111-FeederVisits.csv') == TRUE){
  d.fe = fread(input = 'Data/111-FeederVisits.csv', colClasses = c('character'))
  d.fe[, `:=` (StartTime = fastPOSIXct(StartTime),
             EndTime = fastPOSIXct(EndTime))]
  }

# Login ----
# First Authenticate to receive token:
req <- POST("https://portal.c-lockinc.com/api/login", body=list(user=un, pass=pw))
stop_for_status(req)
TOK <- trimws(content(req))

# Set up URL ----
url = paste0('https://portal.c-lockinc.com/api/',
             'getintake?d=',intake.type, # Specify vists or daily average
             '&fids=',equipIDs, # Specify feeder ids
             '&st=',date.start, # Specify start date
             '&et=',date.end # Specify end date
)

# Download data ----
req = POST(url,body = list(token=TOK))
stop_for_status(req)

# Parse data ----
ct = str_split(content(req, as = 'text'), '\\n')[[1]]
dt = data.table(do.call('rbind', str_split(ct[3:length(ct)],",")))
colnames(dt) = c(str_split(ct[2],',')[[1]])
dt[, `:=` (StartTime = fastPOSIXct(StartTime),
           EndTime = fastPOSIXct(EndTime))]
d.fe = rbindlist(l = list(d.fe,dt), use.names = TRUE)
