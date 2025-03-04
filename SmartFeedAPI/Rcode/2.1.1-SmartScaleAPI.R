# 2.1.1 - Smart Scale API -----
# Libraries ----
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
assignments = as.character(data.frame(Assignment = unlist(strsplit(as.character(params[Parameter == 'Assignments', c('Value')]), split = ','))))
intake.type = ifelse(as.character(params[Parameter=='GET_VISITS', c('Value')])==1,'visits','daily')
equipDB= as.character(params[Parameter == 'EquipmentDB', c('Value')])
cattleDB = as.character(params[Parameter == 'CattleDB', c('Value')])

# Equipment assignments -----
equipment = fread(file = paste0(equipDB,'/SmartScales.csv'))
equipment[, Date_assign := as.Date(Date_assign, format = '%m/%d/%Y')]
# equipment = equipment[, .SD[which.max(Date_assign)], by = 'FeederID'] # Get the last deployment for each device
equipment = equipment[Assignment %in% assignments,] # Get the required assignments
equipment[, Date_assign := NULL]
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
if(file.exists('Data/211-SmartScaleVisits.csv') == TRUE){
  d.fe = fread(input = 'Data/211-SmartScaleVisits.csv', colClasses = c('character'))
  d.fe[, `:=` (StartTime = fastPOSIXct(StartTime),
               EndTime = fastPOSIXct(EndTime))]
  date.end = max(d.fe$StartTime, na.rm = T)
}else{
  date.end = date.end
}
FULL_WEIGHT <- as.numeric(params[Parameter == 'FULL_WT', c('Value')]) #Put 0 for half-body weights
GET_VISITS <- as.numeric(params[Parameter == 'GET_VISITS', c('Value')]) #Put 1 to get individual visits, Put 0 to get daily averages

#Choose a temp folder to download the CSV to
if(Sys.info()["sysname"] == "Darwin"){
  TEMP_DIRECTORY <- "/tmp" #For Mac or Linux
}else{
  TEMP_DIRECTORY <- "."    #For Windows (Maybe put C:\Users\yourusername\Desktop)
}

#Required libraries
suppressWarnings(suppressMessages(try(require(rvest), silent = TRUE)))
suppressWarnings(suppressMessages(try(require(httr), silent = TRUE)))
suppressWarnings(suppressMessages(try(require(RCurl), silent = TRUE)))

#Login URL
login_url <- "https://greenfeed.c-lockinc.com/GreenFeed/checklogon.php"

# Download data
if (GET_VISITS == 1) {
  #Download URL - Individual Visits Weights
  download_url <- paste0("https://greenfeed.c-lockinc.com/GreenFeed/tabledata/sfanimals/getanimalweightdetails.php?dl=1",
                         "&fids=0,", equipIDs,
                         "&st=", date.start,
                         "&et=", date.end,
                         "&full=", FULL_WEIGHT);
} else {
  #Download URL - Daily Average Weights
  download_url <- paste0("https://greenfeed.c-lockinc.com/GreenFeed/tabledata/sfanimals/animalweights.php?dl=1&summary=0",
                         "&fids=0,", equipIDs,
                         "&from=", date.start,
                         "&to=", date.end,
                         "&full=", FULL_WEIGHT);
}

#Log into the website using URL requests
{
  #Create session then download form data
  session <- session("https://greenfeed.c-lockinc.com/GreenFeed")
  form <- html_form(read_html(login_url))[[1]]
  
  #Set login credentials
  form <- set_values(form, username = un)
  form <- set_values(form, password = pw)
  suppressWarnings(form <- set_values(form, redir = "home.php?logout")) #This will give you a warning - we can hide it with suppressWarnings
  
  #Save main page url
  suppressMessages(main_page <- submit_form(session, form))
  
  #Download the data
  download <- jump_to(main_page, download_url)
}

#Because the downloaded data is binary data, you must write it to a temporary file then read it back as a CSV.
#Write data to a file
FILENAME <- paste0(TEMP_DIRECTORY, "/C.txt")
writeBin(download$response$content, FILENAME)

#Read the CSV into a df dataframe
if (GET_VISITS == 1) {
  #SmartScale Headers are: "Link" "StartTime" "StopTime" "FeederID" "Duration" "AnimalName" "RFIDTag" "Weight" "Valid"
  colclasses=c("character", "character", "character", "integer", "integer", "character", "character", "integer", "character");
} else {
  #SmartScale Headers are: "Link" "StartTime" "StopTime" "FeederID" "Duration" "AnimalName" "RFIDTag" "Weight" "Valid"
  colclasses=c("character", "character");
}
d.scale = fread(file = FILENAME, header=TRUE, sep=",", quote="\"", colClasses = colclasses)
# Organize database ----
d.scale[d.scale == 0] <- NA # Assign NA to 0 values
d.scale$Date=as.Date(d.scale$StartTime) # Add a date column
d.scale[, BWkg := Weight*0.453592]

fwrite(d.scale, file = paste0('Data/','211-ScaleVisits.csv'))
