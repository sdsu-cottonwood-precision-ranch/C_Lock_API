# Three Day Rolling Average -----
# Libraries
library(data.table)

# Libraries -----
library(data.table)
library(tidyverse)

# Data -----
if(exists('d.hwts')==F){
  d.hwts = fread(file = 'Data/213-SmartScale_RobReg.csv')
}

# Three Day Average -----
colnames(d.hwts)
d.dailyBW = d.hwts[, .(BWkg = mean(BWkg, na.rm = T)),
                   by = c('Assignment','GroupID','VID','Date','tday')]
d.dailyBW

setorder(d.dailyBW, cols = 'VID','Date','tday')

d.dailyBW[, BWkg3day := fifelse(VID == shift(VID, n=1, type = 'lag') & 
                                  VID == shift(VID, n=1,type = 'lead'),
                                frollapply(BWkg, n = 3, FUN = 'mean', align = 'center'), NA)]
d.dailyBW

fwrite(d.dailyBW, file = 'Data/214-ThreeDayBW.csv')
