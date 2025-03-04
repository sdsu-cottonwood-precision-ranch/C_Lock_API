# 2.1.4 - Average Daily Gain -----

# Library ----
library(data.table)

# Data -----
d.bw = fread(input = 'Data/214-ThreeDayBW.csv')
names(d.bw)
# Model weight
d.adg = d.bw[,list(initialBW=coef(lm(BWkg ~ tday))[1], 
                   ADG=coef(lm(BWkg~tday))[2],
                   TotalGain = max(BWkg) - min(BWkg)),
             by= 'VID']
d.adg

fwrite(d.adg, file = 'Data/214-AverageDailyGain.csv')
