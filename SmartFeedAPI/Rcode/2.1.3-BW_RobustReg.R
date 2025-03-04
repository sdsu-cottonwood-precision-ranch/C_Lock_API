# 2.1.3 - Robust Regression----

# libraries ----
library(data.table)
library(tidyverse)
library(MASS)

# Data -----
if(exists('d.scale')==F){
  d.scale = fread(input = 'Data/211-ScaleVisits.csv')
  
  # Add trial date
  d.dates = data.table(Date = seq.Date(from = date.start, to = as.Date(date.end), by = 1), 
                       tday = seq(from = 0, to = as.numeric(difftime(date.end,date.start, units = 'days')), by = 1))
  d.scale = d.scale[d.dates, on = 'Date'] 
  
  d.scale = d.scale[, GroupID := ifelse('Assignment' %in% names(d.scale), Assignment, FeederID)] # Identify group using either scale or Pasture assignment
}

# add animal information ----

# Animal Assignemnts
d.animal = fread(input = cattleDB, colClasses = 'character')
d.scale[, sEID := str_sub(RFIDTag,-nchar(d.animal$sEID)[1],-1)]
d.scale = d.scale[d.animal, on = 'sEID']

# Robust Regression ----

d.scale = na.omit(d.scale)
m.rob = rlm(BWkg ~ VID + tday, data = d.scale)
d.hwts = data.table(Assignment = d.scale$Assignment, GroupID = d.scale$GroupID, 
                    FeederID = d.scale$FeederID,
                    VID = d.scale$VID, Date = d.scale$Date, tday = d.scale$tday,
                    StartTime = d.scale$StartTime, Duration = d.scale$Duration,
                    BWkg = d.scale$BWkg, resid = m.rob$residuals, hwt = m.rob$w)

p.hwts = d.hwts %>% 
  ggplot(aes(x = hwt, fill = GroupID))+
  geom_histogram()+
  geom_density(color = 'blue')+
  facet_wrap(~Assignment, scale = 'free')+
  theme_classic()+
  theme(panel.grid.major = element_line(color = 'black', linewidth = 0.1),
        panel.grid.minor = element_line(color = 'grey', linewidth = 0.05),
        plot.caption = element_text(hjust = 0),
        legend.position = "none")
  p.hwts
  
## Assign outliers ----
d.hwts[, Outlier := fifelse(hwt > 0.9, 'In Range','Outlier')]
d.hwts = d.hwts[Outlier == 'In Range', ]

# Save Data ----
fwrite(d.hwts, file = 'Data/213-SmartScale_RobReg.csv')  
  