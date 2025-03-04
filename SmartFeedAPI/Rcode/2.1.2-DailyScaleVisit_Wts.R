# 2.1.2 - Smartscale visits per day----

# libraries ----
library(data.table)
library(tidyverse)
library(plotly)

# Scale data -----
# Data -----
if(exists('d.scale')==F){
  d.scale = fread(input = 'Data/211-ScaleVisits.csv')
}
# Add trial date
d.dates = data.table(Date = seq.Date(from = date.start, to = as.Date(date.end), by = 1), 
                     tday = seq(from = 0, to = as.numeric(difftime(date.end,date.start, units = 'days')), by = 1))
d.scale = d.scale[d.dates, on = 'Date'] 

d.scale = d.scale[, GroupID := ifelse('Assignment' %in% names(d.scale), Assignment, FeederID)] # Identify group using either scale or Pasture assignment

# add animal information ----
# Animal Assignemnts
d.animal = fread(input = cattleDB, colClasses = 'character')
d.scale[, sEID := str_sub(RFIDTag,-nchar(d.animal$sEID)[1],-1)]
d.scale = d.scale[d.animal, on = 'sEID']

## Test to see if data exist for each day
d.wb = d.scale[, .(HD = length(unique(VID))), by = c('Date','FeederID')]

# System Status ----
p112.1.syson = d.wb[Date > date.end - as.difftime(7, unit = 'days'),] %>% 
  ggplot(aes(x=Date, y = FeederID))+
  geom_point()+
  scale_x_date(date_labels = '%b %d', breaks = '1 day', minor_breaks = '1 day') +
  theme_classic()+
  theme(panel.grid.major = element_line(color = 'black', linewidth = 0.1),
        panel.grid.minor = element_line(color = 'grey', linewidth = 0.05),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom")
p112.1.syson

# Smartscale Behavior  -----
## Total visits per feeder ----
d.visits=table(d.scale$GroupID,d.scale$VID,d.scale$Date)
d.visits=data.table(d.visits)
colnames(d.visits)=c("Group",'VID',"date","frequency")

d.visits$date=as.Date(d.visits$date)

p112.3.freq = ggplot(d.visits,aes(x=date, y=frequency, fill = Group))+
  geom_col()+
  # geom_line()+
  scale_x_date(date_labels = '%b %d', breaks = '2 day', minor_breaks = '1 day')+
  ylab('Number of visits per day')+
  facet_wrap(~Group)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90),
        panel.grid.major = element_line(color = 'black', linewidth = 0.1),
        panel.grid.minor = element_line(color = 'grey', linewidth = 0.05),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom")
p112.3.freq

d2.vishd = d.scale[, .(N = .N), by = c('GroupID','Date','VID')]
d2.vishd

p112.4.freqhd = d2.vishd %>%
  dplyr::filter(Date > (date.end - as.difftime(7, units = 'days'))) %>% 
  ggplot(aes(x=Date, y = N, color = VID))+
  geom_point()+
  geom_smooth(se = F)+
  labs(x = 'Date',
       y = 'Visit Frequence, N',
       title = 'Visits per animal per day')+
  facet_wrap(~GroupID, scale = 'free')+
  theme_classic()+
  theme(panel.grid.major = element_line(color = 'black', linewidth = 0.1),
        panel.grid.minor = element_line(color = 'grey', linewidth = 0.05),
        plot.caption = element_text(hjust = 0),
        legend.position = "none")
p112.4.freqhd

d3.vishd = d2.vishd[Date >= (date.end - as.difftime(7, units = 'days')),]
d3.vishd = d3.vishd[, .(N_total = sum(N),
                        N_average = mean(N),
                        N_max = max(N),
                        N_min = min(N)),
                    by = c('GroupID', 'Date')]
d3.vishd
