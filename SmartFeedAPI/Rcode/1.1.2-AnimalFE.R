# 1.1.2 - Animal and feeder visits 
# remove(list = ls())
# libraries----
library(tidyverse)
library(data.table)
library(fasttime)
library(stringr)

# Feeding Events -----
d.fe = fread(input = 'Data/111-FeederVisits.csv')

# Animals -----
d.an = fread(input = 'Data/Cattle/Animals.csv', colClasses = c('Character'))

# Merge -----
d.fe[, sEID := str_sub(RFID,-3,-1)]
d.fe

d.fe = d.fe[d.an, on = 'sEID']
d.fe[, `:=` (StartTime = fastPOSIXct(StartTime),
             EndTime = fastPOSIXct(EndTime),
             Date = as.character(fastDate(StartTime)),
             Duration = as.numeric(Duration),
             IntakeKG = as.numeric(IntakeKG))]
# FeedingBehavior -----
d.fbdaily = d.fe[,     .(BVfreq = .N,
                         BVdur.tot = sum(Duration, na.rm = T),
                         BVdur.u = mean(Duration, na.rm = T),
                         BVdur.sd = sd(Duration, na.rm = T),
                         IntakeKG.tot = sum(IntakeKG, na.rm = T),
                         ERkg.min = (sum(IntakeKG, na.rm = T)/(sum(Duration, na.rm = T)/60))),
                 by = list(VID,Date)]
d.fbdaily
fwrite(d.fbdaily, file = 'Data/112-DailyFB.csv')

# FB Plots ----
## Bunk visit duration ----
p.bvdur = d.fbdaily %>% 
  ggplot(aes(x = as.Date(Date), y = BVdur.tot))+
  geom_point(aes(color = VID))+
  geom_smooth(aes(color = VID), se = F)+
  geom_smooth(color = 'black',se = F)+
  labs(x='Date',
       y='Bunk visit duration, seconds per day',
       title = 'Bunk visit duration')+
  theme_classic()+
  theme(panel.grid.major = element_line(color = 'black', linewidth = 0.1),
        panel.grid.minor = element_line(color = 'grey', linewidth = 0.05),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0),
        legend.position = "right")
p.bvdur

p.bvdursd = d.fbdaily %>% 
  ggplot(aes(x = as.Date(Date),y = BVdur.sd,fill = VID))+
  geom_col()+
  geom_smooth(se = F)+
  facet_wrap(~VID, ncol = 2)+
  theme_classic()+
  theme(panel.grid.major = element_line(color = 'black', linewidth = 0.1),
        panel.grid.minor = element_line(color = 'grey', linewidth = 0.05),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0),
        legend.position = "right")
p.bvdursd

p.bvfreq = d.fbdaily %>% 
  ggplot(aes(as.Date(Date), y = BVfreq))+
  geom_point(aes(color = VID))+
  geom_smooth(aes(color = VID), se = F)+
  geom_smooth(se = F)+
  labs(x = 'Date',
       y = 'Bunk visit frequency, n per day',
       title = 'Bunk visit frequency')+
  theme_classic()+
  theme(panel.grid.major = element_line(color = 'black', linewidth = 0.1),
        panel.grid.minor = element_line(color = 'grey', linewidth = 0.05),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0),
        legend.position = "right")
p.bvfreq

p.bvu = d.fbdaily %>% 
  ggplot(aes(x = as.Date(Date), y = BVdur.u))+
  geom_point(aes(color = VID))+
  geom_smooth(aes(color = VID), se = F)+
  geom_smooth(color = 'black',se = F)+
  labs(x='Date',
       y='Bunk visit duration, seconds per visits',
       title = 'Averge bunk visit duration')+
  theme_classic()+
  theme(panel.grid.major = element_line(color = 'black', linewidth = 0.1),
        panel.grid.minor = element_line(color = 'grey', linewidth = 0.05),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0),
        legend.position = "right")
p.bvu

## Feed intake ----
p.erate = d.fbdaily %>% 
  ggplot(aes(x=as.Date(Date), y = ERkg.min))+
  geom_point(aes(color = VID))+
  geom_smooth(aes(color = VID), se = F)+
  geom_smooth(se = F)+
  labs(x = 'Date',
       y = 'Eating rate, kg per minute',
       title = 'Eating rate')+
  theme_classic()+
  theme(panel.grid.major = element_line(color = 'black', linewidth = 0.1),
        panel.grid.minor = element_line(color = 'grey', linewidth = 0.05),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0),
        legend.position = "right")
p.erate

p.intakedaily = d.fbdaily %>% 
  ggplot(aes(x = as.Date(Date), y = IntakeKG.tot, color = VID))+
  geom_point()+
  geom_smooth(se = F)+
  labs(x = 'Date',
       y = 'Daily intake, kg per day',
       title = 'Intake')+
  theme_classic()+
  theme(panel.grid.major = element_line(color = 'black', linewidth = 0.1),
        panel.grid.minor = element_line(color = 'grey', linewidth = 0.05),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0),
        legend.position = "right")
p.intakedaily

setorder(d.fbdaily, cols = 'VID','Date')
d.fbdaily[, cumDMI := cumsum(IntakeKG.tot), by = 'VID']

p.cumintake = d.fbdaily %>% 
  ggplot(aes(x =as.Date(Date), y = cumDMI, color = VID))+
  geom_point()+
  geom_line()+
  labs(x = 'Date',
       y = 'Cumulative intake, kg',
       title = 'Cumulative intake')+
  theme_classic()+
  theme(panel.grid.major = element_line(color = 'black', linewidth = 0.1),
        panel.grid.minor = element_line(color = 'grey', linewidth = 0.05),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0),
        legend.position = "right")

p.cumintake
