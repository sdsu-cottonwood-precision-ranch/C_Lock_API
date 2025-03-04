# 3.1.1 - Feed to gain ----
# Libraries -----
library(data.table)
library(tidyverse)

# Data -----
if(exists('d.adg')==F){
  d.bw = fread(input = 'Data/214-AverageDailyGain.csv')
}

if(exists('d.intake')==F){
  d.fe = fread(input = 'Data/113-TotalIntake.csv')
}

# Merge data ----
names(d.bw)
names(d.fe)
setkey(d.bw, cols = 'VID')
setkey(d.fe, cols = 'VID')
d.bwfe = merge.data.table(d.bw, d.fe)
d.bwfe

d.bwfe[, `:=` (FGkg = TotalIntakeKG/TotalGain,
               GFkg = TotalGain/TotalIntakeKG)]
d.bwfe
