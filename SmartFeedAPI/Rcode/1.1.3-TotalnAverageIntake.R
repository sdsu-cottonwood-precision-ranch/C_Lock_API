# 1.1.3 - Average Daily Feed Intake

if(exists('d.fbdaily') == FALSE){
  d.fbdaily = fread(input = 'Data/112-DailyFB.csv')
}
d.fbdaily
d.bw[,list(initialBW=coef(lm(BWkg ~ tday))[1], ADG=coef(lm(BWkg~tday))[2]), by= 'VID']
# Calculate average and total feed intake -----
d.intake = d.fbdaily[, .(IntakeKG = mean(IntakeKG.tot, na.rm = T),
                             TotalIntakeKG = sum(IntakeKG.tot, na.rm = T)),
                     by = 'VID']
d.intake

fwrite(d.intake, file = 'Data/113-TotalIntake.csv')
