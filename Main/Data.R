library("ggplot2")
library("grid")
library("reshape2")

sepsis_data <- read.table("dataset.csv",sep=",", header=TRUE)
# header = True alows the first variables to be read as column variables
# this allows us to actually properly superset ($)

time_eclisped <- sepsis_data$Hour
heart_rate <- sepsis_data$HR
oxygen_saturation <- sepsis_data$O2Sat
tempature <- sepsis_data$Temp
systolic_blood_pressure <- sepsis_data$SBP
mean_arterial_pressure <- sepsis_data$MAP
diastolic_blood_pressure <- sepsis_data$DBP
respiration_rate <- sepsis_data$Resp
End_tidal_carbon_dioxide <- sepsis_data$EtC02
excess_bicarbonate <- sepsis_data$BaseExcess
HC03_biconarbonate <- sepsis_data$HC03

frame <- data.frame(
  heart_rate,
  oxygen_saturation,
  tempature,
  systolic_blood_pressure,
  diastolic_blood_pressure,
  mean_arterial_pressure,
  respiration_rate
)
#print(frame)
#print(summary(frame))


# plot1<-ggplot(sepsis_data, aes(x=heart_rate, y=mean_arterial_pressure)) +
#   geom_point()
# print(plot1)
#
# plot2<-ggplot(sepsis_data, aes(x=diastolic_blood_pressure, y=oxygen_saturation)) +
#   geom_point()
# print(plot2)

heart_rate.anova <- oneway.test(heart_rate~time_eclisped)
systolic_blood_pressure.anova <- oneway.test(systolic_blood_pressure~time_eclisped)
mean_arterial_pressure.anova <- oneway.test(mean_arterial_pressure~time_eclisped)
respiration_rate.anova <- oneway.test(respiration_rate~time_eclisped)
print(heart_rate.anova)
print(systolic_blood_pressure.anova)
print(mean_arterial_pressure.anova)
print(respiration_rate.anova)

# pairwise.t.test(heart_rate~time_eclisped, )