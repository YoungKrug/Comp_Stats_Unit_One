library("ggplot2")
library("grid")
library("reshape2")

sepsis_data <- read.table("Comp_Stats_Unit_One/Main/Dataset.csv",sep=",", header=TRUE)
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

heart_rate.melt = melt(heart_rate)
time_eclisped.melt <- melt(time_eclisped)
heart_rate.anova = oneway.test(heart_rate~time_eclisped)
print(melt(heart_rate.anova))
systolic_blood_pressure.anova = oneway.test(systolic_blood_pressure~time_eclisped)
mean_arterial_pressure.anova = oneway.test(mean_arterial_pressure~time_eclisped)
respiration_rate.anova= oneway.test(respiration_rate~time_eclisped)

heart_rate.sd <- sd(heart_rate)
systolic_blood_pressure.sd <- sd(systolic_blood_pressure)
mean_arterial_pressure.sd <- sd(mean_arterial_pressure)
respiration_rate.sd <- sd(respiration_rate)

standard_deviations <- c(heart_rate.sd, mean_arterial_pressure.sd,
                         respiration_rate.sd, mean_arterial_pressure.sd)


data_anova <- c(heart_rate.anova, systolic_blood_pressure.anova,
          mean_arterial_pressure.anova, respiration_rate.anova)

data.anova <- data.frame(
  name <- letters[1: length(data_anova)],
  value <- data_anova,
  sd <- standard_deviations
)

ggplot(data.anova) +
    geom_bar( aes(x=name, y=value), stat="identity", fill="skyblue", alpha=0.7) +
    geom_errorbar( aes(x=name, ymin=value-sd, ymax=value+sd), width=0.4, colour="orange", alpha=0.9, linewidth=1.3)


sink("T-Testdata.txt")
pairwise <- pairwise.t.test(heart_rate, mean_arterial_pressure)
bonferroni <- pairwise.t.test(heart_rate, mean_arterial_pressure, p.adjust.method = "bonferroni")
hochberg <- pairwise.t.test(heart_rate, mean_arterial_pressure, p.adjust.method = "hochberg")
print(pairwise)
print(bonferroni)
print(hochberg)
unsink()


data.new <- melt(sepsis_data)
# print(heart_rate.anova)
# print(systolic_blood_pressure.anova)
# print(mean_arterial_pressure.anova)
# print(respiration_rate.anova)
#pairwise.t.test(heart_rate~time_eclisped, )
print(data.new)