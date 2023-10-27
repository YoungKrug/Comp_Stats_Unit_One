library(psych)
fetal_health_data <- read.csv("Comp_Stats_Unit_One/Main/Dataset/fetal_health.csv", header=TRUE)

abnormal_stv <- fetal_health_data$abnormal_short_term_variability
fetal_movement <- fetal_health_data$fetal_movement
baseline <- fetal_health_data$baseline_value
accelerations <- fetal_health_data$accelerations
histogram_peaks <- fetal_health_data$histogram_number_of_peaks
light_decelarations <- fetal_health_data$light_decelerations

df <- data.frame(
  abnormal_stv,
  fetal_movement,
  baseline,
  accelerations,
  histogram_peaks,
  light_decelarations
)
myPCA <-  prcomp(df, cor=TRUE)

plot(df)


