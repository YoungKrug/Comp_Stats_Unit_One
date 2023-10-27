library(psych)
fetal_health_data <- read.csv("Comp_Stats_Unit_One/Main/Dataset/fetal_health.csv", header=TRUE)
fetal_health_scaled <- scale(fetal_health_data)
fa <- fa(r = fetal_health_scaled,
         nfactors = 4,
         rotate = "varimax")
summary(fa)