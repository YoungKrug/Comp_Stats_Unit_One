library("psych")
fetal_health_data <- read.csv("Dataset/fetal_health.csv", header=TRUE)
fetal_health_scaled <- scale(fetal_health_data)
fa <- fa(r = fetal_health_scaled,
         nfactors = 4,
         rotate = "varimax")
summary(fa)