library('mixtools')
library("ggplot2")
covid_data <- read.csv("Comp_Stats_Unit_One/Main/Dataset/covid_data_log_200908.csv", header=TRUE)
state <- covid_data$State
cases <- covid_data$Cases
deaths <- covid_data$Deaths
poverty <- covid_data$Poverty
white_males <- covid_data$W_Male

dataframe <- data.frame(
  state,
  cases,
  deaths,
  poverty,
  white_males
)


subframe <- data.frame(
  cases,
  deaths,
  poverty,
  white_males
)
All = (cases + deaths + poverty + white_males)/4
#set.seed(30)
fitGMM <- normalmixEM(All)
print(fitGMM)
fitGMM_loglik <- fitGMM$loglik
BIC_GMM <- -2*fitGMM_loglik+4*log(150)
print ("BIC for GMM")
print(BIC_GMM)
plot(fitGMM, which = 1)
plot(fitGMM, which = 2)
sink("stats.txt")
print(fitGMM)
print ("BIC for GMM")
print(BIC_GMM)
sink()
