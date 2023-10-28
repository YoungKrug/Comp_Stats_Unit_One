library('mixtools')
library("ggplot2")
covid_data <- read.csv("Dataset/covid_data_log_200908.csv", header=TRUE)
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
fitNORM <- fitdistr(All, densfun="normal")
fitEXP <- fitdistr(All, densfun="exponential")
fitLNORM <- fitdistr(All, densfun="log-normal")
fitGMM <- normalmixEM(All)
fitGMM_loglik <- fitGMM$loglik
BIC_GMM <- -2*fitGMM_loglik+4*log(500000) # 4 parameters and large sample size
BICfit <- BIC(fitNORM,fitLNORM,fitEXP)
print ("BIC for GMM")
print(BIC_GMM)
myplot1 <-ggplot(dataframe, aes(x=All)) + geom_histogram(aes(y=..density..)) + geom_density() + stat_function(fun=dnorm, color="red", args=list(mean = fitNORM$estimate[1], sd = fitNORM$estimate[2]))
myplot2 <-ggplot(dataframe, aes(x=All)) + geom_histogram(aes(y=..density..)) + geom_density() + stat_function(fun=dlnorm, color="red", args=list(meanlog = fitLNORM$estimate[1], sdlog = fitLNORM$estimate[2]))
myplot3 <-ggplot(dataframe, aes(x=All)) + geom_histogram(aes(y=..density..)) + geom_density() + stat_function(fun=dexp, color="red", args=list(rate = fitEXP$estimate[1]))

# for GMM two density functions
myplot4 <-ggplot(dataframe, aes(x=All)) + geom_histogram(aes(y=(..density..))) + geom_density(aes(y=(..density..))) +
   stat_function(fun=dnorm, color="red", args=list(mean = fitGMM$mu[1], sd = fitGMM$sigma[1])) +
   stat_function(fun=dnorm, color="red", args=list(mean = fitGMM$mu[2], sd = fitGMM$sigma[2]))

#View Port plots
library('grid')
pushViewport(viewport(layout = grid.layout(2, 2)))
print(myplot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(myplot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(myplot3, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(myplot4, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
sink("stats.txt")
print(fitGMM)
print(BICfit)
print ("BIC for GMM")
print(BIC_GMM)
sink()

print("goodbye Greg...leaving R")
#q()