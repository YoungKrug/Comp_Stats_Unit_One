
library("ggplot2")
covid_data <- read.csv("Dataset/covid_data_log_200908.csv", header=TRUE)
state <- covid_data$State
cases <- covid_data$Cases
deaths <- covid_data$Deaths
poverty <- covid_data$Poverty
white_males <- covid_data$W_Male


cluster_frame <- data.frame(
  state,
  cases,
  deaths
)
#scatter plot
ggplot(covid_data, aes(cases, deaths, color = state)) +
  geom_point() +
  labs(title = "Scatter Plot with Different Colors for Each State",
       x = "cases",
       y = "deaths")

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

set.seed(2)
mytest <-  kmeans(subframe, 3, nstart = 3)
myplot1 <-ggplot(covid_data, aes(cases, deaths, color = state)) + geom_point()
myplot3 <-ggplot(covid_data, aes(poverty, white_males, color = state)) + geom_point()
myKMEANS <- summary(mytest)
print(myKMEANS)
mytest$cluster <- as.factor(mytest$cluster)
myplot2 <-ggplot(covid_data, aes(cases, deaths, color = mytest$cluster)) + geom_point()
myplot4 <-ggplot(covid_data, aes(poverty, white_males, color = mytest$cluster)) + geom_point()
library('grid')
pushViewport(viewport(layout = grid.layout(2, 2)))
print(myplot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(myplot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(myplot3, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(myplot4, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))