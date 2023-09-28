library("ggplot2")
library("grid")
library("reshape2")
source("Comp_Stats_Unit_One/Main/API/DataReader.R")
source("Comp_Stats_Unit_One/Main/API/ConductStatisticalTest.R")

data_reader <- DataReader$new("Comp_Stats_Unit_One/Main/Dataset/fetal_health.csv")
data_analyzer <- DataAnalyzer$new(data_reader)
data_analyzer$conduct_oneway_anovas()
df = data_reader$Get_Data_Frame("abnormal_short_term_variability")
data_column = data_reader$list_of_column_data["abnormal_short_term_variability"]
mean = mean(df$abnormal_short_term_variability)
sd = sd(df$abnormal_short_term_variability)
df$sd = sd
df$mean = mean
ggplot(df) +
  geom_bar(aes(x=abnormal_short_term_variability, y=mean), stat = "identity") +
  geom_errorbar(aes(x=abnormal_short_term_variability, ymin=mean-sd, ymax=mean+sd))
data_analyzer$conduct_t_test_on_all("bonferroni")
#Have to print to collect data
data_analyzer$conduct_t_test_on_all("hochberg")