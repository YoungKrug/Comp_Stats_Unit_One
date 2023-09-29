library("ggplot2")
library("grid")
library("reshape2")
source("API/ConductStatisticalTest.R")
source("API/DataReader.R")

data_reader <- DataReader$new("Dataset/fetal_health.csv")
data_analyzer <- DataAnalyzer$new(data_reader)
data_analyzer$conduct_oneway_anovas()
df = data_reader$Get_Data_Frame("abnormal_short_term_variability")
data_column = data_reader$list_of_column_data[["abnormal_short_term_variability"]]
data_column_2 = data_reader$list_of_column_data[["baseline_value"]]
mean = mean(df$abnormal_short_term_variability)
sd = sd(df$abnormal_short_term_variability)
df$sd = sd
df$mean = mean


# kruskal.test(data_column~data_column_2)
# ggplot(df) +
#   geom_bar(aes(x=abnormal_short_term_variability, y=mean), stat = "identity") +
#   geom_errorbar(aes(x=abnormal_short_term_variability, ymin=mean-sd, ymax=mean+sd))
# data_analyzer$conduct_t_test_on_all("bonferroni")
# #Have to print to collect data
# data_analyzer$conduct_t_test_on_all("hochberg", TRUE)
data_analyzer$conduct_kruskal_wallis_test("abnormal_short_term_variability", "baseline_value", TRUE)
data_analyzer$correlation_test("abnormal_short_term_variability", "baseline_value", "spearman", TRUE)
data_analyzer$correlation_test("abnormal_short_term_variability", "baseline_value", "pearson", TRUE)

data_fetal_frame <- data.frame(
  x = data_column,
  y = data_column_2
)

gg <- ggplot(data_fetal_frame, aes(x, y))
gg <- gg + geom_point()
gg <- gg + geom_smooth(alpha=0.3, method="lm")
print(gg)