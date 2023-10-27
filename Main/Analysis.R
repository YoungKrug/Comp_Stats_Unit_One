library("ggplot2")
library("grid")
library("reshape2")
source("API/ConductStatisticalTest.R")
source("API/DataReader.R")


#Read the file and create a new data reader class
data_reader <- DataReader$new("Dataset/fetal_health.csv")
#Construct the data analysis
data_analyzer <- DataAnalyzer$new(data_reader)
#Conduct statistical test **One way anovas, means, standard deviations
data_analyzer$conduct_oneway_anovas()
df = data_reader$Get_Data_Frame("abnormal_short_term_variability")
data_column = data_reader$list_of_column_data[["abnormal_short_term_variability"]]
data_column_2 = data_reader$list_of_column_data[["baseline_value"]]
mean = mean(df$abnormal_short_term_variability)
sd = sd(df$abnormal_short_term_variability)
df$sd = sd
df$mean = mean

#Conduct ks, ts, correlation test (pearsons, covariance), linear regression, manova
kruskal.test(data_column~data_column_2)
ggplot(df) +
  geom_bar(aes(x=abnormal_short_term_variability, y=mean), stat = "identity") +
  geom_errorbar(aes(x=abnormal_short_term_variability, ymin=mean-sd, ymax=mean+sd))
data_analyzer$conduct_t_test_on_all("bonferroni")
#Have to print to collect data
data_analyzer$conduct_t_test_on_all("hochberg", TRUE)
data_analyzer$conduct_kruskal_wallis_test("abnormal_short_term_variability", "baseline_value", TRUE)
data_analyzer$correlation_test("abnormal_short_term_variability", "baseline_value", "spearman", TRUE)
data_analyzer$correlation_test("abnormal_short_term_variability", "baseline_value", "pearson", TRUE)
data_analyzer$conduct_linear_regression("abnormal_short_term_variability", "baseline_value")
data_analyzer$conduct_manova("abnormal_short_term_variability",
                             "baseline_value", "fetal_movement")
data_analyzer$conduct_linear_regression("abnormal_short_term_variability", "baseline_value",
                                        "abnormal_short_term_variability")
df3 <- data_reader$list_of_column_data[["accelerations"]]
fetal_health <- data_reader$list_of_column_data[["fetal_health"]]
CompVal <- (0.5 * data_column) + (0.25 * data_column_2) + (0.25 * df3)
#ancova composite code
model <- aov(fetal_health~CompVal)
print(model)
# data_fetal_frame <- data.frame(
#   x = data_column,
#   y = data_column_2
# )
#
# gg <- ggplot(data_fetal_frame, aes(x, y))
# gg <- gg + geom_point()
# gg <- gg + geom_smooth(alpha=0.3, method="lm")
# print(gg)