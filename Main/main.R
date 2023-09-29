library("ggplot2")
library("reshape2")


set.seed(NULL)
distrubtion <- rnorm(5000, 40, 20)
range <- rnorm(5000)
#print(distrubtion)
framed_data <- data.frame(
  distrubtion
)
histogram <- hist(framed_data$distrubtion,
     probability = TRUE,
)
# Lines funtion needs to be separate
lines <- lines(density(framed_data$distrubtion))
# print(histogram)
# print(lines)


sink("desc.txt")
distrubtion.mean <- mean(distrubtion)
distrubtion.sd <- sd(distrubtion)



sprintf("mean %f",  distrubtion.mean)
sprintf("standard deviation %f", distrubtion.sd)
sink()

matrix_distribution <- matrix(distrubtion)
print(matrix_distribution)
y <- apply(matrix_distribution, 2, mean)
print(y)
y.sd <- apply(matrix_distribution, 2, sd)
print(y.sd)
mid <- barplot(y)

data.frame(
  name <- range,
  value <- 10,
  sd <- standard_deviations
)



# ggplot(framed_data) +
 #   geom_bar( aes(x=name, y=value), stat="identity", fill="skyblue", alpha=0.7) +
  #  geom_errorbar( aes(x=name, ymin=value-distrubtion.sd, ymax=value+distrubtion.sd),
 #                  width=0.4, colour="orange", alpha=0.9)

# Add error bars
# arrows(x0=x, y0=distrubtion.mean-distrubtion.sd, x1=x, y1=y+distrubtion.sd, code=3, angle=90, length=0.1)


# plot1 <- ggplot(data = framed_data, aes(x = distrubtion)) +
# geom_histogram(binwidth = 15)
# print(plot1)