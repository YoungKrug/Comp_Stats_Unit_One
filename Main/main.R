library("ggplot2")



set.seed(NULL)

distrubtion <- runif(5000, 0, 500)
#print(distrubtion)

framed_data <- data.frame(
  distrubtion
)

distrubtion.mean <- mean(distrubtion)
distrubtion.sd <- sd(distrubtion)
print(distrubtion.mean)
print(distrubtion.sd)
plot1 <- ggplot(data = framed_data, aes(x = distrubtion)) +
  geom_histogram(binwidth = 50)
print(plot1)