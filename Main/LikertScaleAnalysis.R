library('HH')
library("psych")
library("dplyr")
iris_purchase_csv = read.csv("corrupted-extended_iris_data_sets/iris_csv_purchase.csv", header=TRUE)
iris_species = iris_purchase_csv$species
iris_likely_to_buy = iris_purchase_csv$likelytobuy
iris_color = iris_purchase_csv$color
df = data.frame(
  iris_species,
  iris_likely_to_buy
)
df$iris_likely_to_buy <- lapply(df$iris_likely_to_buy, function(x)
{
  if(x < 0)
  {
    x <- "Disagree"
  }
  else if(x > 0 && x < 2)
  {
    x <- "Neutrual"
  }
  else
  {
    x <- "Agree"
  }
  return(x)
})
amountOfAgree <- sum(df$iris_likely_to_buy == "Agree")
amountOfDisagree <- sum(df$iris_likely_to_buy == "Disagree")
amountOfNeutral <- sum(df$iris_likely_to_buy == "Neutrual")
amountTotal = amountOfAgree + amountOfDisagree + amountOfNeutral
values <- c(amountOfAgree/amountTotal, amountOfDisagree/amountTotal, amountOfNeutral/amountTotal)
new_df <- data.frame(
  Q1 <- values
)
leve <- c("Disagree", "Neutral", "Agree")
likert_results <- likert(new_df, levels = leve,  group.order=names(df),
breaks = c("Disagree", "Neutal", "Agree"), main="LikelyToBuy", xlab="Percentage", ylab="Reactions")

plot(likert_results)