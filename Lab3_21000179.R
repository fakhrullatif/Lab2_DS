#read file in R
getwd()
data = read.csv("Churn_Train.csv",header=TRUE)

data$Total.Charges 
missing_values <- colSums(is.na(data))
# Print the count of missing values for each column
print(missing_values)

library(ggplot2)
library(dplyr)
library(cowplot)

#visualize the plot
ggplot(data, aes(Total.Charges) ) +
  geom_histogram(color = "#000000", fill = "#985732") +
  ggtitle("Variable distribution") +
  theme_classic() +
  theme(plot.title = element_text(size=18))

#activity 1 (imputed mean, median, 0)
value_imputed <- data.frame(
  original = data$Total.Charges,
  imputed_zero = replace(data$Total.Charges, is.na(data$Total.Charges), 0),
  imputed_mean = replace(data$Total.Charges, is.na(data$Total.Charges), mean(data$Total.Charges, na.rm = TRUE)),
  imputed_median = replace(data$Total.Charges, is.na(data$Total.Charges), median(data$Total.Charges, na.rm = TRUE))
)
value_imputed

h1 <- ggplot(value_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(value_imputed, aes(x = imputed_zero)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("Zero-imputed distribution") +
  theme_classic()
h3 <- ggplot(value_imputed, aes(x = imputed_mean)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("Mean-imputed distribution") +
  theme_classic()
h4 <- ggplot(value_imputed, aes(x = imputed_median)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Median-imputed distribution") +
  theme_classic()
plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

#activity 2 (using mice)
library(mice)
data_numeric <- data %>%
  select(Tenure,Monthly.Charges, Total.Charges)
md.pattern(data_numeric)

mice_imputed <- data.frame(
  original = data$Total.Charges,
  imputed_pmm = complete(mice(data_numeric, method = "pmm"))$Total.Charges,
  imputed_cart = complete(mice(data_numeric, method = "cart"))$Total.Charges,
  imputed_lasso = complete(mice(data_numeric, method = "lasso.norm"))$Total.Charges
  )
mice_imputed

h1 <- ggplot(mice_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(mice_imputed, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("Pmm-imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("Cart-imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso-imputed distribution") +
  theme_classic()
plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

#Activity 3 (using missForest)
library(missForest)
missForest_imputed <- data.frame(
  original = data_numeric$Total.Charges,
  imputed_missForest = missForest(data_numeric)$ximp$Total.Charges
)
missForest_imputed

h1 <- ggplot(missForest_imputed, aes(x = imputed_missForest)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("MissForest-imputed distribution") +
  theme_classic()
h2 <- ggplot(missForest_imputed, aes(x = original)) +
  geom_histogram(fill = "#143254", color = "#000000", position = "identity") +
  ggtitle("Original distribution") +
  theme_classic()

plot_grid(h1,h2, nrow = 2, ncol = 2)