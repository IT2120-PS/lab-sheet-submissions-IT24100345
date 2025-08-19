setwd("C:\\Users\\IT24100345\\Desktop\\lab4")

branch_data <- read.csv("Exercise.txt", header = TRUE)

str(branch_data)

fix(branch_data)

boxplot(branch_data$Sales_X1, main="Boxplot of Sales", ylab = "Sales Amount")

summary(branch_data$Advertising_X2)
IQR(branch_data$Advertising_X2)

find_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  
  iqr <- Q3 - Q1
  
  lower_bound <- Q1 - 0.5 * iqr
  upper_bound <- Q3 + 0.5 * iqr
  
  outliers <- x[x < lower_bound | x > upper_bound]
  return(outliers)
}

outliers_years <- find_outliers(branch_data$Years_X3)
outliers_years