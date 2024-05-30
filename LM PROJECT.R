data <- read.csv("C:/Users/bhama/OneDrive/Desktop/data.csv")
 View(data)
 # Check the structure of the data
 str(data)
 # Summary statistics
 summary(data)
 #EDA
 # Load necessary libraries
 library(dplyr)
 library(ggplot2)
 library(corrplot)
 # Summary statistics
 summary(data)
 
 # Histograms of all variables
 # Adjusting margins and plot size
 par(mfrow=c(4,4))
 par(mar=c(2, 2, 2, 2))  # Adjusting margin size
 
 # Loop to create histograms
 for (i in 2:13) {
   hist(data[,i], main = paste("Histogram of", names(data)[i]))
 }
 # Boxplot of each variable
 par(mfrow=c(4,4))
 for (i in 2:13) {
   boxplot(data[,i], main = paste("Boxplot of", names(data)[i]))
 }
 
 # Split the data into training and testing sets
 set.seed(123) # for reproducibility
 train_index <- sample(1:nrow(data), 0.7 * nrow(data))
 train_data <- data[train_index, ]
 test_data <- data[-train_index, ]
 
 # Build the multiple linear regression model
 model <- lm(calories ~ total_fa + sodi + fol + niac + riboflav + vitamin_ + vitamin + calci + magnesi + potassi + protei + wate, data = train_data)
 # Assess goodness of fit
 summary(model)
 
 # Check assumptions
 plot(model)
 

 

 
 

 
 