install.packages("pacman")
library(pacman)
p_load(tidyverse, stringr)

house_training_data <- read.csv("train.csv")
house_test_data <- read.csv("test.csv")

#Get the idea of Minimum and Maximum Price of the house along with mean and others.
summary(house_training_data$SalePrice)

#Add saleprice column to the test data. And assigned it to a new variable. 
house_test_data.SalePrice <- data.frame(SalePrice = rep(NA, nrow(house_test_data)), house_test_data[,])

#combined both the training and test data. It will be easier for analysis. From now on we will work on this dataset.
house_combined <- rbind(house_training_data, house_test_data.SalePrice)

dim(house_combined)  #Dimention of the combined dataset. 

library(ggplot2)
library(scales)

ggplot(house_combined[!is.na(house_combined$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma)
#With this plot we can say: 
#Few people can afford very expensive houses.
#Majority of people bought houses in the range 1,00,000 to 2,50,000.

#Now we have to find which attributes are more significant for SalePrice.

#summary(lm(SalePrice ~ . , data = house_combined))   #This is giving error because some variables have less than 2 levels.
#cor(house_combined)  #As correlation works on numeric type only so we cant check this on entire dataset. 

#We don't need ID. So drop ID column from house_combined
house_combined$Id <- NULL

#Here we have selected only those variables which has type numeric. Now we can check there correlation with SalePrice
numeric.type.variables <- which(sapply(house_combined, is.numeric))

cor(house_combined[, numeric.type.variables])  #Lot of NA's . 
#Need to replace them (with the mean values of the column would be a better option). 








#To check the significance value of the variables.  (All the variables are not included)
summary(lm(SalePrice ~ MSSubClass+MSZoning+Street+LotFrontage+LotArea+LotShape+LandContour+LotConfig+LandSlope+Neighborhood
           +Condition1+Condition2+BldgType+HouseStyle+OverallQual+OverallCond+YearBuilt+YearRemodAdd+RoofMatl
           +Exterior1st, data = house_training_data))

#Alley, Utilities

#Condition1 : alone, some values are significant. With others: Not significant
#OverallCond and YearBuilt is highly dependent (together).
#Roof Style we can ignore
#RoofMatl : alone not significant. Some significance with yearBuilt
