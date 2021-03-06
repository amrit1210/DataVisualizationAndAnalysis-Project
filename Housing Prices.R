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

ggplot(house_training_data[!is.na(house_combined$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma)
#With this plot we can say: 
#Few people can afford very expensive houses.
#Majority of people bought houses in the range 1,00,000 to 2,50,000.

#Now we have to find which attributes are more significant for SalePrice.

#summary(lm(SalePrice ~ . , data = house_combined))   #This is giving error because some variables have less than 2 levels.
#cor(house_combined)  #As correlation works on numeric type only so we cant check this on entire dataset. 

#We don't need ID. So drop ID column from house_combined
house_training_data$Id <- NULL

#Here we have selected only those variables which has type numeric. Now we can check there correlation with SalePrice
numeric.type.variables <- which(sapply(house_training_data, is.numeric))
numeric.type.name.variables <- names(numeric.type.variables)

cor.numeric.variables <- cor(house_training_data[, numeric.type.variables], use="pairwise.complete.obs")  #Lot of NA's . 
#so we use="pairwise.complete.obs".
#Need to replace them (with the mean values of the column would be a better option). 

#sort the correlation with saleprice in decreasing order. So we will get the highly correlated variable at the top.
cor_sorted <- as.matrix(sort(cor.numeric.variables[,'SalePrice'], decreasing = TRUE))
colnames(cor_sorted)<- c("values")

#Select only high correlation
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
#cor.numeric.variables <- cor.numeric.variables[CorHigh, CorHigh]
#sort(cor.numeric.variables[,'SalePrice'], decreasing = TRUE)
#So we got "OverallQual" as the highly significant variable for Saleprice and after that we "GrLivArea" and so on..

model_OverallQual<-lm(SalePrice~OverallQual, data = house_training_data)
summary(model_OverallQual)
ggplot(house_training_data[!is.na(house_training_data$SalePrice),], aes(x= factor(OverallQual), y = SalePrice)) +
  geom_boxplot() + labs(x = "OverallQual", y = "SalePrice") + 
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

#We can Clearly see that increase in the overall quality of the house has increased the saleprice of the house.
house_training_data%>% ggplot(aes(x=OverallQual, y= SalePrice))+geom_point()+geom_smooth(method = "lm")+scale_y_continuous(breaks = seq(0,800000, by=100000))


model_GrLiveArea<-lm(SalePrice~GrLivArea, data = house_training_data)
summary(model_GrLiveArea)
ggplot(house_training_data[!is.na(house_training_data$SalePrice),], aes(x= GrLivArea, y = SalePrice)) +
  geom_point() + geom_smooth(method = "lm") +  labs(x = "GrLivArea", y = "SalePrice") + 
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)
#Next highly correlated variable was "GrLivArea" i.e ground living area square feet. Which also makes sense as the house with
#bigger living area will have high sale price. 
#The two dots at the bottom right seems to be the outliers.


factor.type.variables.names<- which(sapply(house_training_data, is.factor))%>% names()
model_Street_Neighborhood<-lm(SalePrice ~ Street+Neighborhood+GarageCond+KitchenQual+MiscFeature, data = house_training_data)
summary(model_Street_Neighborhood)
#Here, we can see that street and Neighbourhood are significant variables effecting the SalesPrice of an house. The dummy Variables StreetPave,
#NeighborhoodCollgCr, NeighborhoodCrawfor are most significant. The model is very good because it has a high R valué

summary(lm(SalePrice~Street+Neighborhood+OverallQual+GrLivArea+Condition1+Condition2+SaleCondition+SaleType+Heating, data = house_training_data))
#Here, we saw Condition1, Condition2, Saletype, SaleCondition and Heating are not that significant when grouped with Street and Neighbourhood. So we will not take this model.
#Also OverallQual and GrLiveArea when added to Street and Neighborhood are seen as significant for SalePrice. So we will group these 4 together and consider them to train our model for further prediction.

summary(lm(SalePrice~Street+Neighborhood+OverallQual+GrLivArea+Exterior1st+Exterior2nd+GarageCars+GarageArea+BsmtQual+TotalBsmtSF+BsmtCond+FullBath+BsmtFinType1+BsmtFinType2, data = house_training_data))
#BsmtQual, GarageCars and TotalBsmtSF are significant, while street became less significant. 
#So in our final model we are using Neighborhood, OverallQual, GrLiveArea, GarageCars, BsmtCond, TotalBsmtSF for prediction on our test data.

model_trained<-lm(SalePrice~Neighborhood+BsmtQual+OverallQual+GrLivArea+GarageCars+TotalBsmtSF, data = house_training_data)
#Our model is traine . Now we will predict SalePrice on test dataset
summary(model_trained)
pred_lm<-predict.lm(model_trained, house_test_data.SalePrice)

#Checking accuracy of our model
actual_preds<-data.frame(cbind(actuals=house_test_data.SalePrice$SalePrice, predicteds =  pred_lm))
corelation_accuracy<- cor(actual_preds)

#To check the significance value of the variables.  (All the variables are not included)
summary(lm(SalePrice ~ MSSubClass+MSZoning+Street+LotFrontage+LotArea+LotShape+LandContour+LotConfig+LandSlope+Neighborhood
           +Condition1+Condition2+BldgType+HouseStyle+OverallQual+OverallCond+YearBuilt+YearRemodAdd+RoofMatl
           +Exterior1st, data = house_training_data))
