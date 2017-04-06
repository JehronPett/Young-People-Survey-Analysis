
library(caTools)
library(dplyr)
library(pROC)
library(class)
library(plyr)
library(rpart)
library(rpart.plot)

responses <- read.csv("responses.csv")
responses <- mutate(responses, is_female = as.numeric(responses$Gender == 'female')) #Make Gender Binary

#Split the dataset into training and testing at 70/30
set.seed(123)
split <- sample(nrow(responses), 0.7*nrow(responses))
train <- responses[split,]
test <- responses[-split,]

#Find Baseline
summary(test$Gender)

# Logistic Regression -----------------------------------------------------
#Using a Logistic Regression Model, I'd like to predict the 
#gender of a person taking this survey based on their
#feelings towards Romantic Movies, Musicals, Shopping,
#War Movies, and Computers.
logistic_model <- glm(is_female ~ Romantic 
                      + Shopping + War + PC + Musical
                      + Height + Weight,family=binomial,data=train)
log_pred <- predict(object = logistic_model, newdata = test) #Make logistic regression prediction model
roc_curve <- roc(response=test$is_female, predictor = log_pred) #Create an ROC curve
plot(roc_curve) #Plot the ROC curve
auc(roc_curve) #Compute the area under the ROC curve
log_pred <- (log_pred > 0) #Create a vector of predictions
mean(log_pred == test$is_female, na.rm = TRUE) #Compute the accuracy of the predictions


# k-Nearest Neighbors -----------------------------------------------------
#Using a k-Nearest Neighbors Model, I'd like to predict
#the gender of a person taking this survey based on their 
#height and weight.

knn_train <- na.omit(train[,c("Height", "Weight", "Gender")])
knn_test <- na.omit(test[,c("Height", "Weight", "Gender")])

knn_trainLabels <- knn_train$Gender
knn_testLabels <- knn_test$Gender
knn_train <- knn_train[,-3]
knn_test <- knn_test[,-3]

# kNN Classifier
classifier_22 <- knn(train = knn_train, test = knn_test, cl = knn_trainLabels, k = 20)
mean(classifier_22 == knn_testLabels)

# Decision Tree -----------------------------------------------------------

GenderTree <- rpart(Gender ~ Height + Weight, data = train, method="class", minbucket=25)
prp(GenderTree)
predictGender <- predict(GenderTree, newdata = test, type = "class")
mean(test$Gender == predictGender)

# Linear Regression -------------------------------------------------------
#Using a Linear Regression Model, I'd like to predict the Weight of 
#a person, given their height.
height_weight <- na.omit(responses[,c('Height','Weight')]) #Subset our dataset to just Height and Weight
cor(height_weight$Height, height_weight$Weight) #Find the correlation between the two columns

model <- lm(Weight ~ Height, data=train) #Create the linear regression model
pred <- predict(model, test) #Make the prection

sqrt(mean((log(pred)-log(test$Weight))^2, na.rm=TRUE)) #Compute the RMSLE

weight_baseline <- plyr::count(test, vars='Weight') #Find frequency of each unique value in the Weight column
weight_baseline <- weight_baseline[which(weight_baseline$freq == max(weight_baseline$freq, na.rm = TRUE)),] #Find most frequent
sqrt(mean((log(weight_baseline[1,1]) - log(test$Weight))^2, na.rm = TRUE)) #Calculate the RMSLE of this baseline
