#Project 2
#Import the necessary libraries
library(caret)
library(inTrees)
library(C50)
library(randomForest)
library(dplyr)
library(corrgram)
library(MASS)
library(party)
library(DMwR)

#Import the dataset
train_data = read.csv('../robinredhu/Downloads/Train_data.csv')
test_data = read.csv('../robinredhu/Downloads/Test_data.csv')

#Categorising data
View(train_data)
summary(train_data)
glimpse(train_data)
View(test_data)
summary(test_data)
glimpse(test_data)
#Converting area code to factor variable
train_data$area.code = as.factor(train_data$area.code)
test_data$area.code = as.factor(test_data$area.code)

#Missing value analysis
sum(is.na(train_data))
sum(is.na(test_data))

#Feature Selection
#Now for this first of all we need to seperate out numerical and categorical variables
num_index = which(unlist(sapply(train_data, function(x) (class(x) == 'numeric'||class(x) == 'integer'))))
train_num = train_data[,num_index]
train_cat = train_data[,-num_index]
#Correlation matrix will help to find out and remove multi collinearity
corrgram(train_num, order = F, upper.panel = panel.pie, text.panel = panel.txt, main = 'Corelation Matrix')
# From the results obtained we will remove total.day.charge, total.eve.charge, total.night.charge, total.intl.charge variables

#Chi-Sqr test will help to remove unnecessary categorical variables
for(i in 1:ncol(train_cat)){
  print(names(train_cat[i]))
  print(chisq.test(table(train_cat$Churn, train_cat[,i])))
}
# From the results obtained we will remove  area.code, phone.number variables
# c('area.code', 'phone.number', 'total.day.charge', 'total.eve.charge', 'total.night.charge', 'total.intl.charge')
train_data = train_data[,-c(3,4,10,13,16,19)]
test_data = test_data[,-c(3,4,10,13,16,19)]

#Convert all string variables to numeric
for(i in 1:ncol(train_data)){
  if(class(train_data[,i]) == 'factor'){
    train_data[,i] = factor(train_data[,i], labels = 1:length(levels(factor(train_data[,i]))))
  }
}
for(i in 1:ncol(test_data)){
  if(class(test_data[,i]) == 'factor'){
    test_data[,i] = factor(test_data[,i], labels = 1:length(levels(factor(test_data[,i]))))
  }
}


#After pre processing It's time to train the model
#Train model usign logistic regression
logistic_model = glm(Churn~., data = train_data, family = 'binomial')
summary(logistic_model)

prediction = predict(logistic_model, test_data[,-15], type ='response')
prediction = ifelse(prediction>=0.5,1,0)

#Confusion matrix
table(test_data$Churn, prediction)
accuracy_log = ((1397+56)/nrow(test_data))*100
result1 = confusionMatrix(table(test_data$Churn, prediction))
#Logistic Regression Results
#To calculate F1 score 
result1$byClass[7]
#Precision score 
result1$byClass[5]
#Recall score 
result1$byClass[6]

#Using Decision Tree
tree2 = C5.0(Churn~., train_data, trials = 100, rules =T) 
c5_prediction = predict(tree2, test_data[,-15], type = 'class')
summary(tree2)
#Confusion matrix
table(test_data$Churn, c5_prediction)
accuracy_c5.0 = ((1438+159)/nrow(test_data))*100
result2 = confusionMatrix(table(test_data$Churn, c5_prediction))
#Decision Tree Results of C5.0
#To calculate F1 score 
result2$byClass[7]
#Precision score 
result2$byClass[5]
#Recall score 
result2$byClass[6]

#Now we will try for random forest
rand_model = randomForest(Churn~., train_data, importance = T, ntree =200)
rand_model
#Extract rules from trees
treeList = RF2List(rand_model)
rules = extractRules(treeList, X = test_data[, -15])
rules
rf_prediction = predict(rand_model, test_data[, -15])
#Confusion matrix
table(test_data$Churn, rf_prediction)
accuracy_rf = ((1436+67)/nrow(test_data))*100
plot(rand_model)
result3 =confusionMatrix(table(test_data$Churn, rf_prediction))
#Random Forest Results
#To calculate F1 score
result3$byClass[7]
#Precision score
result3$byClass[5]
#Recall score
result3$byClass[6]
