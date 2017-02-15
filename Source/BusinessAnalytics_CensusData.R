#Setting the Working Directory

setwd("G:\\Masters\\Data Analytics with R\\Project")

#Install the packages "rpart" and "ROCR" for the Regression Trees and Visualizing the Classifier Performance

library(rpart)
library(ROCR)

#Importing the Dataset 

#Dataset can be found in the Source\Dataset

Data = read.csv("G:\\Masters\\Data Analytics with R\\Project\\CensusData_Project\\CensusData.csv")

#Removing the Null Valued rows from the dataset

Data[Data == ' ?'] = NA         

GoodData = na.omit(Data) 

GoodData = data.frame(GoodData)

#Data Exploratory functions - To analyze the dataset

head(GoodData)
tail(GoodData)
str(GoodData)
summary(GoodData)

#Relationship between the Independent and Dependant(Income) Variables

Bp1 = boxplot(GoodData$age ~ GoodData$Income, xlab = "Income", ylab = "Age")
Bp2 = boxplot(GoodData$education.num ~ GoodData$Income, xlab = "Income", ylab = "No. of Educated People")
Bp3 = boxplot(GoodData$fnlwgt ~ GoodData$Income, xlab = "Income", ylab = "No. of Census Takers")
Bp4 = boxplot(GoodData$hours.per.week ~ GoodData$Income, xlab = "Income", ylab = "No. of hours per week")

#Removing the outliers from all the variables

Bp1$stats 
#Removing outliers of variable "age"
for(i in 1:nrow(GoodData))
{
  if(GoodData[i, "age"] > 73)
  {
    GoodData[i, "age"] = 73
  }
}

#Removing outliers for Education

Mean = mean(GoodData$education.num)
Sd = sd(GoodData$education.num)
MinLimit = Mean - 2 * Sd
MaxLimit = Mean + 2 * Sd
for(i in 1:nrow(GoodData))
{
  if(GoodData[i, "education.num"] > MaxLimit)
  {
    GoodData[i, "education.num"] = MaxLimit
  }
  else if(GoodData[i, "education.num"] < MinLimit)
  {
    GoodData[i,"education.num"] = MinLimit
  }
}

#Removing outliers for fnlwgt

Mean = mean(GoodData$fnlwgt)
Sd = sd(GoodData$fnlwgt)
MinLimit = Mean - 2 * Sd
MaxLimit = Mean + 2 * Sd
for(i in 1:nrow(GoodData))
{
  if(GoodData[i, "fnlwgt"] > MaxLimit)
  {
    GoodData[i, "fnlwgt"] = MaxLimit
  }
  else if(GoodData[i, "fnlwgt"] < MinLimit)
  {
    GoodData[i,"fnlwgt"] = MinLimit
  }
}

#Removing outliers for hours per week 

Mean = mean(GoodData$hours.per.week)
Sd = sd(GoodData$hours.per.week)
MinLimit = Mean - 2 * Sd
MaxLimit = Mean + 2 * Sd
for(i in 1:nrow(GoodData))
{
  if(GoodData[i, "hours.per.week"] > MaxLimit)
  {
    GoodData[i, "hours.per.week"] = MaxLimit
  }
  else if(GoodData[i, "hours.per.week"] < MinLimit)
  {
    GoodData[i,"hours.per.week"] = MinLimit
  }
}

#Using Stratified Data Sampling & Dividing the Data set to two - Testing & Training Data

GoodData_0 = subset(GoodData, Income == " <=50K")
GoodData_1 = GoodData[GoodData$Income == ' >50K',]

round(nrow(GoodData_0)*0.8,0)
round(nrow(GoodData_1)*0.8,0)

TrainData_0 = GoodData_0[1:(round(nrow(GoodData_0)*0.8,0)),]
TestData_0 = GoodData_0[((round(nrow(GoodData_0)*0.8,0))+1):nrow(GoodData_0),]

TrainData_1 = GoodData_1[1:(round(nrow(GoodData_1)*0.8,0)),]
TestData_1 = GoodData_1[((round(nrow(GoodData_1)*0.8,0))+1):nrow(GoodData_1),]

TestData = rbind(TestData_0, TestData_1)
TrainData = rbind(TrainData_0, TrainData_1)

table(TestData$Income)
table(TrainData$Income)
table(GoodData$Income)

#Performing the Decision Tree Modeling on Training Data

DecisionTree = rpart(Income ~ age + workclass + fnlwgt + education + education.num + marital.status + occupation + relationship + race + sex + capital.gain + capital.loss + hours.per.week + native.country, data = TrainData)

#Predicting the model using the Test Data

DecisionTree_Predict = predict(DecisionTree, TestData, type = "class")

CombinedTree = cbind(TestData$Income, DecisionTree_Predict)
colnames(CombinedTree) = c("Actual", "Predicted")
CombinedTree = data.frame(CombinedTree)

#Validating the performance of the model using confusion matrix & ROCR Curve

table(CombinedTree$Actual, CombinedTree$Predicted)

Prediction_Tree = prediction(as.numeric(DecisionTree_Predict), as.numeric(TestData$Income))
Performance_Tree = performance(Prediction_Tree, "tpr", "fpr")
plot(Performance_Tree)

#Performing The Logistic Regression model on Training Data

LogisticReg = glm(Income ~ age + workclass + fnlwgt + education + education.num + marital.status + occupation + relationship + race + sex + capital.gain + capital.loss + hours.per.week + native.country, data = TrainData, family="binomial")

#Predicting the model using the Test data

LogisticReg_Predict = predict(LogisticReg, TestData, type = "response")
LogisticReg_Predict = round(LogisticReg_Predict,0)

CombinedData = cbind(TestData$Income, LogisticReg_Predict)
colnames(CombinedData) = c("Actual", "Predicted")
CombinedData = data.frame(CombinedData)

#Validating the performance of the model using Confusion Matrix and ROCR Curve

table(CombinedData$Actual, CombinedData$Predicted)

Prediction_Reg = prediction(as.numeric(LogisticReg_Predict), as.numeric(TestData$Income))
Performance_Reg = performance(Prediction_Reg, "tpr", "fpr")
plot(Performance_Reg)
