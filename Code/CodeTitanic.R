#SYS 6018: Applied Data Mining
#Competition 1: Titanic

#Importing the training and the testing datasets
traindata = read.csv("train.csv", stringsAsFactors = FALSE)
testdata = read.csv("test.csv", stringsAsFactors = FALSE)

#Understanding the dataset
str(traindata)
str(testdata)

#Combining train and test to one dataset for manipulation and creating features
train_copy = traindata
train_copy$Survived = NULL
data = rbind(train_copy, testdata)

#Filling in the missing values
data[data==""] = NA
#Age: Grouping by gender and passenger class and filling the median age for each group
#Loading the dplyr package
library(dplyr)
datacopy = data %>% group_by(Sex, Pclass) %>% mutate(AgeM = median(Age, na.rm = TRUE))
data$Age[is.na(data$Age)] = datacopy$AgeM[is.na(data$Age)]

#Fare
#Passenger on row 1044 has an NA Fare value. Let's replace it with the median fare value
data$Fare[1044] <- median(data$Fare, na.rm = TRUE)

#Embarkment
#Passenger on row 62 and 830 do not have a value for embarkment, Since many passengers embarked at Southampton, we give them the value S
data$Embarked[c(62, 830)] <- "S"

#Cabin
#We fill the NA cabin values with unknown "U"
data$Cabin[is.na(data$Cabin)] = "U"

#Creating a new column Title for each passenger
#Loading stringr package
library(stringr)

#Extracting Mr, Miss, Mrs, etc from Name
start = str_locate(data$Name, pattern = ',')
start = start[,1] + 2
end = str_locate(data$Name, pattern = '\\.')
end = end[,1] - 1
data$Title = str_sub(data$Name, start = start, end = end)

#Creating a new column Family Size: Sibsp + Parch + 1 (the person)
data$FamilySize <- data$SibSp + data$Parch + 1

#Relative Family Size: discreting the family size into small and large
data$SmallFam = NA
data$SmallFam[data$FamilySize <= 4] = 1
data$SmallFam[data$FamilySize > 4] = 0

#Creating a new variable Child to indicate whether it is a child or not
data$Child[data$Age <= 18] = 1
data$Child[data$Age > 18] = 0

#Converting the character variables to factors
data$Sex = as.factor(data$Sex)
data$Embarked = as.factor(data$Embarked)
data$Title = as.factor(data$Title)

#Splitting into training and testing data again
train = data[1:891,]
test = data[892:1309,]
#Adding the survivor column and converting it to a categorical variable
train$Survived = traindata$Survived
train$Survived = as.factor(train$Survived)

#Applying Random Forest Algorithm
#Loading the required package
library(randomForest)

#Set seed for reproducability
set.seed(0)

#Creating a random forest model
rf = randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + Child + SmallFam, data = train, importance = TRUE, ntree = 1000)

#Predicting using the test data
prediction = predict(rf, test)

#Creating a dataframe for the solutions
rf_solution <- data.frame(PassengerId = test$PassengerId, Survived = prediction)

#Writing to a csv file
write.csv(rf_solution, file = "rf_solution.csv", row.names = FALSE)

#Converting the categorical variables to factors
data$Pclass = as.factor(data$Pclass)
data$SmallFam = as.factor(data$SmallFam)
data$Child = as.factor(data$Child)

#Splitting into training and testing data again
train = data[1:891,]
test = data[892:1309,]
#Adding the survivor column and converting it to a categorical variable
train$Survived = traindata$Survived
train$Survived = as.factor(train$Survived)
#Replacing the title in the row with the title Dona from the test data with NA since that is not present in the train data
test$Title[test$Title=="Dona"] = NA

#Creating a logistic regression model
lg = glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + Child + SmallFam, data = train, family = "binomial")

#Summary of the model
summary(lg)

#Predicting on the testing data
probs<-as.vector(predict(lg, newdata = test, type="response"))
preds <- rep(0,418)  # Initialize prediction vector
preds[probs > 0.5] <- 1 # p>0.5 -> 1
#Predictions vector
preds

#Creating a dataframe for the solutions
lg_solution <- data.frame(PassengerId = test$PassengerId, Survived = preds)

#Writing to a csv file
write.csv(lg_solution, file = "lg_solution.csv", row.names = FALSE)

