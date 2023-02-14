
##title: "Indian Diabetes Analysis"
##author: 'CSIS3360-001 Group #5'
##date: "25/03/2021"



## ASSOCIATION RULES
### 1. Loading data from .csv file

#Uploading a dataset in R workspace  and take a look at the first 6 rows:

diabetes_data <- read.csv('diabetes(data).csv')
head(diabetes_data)
str(diabetes_data) # check the loaded data frame


### 2. Installing & opening required packages

# Installing Packages 
#install.packages("arules") 
#install.packages("arulesViz") 

#Loading package 
library(arules)
library(arulesViz)


### 3.Train the apriori and fit the model visualize results


# Fitting model 
# Training Apriori on the dataset 

set.seed = 220 # Setting seed 
associa_rules = apriori(data = diabetes_data,  
                        parameter = list(support = 0.004,  confidence = 0.2))

### 4. Visualizing the result

plot(associa_rules, method = "graph", measure = "confidence", shading = "lift")   



## Classification (Naive-Bayes model)

### 1. Installing and adjusting packages


#install.packages("naivebayes")
#install.packages("forecast")
#install.packages("gains")
#install.packages("caret")
#install.packages("e1071")
#install.packages("pROC")

#loading packages for Classification
library("knitr")
library("naivebayes")
library("forecast")
library("gains")
library("caret")
library("e1071")
library("pROC")


### 2. Loading data from .csv file


diabetes_data <- read.csv('diabetes(data).csv', stringsAsFactors=T)
str(diabetes_data) # check the loaded data frame


### 3. Data pre-processing works.

#Factorizing each Column (because it is simple numerical data, not a factor).


## Factorizing "Pregnancies" Column.
diabetes_data <- within(diabetes_data, {
  stat_Preg = character(0)
  stat_Preg[ Pregnancies == 0 ] = "0"
  stat_Preg[ Pregnancies >= 1 & Pregnancies < 4 ] = "Between 1 and 3"
  stat_Preg[ Pregnancies >= 4 & Pregnancies < 7 ] = "Between 4 and 6"
  stat_Preg[ Pregnancies >= 7 & Pregnancies < 10 ] = "Between 7 and 9"
  stat_Preg[ Pregnancies >= 10 ] = "Greater than or equal to 10"
  stat_Preg = factor(stat_Preg, level = c("0", "Between 1 and 3", "Between 4 and 6", "Between 7 and 9", "Greater than or equal to 10"))
})

## Factorizing "Glucose" Column.
diabetes_data <- within(diabetes_data, {
  stat_Glu = character(0)
  stat_Glu[ Glucose < 41 ] = "Less than 41"
  stat_Glu[ Glucose >= 41 & Glucose < 81 ] = "Between 41 and 80"
  stat_Glu[ Glucose >= 81 & Glucose < 121 ] = "Between 81 and 120"
  stat_Glu[ Glucose >= 121 & Glucose < 161 ] = "Between 121 and 160"
  stat_Glu[ Glucose >= 161 & Glucose < 201  ] = "Between 161 and 200"
  stat_Glu = factor(stat_Glu, level = c("Less than 41", "Between 41 and 80", "Between 81 and 120", "Between 121 and 160", "Between 161 and 200"))
})

## Factorizing "BloodPressure" Column.
diabetes_data <- within(diabetes_data, {
  stat_Blood = character(0)
  stat_Blood[ BloodPressure < 26 ] = "Less than 26"
  stat_Blood[ BloodPressure >= 26 & BloodPressure < 51 ] = "Between 26 and 50"
  stat_Blood[ BloodPressure >= 51 & BloodPressure < 76 ] = "Between 51 and 75"
  stat_Blood[ BloodPressure >= 76 & BloodPressure < 101 ] = "Between 76 and 100"
  stat_Blood[ BloodPressure >= 101 & BloodPressure < 126  ] = "Between 101 and 125"
  stat_Blood = factor(stat_Blood, level = c("Less than 26", "Between 26 and 50", "Between 51 and 75", "Between 76 and 100", "Between 101 and 125"))
})

## Factorizing "SkinThickness" Column.
diabetes_data <- within(diabetes_data, {
  stat_Skin = character(0)
  stat_Skin[ SkinThickness < 21 ] = "Less than 21"
  stat_Skin[ SkinThickness >= 21 & SkinThickness < 41 ] = "Between 21 and 40"
  stat_Skin[ SkinThickness >= 41 & SkinThickness < 61 ] = "Between 41 and 60"
  stat_Skin[ SkinThickness >= 61 & SkinThickness < 81 ] = "Between 61 and 80"
  stat_Skin[ SkinThickness >= 81 & SkinThickness < 101  ] = "Between 81 and 100"
  stat_Skin = factor(stat_Skin, level = c("Less than 21", "Between 21 and 40", "Between 41 and 60", "Between 61 and 80", "Between 81 and 100"))
})

## Factorizing "Insulin" Column.
diabetes_data <- within(diabetes_data, {
  stat_Ins = character(0)
  stat_Ins[ Insulin < 201 ] = "Less than 201"
  stat_Ins[ Insulin >= 201 & Insulin < 401 ] = "Between 201 and 400"
  stat_Ins[ Insulin >= 401 & Insulin < 601 ] = "Between 401 and 600"
  stat_Ins[ Insulin >= 601 & Insulin < 801 ] = "Between 601 and 800"
  stat_Ins[ Insulin >= 801 & Insulin < 1001 ] = "Between 801 and 1000"
  stat_Ins = factor(stat_Ins, level = c("Less than 201", "Between 201 and 400", "Between 401 and 600", "Between 601 and 800", "Between 801 and 1000"))
})

## Factorizing "BMI" Column.
diabetes_data <- within(diabetes_data, {
  stat_BMI = character(0)
  stat_BMI[ BMI < 16 ] = "Less than 16"
  stat_BMI[ BMI >= 16 & BMI < 31 ] = "Between 16 and 30"
  stat_BMI[ BMI >= 31 & BMI < 46 ] = "Between 31 and 45"
  stat_BMI[ BMI >= 46 & BMI < 61 ] = "Between 46 and 60"
  stat_BMI[ BMI >= 61 & BMI < 81  ] = "Between 61 and 80"
  stat_BMI = factor(stat_BMI, level = c("Less than 16", "Between 16 and 30", "Between 31 and 45", "Between 46 and 60", "Between 61 and 80"))
})

## Factorizing "DiabetesPedigreeFunction" Column.
diabetes_data <- within(diabetes_data, {
  stat_PedFunc = character(0)
  stat_PedFunc[ DiabetesPedigreeFunction < 0.6 ] = "Less than 0.6"
  stat_PedFunc[ DiabetesPedigreeFunction >= 0.6 & DiabetesPedigreeFunction < 1.1 ] = "Between 0.6 and 1.0"
  stat_PedFunc[ DiabetesPedigreeFunction >= 1.1 & DiabetesPedigreeFunction < 1.6 ] = "Between 1.1 and 1.5"
  stat_PedFunc[ DiabetesPedigreeFunction >= 1.6 & DiabetesPedigreeFunction < 2.1] = "Between 1.6 and 2.0"
  stat_PedFunc[ DiabetesPedigreeFunction >= 2.1 & DiabetesPedigreeFunction < 2.6  ] = "Between 2.1 and 2.5"
  stat_PedFunc = factor(stat_PedFunc, level = c("Less than 0.6", "Between 0.6 and 1.0", "Between 1.1 and 1.5", "Between 1.6 and 2.0", "Between 2.1 and 2.5"))
})

## Factorizing "Age" Column.
diabetes_data <- within(diabetes_data, {
  stat_Age = character(0)
  stat_Age[ Age < 21 ] = "Less than 21"
  stat_Age[ Age >= 21 & Age < 41 ] = "Between 21 and 40"
  stat_Age[ Age >= 41 & Age < 61 ] = "Between 41 and 60"
  stat_Age[ Age >= 61 & Age < 81 ] = "Between 61 and 80"
  stat_Age[ Age >= 81 & Age < 101  ] = "Between 81 and 100"
  stat_Age = factor(stat_Age, level = c("Less than 21", "Between 21 and 40", "Between 41 and 60", "Between 61 and 80", "Between 81 and 100"))
})

diabetes_data[1:10, c(10:12)]
diabetes_data[1:10, c(13:15)]
diabetes_data[1:10, c(16:17, 9)]




#Set the Factors for Outcome column

diabetes_data$Outcome <- factor(diabetes_data$Outcome, levels=c("0","1"))




### 4. Presenting distribution table with X - Y factors

#a) Distribution table (Pregnancies - Satisfaction)

xtabs(~stat_Preg+Outcome, data = diabetes_data)


#b) Distribution table (Glucose - Satisfaction)
xtabs(~stat_Glu+Outcome, data = diabetes_data)


#c) Distribution table (BloodPressure - Satisfaction)
xtabs(~stat_Blood+Outcome, data = diabetes_data)


#d) Distribution table (SkinThickness - Satisfaction)
xtabs(~stat_Skin+Outcome, data = diabetes_data)


#e) Distribution table (Insulin - Satisfaction)
xtabs(~stat_Ins+Outcome, data = diabetes_data)


#f) Distribution table (BMI - Satisfaction)
xtabs(~stat_BMI+Outcome, data = diabetes_data)


#g) Distribution table (DiabetesPedigreeFunction - Satisfaction)
xtabs(~stat_PedFunc+Outcome, data = diabetes_data)


#h) Distribution table (Age - Satisfaction)
xtabs(~stat_Age+Outcome, data = diabetes_data)



### 5. Creating training and validating data for Naive-Bayes modeling

#Setting 'select' variable to fix columns which will be used for testing (Xs).

#- 10th column = Pregnancies
#- 11th column = Glucose
#- 12th column = BloodPressure
#- 13th column = SkinThickness
#- 14th column = Insulin
#- 15th column = BMI
#- 16th column = DiabetesPedigreeFunction
#- 17th column = Age
#- 9th column = Outcome

selected.var <- c(10:17, 9) 


#making index variable for splitting up training and validating data frame.

# size of index = 768 (without 3 rows for testing)
# range of index = from 1 to 768 (without 3 rows for testing)

train.index <- sample(c(1:768), 768)


#making data frame for training by using train.index and selected.var(columns)

train.df <- diabetes_data[train.index, selected.var] 


#making data frame for validating by using -train.index and selected.var(columns)

valid.df <- diabetes_data[-train.index, selected.var] 


#checking the structures of each data frame
str(train.df)
str(valid.df)



### 6.Running Naive-Bayes classification

#Initializing result variable with naiveBayes function by using train data frame


results.nb <- naive_bayes(Outcome ~., data = train.df, laplace = 1)
results.nb



### 7. Creating probability tables to compare values of each factor

results_Preg.t <- table(train.df$stat_Preg, train.df$Outcome)
results_Glu.t <- table(train.df$stat_Glu, train.df$Outcome)
results_Blood.t <- table(train.df$stat_Blood, train.df$Outcome)
results_Skin.t <- table(train.df$stat_Skin, train.df$Outcome)
results_Ins.t <- table(train.df$stat_Ins, train.df$Outcome)
results_BMI.t <- table(train.df$stat_BMI, train.df$Outcome)
results_Ped.t <- table(train.df$stat_PedFunc, train.df$Outcome)
results_Age.t <- table(train.df$stat_Age, train.df$Outcome)

# margin=2 means that considering 'column' as 100%.
results.p1 <- prop.table(results_Preg.t, margin = 2) 
results.p2 <- prop.table(results_Glu.t, margin = 2)
results.p3 <- prop.table(results_Blood.t, margin = 2)
results.p4 <- prop.table(results_Skin.t, margin = 2)
results.p5 <- prop.table(results_Ins.t, margin = 2)
results.p6 <- prop.table(results_BMI.t, margin = 2)
results.p7 <- prop.table(results_Ped.t, margin = 2)
results.p8 <- prop.table(results_Age.t, margin = 2)


#a) Probability table (Pregnancies - Satisfaction)
results.p1


#b) Probability table (Glucose - Satisfaction)
results.p2


#c) Probability table (BloodPressure - Satisfaction)
results.p3


#d) Probability table (SkinThickness - Satisfaction)
results.p4


#e) Probability table (Insulin - Satisfaction)
results.p5


#f) Probability table (BMI - Satisfaction)
results.p6


#g) Probability table (DiabetesPedigreeFunction - Satisfaction)
results.p7


#h) Probability table (Age - Satisfaction)
results.p8


### 8. Predicting probabilities with validating data

##- Q1. Assuming results as a probability of a 46-year-old patient with only one pregnancy with 121 glucose concentration, 88 blood pressure, 32 skin thickness, 82 insulin, 24.2 BMI and 0.286 diabetesPedigreeFunction,

##- Q2. Assuming results as a probability of a 37-year-old patient with 4 pregnancies with 57 glucose concentration, 61 blood pressure, 24 skin thickness, 190 insulin, 35.5 BMI and 0.871 diabetesPedigreeFunction,

##- Q3. Assuming results as a probability of a 52-year-old patient with 5 pregnancies with 154 glucose concentration, 89 blood pressure, 33 skin thickness, 164 insulin, 42.7 BMI and 0.423 diabetesPedigreeFunction,


# newdata = new data for prediction
# type = raw(or, prob = probability prediction) , class (belonging class)

pred.prob <- predict(results.nb, newdata = valid.df, type = "prob")
pred.class <- predict(results.nb, newdata = valid.df, type = "class")

# actual = actual data
# predicted = predicted data 

df <- data.frame(actual = valid.df$Outcome,
                 predicted = pred.class, 
                 pred.prob)

df # check the result

#### ANSWER : This result makes it possible to predict that the first two patients will not have diabates, and the last patient will have diabetes. [0, 0, 1]


### 9. Confusion Matrix

#The misclassification is 0.2382812

## Confusion Matrix - train data
p1 <- predict(results.nb, train.df)
tab1 <- table(p1, train.df$Outcome)

tab1 # check misclassification table

# misclassification
1 - sum(diag(tab1)) / sum(tab1)
```

#The misclassification is NaN ()

## Confusion Matrix - test data
p2 <- predict(results.nb, valid.df)
tab2 <- table(p2, valid.df$Outcome)

tab2 # check misclassification table

# misclassification
1 - sum(diag(tab2)) / sum(tab2)



## Plots

### 1. Density Plots for each distribution table

##Result plot of each distribution table


ggplot(data=train.df) + 
  geom_density(mapping=aes(x=stat_Preg, colour = Outcome)) +
  ggtitle("Density Plot <Pregnancies - Outcome>")
ggplot(data=train.df) + 
  geom_density(mapping=aes(x=stat_Glu, colour = Outcome)) +
  ggtitle("Density Plot <Customer type Glucose - Outcome>")
ggplot(data=train.df) + 
  geom_density(mapping=aes(x=stat_Blood, colour = Outcome)) +
  ggtitle("Density Plot <Blood Pressure - Outcome>")
ggplot(data=train.df) + 
  geom_density(mapping=aes(x=stat_Skin, colour = Outcome)) +
  ggtitle("Density Plot <SkinThickness - Outcome>")
ggplot(data=train.df) + 
  geom_density(mapping=aes(x=stat_Ins, colour = Outcome)) +
  ggtitle("Density Plot <Insulin - Outcome>")
ggplot(data=train.df) + 
  geom_density(mapping=aes(x=stat_BMI, colour = Outcome)) +
  ggtitle("Density Plot <BMI - Outcome>")
ggplot(data=train.df) + 
  geom_density(mapping=aes(x=stat_PedFunc, colour = Outcome)) +
  ggtitle("Density Plot <DiabetesPedigreeFunction - Outcome>")
ggplot(data=train.df) + 
  geom_density(mapping=aes(x=stat_Age, colour = Outcome)) +
  ggtitle("Density Plot <Age - Outcome>")


### 2. Result plot for Naive-Bayes classification

##Result plot of Naive-Bayes classification

results.nb <- naive_bayes(Outcome ~., data = train.df) # for making simple plots
plot(results.nb)



## CLUSTERING
### Partitioning Algorithms

### 1. Loading packages

#install.packages("factoextra")
#install.packages("cluster")
#install.packages("magrittr")

library("cluster")
library("factoextra")
library("magrittr")

### 2. Data Preparation
##Handling Null values (missing data).

#Read csv file
diabetes_data <- read.csv("diabetes(data).csv", header=TRUE)

sapply(diabetes_data, function(x) sum(is.na(x)))



##A result without Null values. However, in this dataset is still exist some value of 0. 
##Therefore, we will treat them as Null value. By using %>%
  

diabetes_data = diabetes_data %>%
  na.omit() %>%
  scale()

### 3.Determine kmeans and compute the cluster
## Evaluate the optimal number of cluster

fviz_nbclust(diabetes_data, kmeans, method = "gap_stat")



#From this result, k will set as 3, nstart will be 100, in which:
  
#Seeds are used to create a starting point for numbers that are created randomly.
#nstart option that attempts multiple initial configurations and reports on the best one within the kmeans function.

set.seed(120)
km.res <- kmeans(diabetes_data, 3, nstart = 100)

### 4. Visualize Partitioning Clustering

fviz_cluster(km.res, data = diabetes_data,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

