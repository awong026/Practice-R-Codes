#Predict whether a cancer is malignant or benign from biopsy details
#Import data 
install.packages("mlbench")
library(mlbench)
data(BreastCancer)
dataset <- BreastCancer

######################################################################################
#Check out dim of dataset
dim(dataset)

#View dataset
View(dataset)

#Check out type of each attribute
sapply(dataset, class)

#Check out first 5 rows
head(dataset)

#Check out bottom 5 rows
tail(dataset)

#Check out levels of column with malignant or benign to make sure those are the only things there
levels(dataset$Class)

#See what percentage of data is either m or b
percentage <- prop.table(table(dataset$Class)) * 100
cbind(freq = table(dataset$Clas), percentage = percentage)

#Check out structure of dataset
str(dataset)

#Get a summary of dataset
summary(dataset)

##########################################################################
#Create train and test set
library(caret)
validation_index <- createDataPartition(dataset$Class, p = .8, list = F)
test <- dataset[-validation_index,]
dataset <- dataset[validation_index,]

############################################################################
#Create plots to visualize dataset and look for patterns
#Split input and output of data (x = input, y = output) 
x = dataset[,1:10]

#Split vector and factor attributes
xv = dataset[,1:5]
xf = dataset[,6:9]
y = dataset[,11] 

#Create univariate boxplot of numeric data to see how each attribute is structured
par(mfrow = c(1,5))
for(i in 2:5) {
  boxplot(x[,i], main = names(dataset)[i])
}

#Create histogram with stat = "count" to get a visual of factor attributes
install.packages("gridExtra")
library(ggplot2)
library(gridExtra)
myplot <- list()
for(i in 6:10)
  local({
    i <- i
    p1<- ggplot(x, aes(x = x[,i])) + geom_histogram(stat = "count") + xlab(names(dataset)[i])
    print(i)
    print(p1)
    myplot[[i]] <- p1
})

#**************************A better way to do previous chart ************************#

plot_data_column = function(data, column)
  ggplot(data = x, aes_string(x = column)) + geom_histogram(stat = "count") + xlab(column)

myplots2 <- lapply(colnames(x), plot_data_column, data = x)
do.call(grid.arrange, c(myplots2, ncol = 3))

#************************************************************************************#


##Multivariate plots to see how attributes interact
for(i in 2:9){
  print(ggplot(dataset, aes(x= dataset[,i], fill = Class)) + geom_histogram(stat ="count") + facet_grid(.~Class) + xlab(names(dataset)[i]))
}

#*****************************Another Way to get previous chart ***********************#

plot_multivariate = function(data, column)
  ggplot(data = dataset, aes_string(x= column)) + geom_histogram(stat ="count") + facet_grid(.~Class) + xlab(column)


multiplots <- lapply(colnames(dataset), plot_multivariate, data = dataset)
do.call(grid.arrange, c(multiplots, ncol = 3))

#**********************************************************************************************************************#

#Evaluate some models against others (1. setup test harness to use 10 fold cross validation, 2. build 5 models to predict species from flower measurements, 3. select best model)

##Use 10 fold cross validtion to estimate accuracy
#Splits dataset into 10 parts, train in 9 and test in 1 and release for combinations of train-test splits. We will also repeat the process 3 times for each algorithm with diff split of the data into 10 groups, in an effort to get a more accurate estimate. 
control <- trainControl(method = "cv", number = 10)

#Using accuracy metric to judge how well our model works. Ratio of number of correctly predicted individed by the total number of instances then multipled by 100 to get a percentage
metric <- "Accuracy"


#-------------------------# 
#Use a decision tree to create model to figure if tumor is beign or malgnant

library(rpart)

#Since we only want either 1 or zero (b or m) we will use method = class instead of annova which is for continuous variable. 
fit <- rpart(Class ~ Cl.thickness + Cell.size + Cell.shape + Marg.adhesion + Epith.c.size + Bare.nuclei + Bl.cromatin + Normal.nucleoli + Mitoses, 
              data = dataset,
              method = "class"
             )
#Get correct syntax for variable names
colnames(dataset)

#Plot and see text of plot
plot(fit)
text(fit)


#Not very insightful. Need to add in more packages to see more in detail
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#Render the tree to look nicer with more details. Rattle package
fancyRpartPlot(fit)

#Check let's check the accuracy of this model against our test set next. ???????????????????????????????????????????
f_pred = predict(fit, test, type = "Class")
t = test["Class"]
confMat <- table(dataset$Class, )
accuracy <- sum(diag(confMat))/sum(confMat)


#Instead we will try using complexity parameter and cross validation error to check decision tree
#printcp and plotcp functions
#Syntax: printcp(x) where x is the rpart object: This function provides the opt pruning on the cp value

printcp(fit)


#Select trimming that has least error
ptree<- prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

#Plot pt decision tree
fancyRpartPlot(ptree, uniform=TRUE,main="Pruned Classification Tree")




#_________________********* Other possible code for making model and checking accuracy
library(caret)
library(tidyverse)
library(mlbench)

set.seed(123)

data(BreastCancer)

df <- BreastCancer %>% 
  select(-Id)

index <- createDataPartition(df$Class, p = 0.75, list = F)
trainSet <- df[ index,]
testSet <- df[-index,]

outcomeName <- 'Class'
predictors <- names(trainSet)[!names(trainSet) %in% outcomeName]

ctrl <- trainControl(method = 'cv', number = 10, repeats = 5, savePredictions = T, classProbs = T, allowParallel = T)

if (require('parallel', quietly = T, warn.conflicts = F)) {
  ctrl$workers <- parallel:::detectCores()
  ctrl$computeFunction <- mclapply
  ctrl$computeArgs <- list(mc.preschedule = F, mc.set.seed = F)
}

fit <- train(trainSet[,predictors], trainSet[,outcomeName], method = 'rpart', trControl = ctrl, metric = 'Accuracy')

pred <- predict.train(object = fit, trainSet[, predictors], type = 'raw')

confusionMatrix(pred, trainSet[,outcomeName])

