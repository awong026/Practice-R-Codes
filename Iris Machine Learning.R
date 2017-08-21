#https://machinelearningmastery.com/machine-learning-in-r-step-by-step/

#Install Caret Package
install.packages("caret", dependencies =  c("Depends", "Suggests"))
library(caret)

#Attach Iris data to environment
data(iris)
dataset <- iris

#Create train and test set
#Create a list of 80% of the rows for training and 20% for test
validation_index <- createDataPartition(dataset$Species, p = .80, list = F)

#test set
validation <- dataset[-validation_index,]
#train set
dataset <- dataset[validation_index,]

#Look at dimensions of data
dim(dataset)

#Know types of attributes in dataset
sapply(dataset, class)

#Look at top 5 entries of dataset
head(dataset)

#Look at last 5 entries of dataset
tail(dataset)


## Look into what are in the factorized column of Species
levels(dataset$Species)
 

# Look at how many rows belong to each class of Species
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq= table(dataset$Species), percentage = percentage)

#Get a summary of each thing in the dataset 
summary(dataset)

--------------------------------------------------------------------------------------------------------
  
#Create univarte plot of data to see how each attribute is structured
  
#Split input and output of data (x = input, y = output) We put numerics together and the lone factor column alone
  
x <- dataset[,1:4]
y <- dataset[,5]


#Create boxplot of each input variable

par(mfrow = c(1,4))
  for(i in 1:4) {
    boxplot(x[,i], main = names(iris)[i])
  }

#Plot the y variable to see how factor column is distributed (We already did this without a graph)
par(mfrow = c(1,1))
plot(y)


#Mulivarate plots to see how variables interact (Use ellipse to see clear relationships between the input attributes (trends) and between attributes and the class values)
featurePlot(x=x, y=y, plot= "ellipse")

#Use boxplot to clearly see different distributions of the attributes for each class value
featurePlot(x=x, y=y, plot = "box")

#We can get idea of distribution for each attribute broken down by class value using not just box, but histogram. In this case we will use some probability density plots to give us nice smooth lines for each dist.
scales <- list(x = list(relation="free"), y = list(relation = "free"))
featurePlot(x=x, y=y, plot ="density", scales = scales)


-----------------------------------------------------------------------
  
#Evaluate some models against others (1. setup test harness to use 10 fold cross validation, 2. build 5 models to predict species from flower measurements, 3. select best model)
  
##Use 10 fold cross validtion to estimate accuracy
  #Splits dataset into 10 parts, train in 9 and test in 1 and release for combinations of train-test splits. We will also repeat the process 3 times for each algorithm with diff split of the data into 10 groups, in an effort to get a more accurate estimate. 
control <- trainControl(method = "cv", number = 10)

#Using accuracy metric to judge how well our model works. Ratio of number of correctly predicted individed by the total number of instances then multipled by 100 to get a percentage

metric <- "Accuracy"

##Time to build models (5 possible models)
#1. Linear Discrminant Analysis (LDA), 2. Classification and Regression Trees (CART), 3. k-Nearest Neighbors(KNN), 4. Support Vector Machines (SVM) with a lienar kernel, 5. RAndom Forest(RF)
#Models are a mix of simple linear(LDA), nonlinear(CART, kNN) and complex nonlinear methods(SVM, RF). 
#Need to make sure to set seed before each model run to make sure that each run splits the data and performs things the same

#a. Linear algorithms 

#LDA
set.seed(7)
fit.lda <- train(Species ~. , data = dataset , method = "lda", metric = metric, trControl = control)

#b. Nonlinear algorithms

#CART
set.seed(7)
fit.cart <- train(Species ~. , data = dataset, method = "rpart", metric = metric, trControl = control)

#knn3
set.seed(7)
fit.knn <- train(Species ~., data = dataset, method = "knn", metric = metric, trControl = control)

#c. Advanced algorithms

#SVM
set.seed(7)
fit.svm <- train(Species ~., data = dataset, method = "svmRadial", metric = metric, trControl = control)

#Random Forest
set.seed(7)
fit.rf <- train(Species ~., data = dataset, method = "rf", metric = metric, trControl = control)

##Select the best model. Create a list of each model and summarize to get accuracy of each model
results <- resamples(list(lda = fit.lda, cart = fit.cart, knn = fit.knn, svm = fit.svm, rf = fit.rf))
summary(results)

#Look at each models mean to compare accuracy. (Rem each model was eval 10 times because of the 10 fold crossvalidation setup)
#Use dotplot to see each means
dotplot(results)

#From this we see that LDA is the most accurate model
#Summarize best model
print(fit.lda)

#Run best model on test set to make sure it's good there and we didn't make a slip like overfitting
predictions <- predict(fit.lda, validation)

#Now put it in a matrix to see results
confusionMatrix(predictions, validation$Species)

#Result says that it was 100% accurate, which is in our 97% +/- 4% margin. 
