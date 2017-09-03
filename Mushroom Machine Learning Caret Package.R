#PokemonGo data from kaggle
#Purpose is to predict which mushrooms are edible and which are not
#Activate R package
library(caret)

#Import mushroom Data

dataset <- read.csv("C:/Users/awong/Downloads/mushrooms.csv")

#Create test and train set
validation_index <- createDataPartition(dataset$class, p = .80, list = F)
#train set
train <- dataset[validation_index,]

#test set
test <- dataset[-validation_index,]

#Now investigate dataset
#Check dim
dim(dataset)

#Check type of attributes in dataset
sapply(dataset, class)

#Check first 5 entries 
head(dataset)

#Check last 5 entries
tail(dataset)

#Look to more into the attribute we are trying to predict for (class, either e or p)
levels(dataset$class)

#See how proportioned the table is according to either e or p
percentage <- prop.table(table(dataset$class))*100
cbind(freq = table(dataset$class), percentage = percentage)

#We see that 4208 are e and 3916 are p. That means that 51.8 are e and 48.2 are p

#Summary to get an overview of the whole dataset
summary(dataset)

####################################################



#Create univarte plots to see how each attribute is structured
#We put class colukmn into it's own subset and every other attribute into its own
  
x <- dataset[,2:23]
y <- dataset[,1]


for (i in 2:23){
  plot(x[,i], main = names(dataset)[i])
}



#start creating a model with only one variable first
#See row proportioned for each group (1)
prop.table(table(dataset$class, dataset$bruises),1)

#We see that many p are not bruised and many e are
#Let's create this first model and see how accurcate it is

test$class <- "p"
test$class[test$bruises == "t"] <- "e"

##Use 10 fold cross validtion to estimate accuracy
#Splits dataset into 10 parts, train in 9 and test in 1 and release for combinations of train-test splits. We will also repeat the process 3 times for each algorithm with diff split of the data into 10 groups, in an effort to get a more accurate estimate. 
control <- trainControl(method = "cv", number = 10)

#Using accuracy metric to judge how well our model works. Ratio of number of correctly predicted individed by the total number of instances then multipled by 100 to get a percentage

metric <- "Accuracy"

#Predict
p1<- predict()
