#Purpose
One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, the goal is to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants.The goal of the project is to predict the manner in which they did the exercise. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. This is the "classe" variable in the training set.
Data
The training data for this project are available here:
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
The test data are available here:
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv
The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har

#Overview
Firstly the data is cleaned by excluding variables not useful for prediction because they contain too much NA data or do not influence the exercise manner. During preprocessing correlated variable are excluded to avoid overfitting and working out too much input, preprocessing with PC gave 20% lower accuracy so isn`t used in the final model. Model fitting was done with boosting which is one of the most accurate methods and worked on my laptop much faster than random forest method. I also tryed regression trees, but the accuracy was very low in this case. During model fitting cross validation on 3 sets is applyed. The accuracy gained is 
For its reproducible, I use set.seed, 

##Used packages



#Training and testing data was downloaded with RStudio functionality. 
pml-testing` <- read.csv("/tmp/RtmpwNs13E/datafbc134e2793")
##training set: 19622 obs. of 160 var.
train<-`pml-training`
quiz<-`pml-testing`

#Cleaning data
##Replace all empty and '#DIV/0!' entries with NA
train[train=="'#DIV/0!'"]<-NA
train[train==""]<-NA

##Delete all columns were are mostly NAs (more than 1/2 of all data)
### NAcols is a vector with those columns; default there are no NA
NAcols <- rep(FALSE, ncol(train)) 
### Loop over the columns and flag those were are mostly NAs
for (i in 1:ncol(train)){
  if (sum(is.na(train[,i]))>10000){
    NAcols[i]<-T
  }
}
###delete variables 
###now training set: 19622 obs. of 60 var.
train2 <- train[,!NAcols]

##delete columns which are not useful for parediction with:
##"X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "new_window", "num_window"   
## they are first 7 columns
train1 <- train2[,-c(1:7)]
###now training set: 19622 obs. of 53 var.

#Preprocessing
#DataPartition
set.seed(3433)
inTrain<-createDataPartition(y=train3$classe, p=0.75, list=FALSE)
training<-train1[inTrain,]
validation<-train1[-inTrain,]
##the same before here

##lets find and exclude correlated variable to avoid overfitting and working out too much input
###exclude non-numeric var. classe-it is in the last col. ????!!!!! is it???
###dim(training)[2]-number of var.s
corMat <- cor(training[,-dim(training)[2]]) 
corrplot(corMat, method = "color", type="lower", order="hclust", tl.cex = 0.65, tl.col="black", tl.srt = 45)
## Extract highly, r > 0.8, correlated variables and take them out of the training dataset
highlyCor <- findCorrelation(corMat, cutoff = 0.8)
training1<- training[,-highlyCor]
validation1<-validation[, -highlyCor]
###now 41 var.s

#modelfitting
### set seed for reproducibility
set.seed(134)
##boosting
trainControl <- trainControl(method='cv', number = 3)
model_gbm <- train(classe ~ ., data=training1, trControl=trainControl, method='gbm', verbose=FALSE)

#Validation
##1_boosting
## Apply predictions
pred_gbm <- predict(model_gbm , newdata=validation1)
## Extract the confusion matrix to assess model validity
confMat_gbm <- confusionMatrix(pred_gbm, validation1$classe)
confMat_gbm

## Run model on the testing dataset

quiz2 <- quiz[,!NAcols]
quiz1 <- quiz2[,-c(1:7)]
quiz3<-quiz1[, -highlyCor]
answers <- predict(model_gbm, newdata=quiz)
print(answers)

