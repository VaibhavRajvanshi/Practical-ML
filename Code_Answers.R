
# Setting up the directory
setwd("D://Course_Era")

# load training data
Raw_training <- read.csv("pml-training.csv", na.strings=c("", "NA", "NULL"))
dim(Raw_training)


# load testing data
Main_testing <- read.csv("pml-testing.csv", na.strings=c("", "NA", "NULL"))
dim(Main_testing)

# Remove variables that we believe have too many NA values.
training.zero <- Raw_training[ , colSums(is.na(Raw_training)) == 0]

dim(training.zero)

# Remove unrelevant variables

extra <-  c('X', 'user_name', 'raw_timestamp_part_1', 'raw_timestamp_part_2', 'cvtd_timestamp', 'new_window', 'num_window')

training.rev <- training.zero[, -which(names(training.zero) %in% extra)]
dim(training.rev)

library(caret)

# Check the variables that have extremely low variance 

zeroVar= nearZeroVar(training.rev[sapply(training.rev, is.numeric)], saveMetrics = TRUE)

training.nonzerovar = training.rev[,zeroVar[, 'nzv']==0]
dim(training.nonzerovar)

# Remove highly correlated variables 90%

# only numeric variabls can be evaluated in this way
corrMatrix <- cor(na.omit(training.nonzerovar[sapply(training.nonzerovar, is.numeric)]))
dim(corrMatrix)

corrDF <- expand.grid(row = 1:52, col = 1:52)
corrDF$correlation <- as.vector(corrMatrix)
levelplot(correlation ~ row+ col, corrDF)

# Remove those variable which have high correlation

removecor = findCorrelation(corrMatrix, cutoff = .90, verbose = TRUE)

training.decor = training.nonzerovar[,-removecor]
dim(training.decor)

inTrain <- createDataPartition(y=training.decor$classe, p=0.7, list=FALSE)

training <- training.decor[inTrain,]
testing <- training.decor[-inTrain,]
dim(training)
dim(testing)

# Apply Random forest on the data
require(randomForest)
set.seed(12345)

rf.training=randomForest(classe~.,data=training,ntree=100, importance=TRUE)
rf.training


#plot(rf.training, log="y")
varImpPlot(rf.training,)

# Out-of Sample Accuracy

tree.pred=predict(rf.training,testing,type="class")

predMatrix = with(testing,table(tree.pred,classe))
sum(diag(predMatrix))/sum(as.vector(predMatrix)) # error rate

course_era.answers <- predict(rf.training, Main_testing)
course_era.answers
