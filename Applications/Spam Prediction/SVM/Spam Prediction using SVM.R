#Required Packages
install.packages("caret")
require(caret)
install.packages("kernlab")
require(kernlab)
install.packages("doMC")
require(doMC)

#Research Question: Which email should be marked as spam?
 
#Supervised Learning Problem Dependent Variable present

#Import Dataset and Column Names or Variable Names

#Email Dataset from UCI https://archive.ics.uci.edu/ml/datasets/Spambase
eData <- read.csv(url("http://thinktostart.com/data/data.csv"), header = FALSE, sep = ";") 

#Column Names for Dataset
cData <- read.csv(url("http://thinktostart.com/data/names.csv"), header = FALSE, sep = ";")

#Merge names into Email Dataset

names(eData) <- lapply((1:nrow(cData)),function(i) toString(cData[i,1]))


#SVM Approach: Using SVM

#Dependent Variable is numerical with binary values and we need to convert it to factor to escape regression by default
eData$y <- as.factor(eData$y)


#Sampling, Training & Testing Dataset from eData
sample <- eData[sample(nrow(eData), 1000),]
Pos <- createDataPartition(sample$y, p = .8, list = FALSE, times = 1)
train <- sample[Pos,]
test <- sample[-Pos,]

levs <- unique( unlist(lapply( train, levels )))


registerDoMC(cores = 5) #Parallel Processing Multicore


#Modelling

#Optimal Value for Tuning Parameter 
sigDist <- sigest(y ~ ., data = train, frac = 1) #kernlab
svmTuneGrid <- data.frame(.sigma = sigDist[1], .C = 2^(-2:7))

#SVM Training

svm <- train(y ~ ., data = train, method = "svmRadial", preProc = c("center", "scale"), tuneGrid = svmTuneGrid, trControl = trainControl(method = "repeatedcv", repeats = 6, classProbs = TRUE))

#Testing & Evaluating

svm_pred <-predict(svm, test[, 1:57])

acc <- confusionMatrix(svm_pred, test$y)

write.csv(acc, row.names = FALSE, "Spam Prediction.csv")

