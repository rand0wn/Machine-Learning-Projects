#Iris Dataset with SVM as Polynomial Classification

#Data Set Information:
  
#This is perhaps the best known database to be found in the pattern recognition literature. Fisher's paper is a classic in the field and is referenced frequently to this day. (See Duda & Hart, for example.) The data set contains 3 classes of 50 instances each, where each class refers to a type of iris plant. One class is linearly separable from the other 2; the latter are NOT linearly separable from each other.

#Predicted attribute: class of iris plant.

#This is an exceedingly simple domain.

#This data differs from the data presented in Fishers article (identified by Steve Chadwick, spchadwick '@' espeedaz.net ). The 35th sample should be: 4.9,3.1,1.5,0.2,"Iris-setosa" where the error is in the fourth feature. The 38th sample: 4.9,3.6,1.4,0.1,"Iris-setosa" where the errors are in the second and third features.

#Attribute Information:

#1. sepal length in cm
#2. sepal width in cm
#3. petal length in cm
#4. petal width in cm
#5. class:
#-- Iris Setosa
#-- Iris Versicolour
#-- Iris Virginica

#Q : How can species be explained by other factors in the dataset?

#Import Dataset
dname <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
dset <- read.table(url("https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), header = FALSE, fill = TRUE, col.names = dname  , sep = ",")
dset$Species = as.factor(dset$Species) #For Classification

#Scatterplot matrix to understand relations
pairs(Species~ ., data = dset, main = "Simple Scatterplot matrix")

require(caret)

#Sampling
sample <- dset[sample(nrow(dset), 100),]
Pos <- createDataPartition(sample$Species, p = .8, list = FALSE, times = 1)
train <- sample[Pos,]
test <-sample[-Pos,]
levs <- unique( unlist(lapply( train, levels )))

#Multicore
require(doMC)
registerDoMC(cores = 5)

require(kernlab)
#Tuning
sigDist <- sigest(Species ~ ., data = train, frac = 1) 
svmTuneGrid <- data.frame(.sigma = sigDist[1], .C = 0.5)


#Training Model
m_svm <- train(Species ~ ., data = train, kernel = "ploynomial", type = "C-classification")
summary(m_svm)
plot(m_svm)

#Testing
m_pred <- predict(m_svm, test)
m_pred

table(m_pred, test$Species)

#Polynomial Kernel with Classifcation Yields Perfect Results on Test

#m_pred            Iris-setosa Iris-versicolor Iris-virginica
#Iris-setosa               7               0              0
#Iris-versicolor           0               7              0
#Iris-virginica            0               0              5