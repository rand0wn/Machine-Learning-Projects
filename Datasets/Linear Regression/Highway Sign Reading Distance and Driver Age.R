#Highway Sign Reading Distance and Driver Age

#The data are n = 30 observations on driver age and the maximum distance (feet) at which individuals can read a highway sign (signdist.txt). 
#(Data source: Mind On Statistics, 3rd edition, Utts and Heckard).

#Import Dataset
dset <- read.table(url("https://onlinecourses.science.psu.edu/stat501/sites/onlinecourses.science.psu.edu.stat501/files/examples/signdist.txt"), header = TRUE)

#Scatterplot matrix to understand relations
pairs(~ ., data = dset, main = "Simple Scatterplot matrix")

#Training & Testing Set
require(caret)  
Pos <- createDataPartition(dset$Distance, p =.8, list =FALSE, times = 1 )
train <- dset[Pos,]
test <- dset[-Pos,]

#Model
m <- lm(Distance ~ Age, data = train)
summary(m)

#Testing
t_pred <- predict(m, test, se.fit = TRUE, interval = "prediction")
t_pred
