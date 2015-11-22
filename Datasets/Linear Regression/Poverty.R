#Teen Birth Rate and Poverty Level Data

#This dataset of size n = 51 are for the 50 states and the District of Columbia in the United States (poverty.txt). 
#The variables are y = year 2002 birth rate per 1000 females 15 to 17 years old and x = poverty rate, which is the percent of the state’s population living in households with incomes below the federally defined poverty level.
#(Data source: Mind On Statistics, 3rd edition, Utts and Heckard).

#Import Dataset
dset <- read.table(url("https://onlinecourses.science.psu.edu/stat501/sites/onlinecourses.science.psu.edu.stat501/files/examples/poverty.txt"), header = TRUE)

#Scatterplot matrix to understand relations
pairs(~ ., data = dset, main = "Simple Scatterplot matrix")

#Training & Testing Set
require(caret)  
Pos <- createDataPartition(dset$Brth15to17, p =.8, list =FALSE, times = 1 )
train <- dset[Pos,]
test <- dset[-Pos,]

#Training Model
m1 <- lm(Brth15to17 ~ PovPct, data = train)
summary(m1)

#Plot of Model
plot(m1)

#Test the trained model
t_pred <- predict(m1, test, se.fit = TRUE, interval = "prediction")
t_pred

