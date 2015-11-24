#Predicting Survival of Passenger in Titanic using Decision Tree and Random Forests

#The sinking of the RMS Titanic is one of the most infamous shipwrecks in history.
#On April 15, 1912, during her maiden voyage, the Titanic sank after colliding with an iceberg, killing 1502 out of 2224 passengers and crew.
#This sensational tragedy shocked the international community and led to better safety regulations for ships.

#One of the reasons that the shipwreck led to such loss of life was that there were not enough lifeboats for the passengers and crew.
#Although there was some element of luck involved in surviving the sinking, some groups of people were more likely to survive than others, such as women, children, and the upper-class.

#In this challenge, we ask you to complete the analysis of what sorts of people were likely to survive.
#In particular, we ask you to apply the tools of machine learning to predict which passengers survived the tragedy.

#VARIABLE DESCRIPTIONS:
#  survival        Survival
#(0 = No; 1 = Yes)
#pclass          Passenger Class
#(1 = 1st; 2 = 2nd; 3 = 3rd)
#name            Name
#sex             Sex
#age             Age
#sibsp           Number of Siblings/Spouses Aboard
#parch           Number of Parents/Children Aboard
#ticket          Ticket Number
#fare            Passenger Fare
#cabin           Cabin
#embarked        Port of Embarkation
#(C = Cherbourg; Q = Queenstown; S = Southampton)

#Import Dataset
train <- read.csv("train.csv")
test <- read.csv("test.csv")

#Libraries Required
require(rpart)
require(randomForest)
require(party)

# Combine Training and Testing Sets
test$Survived <- NA
combi <- rbind(train, test)

#Modifying Variables for better Modeling

# String
combi$Name <- as.character(combi$Name)

# Title
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)

# Combine small title groups
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

# Title as factor
combi$Title <- factor(combi$Title)

# Family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Family
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'


famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'

combi$FamilyID <- factor(combi$FamilyID)

#Fill Missing
summary(combi$Age)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, 
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

summary(combi)

#Embarked
summary(combi$Embarked)
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)

# Fare
summary(combi$Fare)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

# 32 levels only allowed so change

combi$FamilyID2 <- combi$FamilyID

# String

combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'

# And factor
combi$FamilyID2 <- factor(combi$FamilyID2)

# Split back into test and train sets
train <- combi[1:891,]
test <- combi[892:1309,]

# Random Forest Ensemble
set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2,
                    data=train, importance=TRUE, ntree=2000)

# Plot
varImpPlot(fit)

# Prediction
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "Results_f.csv", row.names = FALSE)

# Inference 
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))

# Inference and Prediction
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "Results_f_i.csv", row.names = FALSE)

