#Measurements of College Students

#Q: How much height is reduced by other variables?

#For n = 55 college students, we have measurements (Physical.txt) for the following five variables:
  
#Y = height (in)
#X1 = left forearm length (cm)
#X2 = left foot length (cm)
#X3 = head circumference (cm)
#X4 = nose length (cm)

#Import Dataset

dset <- read.table(url("https://onlinecourses.science.psu.edu/stat501/sites/onlinecourses.science.psu.edu.stat501/files/data/Physical.txt"), header = TRUE)

#Scatterplot matrix to understand relations
pairs(Height~ ., data = dset, main = "Simple Scatterplot matrix") #Dependent Variable Known

#Sampling not feasible as small dataset

#ANOVA
ava <- aov(Height ~ ., data = dset)
summary(ava)

#Model

#All independent variables
m_all <- lm(Height~ . , data = dset)
summary(m_all)
plot(m_all)

#Dropping based on T-Value & P-Value
m_new <- lm(Height~ . - LeftHand - RtArm - HeadCirc - nose - RtHand, data = dset )
summary(m_new)
plot(m_new)

#There is a trade-off b/w Adj R-Sq and Multiple R-Sq in model m_all and m_new
