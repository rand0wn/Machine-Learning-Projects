#This dataset consists of variables possibly relating to blood pressures of n = 39 Peruvians who have moved from rural high altitude areas to urban lower altitude areas.

#Q: How much blood pressure is reduced by other factors?

#The variables in this dataset are:
  
#Y = systolic blood pressure
#X1 = age
#X2 = years in urban area
#X3 = X2 /X1 = fraction of life in urban area
#X4 = weight (kg)
#X5 = height (mm)
#X6 = chin skinfold
#X7 = forearm skinfold
#X8 = calf skinfold
#X9  = resting pulse rate


#Import Dataset

dset <- read.table(url("https://onlinecourses.science.psu.edu/stat501/sites/onlinecourses.science.psu.edu.stat501/files/data/peru.txt"), header = TRUE)

#Scatterplot matrix to understand relations
pairs(~ ., data = dset, main = "Simple Scatterplot matrix")

#Sampling not feasible as dset is small

#ANOVA
ava <- aov(Systol ~ ., data = dset)
summary(ava)

#Models

#All independent variables
m_all <- lm(Systol ~ ., data = dset)
summary(m_all)
plot(m_all)

#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 117.65501   57.60797   2.042 0.050304 .  
#Age          -0.21770    0.28583  -0.762 0.452425    
#Years        -0.56277    0.22116  -2.545 0.016523 *  
#Weight        1.84852    0.48825   3.786 0.000713 ***
#Height       -0.06586    0.04218  -1.561 0.129300    
#Chin         -1.00756    0.88763  -1.135 0.265623    
#Forearm      -0.86086    1.40854  -0.611 0.545846    
#Calf         -0.01008    0.63980  -0.016 0.987543    
#Pulse         0.05049    0.19916   0.254 0.801645    
#Diastol       0.26295    0.16539   1.590 0.122691    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 10.18 on 29 degrees of freedom
#Multiple R-squared:  0.5399,	Adjusted R-squared:  0.3972 
#F-statistic: 3.782 on 9 and 29 DF,  p-value: 0.002991

#Based on P-Values eliminating Calf, Pulse, Forearm, Age(Dropping P values) but should use AIC

m_new <- lm(Systol ~ . - Calf  - Pulse  - Forearm  - Age, data = dset)
summary(m_new)

#GLR Plot
plot(m_new)
