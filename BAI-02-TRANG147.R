### NGUYEN THANH HUY - 29024
dataSet_2_147 <- read.csv('HoustonChronicle.csv')
### linear regression model between X: percentage of low income students, and Y: percentage of students repeating first grade
X <- dataSet_2_147$X.Low.income.students
Y <- dataSet_2_147$X.Repeating.1st.Grade
model_A <- lm(Y~X)
model_A_summary <- summary(model_A)

###
subset_94 <- subset(dataSet_2_147, Year == 1994)
subset_04 <- subset(dataSet_2_147, Year == 2004)
t.test(dataSet_2_147$X.Repeating.1st.Grade ~ dataSet_2_147$Year)

###
X2 <- dataSet_2_147$Year
model_C <- lm(Y~ X + X2)
