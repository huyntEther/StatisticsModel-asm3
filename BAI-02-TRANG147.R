### NGUYEN THANH HUY - 29024
### bai tap 2 trang 147
data2 <- read.csv('HoustonChronicle.csv')
### cau a
### linear regression model between X: percentage of low income students, and Y: percentage of students repeating first grade
data2$groupYear <- with(data2, ifelse(Year == 1994, 0, 1))
X <- data2$X.Low.income.students
Y <- data2$X.Repeating.1st.Grade
model_A <- lm(Y~X)
model_A_summary <- summary(model_A)
model_A_summary
### cau b
#subset_94 <- subset(data2, Year == 1994)
#subset_04 <- subset(data2, Year == 2004)
var.test(data2$X.Repeating.1st.Grade ~ data2$Year)
t.test(data2$X.Repeating.1st.Grade ~ data2$Year, var.equal = TRUE)
###
Z <- data2$groupYear
model_B_full <- lm(Y ~ X + Z + X:Z)
model_B_2 <- lm(Y ~ X + X:Z)
model_B_3 <- lm(Y ~ X + Z)
summary(model_B_full)
summary(model_B_2)
summary(model_B_3)
anova(model_A, model_B_2)
anova(model_A, model_B_3)
anova(model_A, model_B_full)
