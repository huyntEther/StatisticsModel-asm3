library(readxl)
data1 <- read_excel("data1.xls")
head(data1)
#add var
data1$multi <- data1$salary * data1$lsalary
data1$devide <- data1$salary / data1$lsalary

attach(data1)
# m_full
m_full_linear <- lm(salary ~ . - lsalary - multi - devide,data = data1)
summary(m_full_linear)

X <- cbind(age, comten, ceoten, sales, profits, mktval, lsales, lmktval, comtensq, ceotensq, profmarg)
library(leaps)
b <- regsubsets(as.matrix(X), salary)
rs <- summary(b)
detach(data1)
