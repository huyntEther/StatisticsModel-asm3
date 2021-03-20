library(readxl)
data1 <- read_excel("data1.xls")
head(data1)
m1 <- lm(data1$salary  ~ data1$age + data1$comten + data1$ceoten + data1$sales + data1$profits + data1$mktval)
summary(m1)
m2 <- lm(data1$salary ~ data1$comtensq)