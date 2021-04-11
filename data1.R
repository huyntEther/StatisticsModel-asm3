library(readxl)
data1 <- read_excel("data1.xls")
head(data1)
#m1 <- lm(data1$salary  ~ data1$age + data1$comten + data1$ceoten + data1$sales + data1$profits + data1$mktval)
#summary(m1)
#m2 <- lm(data1$salary ~ data1$comtensq)

pairs(salary ~ age + college + grad + comten + ceoten + sales + profits + mktval + lsalary + lsales + lmktval + comtensq + ceotensq + profmarg,data = data1)

#m_log <- lm(lsalary ~ lsales + lmktval + comtensq + ceotensq + profmarg, data= data1)

#stepwise cho model theo log_salary
m_full_log <- lm(lsalary ~ . - salary, data=data1)
backAIC_log <- step(m_full_log,direction="backward")
m_log_model <- lm(lsalary ~ grad + comten + ceoten + lsales + lmktval + ceotensq, data=data1)
summary(m_log_model) # bỏ bớt biến
m_log_model_reduced <- lm(lsalary ~  comten + ceoten + lsales + lmktval + ceotensq, data=data1)
anova(m_log_model_reduced,m_log_model) #mô hình reduced

#stepwise cho model theo salary
m_full_linear <- lm(salary ~ . - lsalary, data=data1)
backAIC_linear <- step(m_full_linear,direction="backward")
m_linear_model <- lm(salary ~ comten + ceotensq + mktval + lsales + ceotensq, data = data1)
summary(m_linear_model)
m_linear_model_reduced <- lm(salary ~   mktval + + lsales , data = data1)
summary(m_linear_model_reduced)
anova(m_linear_model_reduced,m_linear_model) #mô hình reduced

### chọn giữa log và linear
summary(m_linear_model_reduced)
summary(m_log_model_reduced)
# mô hình log có R2 adjusted bé hơn