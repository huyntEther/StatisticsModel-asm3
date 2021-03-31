##exemple
#setwd("/Users/jadenguyen/Desktop/GiaoAn/DH_KHTN/HKII_2020-2021/CaoHoc/MHHTK/exemple_Boston")

###data
#data1 <- read.table(file.choose(),sep = " ",header = TRUE)
wine <- read.csv(file = "wine.csv", header = TRUE)
dim(wine)


###stepAIC : stepwise regression proceeded
mod <- lm(Price ~ ., data = wine)# Full model
modAIC <- MASS::stepAIC(mod, k = 2) # With AIC  k = 2
summary(modAIC)
modBIC <- MASS::stepAIC(mod, k = log(nrow(wine))) ## With BIC k = log(nrow(wine))
summary(modBIC)
modBIC$anova


residuals(modAIC)
shapiro.test(residuals(modAIC))

fitted(modAIC)
resid(modAIC)
mean(resid(modAIC))
op <- par(mfrow=c(2,2))
plot(modAIC)

# Add an irrelevant predictor to the wine dataset
set.seed(123456)
wineNoise <- wine
n <- nrow(wineNoise) 
wineNoise$noisePredictor <- rnorm(n)

###direction = "backward",direction = "forward" or direction = "both"
####1) Backward selection:
# 1i)Starting from the model with all the predictors
modAll <- lm(formula = Price ~ ., data = wineNoise)
modbest<- MASS::stepAIC(modAll, direction = "backward", k = log(n))

# 1ii) Starting from an intermediate model
modInter <- lm(formula = Price ~ noisePredictor + Year + AGST, data = wineNoise) 
MASS::stepAIC(modInter, direction = "backward", k = log(n))

#####2) Forward selection: 
# 2i) Starting from the model with no predictors, only intercept (denoted as ~ 1)
modZero <- lm(formula = Price ~ 1, data = wineNoise) 
modbest1<- MASS::stepAIC(modZero, direction = "forward", scope = list(lower = modZero, upper = modAll), k = log(n))

# 2ii) Starting from an intermediate model
MASS::stepAIC(modInter, direction = "forward",
              scope = list(lower = modZero, upper = modAll), k = log(n))

###### 3)  direction "both"
# 3i) Starting from an intermediate model
modInter <- lm(formula = Price ~ noisePredictor + Year + AGST, data = wineNoise) 
modbest2<-MASS::stepAIC(modInter, direction = "both",
              scope = list(lower = modZero, upper = modAll), k = log(n))
# # 3ii)Using the defaults from the full model
MASS::stepAIC(modAll, direction = "both", k = log(n))

#### Omit lengthty outputs
MASS::stepAIC(modAll, direction = "both", trace = 0,
              scope = list(lower = modZero, upper = modAll), k = log(n))
