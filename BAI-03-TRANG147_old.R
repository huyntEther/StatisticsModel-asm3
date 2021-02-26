### NGUYEN THANH HUY - 29024
data_3 <- read.csv('Latour.txt', sep = '\t')
Quality <- data_3$Quality
EndOfHarvest <- data_3$EndofHarvest
Rain <- data_3$Rain
### cau a
model_A <- lm(Quality ~ EndOfHarvest+ Rain + Rain*EndOfHarvest)
model_B <-  lm(Quality ~ EndOfHarvest+ Rain )
model_C <-  lm(Quality ~ EndOfHarvest)
summary(model_A)
summary(model_B)
summary(model_C)
anova(model_A, model_B)
anova(model_A, model_C)
### cau b
subset_isRain <- subset(data_3,Rain == 1)
subset_noRain <- subset(data_3,Rain == 0)
model_isRain <- lm(subset_isRain$Quality ~ subset_isRain$EndofHarvest)
model_noRain <- lm(subset_noRain$Quality ~ subset_noRain$EndofHarvest)
summary(model_isRain)
summary(model_noRain)
