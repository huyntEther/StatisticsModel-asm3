### NGUYEN THANH HUY - 29024
data_3 <- read.csv('Latour.txt', sep = '\t')
Y <- data_3$Quality
E <- data_3$EndofHarvest
R <- data_3$Rain
# cau a
model_A <- lm(Y ~ E + R + E:R)
model_B <- lm(Y ~ E + R)
model_C <- lm(Y ~ E)
summary(model_A)
summary(model_B)
summary(model_C)
anova(model_C, model_B)
anova(model_B, model_A)
# cau b
subset_isRain <- subset(data_3, Rain == 1)
subset_noRain <- subset(data_3, Rain == 0)
model_isRain <- lm(subset_isRain$Quality ~ subset_isRain$EndofHarvest)
model_noRain <- lm(subset_noRain$Quality ~ subset_noRain$EndofHarvest)
summary(model_isRain)
summary(model_noRain)
