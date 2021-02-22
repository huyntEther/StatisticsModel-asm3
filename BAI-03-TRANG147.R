### NGUYEN THANH HUY - 29024
dataSet_3_147 <- read.csv('Latour.txt', sep = '\t')
Quality <- dataSet_3_147$Quality
EndOfHarvest <- dataSet_3_147$EndofHarvest
Rain <- dataSet_3_147$Rain
#model
model_A <- lm(Quality ~ EndOfHarvest+ Rain + Rain*EndOfHarvest)
