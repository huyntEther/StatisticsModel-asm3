### Statisque 1

### exercise 1
data1 <- c(    22.28 ,    23.18 ,    23.48 ,    23.61 ,    24.09 ,    24.43 ,    22.57 ,    23.29 ,    23.51 ,    23.62 ,    24.27 ,    24.63 ,    22.69 ,    23.34 ,23.53 ,    23.68 ,    24.32 ,    24.83 ,    22.78 ,    23.35 ,    23.55 ,    23.72 ,    24.36 ,    24.95 ,    23.05 ,    23.39 ,    23.57 ,    23.99 ,    24.41 ,    25.48)

# 1b
#summary(data1)
mean(data1)
var(data1)
sqrt(var(data1)) #sd
median(data1)
sd(data1)
# 1d
hist(data1, col="green", xlab = "X")
lines(density(na.omit(data1)), col="red", lwd = 2)

### Exercise 2
data2_X <- 0:11 #c(    0 ,    1 ,2 ,    3 ,4 ,    5 ,    6 ,    7 ,    8 ,    9 ,    10 )
data2_Y <- c(    5 ,15 ,    43 ,    53 ,    86 ,    70 ,    54 ,    37 ,    18 ,    10 ,    9 )
data2_freqTable <- list(breaks= data2_X, counts = data2_Y, density = data2_Y / diff(data2_X))
plot(data2_freqTable)

###### BAI TAP THEM
### 7
data7 <- c(177.8, 186.7 ,174.0 ,162.6 ,147.3 ,171.5)
mean(data7)
me <- qt(1-0.05/2, length(data7)-1) * sd(data7) / sqrt(length(data7))
mean(data7) - me
mean(data7) + me
