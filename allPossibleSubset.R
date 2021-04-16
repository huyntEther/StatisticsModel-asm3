bridge <- read.table("bridge.txt", header = TRUE)
attach(bridge)

m1 <- lm(log(Time) ~log(DArea)+log(CCost)+log(Dwgs)+log(Length)+log(Spans) )

logDArea <- log(DArea)
logCCost <- log(CCost)
logDwgs <- log(Dwgs)
logLength <- log(Length)
logSpans <- log(Spans)
X <- cbind(logDArea,logCCost,logDwgs,logLength,logSpans)

library(leaps)

b <- regsubsets(as.matrix(X),log(Time))
rs <- summary(b)

library(car)

subsets(b,statistic=c("adjr2"))

rs$adjr2

par(mfrow=c(1,2))
plot(1:5,rs$adjr2,xlab="Subset Size",ylab="Adjusted R-squared")
subsets(b,statistic=c("adjr2"))


detach(bridge)
