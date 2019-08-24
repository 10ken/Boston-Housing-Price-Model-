library("mosaic")
library("car")
library("MASS")

setwd("~/Documents/Kenneth/Courses/STAC67/Data Project")
housingprop <- read.csv("housing.proper.csv")
colnames(housingprop) <- c( "x1", "x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","y")
train <- housingprop[1:300,]
test <- housingprop[301:506,]


aicfit <- lm(y ~ x1 + x5 + x6 + x7 + x8 + x10 + x11 + x12 + x13, data = train) 
aicfitvar <- c("x1","x5","x6","x7","x8","x10","x11","x12","x13")
f2 = train[aicfitvar]
par(mfrow=c(2,2))
plot(aicfit)


aiclnfit <- lm(log(train$y) ~ x5 + x6 + x7 + x8 + x10 + x11 + x12 + x13, data = train )
aiclnfitvar <- c("x5","x6","x7","x8","x10","x11","x12","x13")
f3 <- train[aiclnfitvar]
par(mfrow=c(2,2))
plot(aiclnfit)


lnfit <- lm (log(train$y) ~ ., data = train)
s <- summary(lnfit)$sigma
n <- nrow(train)
select_criteria = function(model, n, s)
{
  SSres <- sum(model$residuals^2)
  Rsq <- summary(model)$r.squared
  Rsq_adj <- summary(model)$adj.r.squared
  p_prime <- length(model$coefficients)
  C <- SSres/s^2 + 2*p_prime - n
  AIC <- n*log(SSres) - n*log(n) + 2*p_prime
  res <- c(SSres, Rsq, Rsq_adj, C, AIC)
  names(res) <- c("SSres", "Rsq", "Rsq_adj", "C", "AIC")
  return(res)
}

round(rbind(
  select_criteria(lm(log(train$y) ~ x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, data = train), n, s), #given by aic
  select_criteria(lm(log(train$y) ~ x5 + x6 + x7 + x8 + x10 + x11 + x12 + x13, data = train), n, s), #x9 was insign in anova,omit
  select_criteria(lm(log(train$y) ~ x6 + x7 + x8 + x10 + x11 + x12 + x13, data = train), n, s),#omit x5,not much change in x5
  select_criteria(lm(log(train$y) ~ x6 + x7 + x8 + x10 + x11 + x13, data = train), n, s),
  
  select_criteria(lm(log(train$y) ~ x5 + x7 + x8 + x10 + x11 + x12 + x13, data = train), n, s),
  select_criteria(lm(log(train$y) ~ x5 + x6 + x8 + x10 + x11 + x12 + x13, data = train), n, s),
  select_criteria(lm(log(train$y) ~ x5 + x6 + x7 + x10 + x11 + x12 + x13, data = train), n, s),
  select_criteria(lm(log(train$y) ~ x5 + x6 + x7 + x8 + x11 + x12 + x13, data = train), n, s),
  select_criteria(lm(log(train$y) ~ x5 + x6 + x7 + x8 + x10 + x12 + x13, data = train), n, s),
  select_criteria(lm(log(train$y) ~ x5 + x6 + x7 + x8 + x10 + x11 + x13, data = train), n, s),
  
  select_criteria(lm(log(train$y) ~ x6 + x8 + x10 + x11 + x12 + x13, data = train), n, s),
  select_criteria(lm(log(train$y) ~ x6 + x7 + x10 + x11 + x12 + x13, data = train), n, s),
  select_criteria(lm(log(train$y) ~ x6 + x7 + x8 + x11 + x12 + x13, data = train), n, s),
  select_criteria(lm(log(train$y) ~ x6 + x7 + x8 + x10 + x12 + x13, data = train), n, s),
  select_criteria(lm(log(train$y) ~ x6 + x7 + x8 + x10 + x11 + x13, data = train), n, s),
  select_criteria(lm(log(train$y) ~ x6 + x7 + x8 + x10 + x11 + x12, data = train), n, s)
  
), digits = 2)


fit5 <- lm(log(train$y) ~ x6 + x7 + x8 + x10 + x11 + x12 + x13, data = train )
newx5 <- test[, c(6,7,8,10,11,12,13)]
colnames(newx5) <- c("x6","x7","x8","x10","x11","x12","x13")
Y_pred <- predict(fit5, newx5)
Y_obs <- log(test$y)
n_star <- nrow(train)
MSPE <- sum( (Y_obs-Y_pred)^2/n_star )
MS_res <- (summary(fit5)$sigma)^2
MSPE
MS_res


lny <- log(train$y)
# Statistical test
#outlierTest(fit5)
# Studentized deleted residuals
t <- rstudent(fit5)
alpha <- 0.05
n <- length(lny)
p_prime = length(coef(fit5))
t_crit <- qt(1-alpha/(2*n),n-p_prime-1)
#round(t,2)
# t_crit #dont know what this is
which(abs(t) > t_crit) #does this mean we have no y outliers?


Pii <- hatvalues(fit5)
#round(Pii, 2)                           #normal QQ plot shows us we have 3 outliers. Not sure why we have 0 number of outliers
which(Pii > 2*p_prime/n)                #not entirely sure if normal QQ plot gives outliers
which(Pii > 0.5)


DFFITS <- dffits(fit5)
which(DFFITS > 1)
D <- cooks.distance(fit5)
which(D > qf(0.2, p_prime, n-p_prime))
DFBETAS <- dfbetas(fit5)


VIF <- vif(fit5)
VIF
VIFbar <- mean(vif(fit5))
VIFbar


