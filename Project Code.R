# all code and statistical reasoning used to build regression model


library("mosaic")
library("car")
library("MASS")

setwd("~/Documents/Kenneth/Courses/STAC67/Data Project")
housingprop <- read.csv("housing.proper.csv")
colnames(housingprop) <- c( "x1", "x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","y")
nrow(housingprop)
train <- housingprop[1:300,]
test <- housingprop[301:506,]
nrow(train)
nrow(test)

cor(train)
#multicollinearity may exist bc of the large variences 
# should show collinearity graphically 

summary(housingprop)

######### MODEL SELECTION ##########

fit1 <- lm(y ~ ., data = train)

plot(fit1) 
#residuals have a pattern, lm is not correct. More work needs to be done


summary(fit1)
#fit1 is the lm set up with all predictors to perform stepAIC

step = stepAIC(fit1, direction = "both")
# helps find a model with lowest AIC 
# should plot and show multicollinearity still exists
# introduce a lm with lny or logy ~ try about, make sure theyre both the same
# verify by VIF and Cooks distnace 


aicfit <- lm(y ~ x1 + x5 + x6 + x7 + x8 + x10 + x11 + x12 + x13, data = train) #best model by lowest AIC from training set
aicfitvar <- c("x1","x5","x6","x7","x8","x10","x11","x12","x13") #make set with only AIC significant predictors
f2 = train[aicfitvar]
cor(f2) # still has large correlations 
#should also check if residuals have a pattern or not ~ ? to reinforce the presense of collinearity ?
# residual to fitted plot should be random, if not random, more work needs to be done

plot(aicfit)
#residuals to fitted have pattern, lm is not correct, more work needs to be done with collinearity 

lnfit <- lm (log(train$y) ~ ., data = train)
#summary(lnfit)
#we consider a logarithmic transformation of y
#lnfit set up with all predictors to perform stepAIC
plot(lnfit)
step = stepAIC(lnfit, direction ="both")

aiclnfit <- lm(log(train$y) ~ x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, data = train )
#summary(aiclnfit)
#lm from AIC of log transformation of y
aiclnfitvar <- c("x5","x6","x7","x8","x9","x10","x11","x12","x13")
f3 <- train[aiclnfitvar]
#View(f3)
cor(f3) #still have the same corr problem ~ double check how to read corr matrix
plot(aiclnfit)
#log transmformation is better bc res vs fitted is more random, less cluttered, has abline closer to h = 0
anova(aiclnfit)
#could take out x9 since it has a large pvalue

log(train$y) #used this to verify log(y) values



aiclnfit2 <- lm(log(train$y) ~ x5 + x6 + x7 + x8 + x10 + x11 + x12 + x13, data = train )
anova(aiclnfit2)
summary(aiclnfit2)
aiclnfit2var <- c("x5","x6","x7","x8","x10","x11","x12","x13")
f4 <- train[aiclnfit2var]
cor(f4)
mplot(aiclnfit2)
plot(aiclnfit2)

# continue selection procress
# not sure how the selection criteria code below works
s <- summary(lnfit)$sigma
#sigma is msres, s
s
n <- nrow(train)
n
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

res #output of res if model is aiclnfit2 is below
#        SSres           Rsq       Rsq_adj             C           AIC 
#    3.4350040     0.8893115     0.8858764    10.0000000 -1320.9293186 
# R and Radj are both high. C is close to 9, AIC is a 
#x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13
round(rbind(
  select_criteria(lm(log(train$y) ~ 1, data = train), n, s),
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
  select_criteria(lm(log(train$y) ~ x6 + x7 + x8 + x10 + x11 + x12, data = train), n, s),
  
  select_criteria(lm(log(train$y) ~ n6 + n7 + n8 + n10 + n11 + n12 + n13, data = nset), n, s),
  
  select_criteria(lm(yinv ~ x6 + x7 + x10 + x11 + x12inv + x13, data = train3), n, s)
), digits = 2)

# x9 was shown to be insignificant, therefore took it out in the second model
# tested the absence of each remaining variable w/o x9

step2 = stepAIC(lm(log(train$y) ~ x5 + x6 + x7 + x8 + x10 + x11 + x12 + x13, data = train),direction = "both")
# have not yet done VIF, cooks distnace, DFBETAS, or MSPE or other diagnoistic/validation
#look at context of the ob 

fit5 <- lm(log(train$y) ~ x6 + x7 + x8 + x10 + x11 + x12 + x13, data = train )
anova(fit5)
fit5var <- c("x6","x7","x8","x10","x11","x12","x13")
f5 <- train[fit5var]
cor(f5)
mplot(fit5)
plot(fit5)



#### RECODE VAR: centered along its mean, dummy var not present
x6 <- train$x6
n6 = x6 - mean(x6)

x7 <- train$x7
n7 <- x7 - mean(x7)

x8 <- train$x8
n8 <- x8 - mean(x8)

x10 <- train$x10
n10 <- x10 - mean(x10)

x11 <- train$x11
n11 <- x11 - mean(x11)

x12 <- train$x12
n12 <- x12 - mean(x12)

x13 <- train$x13
n13 <- x13 - mean(x13)

nset <- data.frame(n6, n7, n8, n10, n11, n12, n13)
View(train)
View(nset)
cor(nset)


###### MODEL VALIDATION SECTION ########

fit5 <- lm(log(train$y) ~ x6 + x7 + x8 + x10 + x11 + x12 + x13, data = train)

##surgic_valid <- read.csv("CH09TA05.txt", sep = "", header = FALSE)
##newx <- surgic_valid[,c(1, 2, 3)] #validation set is named test
##colnames(newx) <- c("X1","X2","X3")
newx5 <- test[, c(6,7,8,10,11,12,13)]
colnames(newx5) <- c("x6","x7","x8","x10","x11","x12","x13")
Y_pred <- predict(fit5, newx5)
Y_obs <- log(test$y)
n_star <- nrow(train)
MSPE <- sum( (Y_obs-Y_pred)^2/n_star )
MS_res <- (summary(fit5)$sigma)^2
round(MSPE, 5) # is 0.0765
round(MS_res, 5) # is 0.01217
#MSPE and MS_res have a small discrepency. There we can validate the model by MSPE to MR_res
summary(fit5)



#########testing to see if centering variables has an effect#########
nrow(nset)
fit_sel2 <- lm(log(train$y) ~ n6 + n7 + n8 + n10 + n11 + n12 + n13, data = nset)
##surgic_valid <- read.csv("CH09TA05.txt", sep = "", header = FALSE)
##newx <- surgic_valid[,c(1, 2, 3)] #validation set is named test
newx2 <- test[,c(6,7,8,10,11,12,13)]
colnames(newx2) <- c("n6","n7","n8","n10","n11","n12","n13")
Y_pred <- predict(fit_sel2, newx2)
Y_obs <- log(test$y)
n_star <- nrow(newx)
MSPE <- sum( (Y_obs-Y_pred)^2/n_star )
MS_res <- (summary(fit_sel2)$sigma)^2
round(MSPE, 5) # is 1.35517
round(MS_res, 5) # is 0.01217
# making a subset of centered datapoints along the mean  made a larger MSPE, 
# therefore should not centre datapoints. Revert to previous model, fit_sel = fit5



###
boxcox(log(train$y) ~ x12)
#gave optimal value of -1 --> y^-1 
###
newx12 <- (1/train$x12)
train2 <- data.frame(train,newx12)
newtestx12 <- (1/test$x12)
test2 <- data.frame(test, newtestx12)
View(test2)

View(train2)
fit6 <- lm(log(train$y) ~ x6 + x7 + x8 + x10 + x11 + x13 + I(1/x12), data = train2)
summary(fit6)
newx3 <- test2[, c(6,7,8,10,11,13,15)]
View(newx3)
colnames(newx) <- c("x6","x7","x8","x10","x11","x13","newx12")
nrow(newx3)
nrow(train2)
Y_pred <- predict(fit6, newx3)
Y_obs <- log(test2$y)
n_star <- nrow(train)
MSPE <- sum( (Y_obs-Y_pred)^2/n_star )
MS_res <- (summary(fit6)$sigma)^2
round(MSPE, 5) # is 0.0765
round(MS_res, 5) # is 0.01217
#MSPE and MS_res have a small discrepency. There we can validate the model by MSPE to MR_res
summary(fit6)
anova(fit6)
###



##### MODEL DIAGNOSTICS ##########
fit5 <- lm(log(train$y) ~ x6 + x7 + x8 + x10 + x11 + x12 + x13, data = train)

summary(fit5)
attach(train)
boxcox(train)
# Functional form
par(mfrow = c(2,2), oma = c(1,1,0,0), 
    mar = c(2,2,2,2), tcl = -0.1, mgp = c(1,0,0))
plot(fit5$residuals~x6, xlab = "x6", ylab = "Residuals")
#residuals are faily random, theref ore a lm is a fairly good fit
abline(h = 0)
plot(fit5$residuals~x7, xlab = "x7", ylab = "Residuals") 
#residuals are random, therefore a lm is a good fit
abline(h = 0)
plot(fit5$residuals~x8, xlab = "x8", ylab = "Residuals") 
#residuals are random, therefore a lm is a good fit
abline(h = 0)
plot(fit5$residuals~x10, xlab = "x10", ylab = "Residuals")  
abline(h = 0)
#lm is a good fit for x10
plot(fit5$residuals~x11, xlab = "x11", ylab = "Residuals")  
abline(h = 0)
#lm is a good fit for x11
plot(fit5$residuals~ x12, xlab = "x12", ylab = "Residuals")  
abline(h = 0)
#unhealthy residual plot
plot(fit5$residuals~x13, xlab = "x13", ylab = "Residuals")  
abline(h = 0)
#data is fairly random, is good fit for lm
plot(fit5$residuals~fit5$fitted.values, xlab = "Fitted values", ylab = "Residuals")  
#residuals are faily random, therefore a lm is a fairly good fit
abline(h = 0)
par(mfrow = c(1,1))
#therefore we see a lm is a good fit for the 

#diagnostic for fit6

fit6 <- lm(log(train$y) ~ x6 + x7 + x8 + x10 + x11 + x13 + I(1/(x12)), data = train2)
#log transformation on y and inverse transformation of predictor variable x12
summary(fit6)
attach(train2)
# Functional form
par(mfrow = c(2,2), oma = c(1,1,0,0), 
    mar = c(2,2,2,2), tcl = -0.1, mgp = c(1,0,0))
plot(fit6$residuals~x6, xlab = "x6", ylab = "Residuals")
#residuals are faily random, therefore a lm is a fairly good fit
abline(h = 0)
plot(fit6$residuals~x7, xlab = "x7", ylab = "Residuals") 
#residuals are random, therefore a lm is a good fit
abline(h = 0)
plot(fit6$residuals~x8, xlab = "x8", ylab = "Residuals") 
#residuals are random, therefore a lm is a good fit
abline(h = 0)
plot(fit6$residuals~x10, xlab = "x10", ylab = "Residuals")  
abline(h = 0)
#lm is a good fit for x10
plot(fit6$residuals~x11, xlab = "x11", ylab = "Residuals")  
abline(h = 0)
#lm is a good fit for x11
plot(fit6$residuals~ newx12, xlab = "x12", ylab = "Residuals")  
abline(h = 0)
#unhealthy residual plot
plot(fit6$residuals~x13, xlab = "x13", ylab = "Residuals")  
abline(h = 0)
#data is fairly random, is good fit for lm
plot(fit6$residuals~fit5$fitted.values, xlab = "Fitted values", ylab = "Residuals")  
#residuals are faily random, therefore a lm is a fairly good fit
abline(h = 0)
par(mfrow = c(1,1))
#therefore we see a lm is a good fit for the 


#an inverse transformation on only y gave an output which was the relfected plots of y
#therefore no effect
#consder a model with an inverse transformation on y and x12 at the same time
yin <- train$y
x12in <- train$x12
yinv <- (yin)^-1
x12inv <- (x12in)^-1
#set up new inverse objects for x12 and y
train3 <- data.frame(train,yinv,x12inv)
View((train3))
fit7 <- lm(yinv ~ x6 + x7 + x10 + x11 + x12inv + x13, data = train3)
summary(fit7)
anova(fit7)
plot(fit7)
#all predictors are still significant by pvalue. 
# in selection criteria, there is a very negative AIC, very negative C value and 0.84 for R and Radj
plot(fit7$residuals~ x12inv, xlab = "x12", ylab = "Residuals")  
abline(h = 0)
#no change in position with hetero/homo ???
###

###outlying observations
# Outlying Y observations
lny <- log(train$y)
# Statistical test
outlierTest(fit5)
# Studentized deleted residuals
t <- rstudent(fit5)
alpha <- 0.05
n <- length(lny)
p_prime = length(coef(fit5)) 
t_crit <- qt(1-alpha/(2*n),n-p_prime-1)
round(t,2)
t_crit #dont know what this is 
which(abs(t) > t_crit) #does this mean we have no y outliers? 
#no outliers????

# Outlying X observations 
Pii <- hatvalues(fit5)
round(Pii, 2)                           #normal QQ plot shows us we have 3 outliers. Not sure why we have 0 number of outliers
which(Pii > 2*p_prime/n)                #not entirely sure if normal QQ plot gives outliers
which(Pii > 0.5)
#not outliers???? 

#influential observations
influencePlot(fit5,	id.method="identify", 
              main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance" )
summary(fit5)
DFFITS <- dffits(fit5)
which(DFFITS > 1)
D <- cooks.distance(fit5)
which(D > qf(0.2, p_prime, n-p_prime))
#choose cooks distnace to measure influence


#leverage points
leveragePlots(fit5)
#may not need
#not sure about this :) 

#multicollinearity -> test VIFs
VIF0 <- vif(fit5)
VIF0
#note: fit5 <- lm(log(train$y) ~ x6 + x7 + x8 + x10 + x11 + x12 + x13, data = train)=
#    x6       x7       x8      x10      x11      x12      x13 
# 2.314536 2.615516 1.943063 1.197616 1.152337 1.160797 2.991531
#could be okay, online source has a collinearity threshold of 5, katherine said can have a threshold of 3, otherwise weak collinearity, which is okay
#fit5 = lm(log(train$y) ~ x6 + x7 + x8 + x10 + x11 + x12 + x13, data = train)
VIFbar <- mean(vif(fit5))
VIFbar
#1.911 --> needs to be below one
summary(fit5)



VIF1 <- vif(aiclnfit2)
VIF1
#x5       x6       x7       x8      x10      x11      x12      x13 
#4.027683 2.354901 2.766142 2.753190 1.297899 1.454300 1.265430 3.082881 
#threshold of VIF is 3. x5 shows moderate or strong ?.... collineratiy. the selection_criteria didnt change much when we took out x5 and the all
#variables were significant without x5. Safe to say we can ignore x5 -> collinear
VIFbar1 <- mean(VIF1)
VIFbar1
#VIFbar1 = 2.375303 > 1...... is it somewhat significantly larger than one?.... just something to explain why we omit x5



VIF2 <- vif(lm(log(train$y) ~ n6 + n7 + n8 + n10 + n11 + n12 + n13, data = nset))
VIF2
VIFbar2 <- mean(vif(lm(log(train$y) ~ n6 + n7 + n8 + n10 + n11 + n12 + n13, data = nset)))
VIFbar2 # 1.9 > 1 .... 
#we tried to fix collinearity with the methods that we know, by transformation and centering the data points along the mean,
#but there was no significant change in the collinearity 