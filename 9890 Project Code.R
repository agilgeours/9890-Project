install.packages("randomForest")
library(plyr)
library(glmnet)
library(randomForest)
library(dplyr)
#### Part 2 of Project ####### Proposal ####


biodeg = read.csv("/Users/arielgilgeours/Documents/biodegradation.csv")
biodeg

#Standarizing the numerical predictors using equation (6.6) in ISLR
biodeg1 = biodeg %>%
  select(-ExpC.RV) %>%
  mutate_all(.funs = function(x) {x / sqrt(mean((x-mean(x))^2))}) %>%
  mutate(ExpC.RV=biodeg$ExpC.RV)

glimpse(biodeg1)

n = 1055
p = 41

##### Part 3 of Project ####

# For each ntrain = 0.8n, repeat the following 100 times, plot the box-plots of the errors
# of the different models mentioned below.
# ntrain = 844
# ntest is equal to 211

# (a)   Randomly split the dataset into two mutually exclusive datasets Dtest and Dtrain
# with size ntest and ntrain such that ntrain + ntest = n
#ISLR pg 264 pg 253/248 two different ways 


ntrain = floor((0.8)*(n))
ntest = n - ntrain
train = sample.int(n, ntrain)
dtrain = biodeg1[train,]
dtest = biodeg1[-train,]


# (b)  Use Dlearn to fit lasso, elastic-net α = 0.5, ridge, and random forrests
# The matrices were then subjected to multiple linear and non-linear regression
#analyses to determine probability coefficients for each fragment. 
# (c) Tune the λs using 10-fold CV.
# (d) For each estimated model calculate *the formula can be found in project description




M = 100
lasso.rsquare.train =  rep(0,M)  
lasso.rsquare.test = rep(0,M)
elastic.rsquare.train = rep(0,M)  
elastic.rsquare.test = rep(0,M)
ridge.rsquare.train = rep(0,M)
ridge.rsquare.test = rep(0,M)
ranfor.rsquare.train = rep(0,M)
ranfor.rsquare.test = rep(0,M)

for (m in c(1:M)) {
  
Xtrain = data.matrix(select(dtrain, -Sp_Max_L))
Xtest = data.matrix(select(dtest, -Sp_Max_L))
ytrain = dtrain$Sp_Max_L
ytest = dtest$Sp_Max_L
y = biodeg1$Sp_Max_L


#lasso
lassocv = cv.glmnet(Xtrain, ytrain, alpha =1, nfolds = 10)
lassofit = glmnet(Xtrain, ytrain, alpha =1, lambda = lassocv$lambda.min)
lasso.y.train = predict(lassofit, Xtrain)
lasso.y.test = predict(lassofit, Xtest)
#lasso residuals
lasso.residual.train = as.vector(ytrain - lasso.y.train)
lasso.residual.test = (ytest - lasso.y.test) 
# lasso r square
lasso.rsquare.train[m] = 1 - mean((lasso.residual.train)^2) / mean((y-mean(y))^2)
lasso.rsquare.test[m] = 1 - mean((lasso.residual.test)^2) / mean((y-mean(y))^2)


#elastic-net a= 0.5
elasticcv = cv.glmnet(Xtrain, ytrain, alpha =0.5, nfolds = 10)
elasticfit = glmnet(Xtrain, ytrain, alpha =0.5, lambda = elasticcv$lambda.min)
elastic.y.train = predict(elasticfit, Xtrain)
elastic.y.test = predict(elasticfit, Xtest)
#elastic-net residuals
elastic.residual.train = as.vector(ytrain - elastic.y.train)
elastic.residual.test = (ytest - elastic.y.test) 
#elastic-net r square
elastic.rsquare.train[m] = 1 - mean((elastic.residual.train)^2) / mean((y-mean(y))^2)
elastic.rsquare.test[m] = 1 - mean((elastic.residual.test)^2) / mean((y-mean(y))^2)


#ridge
ridgecv = cv.glmnet(Xtrain, ytrain, alpha = 0, nfolds = 10)
ridgefit = glmnet(Xtrain, ytrain, alpha = 0, lambda = ridgecv$lambda.min)
ridge.y.train = predict(ridgefit, Xtrain)
ridge.y.test = predict(ridgefit, Xtest)
#ridge residuals
ridge.residual.train = as.vector(ytrain - ridge.y.train)
ridge.residual.test = (ytest - ridge.y.test) 
# ridge r square
ridge.rsquare.train[m] = 1 - mean((ridge.residual.train)^2) / mean((y-mean(y))^2)
ridge.rsquare.test[m] = 1 - mean((ridge.residual.test)^2) / mean((y-mean(y))^2)


#random forests
ranforfit = randomForest(Xtrain, ytrain, mtry = sqrt(p), importance = TRUE)
ranfor.y.train = predict(ranforfit, Xtrain)
ranfor.y.test = predict(ranforfit, Xtest)
#random forrest residuals
ranfor.residual.train = as.vector(ytrain - ranfor.y.train)
ranfor.residual.test = (ytest - ranfor.y.test) 
#random forrest r square
ranfor.rsquare.train[m] = 1 - mean((ranfor.residual.train)^2) / mean((y-mean(y))^2)
ranfor.rsquare.test[m] = 1 - mean((ranfor.residual.test)^2) / mean((y-mean(y))^2)
 
cat(sprintf("m=%3.f| lasso.rsquare.train=%.2f,  lasso.rsquare.test=%.2f| elastic.rsquare.train=%.2f,  elastic.rsquare.test=%.2f| ridge.rsquare.train=%.2f,  ridge.rsquare.test=%.2f| ranfor.rsquare.train=%.2f,  ranfor.rsquare.test=%.2f| \n", m, lasso.rsquare.train[m], lasso.rsquare.test[m], elastic.rsquare.train[m], elastic.rsquare.test[m], ridge.rsquare.train[m], ridge.rsquare.test[m], ranfor.rsquare.train[m], ranfor.rsquare.test[m]))

}

# 4. Create a presentation with less than 6 slides. Your objective is to be clear and concise.
# (a) a brief description of the nature of the data, shape, etc as discussed above. (1 slide)

# (b) Show the side-by-side boxplots of Rtest,Rtrain



boxplot(lasso.rsquare.train, lasso.rsquare.test,
        main = "Lasso Regression",
        names = c("Train", "Test"))

boxplot(elastic.rsquare.train, elastic.rsquare.test,
        main = "Elastic Net Regression",
        names = c("Train", "Test"))

boxplot(ridge.rsquare.train, ridge.rsquare.test,
        main = "Ridge Regression",
        names = c("Train", "Test"))

boxplot(ranfor.rsquare.train, ranfor.rsquare.test,
        main = "Random Forest",
        names = c("Train", "Test"))



# (c) For one on the 100 samples, create 10-fold CV curves for lasso, elastic-net α = 0.5, ridge.

plot(lassocv, main = "Lasso",)
plot(elasticcv,main = "Elastic-Net")
plot(ridgecv, main = "Ridge")

# (d) For one on the 100 samples, show the side-by-side boxplots of train and test residuals (1 slide). 
#Comment on the distribution and size of the residuals.

boxplot(lasso.residual.train, lasso.residual.test, main = "Lasso Regression",
        names = c("Train", "Test"))
boxplot(elastic.residual.train, elastic.residual.test,  main = "Elastic Net Regression",
        names = c("Train", "Test"))
boxplot(ridge.residual.train, ridge.residual.test,  main = "Ridge Regression",
        names = c("Train", "Test"))
boxplot(ranfor.residual.train, ranfor.residual.test,  main = "Random Forest Regression",
        names = c("Train", "Test"))


# (e) Present bar-plots (with bootstrapped error bars) of the estimated coefficients, and the importance of the parameters.
# If you have something interesting to say about coefficients that are (or are not important) say it. (1 slide)

X = biodeg1 %>% select(-ExpC.RV) %>% data.matrix()

bootstrapSamples =     100
beta.la.bs       =     matrix(0, nrow = p, ncol = bootstrapSamples)    
beta.en.bs       =     matrix(0, nrow = p, ncol = bootstrapSamples)         
beta.ri.bs       =     matrix(0, nrow = p, ncol = bootstrapSamples)    
beta.rf.bs       =     matrix(0, nrow = p, ncol = bootstrapSamples)         


for (m in 1:bootstrapSamples){
  bs_indexes       =     sample(n, replace=T)
  X.bs             =     X[bs_indexes, ]
  y.bs             =     y[bs_indexes]
  
  #fit bs lasso
  lasso.cv.fit           =     cv.glmnet(X.bs, y.bs, intercept = FALSE, alpha = 1, nfolds = 10)
  lasso.fit              =     glmnet(X.bs, y.bs, intercept = FALSE, alpha = 1, lambda = lasso.cv.fit$lambda.min)  
  beta.la.bs[,m]   =     as.vector(lasso.fit$beta)
  #fit bs ridge 
  ridge.cv.fit           =     cv.glmnet(X.bs, y.bs, intercept = FALSE, alpha = 0, nfolds = 10)
  ridge.fit              =     glmnet(X.bs, y.bs, intercept = FALSE, alpha = 0, lambda = ridge.cv.fit$lambda.min)  
  beta.ri.bs[,m]   =     as.vector(ridge.fit$beta)
  # fit bs rf
  rf               =     randomForest(X.bs, y.bs, mtry = sqrt(p), importance = TRUE)
  beta.rf.bs[,m]   =     as.vector(rf$importance[,1])
 
   # fit bs en
  elastic.cv.fit           =     cv.glmnet(X.bs, y.bs, intercept = FALSE, alpha = 0.5, nfolds = 10)
  elastic.fit              =     glmnet(X.bs, y.bs, intercept = FALSE, alpha = 0.5, lambda = elastic.cv.fit$lambda.min)  
  beta.en.bs[,m]   =     as.vector(elastic.fit$beta)
  cat(sprintf("Bootstrap Sample %3.f \n", m))
  
}

#calculate bootstrapped standard errors / alternatively you could use qunatiles to find upper and lower bounds
la.bs.sd = apply(beta.la.bs, 1, "sd")
ri.bs.sd = apply(beta.ri.bs, 1, "sd")
rf.bs.sd = apply(beta.rf.bs, 1, "sd")
en.bs.sd = apply(beta.en.bs, 1, "sd")


# fit rf to the whole data
rf  = randomForest(X, y, mtry = sqrt(p), importance = TRUE)

# fit en to the whole data
elastic.cv.fit = cv.glmnet(X, y, alpha = 0.5, nfolds = 10)
elastic.fit = glmnet(X, y, alpha = 0.5, lambda = elastic.cv.fit$lambda.min)

# fit la to the whole data
lasso.cv = cv.glmnet(X, y, alpha = 1)
lasso.fit = glmnet(X, y, alpha = 0.5, lambda = lasso.cv$lambda.min)
# fit ridge to the whole data
ridge.cv = cv.glmnet(X, y, alpha = 0)
ridge.fit = glmnet(X, y, alpha = 0.5, lambda = ridge.cv$lambda.min)

cols = 1:length(names(X[1,])) %>% as.factor()

betaS.lasso = data.frame(feature = cols, 
                          value = as.vector(lasso.fit$beta), 
                          error = 2*la.bs.sd, 
                          model = "elastic")
betaS.ridge = data.frame(feature = cols, 
                          value = as.vector(ridge.fit$beta), 
                          error = 2*ri.bs.sd, 
                          model = "elastic")
betaS.rf = data.frame(feature = cols, 
                       value = as.vector(rf$importance[,1]), 
                       error = 2*rf.bs.sd, 
                       model = "elastic")
betaS.elastic = data.frame(feature = cols, 
                          value = as.vector(elastic.fit$beta), 
                          error = 2*en.bs.sd, 
                          model = "elastic")


rfPlot =  ggplot(betaS.rf, aes(x=feature, y=value)) +
  geom_bar(stat = "identity", fill="white", colour="black")    +
  geom_errorbar(aes(ymin=value-error, ymax=value+error), width=.2)
rfPlot

elasticPlot =  ggplot(betaS.elastic, aes(x=feature, y=value)) +
  geom_bar(stat = "identity", fill="white", colour="black")    +
  geom_errorbar(aes(ymin=value-error, ymax=value+error), width=.2)

elasticPlot






# we need to change the order of factor levels by specifying the order explicitly.
betaS.rf$feature     =  factor(betaS.rf$feature, levels = betaS.rf$feature[order(betaS.rf$value, decreasing = TRUE)])
betaS.elastic$feature     =  factor(betaS.elastic$feature, levels = betaS.rf$feature[order(betaS.rf$value, decreasing = TRUE)])



rfPlot =  ggplot(betaS.rf, aes(x=feature, y=value)) +
  geom_bar(stat = "identity", fill="white", colour="black")    +
  geom_errorbar(aes(ymin=value-error, ymax=value+error), width=.2) +
  ggtitle("Random Forest") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
rfPlot

elasticPlot =  ggplot(betaS.elastic, aes(x=feature, y=value)) +
  geom_bar(stat = "identity", fill="white", colour="black")    +
  geom_errorbar(aes(ymin=value-error, ymax=value+error), width=.2) +
  ggtitle("Elastic Net")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
elasticPlot

grid.arrange(rfPlot, elasticPlot, nrow = 2)


# (f) Summary slide: summarize the performance and the time need to train each model in a table and comment on it. (1 slide)















