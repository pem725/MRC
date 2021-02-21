# scratch file

## taken from my EFA shiny app:

GenDataReg <- function(N,Np,Nc,COR,Wts,R2){ ## where N=sample size, Np=num of pred, Nc=num of common factors of predictors, COR=avg cor among pred, Wts=regression wts, R2=rel bet yhat and yobs
  # first create the predictor variables  
  NV <- round((Np/Nc)) ## evenly distributed number of vars per factor
  pdat <- data.frame(id=1:N)
  for(i in 1:Nc){
    cormat <- matrix(rbinom(NV*NV,100,COR)/100,NV,NV)
    diag(cormat) <- 1
    U <- t(chol(cormat))
    random.normal <- matrix(rnorm(NV*N,0,1),NV,N)
    tmp2 <- as.data.frame(t(U %*% random.normal))
    names(tmp2) <- paste("P",1:NV,i,sep="")
    pdat <- cbind(pdat,tmp2)
  }
  pdat <- pdat[,-1]

  ## now created the Y vars
  if (is.null(Wts)){
    Wts <- rep(1,ncol(pdat))
  } else if (length(Wts) != ncol(pdat)){
      Wts <- rep(1,ncol(pdat))
  }
  xw <- pdat
  for (i in 1:length(Wts)){
    xw[,i] <- pdat[,i]*Wts[i]
  }
  y.hat <- rowSums(xw)
  err1 <- rnorm(N, mean(y.hat), sd(y.hat)) # random error
  y.obs <- scale(y.hat)*sqrt(R2) + scale(residuals(lm(err1~y.hat))) * sqrt(1-R2)
  out <- data.frame(ID=1:N,Y=y.obs,Y.hat=y.hat,pdat)
  return(out)
}


test <- GenDataReg(100,4,2,.4,c(1,2,3,4),.4)
str(test)

test <- PDat(100,6,2,.5)
str(test)
test <- YDat(test,R2=.3)
str(test)

test2 <- test[,c(2,4:ncol(test))]

lm1 <- lm(Y~.,data=test2)
summary(lm1)

createCorVars <- function(N,rxx){
  v1 <- rnorm(N)
  err1 <- rnorm(N)
  v2 <- scale(v1)*sqrt(rxx) + scale(residuals(lm(err1~v1))) * sqrt(1-rxx)
  obscor <- cor(v1,v2)
  expcor <- sqrt(rxx)
  delta <- obscor - expcor
  dat <- data.frame(v1,v2)
  out <- list(data=dat,Expected=expcor,Observed=obscor,Delta=delta)
  return(out)
}


testme <- createCorVars(100,.2)
library(psych)
describe(testme$data)
summary(testme$data)

mydat <- testme$data

plot(mydat$v1,mydat$v2)
plot(mydat$v1)
plot(mydat$v2)
hist(mydat$v2)

lm1 <- lm(v2~v1,data=mydat)
lm1a <- lm(v2~1,data=mydat)
plot(lm1,ask=F)
hist(resid(lm1))
str(lm1)
summary(lm1)

lm2 <- lm(v2~v1,data=mydat[-c(30,62,91),])
summary(lm2)
plot(lm2,ask=F)

anova(lm1,lm1a)

lm1.z <- lm(scale(v2)~scale(v1)-1,data=mydat)
summary(lm1.z)



v1 <- rnorm(10000)
v2 <- v1 + rnorm(10000,0,sqrt(3))
cor(v1,v2)

