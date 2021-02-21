emma <- data.frame(pet=gl(2,5,labels=c("cat","dog")),happiness=c(rnorm(5,1,1),rnorm(5,5,1)))
lm.emma <- lm(happiness~pet,data=emma)
summary(lm.emma)
aggregate(emma$happiness,list(emma$pet),mean)
coef(lm.emma)[[2]]
coef(lm.emma)[[2]] + coef(lm.emma)[[1]]

# or
hapX <- aggregate(emma$happiness,list(emma$pet),mean)[,2]

tab_model(lm.emma)

tmp <- data.frame(Y=1:10,X=11:20)
tmp[c(1,5,2), c(1,2,1)]
