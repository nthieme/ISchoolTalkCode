library(mgcv)
library(MASS)
library(glmnet)
library(ggplot2)
###unbiased v efficient
samps.b1=replicate(1000,mean(rnorm(10,5,5)))
samps.b2 = replicate(1000, rnorm(10,5,5)[1])
hist(samps.b2, breaks = 50, col = "grey", main="Which is which estimator?", xlab="estimate of the mean")
hist(samps.b1, add=TRUE, breaks = 50, col = "#808080")
abline(v=mean(samps.b2), col = "maroon", lwd = 4)
abline(v=mean(samps.b1), col = "black", lwd = 4)

###multiple comparisons

f= function(n){
  X = replicate(n, rnorm(100))
  X = t(X)
  
  Y = matrix("g", nrow = nrow(X), ncol=nrow(X))
  for(i in 1:nrow(X)){
    for(j in 1:i){
      ttest=t.test(X[i,], X[j,])
      Y[i,j]=ttest$p.value
    }
  }
  
  Y = as.vector(Y)
  Y = as.numeric(Y[-which(Y=="g")])
  return(Y)
}

totals = c(10,50,100,500,1000)
vals = rep(0, length(totals))
ps= vector(length(vals), mode = "list")
for(i in 1:length(totals)){
  ps[[i]]=f(totals[i])
  vals[i]=length(which(ps[[i]]<.05))
}

bar=barplot(val, names.arg=totals, ylim = c(0, 28000), main="Number of p-vaues<.05", xlab="Number of variables to be compared")
text(x = bar, y = val+1000, label =val)

g = function(x){return(length(which(x<.05)))}
adj.vals = unlist(lapply(lapply(ps, p.adjust), g))
bar=barplot(adj.vals, names.arg=totals, ylim = c(0, 10), main="Number of p-vaues<.05; p-values adjusted", xlab="Number of variables to be compared")
text(x = bar, y = adj.vals+1, label =adj.vals)

###lm
plot(cars$speed, cars$dist, pch = 16,cex = .8, main="Car speed v. stopping distance", xlab = "Speed (mph)", ylab="Stopping distance (ft)")
mod = lm(dist~speed, data =cars)
lines(cars$speed,fitted(mod))
text(10,80, expression(paste(beta[0],"=", -17.5, collapse="")) , cex = 1.5)
text(10,70, expression(paste(beta[1],"=", 3.93, collapse="")) , cex = 1.5)

###mls
data(trees)
mod2 = lm(Volume~Girth*Height, data=trees)
plot(fitted(mod2), resid(mod2), main = "Residuals v Fitted", pch = 16)
###produce diagnostics
par(mfrow=c(2,2))
plot(mod2)

###aic/bic
non=rnorm(nrow(trees))
trees2 = cbind(trees,non)
mod3 = lm(Volume~Girth*non*Height, data=trees2)

###choosing by p-value issues
x1=rnorm(20)
y = x1+rnorm(20)
mod =lm(y~poly(x1,17))
mod1  = lm(y~x1)
summary(mod)
summary(mod1)
mean((mod$residuals)^2)
mean((mod1$residuals)^2)

fitted.t = fitted(mod1)
plot(x1, y, main="Poly model", ylab="y", pch = 16)
linp=cbind(x1,fitted(mod))
linp=linp[order(x1),]
lines(linp, lty=3)

plot(x1, y, main="True model", ylab="y", pch = 16)
lint=cbind(x1,fitted(mod1))
lint=lint[order(x1),]
lines(lint)

###test v train
D.tot = cbind(x1,y)
samp=sample((1:nrow(D.tot)),round(nrow(D.tot)/5))
D.test = D.tot[samp,]
D.train = D.tot[-samp,]
mod =lm(y~poly(x1,10), data.frame(D.train))
mod1  = lm(y~x1, data.frame(D.train))
mean((predict(mod, data.frame(D.test))-D.test[,2])^2)
mean((predict(mod1, data.frame(D.test))-D.test[,2])^2)

###ridge and lasso
D.use = Auto
samp = sample((1:nrow(D.use)), round(nrow(D.use)/5))
D.tr=D.use[-samp,]
D.te=D.use[samp,]
y = D.tr$mpg
X = data.frame(D.tr[,-c(1,9)])
y.te = D.te$mpg
X.te = data.frame(D.te[,-c(1,9)])
mod.r=glmnet(as.matrix(X), y, alpha = 0)
par(mar=c(4.5,4.5,1,4))
plot(mod.r, "lambda")
vnat=coef(mod.r)
vnat=vnat[-1,ncol(vnat)] # remove the intercept, and get the coefficients at the end of the path
text(7,vnat[c(1,5,6,7)],names(vnat)[c(1,5,6,7)]) 

mod.l=glmnet(as.matrix(X), y, alpha = 1)
par(mar=c(4.5,4.5,1,4))
plot(mod.l, "lambda")
vnat=coef(mod.l)
vnat=vnat[-1,ncol(vnat)] # remove the intercept, and get the coefficients at the end of the path
text(0,vnat[c(1,5,6,7)],names(vnat)[c(1,5,6,7)]) 

###cross validating lambda
cv.r=cv.glmnet(as.matrix(X),y, alpha=0)
cv.l=cv.glmnet(as.matrix(X),y, alpha=1)

mod.r=glmnet(as.matrix(X), y, alpha = 0, lambda = cv.r$lambda.min)
mod.l=glmnet(as.matrix(X), y, alpha = 1, lambda = cv.r$lambda.min)
mean((predict(mod.r, as.matrix(X.te))-y.te)^2)
mean((predict(mod.l, as.matrix(X.te))-y.te)^2)

mean(abs((predict(mod.r, as.matrix(X.te))-y.te)/y.te))
mean(abs((predict(mod.l, as.matrix(X.te))-y.te)/y.te))

lm.mod=lm(y~., data = X)
mean((predict(lm.mod, X.te)-y.te)^2)
mean(abs((predict(lm.mod, X.te)-y.te)/y.te))


###pca
num.D = Auto[,-c(1,9)]
pca=prcomp(~., data=num.D)
mpg = Auto$mpg
origin = ifelse(Auto$origin== 1, "USA", Auto$origin)
origin = ifelse(origin==2, "Eur", origin)
origin = ifelse(origin==3, "Japan", origin)
new.dat = data.frame(pca$x[,c(1,2)], mpg, as.factor(origin))
names(new.dat)=c("PC1", "PC2", "mpg", "origin")
par(mar=c(5,4,4,2))
cr <- colorRamp(c("green", "black"))
p <- ggplot(new.dat, aes(x = PC1, y = PC2)) + geom_point(aes(col = mpg, shape=origin), size=3)
p + scale_colour_gradientn(colours=c("green","black")) + theme_bw()

