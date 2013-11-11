#R code
library('tseries')
library("lattice")
 
from='2013-01-01'
to='2013-11-8'
symbols=c('GG','ABX','DO','OIS','GE','SPY','OIH','GLD')
allReturns = c()
for(s in symbols) {
tmp = get.hist.quote(s,from,to)
tmpClose = rev(as.numeric(tmp$Close))
tmpReturn = log(tmpClose[-length(tmpClose)]/tmpClose[-1])
allReturns = cbind(allReturns, tmpReturn)
}
 
colnames(allReturns)=symbols
levelplot(cor(allReturns))

#R code
 
allReturns = allReturns[order(-1:-length(allReturns[,1])),]
meanReturns=matrix(colMeans(allReturns),nrow=1)
returnCov = cov(allReturns)
targetReturn = min(meanReturns)
 
plot(
sqrt(diag(returnCov)), meanReturns,
xlim = sqrt(c(0,max(diag(returnCov)))),
ylim = range(c(0,meanReturns)),
pch = 15, las = 1,
xlab = "Risk", ylab = "Return"
)
text( sqrt(diag(returnCov)), meanReturns, colnames(returnCov), adj=c(.5,-.5) )
 
f <- function(r) {
#long execution
if(length(meanReturns) > 100) cat("*")
sol = portfolio.optim(x = meanReturns, pm = r, covmat = returnCov, shorts = T)
return (sol$pw)
}
 
maxRet = max(abs(meanReturns))
minRet = -maxRet
returns = seq(minRet, maxRet,length=100)
 
require(parallel)
w <- mclapply(returns, f)
w <- do.call(rbind, w)
x <- sqrt( colSums(t(w) * (returnCov %*% t(w)) ))
y=w %*% t(meanReturns)
lines( x, y, col="grey" )


#R code
n=dim(allReturns)[2]
#assume equal weight
w1=rep(1/n,n)
pReturn=allReturns %*% t(t(w1))
allRet=as.data.frame(allReturns)
allRet=cbind(allRet,pReturn)
fit <- lm(pReturn~SPY+OIH+GLD,data=allRet)
layout(matrix(c(1,2,3,4),2,2))
plot(fit)


V=returnCov
x = sqrt( t(w) %*% V %*% w )
y=w %*% t(meanReturns)
points( x, y, pch=15, cex=2, col="#4444AA" )
text(x,y, "Market Neutral", adj=c(.5,-1))
