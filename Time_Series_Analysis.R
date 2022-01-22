y<-read.csv(file.choose(),header=TRUE)
y.ts<-ts(y[,2],frequency=12)
plot(y.ts,main="Time series Plot of y(t)")
acf(y.ts,lag.max=50, main="ACF for y(t)")
pacf(y.ts,lag.max=50,main="PACF for y(t)")
y.ln<-log(y.ts)
plot(y.ln,main="Time series Plot of y(t)")

y011111 <- arima(y.ln,order=c(0,0,1),seasonal=list(order=c(0,1,1),period=12))
y011111

acf(resid(y011111),lag=50,main='The residuals of ACF plot of SARIMA(1,0,1),(0,1,1)12')

library(forecast)
y.best=auto.arima(y.ln,max.p=3,max.d=2,max.q=3,max.P=3,max.D=2,max.Q=3,ic='aic',stationary=FALSE,stepwise=FALSE,trace=TRUE)

z<-diff(y.ln,lag=1)
plot(z,main="Time series Plot of z(t)")
acf(z,lag.max=50,main="ACF for z(t)")
pacf(z,lag.max=50,main="PACF for z(t)")

w<-diff(z,lag=12)
plot(w,main="Time series Plot of w(t)")
acf(w,lag.max=50,main="ACF for w(t)")
pacf(w,lag.max=50,main="PACF for w(t)")

best.model<-function(y.ln,maxord=c(2,1,2,2,1,2)) {
  best.aic<-Inf
  n<-length(y.ln)
  for (p in 0:maxord[1]) for (d in 1:maxord[2]) for (q in 1:maxord[3])
    for (P in 1:maxord[4]) for (D in 1:maxord[5]) for (Q in 1:maxord[6])
    {
      fit<-arima(y.ln,order=c(p,d,q),
                 seas=list(order=c(P,D,Q),
                           frequency(y.ln)),method="CSS")
      fit.aic<- -2*fit$loglik+2*length(fit$coef)
      if(fit.aic<best.aic)
      {
        best.aic<-fit.aic
        best.fit<-fit
        best.model<-c(p,d,q,P,D,Q)
      } }
  list(best.aic,best.fit,best.model)
}
best.arima.y<-best.model(y.ln,maxord=c(2,1,2,2,1,2))
best.arima.y[[3]]
    
y.omit<-y.ln[1:108]
y.lomit<-log(y.omit)
y.act<-y.ts[109:120]
SSE<-function(sarima)
  
{
  ord<-sarima
  x<-data.frame("n-step-ahead"=1:12,"SSE"=0)
  for (i in 1:12)
  {
    y.pred=0
    SSEval=0
    fit.sse<-arima(y.lomit,order=c(ord[1],ord[2],ord[3]), seasonal=list(order=c(ord[4],ord[5],ord[6]),period=12))
    y.pred=exp(predict(fit.sse,i)$pred)
    for (j in 1:i)
    {
      SSEval=SSEval+(y.act[j]-y.pred[j])^2
    }
    x[i,2]=SSEval
  }
  return(x) }
SSE.011011=SSE(c(2,1,2,2,1,2))[12,2];SSE.011011
SSE.101011=SSE(c(0,2,2,3,0,0))[12,2];SSE.101011

best.fit.indus<-y011011
acf(resid(best.fit.indus))
exp(predict(best.fit.indus,12)$pred)
ts.plot(cbind(window(y.ts,start=2000),
              exp(predict(best.fit.indus)$pred)),lty=1:2)

y.res<-as.vector(residuals(y011011))
y.fit<-as.vector(fitted(y011011))
#4-in-1 plot of residuals
par(mfrow=c(2,2),oma=c(0,0,0,0))
qqnorm(y.res,datax=TRUE,pch=16,xlab='Residual',main='')
qqline(y.res,datax=TRUE)
plot(y.fit,y.res,pch=16,xlab='Fitted Value',ylab='Residual') > abline(h=0)
hist(y.res,col="gray",xlab='Residual',main='')
plot(y.res,type="l",xlab='Observation Order',ylab='Residual') > points(y.res,pch=16,cex=0.5)
abline(h=0)
