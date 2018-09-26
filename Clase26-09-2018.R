 x =c(1.8,1.9,2,2.1,2.2)
 h = c(0.5,0.4,0.3,0.2,0.1)
 
    
 F <- function(x) return(x*exp(x))
 
 fx <- F(x[1:5])

DT = data.table(x, fx)
DT

Fp <- function(h, x0) return((1/(12*h))*(F(x0-2*h)-8*F(x0-h)+8*F(x0+h)-F(x0+2*h)))

Fx<- function(x) return(exp(x)*(x+1))

error<-function(f1x,x) return(Fx(x)-f1x)


f1x = Fp(h[1:5],2)
f1x

Error = error(f1x[1:5],2)

Error

plot(h,Error,type="o",col="blue",lwd=3)
