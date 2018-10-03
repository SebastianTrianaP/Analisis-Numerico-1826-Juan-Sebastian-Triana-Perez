specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

#f <- function(x) return((1/sqrt(2*pi))*exp(1)^(-(1/2)*x^2))


f <- function(x) return(x^2)
g <- function(x) return(-(x^2)+2)


sumatoriaAreas <- function(a,b,n,f) {
  h = (b-a)/n
  suma = 0
  aux = a
  
  for (i in 1:n){
    
    polygon(0, 10)
    
    rect(aux, 0, aux+h, f(aux+(h/2)))
    suma = suma + (h*(aux+(h/2)))
    aux = aux + h
    
    
    
  }
  
  return(specify_decimal(suma, 4))
  #return(suma)
}

sumatoriaAreasTrapecio <- function(a,b,n,f) {
  h = (b-a)/n
  suma = 0
  aux = a
  
  for (i in 1:n){
    
    
    polygon(c(aux,aux,aux+h,aux+h), c(f(aux),0,0,f(aux+h)), angle = c(90, 90,90,90))
   
    suma = suma + ((h*(f(aux)+f(aux+h))/2))
    aux = aux + h
    
   
   
    
    
    
  }
  
  return(specify_decimal(suma, 4))
  #return(suma)
}




sumatoriaAreasCurvasTrapecio <- function(a,b,n,f,g) {
  h = (b-a)/n
  suma = 0
  aux = a
  
  for (i in 1:n){
    
    
    polygon(c(aux,aux,aux+h,aux+h), c(f(aux),g(aux),g(aux+h),f(aux+h)), angle = c(90, 90,90,90))
    
    primerAltura = f(aux)-g(aux)
    segundaAltura = f(aux+h)-g(aux+h)
    
    suma = suma + ((h*(primerAltura+segundaAltura)/2))
    aux = aux + h
    
    
    
    
    
    
  }
  
  return(specify_decimal(suma, 4))
  #return(suma)
}


a=-1
b=1
n=100

plot(f, from = a, to = b, main= 'Grafica función', xlab = 'X', ylab = 'Y', col="red",xlim=c(-2,2),ylim=c(-2,2))
par(new=TRUE)
plot(g, from = a, to = b, main= 'Grafica función', xlab = 'X', ylab = 'Y', col="red",xlim=c(-2,2),ylim=c(-2,2))




x = 0.00 
resultado = sumatoriaAreasCurvasTrapecio(a,b,n,g,f)
resultado


prueba<-function()
{
  while(x<4){
    resultado = sumatoriaAreasTrapecio(a,x,n,f)
    print(x)
    print(resultado)
    x=x+0.01
  }
}
prueba()



#cambio de coordenadas
#hacer integral flor
#hacer doble integrales
#hacer integrales triples
#hacer intregal curva parametrica
