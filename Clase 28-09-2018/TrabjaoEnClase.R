specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

F <- function(x) return((1/sqrt(2*pi))*exp(1)^(-(1/2)*x^2))

sumatoriaAreas <- function(a,b,n,f) {
  h = (b-a)/n
  suma = 0
  aux = a
  
  for (i in 1:n){
    
    
    
    rect(aux, 0, aux+h, f(aux+(h/2)))
    suma = suma + (h*f(aux+(h/2)))
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
    
    
    
    
    suma = suma + ((h*(f(aux)+f(aux+h))/2))
    aux = aux + h
    
    
    
  }
  
  return(specify_decimal(suma, 4))
  #return(suma)
}




a=-4
b=4
n=20

  plot(F, from = a, to = b, main= 'Grafica función', xlab = 'X', ylab = 'Y', col="red")

 x = 0.00 
  

  
prueba<-function()
{
  while(x<4){
    resultado = sumatoriaAreas(a,x,n,F)
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





