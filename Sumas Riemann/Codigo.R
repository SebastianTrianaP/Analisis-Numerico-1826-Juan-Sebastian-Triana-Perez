specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

F <- function(x) return(x^2)

sumatoriaAreas <- function(a,b,n,f) {
  div = (b-a)/n
  suma = 0
  aux = a
  
  for (i in 1:n){
    rect(aux, 0, aux+div, f(aux+(div/2)))
    suma = suma + (div*f(aux+(div/2)))
    aux = aux + div
  }
  
  return(specify_decimal(suma, 4))
  #return(suma)
}

a=0
b=2
n=4

plot(F, from = a-0.5, to = b+0.5, ylim=c(0,10), main= 'Grafica función', xlab = 'X', ylab = 'Y', col="red")
resultado = sumatoriaAreas(a,b,n,F)
resultado
