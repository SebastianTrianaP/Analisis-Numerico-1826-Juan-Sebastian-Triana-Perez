#Avanzar en n de 2 en 2 desde  2
n=2
z <- fractalcurve(n, which="snowflake")
x <- z$x; y <- z$y
plot(x, y, type='l', col="darkred", lwd=2)
title("Snowflake Curve")

tam = length(x)

Distancia <- function(x,y) {
  suma = 0
  for (i in 2:tam-1){
    suma = suma + sqrt((x[i+1]-x[i])^2+(y[i+1]-y[i])^2)
  }
  return(suma)
}

resultado = Distancia(x,y)
resultado
