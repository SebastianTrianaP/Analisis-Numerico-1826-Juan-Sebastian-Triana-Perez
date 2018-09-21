install.packages("ggplot2")#instalar paquete
library(ggplot2)
# Instalar libreria rSymPy, matrix
x=c (0.9, 1.3, 1.90, 2.1, 2.6, 3.0, 3.9, 4.40, 4.70, 5.0, 6.00, 7.0, 8.00, 9.20, 10.5, 11.3, 11.6, 12, 12.6, 13, 13.3, 13.3, 12, 10.7, 10, 8.50, 8.0, 7.5, 7.0, 6, 4.75, 5.10, 5.48, 5.6, 5.52, 5.0, 4.2, 3.50, 3, 2.55, 2.1,0.9)
fx=c (1.3, 1.5, 1.85, 2.1, 2.6, 2.7, 2.4, 2.15, 2.05, 2.1, 2.25, 2.3, 2.25, 1.95, 1.40, 0.90, 0.70, 0.6, 0.50, 0.4, 0.25, 0.2, -0.25, 0, 0.25, -0.5, -2.0, -3,-4, -4.80, -5.1, -4.0, -2.0, -1.0, 0.1, 0.5, 1.01, 1, 1.01, 1.05, 1.2,1.3)
difNewton <- function(x, y, x0) {
  require(rSymPy)
  n <- length(x)
  q <- matrix(data = 0, n, n)
  q[,1] <- y
  f <- as.character(round(q[1,1], 5))
  fi <- ''
  for (i in 2:n) {
    for (j in i:n) {
      q[j,i] <- (q[j,i-1] - q[j-1,i-1]) / (x[j] - x[j-i+1])
    }
    fi <- paste(fi, '*(x - ', x[i-1], ')', sep = '', collapse = '')
    f <- paste(f, ' + ', round(q[i,i], 5), fi, sep = '', collapse = '')
  }
  x <- Var('x')
  sympy(paste('e = ', f, collapse = '', sep = ''))
  approx <- sympy(paste('e.subs(x, ', as.character(x0), ')', sep = '', collapse = ''))
  return(list('Aproximacion de la interpolacion'=as.numeric(approx), 
              'Funcion Interpolada'=f, 
              'Diferencias Divididas'=q))
}

px <- function(x) {
  f <- x
}

# Genera el polinomio completo, pero tomamos una parte de los puntos del borde para ver la forma del polinomio en esa fraccion
DatosX1 = x[10:11]; DatosY1= fx[10:11]
difNewton(DatosX1,DatosY1,5)
dat <- data.frame(cbind(x, fx))
sc <-plot(dat, pch=20, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Pato")+ 
  geom_point(size=2, colour='red') + 
  stat_function(fun = px, size=1.25, alpha=0.4)

sc<-sc + scale_x_continuous(name="x", limits=c(0, 20)) +
  scale_y_continuous(name="fx", limits=c(0, 2))+ 
  lines(x,fx,col="red")
sc
