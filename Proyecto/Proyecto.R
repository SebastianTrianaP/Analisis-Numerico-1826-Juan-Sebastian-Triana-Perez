#Metodo de triseccion
#------------------------------------------
triseccion = function (a, b, f, tol, prec){
  print("Metodo de triseccion")
  if (f(a)*f(b) > 0){
    print("No se puede aplicar el metodo de la triseccion.")
    cat("Razón:","f(a)=", f(a) , "y", "f(b)=", f(b), "tienen el mismo signo.\n")
  }
  else {
    plot(f, to=b, from=a, type = "l", main = "Gráfica función", xlab = "X", ylab = "Y", col = "red")
    cat("Tolerancia:", tol, "\n")
    cat("Precisión:", prec, "\n")
    xr1=((2*a)+b)/3
    xr2=(a+(2*b))/3
    while (abs(f(xr1)) >= tol || abs(f(xr2)) >= tol){
      points(x= xr1, y= f(xr1), col = "blue")
      points(x= xr2, y= f(xr2), col = "green")
      if (f(xr1)*f(a)<0)
      {
        b=xr1
      }
      else if (f(xr1)*f(xr2)<0)
      {
        a=xr1
        b=xr2
      }
      else if (f(xr2)*f(b)<0)
      {
        a=xr2
      }
      
      xr1=((2*a)+b)/3
      xr2=(a+(2*b))/3
    }
    
    if (abs(f(xr1)) <= tol){
      cat("Error:", abs(0-f(xr1)), "\n")
      return(round(xr1, prec))
    }
    if (abs(f(xr2)) <= tol){
      cat("Error:", abs(0-f(xr2)), "\n")
      return(round(xr2, prec))
    }
  }
}

#Metodo de newton mejorado
#------------------------------------------
newtonMejorado = function(f, xi, tol, prec){
  library(Deriv)
  print("Metodo newton mejorado")
  cat("Tolerancia:", tol, "\n")
  cat("Precisión:", prec, "\n")
  plot(f, to=xi-1, from=xi+1, type = "l", main = "Gráfica función", xlab = "X", ylab = "Y", col = "red")
  f1 = Deriv(f,"x", nderiv=1)
  f2 = Deriv(f,"x", nderiv=2)
  ea = 1000
  x<-xi
  points(x= x, y= f(x), col = "blue")
  while (ea > tol){
    g = f(x)
    h = f1(x)
    k = f2(x)
    j = x-(g*h)/(h^(2)-(g*k))
    ea=abs(j-x)
    x = j
    points(x= x, y= f(x), col = "blue")
  }
  cat("Error:", abs(0-f(x)), "\n")
  return(round(x, prec))
}

raizEcuNoLin = function (a = NULL, b = NULL, f, xi=NULL, tol, prec){
  if(!is.null(a)){
      return(triseccion (a, b, f, tol, prec))
  }
  else if (!is.null(xi)){
    return(newtonMejorado(f, xi, tol, prec))
  }
}

#Uso para el metodo de triseccion : raizEcuNoLin(a = a, b = b, f = f, tol = tol, prec = prec)
#Uso para el metodo de newton mejorado : raizEcuNoLin(f = f, xi = xi, tol = tol, prec = prec)
