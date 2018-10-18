triseccion = function (a, b, f, tol, prec){
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
      cat("Error:", abs(0-f(xr1)))
      return(round(xr1, prec))
    }
    if (abs(f(xr2)) <= tol){
      cat("Error:", abs(0-f(xr2)))
      return(round(xr2, prec))
    }
  }
}

F = function(x) return (x+3)
a = -4
b = -2
tol = 1e-8
prec = 5
resultado = triseccion (a, b, F, tol, prec)
resultado
