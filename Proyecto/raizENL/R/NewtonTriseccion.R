#' @import Deriv
#' @title Busca una raiz con el metodo de triseccion
#' @description Por medio de una particion en 3 intervalos de un intervalo a,b, iterativamente el programa busca una raiz para una ecuacion
#' no lineal
#' @param a limite menor del intervalo
#' @param b limite mayor del intervalo
#' @param f ecuacion no lineal
#' @param tol tolerancia
#' @param prec precision de la respuesta
#' @return la aproximacion a la raiz de f, si existe
#' @export triseccion
#' @examples
#' r=raizENL(f,a,b,tol,prec);
#' r=raizENL(f,a,b,,prec);
#' r=raizENL(f,a,b);
#'
#'
#------------------------------------------
triseccion = function (a, b, f, tol, prec){
  print("Metodo de triseccion")
  if (f(a)*f(b) > 0){
    print("No se puede aplicar el metodo de la triseccion.")
    cat("Raz?n:","f(a)=", f(a) , "y", "f(b)=", f(b), "tienen el mismo signo.\n")
  }
  else {
    plot(f, to=b, from=a, type = "l", main = "Gr?fica funci?n", xlab = "X", ylab = "Y", col = "red")
    cat("Tolerancia:", tol, "\n")
    cat("Precisi?n:", prec, "\n")
    xr1=((2*a)+b)/3
    xr2=(a+(2*b))/3
    if(f(a)==0){
      xr1 = a
    }
    else if(f(b)==0){
      xr2 = b
    }

    while (abs(f(xr1)) >= tol && abs(f(xr2)) >= tol && !abs(f(xr1))==0 && !abs(f(xr2)) ==0){
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
      respuesta<-data.frame(resultado =round(xr1, prec), error = abs(0-f(xr1)) )
      return(respuesta)
    }
    if (abs(f(xr2)) <= tol){
      cat("Error:", abs(0-f(xr2)), "\n")
      respuesta<-data.frame(resultado =round(xr2, prec), error = abs(0-f(xr2)) )
      return(respuesta)
    }
  }
}

#' @title Busca una raiz con el metodo de newton mejorado
#' @description Por medio del metodo de newton, busca una raiz aproximada de una ecuacion no lineal
#' @param f ecuacion no lineal
#' @param xi x inicial
#' @param tol tolerancia
#' @param prec precision de la respuesta
#' @return la aproximacion a la raiz de f, si existe
#' @export newtonMejorado
#' @examples
#' r=raizENL(f,xi,tol,prec);
#' r=raizENL(f,xi,tol);
#' r=raizENL(f,xi);
#'
#'
#------------------------------------------
newtonMejorado = function(f, xi, tol, prec){

  install.packages(Deriv)
  library(Deriv)
  print("Metodo newton mejorado")
  cat("Tolerancia:", tol, "\n")
  cat("Precisi?n:", prec, "\n")
  plot(f, to=xi-1, from=xi+1, type = "l", main = "Gr?fica funci?n", xlab = "X", ylab = "Y", col = "red")
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

  respuesta<-data.frame(resultado =round(x, prec), error = abs(0-f(x)) )

  return(respuesta)
}
#' @export raizENL
raizENL = function (f, a = NULL, b = NULL, xi=NULL, tol = 1e-5, prec = 4){
  if(!is.null(a)&&!is.null(b)){
    return(triseccion (a, b, f, tol, prec))
  }
  else if (!is.null(xi)){
    return(newtonMejorado(f, xi, tol, prec))
  }
}

