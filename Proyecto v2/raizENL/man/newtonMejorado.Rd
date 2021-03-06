% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NewtonMejorado.R
\name{newtonMejorado}
\alias{newtonMejorado}
\title{Busca una raíz con el método de newton mejorado}
\usage{
newtonMejorado(f, xi, tol = 1e-08, prec = 6)
}
\arguments{
\item{f}{función de una ecuación no lineal}

\item{xi}{x inicial}

\item{tol}{tolerancia (por defecto : 1e-8)}

\item{prec}{precisión de la respuesta (por defecto: 6)}
}
\value{
la aproximación a la raíz de f, si existe y el error
}
\description{
Por medio del método de newton, busca una raíz aproximada de una ecuación no lineal
}
\examples{

f = function (x) x^2-3
xi = 1.6
tol = 1e-6
prec = 5
res = triseccion (f, xi, tol, prec)
res

res = triseccion (f, xi, tol)
res

res = triseccion (f, xi)
res


}
