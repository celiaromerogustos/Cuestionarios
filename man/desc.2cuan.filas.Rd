% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/desc.2cuan.filas.R
\name{desc.2cuan.filas}
\alias{desc.2cuan.filas}
\title{Tabla de resúmenes numéricos para dos variables cuantitativa  por filas}
\usage{
desc.2cuan.filas(x1, x2, nombrex1, nombrex2, título)
}
\arguments{
\item{Datos}{data.frame con los datos a analizar.}

\item{X1}{vector de respuestas de la primera variable cuantitativa  (tamaño n)}

\item{X2}{vector de respuestas de la segunda variable cuantitativa  (tamaño n)}

\item{Nombrex1}{cadena de caracteres indicando el nombre de la primera variable ( No admite espacios)}

\item{Nombrex2}{cadena de caracteres indicando el nombre de la segunda variable ( No admite espacios)}

\item{Título}{cadena de caracteres que indica el nombre que se le quiere dar a la tabla}
}
\description{
función que devuelve una tabla con las medidas descriptivas
 más relevante del análisis de variables numéricas, n (tamaño muestral),
 media, mediana, desv(desviación estandar), IC1, IC2, el intervalo de
 confianza con un nivel del 95% de X1 y X2 independiente uno de otro, dispuestos por filas
}
