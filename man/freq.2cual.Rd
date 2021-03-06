% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/freq.2cual.R
\name{freq.2cual}
\alias{freq.2cual}
\title{Tabla de frecuencias de dos variables cualitativas.}
\usage{
freq.2cual(
  datos,
  X1,
  X2,
  etiquetasx1,
  etiquetasx2,
  nombrex1,
  nombrex2,
  título1,
  título2
)
}
\arguments{
\item{X1}{vector de respuestas de la variable cualitativa 1 (filas)(tamaño n)}

\item{X2}{vector de respuestas de la variable cualitativa 2(columnas)(tamaño n)}

\item{Datos}{data.frame con los datos a analizar.}

\item{Etiquetasx1}{vector de cadena de caracteres con las posibles respuestas no numéricas de la variable 1}

\item{Etiquetasx2}{vector de cadena de caracteres con las posibles respuestas no numéricas de la variable 2}

\item{Nombrex1}{cadena de caracteres indicando la pregunta realizada para la variable x1}

\item{Nombrex2}{cadena de caracteres indicando la pregunta realizada para la variable x2}

\item{Título1}{cadena de caracteres indicando el nombre que se le quiere dar a la tabla de frecuencias absolutas}

\item{Título2}{cadena de caracteres indicando el nombre que se le quiere dar a la tabla de frecuencias relativas}
}
\description{
Función que genera una tabla de frecuencias absolutas y relativas
de las variable x1 y x2.
}
