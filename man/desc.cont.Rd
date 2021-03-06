% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/desc.cont.R
\name{desc.cont}
\alias{desc.cont}
\title{Tabla de resúmenes numéricos para variables continuas.}
\usage{
desc.cont(datos, x, nombre, título)
}
\arguments{
\item{Datos}{data.frame con los datos a analizar.}

\item{X}{vector de respuestas de la variable continua a analizar (tamaño n)}

\item{Nombre}{cadena de caracteres indicando la pregunta realizada. (No admite espacios)}

\item{Título}{cadena de caracteres que indica el nombre que se le quiere dar a la tabla}
}
\description{
Función que devuelve una tabla con las medidas descriptivas
 más relevante del análisis de variables numéricas, n (tamaño muestral),
 media, mediana, desv(desviación estandar), IC1, IC2, el intervalo de
 confianza con un nivel del 95% de X.
}
