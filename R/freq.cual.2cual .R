#' @import dplyr
#' @import tables
#' @title Tabla de frecuencias de tresz variables cualitativas
#' @description Función que genera una tabla de frecuencias absolutas
#' de las variable x1, x2 y x3.
#' @param Datos data.frame con los datos a analizar.
#' @param X1 vector de respuestas de la variable cualitativa 1 (filas)(tamaño n)
#' @param X2 vector de respuestas de la variable cualitativa 2 (filas)(tamaño n)
#' @param X2 vector de respuestas de la variable cualitativa 3 (columnas)(tamaño n)
#' @param etiquetasx1 vector de cadena de caracteres con las posibles respuestas no numéricas de la variable 1
#' @param etiquetasx2 vector de cadena de caracteres con las posibles respuestas no numéricas de la variable 2
#' @param etiquetasx3 vector de cadena de caracteres con las posibles respuestas no numéricas de la variable 3
#' @param Nombre1 cadena de caracteres indicando la pregunta realizada para la variable x1
#' @param Nombre2 cadena de caracteres indicando la pregunta realizada para la variable x2
#' @param Nombre3 cadena de caracteres indicando la pregunta realizada para la variable x3
#' @param Título cadena de caracteres indicando el nombre que se le quiere dar a la salida
#' @export

freq.cual.2cual = function(datos,X1,X2, X3, etiquetasx1, etiquetasx2,etiquetasx3,
                     nombre1, nombre2,nombre3, título ) {
  datos1 <- mutate(datos, v1 = factor(X1, labels = etiquetasx1))
  datos2 <- mutate(datos, v2 = factor(X2, labels = etiquetasx2))
  datos3 <- mutate(datos, v3 = factor(X3, labels = etiquetasx3))

  #
  tabla <- "(nombre1 = datos1$v1 ) * ( nombre2 = datos2$v2 ) +
  ( Total = 1 ) ~  (  nombre3 = datos3$v3 )+ ( Total = 1 )"
  tabla <- gsub("nombre1", nombre1, tabla)
  tabla <- gsub("nombre2", nombre2, tabla)
  tabla <- gsub("nombre3", nombre3, tabla)
  tt <- tabular (tabla, data = datos1)
  tablaLatex( tt, caption= título)

}
