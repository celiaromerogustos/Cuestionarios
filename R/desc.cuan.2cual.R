#' @import dplyr
#' @import tables
#' @title Tabla de resúmenes numéricos de una variable cuantitativa respecto de dos cualitativas
#' @description Esta funcion devuelve la tabla de resúmenes numéricos básicos de la variable
#'  cuantitativa respecto de la variable cualitativa, incluyendo el tamaño muestral, media,
#'   mediana, desviación estandar e intervalo de confianza al 95% de X1 respecto de X2 y X3
#' @param Datos data.frame con los datos a analizar.
#' @param X1 vector de respuestas de la variable cuantitatva (tamaño n)
#' @param X2 vector de respuestas de la variable cualitativa (tamaño n)
#' @param X3 vector de respuestas de la variable cualitativa (tamaño n)
#' @param Etiquetasx2 vector de cadena de caracteres con las posibles respuestas no numéricas de la variable 2
#' @param Etiquetasx3 vector de cadena de caracteres con las posibles respuestas no numéricas de la variable 3
#' @param Nombre1 cadena de caracteres indicando la pregunta realizada para la variable x1 (no admite espacios)
#' @param Nombre2 cadena de caracteres indicando la pregunta realizada para la variable x2 (no admite espacios)
#' @param Nombre3 cadena de caracteres indicando la pregunta realizada para la variable x3 (no admite espacios)
#' @param Título cadena de caracteres indicando el nombre que se le quiere dar a la tabla
#' @export

desc.cuan.2cual= function (datos, X1, X2, X3, etiquetas2, etiquetas3,
                            nombre1, nombre2, nombre3, título) {
  datos1 <- mutate(datos, v1 = factor(X2, labels = etiquetas2))
  datos2 <- mutate(datos, v2 = factor(X3, labels = etiquetas3))

  IC1 <- function(X1){
    mean(X1) - qt(0.95, df = length(X1)- 1) * sd(X1) / sqrt(length(X1))}
  IC2 <- function(X1){
    mean(X1) + qt(0.95, df = length(X1)- 1) * sd(X1) / sqrt(length(X1))}
  Media <- function(x){mean(x)}
  Mediana<- function (x) {median(x)}
  Desv<- function (x) {sd(x)}

  tabla <- "(nombre2 =datos1$v1 )+( nombre3 =datos2$v2 )+( Total = 1 ) ~
             (n=1)+ ( nombre1 = X1 )*(Media + Mediana + Desv + IC1+ IC2)"
  tabla <- gsub("nombre1", nombre1, tabla)
  tabla <- gsub("nombre2", nombre2, tabla)
  tabla <- gsub("nombre3", nombre3, tabla)
  tt <- tabular (tabla, data = datos1)
  tt
}
