#' @import dplyr
#' @import tables
#' @title Tabla de resúmenes numéricos de la variable cuantitativa respecto de la cualitativa.
#' @description Esta función devuelve la tabla de resúmenes numéricos básicos de la variable
#'  cuantitativa respecto de la variable cualitativa, incluyendo el tamaño muestral, media,
#'   mediana, desviación estandar e intervalo de confianza al 95% de X1 respecto de X2
#' @param Datos data.frame con los datos a analizar.
#' @param X1 vector de respuestas de la variable cuantitatva (tamaño n)
#' @param X2 vector de respuestas de la variable cualitativa (tamaño n)
#' @param Etiquetasx2 vector de cadena de caracteres con las posibles respuestas no numéricas de la variable 2
#' @param Título cadena de caracteres indicando el nombre que se le quiere dar a la salida
#' @export


desc.cuan.cual= function (datos,X1,X2, etiquetasX2, título) {
  datos1 <- mutate(datos, v1 = factor(X2, labels = etiquetas2))

  IC1 <- function(X1){
    mean(X1) - qt(0.95, df = length(X1)- 1) * sd(X1) / sqrt(length(X1))}
  IC2 <- function(X1){
    mean(X1) + qt(0.95, df = length(X1)- 1) * sd(X1) / sqrt(length(X1))}
  Media <- function(x){mean(x)}
  Mediana<- function (x) {median(x)}
  Desv<- function (x) {sd(x)}

  tabla <- " ( nombre2 =datos1$v1 ) + ( Total = 1 ) ~
            (n=1)+ ( nombre1 = X1 ) * (Media + Mediana + Desv + IC1+ IC2)"

  tt <- tabular (tabla, data = datos1)

  df <- data.frame(matrix(unlist(tt), nrow=length(etiquetasX2)+1, byrow=F))
  colnames(df) = c("n", "Media", "Mediana", "Desv", "IC1", "IC2")
  rownames(df) = c(etiquetasX2,"Total")

  kable(df, digits = 2, caption = título)


}
