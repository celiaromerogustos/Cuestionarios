#' @import tables
#' @title Tabla de resúmenes numéricos para dos variables cuantitativas  por columnas
#' @description Función que devuelve una tabla con las medidas descriptivas
#'  más relevante del análisis de variables numéricas, n (tamaño muestral),
#'  media, mediana, desv(desviación estandar ), IC1, IC2, el intervalo de
#'  confianza con un nivel del 95% de X1 y X2 independiente uno de otro, dispuestos por columnas
#' @param Datos data.frame con los datos a analizar.
#' @param X1 vector de respuestas de la primera variable cuantitativa (tamaño n)
#' @param X2 vector de respuestas de la segunda variable cuantitativa (tamaño n)
#' @param Título cadena de caracteres que indica el nombre que se le quiere dar a la tabla
#' @export


desc.2cuan.colum= function (datos,x1,x2, título) {
  IC1 <- function(x) {
    mean(x) - qt( 0.95, df = length(x) - 1) * sd(x) / sqrt(length(x))}
  IC2 <- function(x){
    mean(x) + qt( 0.95, df = length(x) - 1) * sd(x) / sqrt(length(x))}
  Media <- function(x){mean(x)}
  Mediana<- function (x) {median(x)}
  Desv<- function (x) {sd(x)}

  tabla <- "~(n=1)+Format(digits=2)*((nombrex1=x1)+(nombrex2=x2))*
             (Media+Mediana+Desv+IC1+IC2)"

  tt <- tabular (tabla, data = datos)

  df <- data.frame(matrix(unlist(tt), nrow=1, byrow=F))
  colnames(df) = c("n", "Media", "Mediana", "Desv", "IC1", "IC2",
                   "Media", "Mediana", "Desv", "IC1", "IC2")

  kable(df, digits = 2, caption = título) }
