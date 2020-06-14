#' @import tables
#' @title Tabla de resúmenes numéricos para variables discretas.
#' @description Función que devuelve una tabla con las medidas descriptivas
#'  más relevante del análisis de variables numéricas, n (tamaño muestral),
#'  media, mediana, desv(desviación estandar), IC1, IC2, el intervalo de
#'  confianza con un nivel del 95% de X
#' @param Datos data.frame con los datos a analizar.
#' @param X vector de respuestas de la variable discreta a analizar (tamaño n)
#' @param Título cadena de caracteres que indica el nombre que se le quiere dar a la tabla
#' @export

desc.disc= function (datos, x, título) {

  IC1 <- function(x) {
    mean(x) - qt( 0.95, df = length(x) - 1) * sd(x) / sqrt(length(x))}
  IC2 <- function(x){
    mean(x) + qt( 0.95, df = length(x) - 1) * sd(x) / sqrt(length(x))}
  Media <- function(x){mean(x)}
  Mediana<- function (x) {median(x)}
  Desv<- function (x) {sd(x)}

  tabla <- "~(n=1)+(nombre =x)*(Media+Mediana+Desv+IC1+IC2)"
  tt <- tabular (tabla, data = datos)

  df <- data.frame(matrix(unlist(tt), nrow=1, byrow=F))
  colnames(df) = c("n", "Media", "Mediana", "Desv", "IC1", "IC2")

  kable(df, digits = 2, caption = título)
}
