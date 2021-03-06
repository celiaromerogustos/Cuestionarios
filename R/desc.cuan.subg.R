#' @import tables
#' @title Tabla de resúmenes numéricos para una variable cuantitava dividida en dos partes.
#' @description Esta función devuelve una tabla de resúmenes númericos básicos, tamaño muestral,
#'  media, mediana, desviación estandar e intervalo de confianza al 95% de una variable cuantitativa
#'  dada dividida en dos partes, la primera menor que un valor y la segunda mayor que ese mismo valor dado.
#' @param Datos data.frame con los datos a analizar.
#' @param X vector de respuestas de la variable a analizar (tamaño n)
#' @param Título cadena de caracteres que indica el nombre que se le quiere dar a la tabla
#' @param Valor de tipo númerico, valor dado para separar nuestra variable en dos partes.
#' @export


desc.cuan.subg= function (datos, x, valor, título ) {
  IC1 <- function(x) {
    mean(x) - qt( 0.95, df = length(x) - 1) * sd(x) / sqrt(length(x))}
  IC2 <- function(x){
    mean(x) + qt( 0.95, df = length(x) - 1) * sd(x) / sqrt(length(x))}
  Media <- function(x){mean(x)}
  Mediana<- function (x) {median(x)}
  Desv<- function (x) {sd(x)}

  tabla <- "( Mayorquevalor = x > valor ) + ( Menorquevalor= x <= valor )~
  (n=1)+((nombre=x))* (Media+Mediana+Desv+IC1+IC2)"

  tt <- tabular (tabla, data = datos)

  df <- data.frame(matrix(unlist(tt), nrow=2, byrow=F))
  colnames(df) = c("n", "Media", "Mediana", "Desv", "IC1", "IC2")
  rownames(df) = c("mayor que" , "menor que")
  kable(df, digits = 2, caption = título)

}
