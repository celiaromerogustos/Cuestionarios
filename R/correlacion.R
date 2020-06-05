#' @title Coeficiente de correlación
#' @description Devuelve un mensaje con el valor del coeficiente
#'  de correlación lineal en función del metodo indicado. Pearson,
#'   Kendall o Spearman.
#' @param X: vector numérico.
#' @param Y: vector numérico.
#' @param Nombres: vector de caracteres con las preguntas o afirmaciones
#'  de las variables (el número de columnas de data debe ser igual a p).
#' @param Método : cadena de caracteres indicando qué coeficiente de
#'  correlación se debe calcular. "pearson", "kendall" o "spearman".
#' @export


correlacion= function(X, Y , nombres, método){
  a= cor(X,Y , method = método)
  cat("El valor del coeficiente de correlación de", nombres[1],
      "y", nombres[2], "por el método \n", método,  "es",  round(a,4))
}
