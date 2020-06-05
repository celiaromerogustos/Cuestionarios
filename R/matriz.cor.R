#' @title Matriz de correlaciones
#' @description Esta función genera una matriz con las correlaciones
#'  dos a dos de hasta p variables en función del método indicado. 
#' @param Data: data.frame formada por las p variables que se desea estudiar
#'  su correlación.
#' @param Nombres: vector de caracteres con las preguntas o nombres de las variables (el número de columnas de data debe ser igual a p).
#' @param Título : cadena de caracteres indicando el nombre que se le quiere dar a la salida.
#' @param Método : cadena de caracteres indicando qué coeficiente de correlación
#'  se debe calcular. "pearson", "kendall" o "spearman".
#' @export

matriz.cor= function( data, nombres, título, método){
  colnames(data) <- nombres
  kable(cor(data, method = método) ,
        caption = título , booktabs = TRUE,escape=FALSE)
}