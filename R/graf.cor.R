#' @import corrplot
#' @title Visualización gráfica matriz de correlaciones
#' @description La función graf.cor genera un gráfico muy visual de las correlaciones entre varias variables.
#'  Se muestra la matriz con los coeficientes de correlación. En la diagonal están las variables, por encima
#'  están unos círculos de colores, entre más intensidad del color, ya sea azul o rojo, mayor es la correlación,
#'  colores ténues significan correlación baja; el tamaño de los círculos está asociado al valor absoluto de correlación.
#'  Por debajo de la diagonal se observan los valores exactos de correlación en colores
#' @param Data: data.frame formada por las p variables que se desea estudiar su correlación.
#' @param Nombres: vector de caracteres con los nombres de las variables (su longitud igual a p).
#' @param Título : cadena de caracteres indicando el nombre que se le quiere dar a la salida.
#' @param Método : cadena de caracteres indicando qué coeficiente de correlación se debe calcular. "pearson", "kendall" o "spearman".
#' @export



graf.cor=function(data, nombres, título, método) {
  m=cor(data, method = método)
  corrplot.mixed(m, tl.pos= "d", tl.col="black")
  
}