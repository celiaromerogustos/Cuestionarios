#' @import DescTools
#' @title Medida de asociación Lambda de Goodman Kruskal.
#' @description devuelve un mensaje con el coeficiente Lambda simétrico para variables nominales.
#'  Además de un intervalo con un nivel de confianza dado.
#' @param X vector numérico.
#' @param Y vector numérico.
#' @param Conf Valor que indica el nivel del confianza para crear el intervalo (0<conf<1).
#' @param Nombrex cadena de caracteres que indica nombre de la variable X.
#' @param Nombrey cadena de caracteres que indica nombre de la variable Y.
#' @export




 asocnom= function(X, Y, conf, nombrex, nombrey) {
  a=Lambda(X, Y, conf.level = conf)

  cat("El valor de Lambda de Goodman Kruskal para", nombrex,
      "y", nombrey, "es \n",  round(a[1],4) ,
      "con un intervalo de confianza al", coef*100,
      "% de: (",round(a[2],4),",",round(a[3],4),")")

}
