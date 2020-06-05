#' @import knitr
#' @import dplyr
#' @title Tabla de frecuencias para variables ordinales
#' @description Función que genera una tabla de frecuencias absolutas y relativas junto con sus acumuladas
#' de la variable x.
#' @param datos data.frame con los datos a analizar.
#' @param X vector de respuestas de la variable ordinal a analizar (tamaño n)
#' @param Etiquetas vector de cadena de caracteres con las posibles respuestas.
#' @param Título cadena de caracteres que indica el nombre que se le quiere dar a la tabla
#' @return
#' @export

freq.ord= function(datos, X, etiquetas, título) {
  datos1 <- mutate(datos, v1 = factor(X, labels = etiquetas))

  Frec.abs=table(datos1$v1)
  Frec.rel=round(prop.table(Frec.abs),2)
  Frec.abs.acum = cumsum(Frec.abs)
  Frec.rel.acum = cumsum(Frec.rel)
  tabla=cbind(Frec.abs, Frec.rel ,Frec.abs.acum, Frec.rel.acum )
  kable( tabla, caption = título)

}
