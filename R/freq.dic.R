#' @import dplyr
#' @import knitr
#' @title Tabla de frecuencias para variables binarias o dicotómicas.
#' @description Función que genera una tabla de frecuencias absolutas, relativas y acumuladas
#' para la variable x.
#' @param Datos data.frame con los datos a analizar.
#' @param X vector de respuestas de la variable dicotómica a analizar (tamaño n)
#' @param Etiquetas vector de cadena de caracteres con las posibles respuestas.
#' @param Título cadena de caracteres indicando el nombre que se le quiere dar a la tabla
#' @export

freq.dic= function(datos, X, etiquetas, titulo)
 {
  datos1 <- mutate(datos, v1 = factor(X, labels = etiquetas))
  ##transforma los datos numéricos a factor

  Frec.abs=table(datos1$v1)
  Frec.rel=round(prop.table(Frec.abs),2)

  tabla=cbind(Frec.abs,Frec.rel )
  tabla=rbind(tabla, Total = colSums(tabla))
  kable( tabla, caption = título)
}




