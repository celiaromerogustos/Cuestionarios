#' @import tables
#' @import dplyr
#' @title Tabla de frecuencias de dos variables cualitativas.
#' @description Función que genera una tabla de frecuencias absolutas y relativas
#' de las variable x1 y x2.
#' @param Datos data.frame con los datos a analizar.
#' @param X1 vector de respuestas de la variable cualitativa 1 (filas)(tamaño n)
#' @param X2 vector de respuestas de la variable cualitativa 2(columnas)(tamaño n)
#' @param Etiquetasx1 vector de cadena de caracteres con las posibles respuestas no numéricas de la variable 1
#' @param Etiquetasx2 vector de cadena de caracteres con las posibles respuestas no numéricas de la variable 2
#' @param Nombrex1 cadena de caracteres indicando la pregunta realizada para la variable x1
#' @param Nombrex2 cadena de caracteres indicando la pregunta realizada para la variable x2
#' @param Título1 cadena de caracteres indicando el nombre que se le quiere dar a la tabla de frecuencias absolutas
#' @param Título2 cadena de caracteres indicando el nombre que se le quiere dar a la tabla de frecuencias relativas
#' @export

freq.2cual= function(datos, X1, X2, etiquetasx1, etiquetasx2,
                      nombrex1, nombrex2, título1, título2 ) {
  datos1 <- mutate(datos, v1 = factor(X1, labels = etiquetasx1))
  datos2 <- mutate(datos, v2 = factor(X2, labels = etiquetasx2))
  tabla <- "(nombre1 = datos1$v1) + ( Total = 1 ) ~
            (nombre2 = datos2$v2) + ( Total = 1 )"
  tabla <- gsub("nombre1", nombrex1, tabla)
  tabla <- gsub("nombre2", nombrex2, tabla)
  tt <- tabular (tabla, data = datos1)

  tablaLatex( tt, caption= título1)


  Frec.abs=table(datos1$v1, datos2$v2)
  Frec.rel=round(prop.table(Frec.abs),2)
  tabla=cbind(Frec.rel )
  tabla=cbind(tabla, Total = rowSums(tabla))
  tabla=rbind(tabla, Total = colSums(tabla))
  kable( tabla, caption = título2)


}
