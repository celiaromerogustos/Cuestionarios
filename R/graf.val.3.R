#' @import sjPlot
#' @import dplyr
#' @title Visualización gráfica para variables de valoración (Escala Likert) en bloques de tres.
#' @description Función que genera un diagrama de barras para una fácil
#'  visualización y comparación de tres variables de tipo ordinal con las mismas etiquetas.
#' @param Datos data.frame donde se encuentra X
#' @param X1 vector de respuestas de la variable 1 (tamaño n)
#' @param X2 vector de respuestas de la variable 2 (tamaño n)
#' @param X3 vector de respuestas de la variable 3 (tamaño n)
#' @param Etiquetas vector de cadenas de caracteres con las posibles respuestas no numéricas
#' @param Preguntas vector de cadenas de caracteres con las preguntas o afirmaciones en orden de x1, x2 y x3
#' @return Diagrama de barras, mapa de calor y gráfico de densidad.
#' @export

graf.val3=function(datos,x1,x2,x3, etiquetas, preguntas) {
  datos <- mutate(datos, factor1 = factor(x1,   labels = etiquetas))
  datos <- mutate(datos, factor2 = factor(x2,   labels = etiquetas))
  datos <- mutate(datos, factor3 = factor(x3,   labels = etiquetas))

  data=cbind.data.frame(datos$factor1,datos$factor2, datos$factor3)
  names(data)=preguntas

   #Gráfico de barras
  plot_likert(data, show.n=FALSE,
              geom.colors = "RdBu",
              show.prc.sign = FALSE,
              cat.neutral = 3,
              value = "sum.outside")


}
