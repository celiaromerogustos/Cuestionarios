#' @import sjPlot
#' @import dplyr
#' @title Visualización gráfica variables de valoración (Escala Likert)
#' @description Función que genera un diagrama de barras de la variable X.
#' @param Datos data.frame donde se encuentra X
#' @param X vector de respuestas de la variable cualitativas a analizar (tamaño n)
#' @param Etiquetas vector de cadena de caracteres con las posibles respuestas no numéricas
#' @param Nombre cadena de caracteres indicando la pregunta realizada o nombre de X
#' @return Diagrama de barras, mapa de calor y gráfico de densidad.
#' @export

graf.val1=function(datos, x, etiquetas, nombre) {

  datos <- mutate(datos, factor = factor(x, labels = etiquetas))
  data=cbind.data.frame(datos$factor)
  names(data)= nombre
  Result = likert(data)

  #Grafico de barras

  plot_likert(data, show.n=FALSE,
              geom.colors = "RdBu",
              show.prc.sign = FALSE,
              cat.neutral = 3)



}
