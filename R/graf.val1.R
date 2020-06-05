#' @import likert
#' @import dplyr
#' @title Visualización gráfica variables de valoración (Escala Likert)
#' @description Función que genera un diagrama de barras, mapa de calor y gráfico de densidad de la variable X .
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

  print( likert.bar.plot(Result, xlab="Porcentaje", low.color = "coral",
                         high.color = "lightblue", neutral.color = "azure",
                         neutral.color.ramp = "white", plot.percent.low = TRUE,
                         plot.percent.high = TRUE, plot.percent.neutral = TRUE,
                         plot.percents = FALSE, text.size = 3,
                         text.color = "black",
                         centered = TRUE,
                         legend = "Respuestas",
                         legend.position = "bottom",
                         panel.strip.color = "#F0F0F0"))

  # Mapa de calor

  print(plot(Result,
             type = 'heat',
             low.color = "white",
             high.color = "coral",
             text.color = "black",
             text.size = 3,
             wrap = 50)  +
          theme(legend.position =  'none '))

  #Densidad

  print(plot(Result,
             type="density",
             facet = TRUE,
             bw = 0.5))


}
