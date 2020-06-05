#' @import likert
#' @import dplyr
#' @title Visualización gráfica para variables de valoración (Escala Likert) en bloques de tres.
#' @description Función que genera un diagrama de barras, mapa de calor y gráfico de densidad para una fácil
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
  mis.colores <- colorRampPalette(c("coral", "lightblue"))
  netiq=length(etiquetas)

  Result = likert(data)

  #Grafico de barras
  print( likert.bar.plot(Result, low.color = "coral",
                         high.color = "lightblue", neutral.color = "azure",
                         neutral.color.ramp = "white", plot.percent.low = TRUE,
                         plot.percent.high = TRUE, plot.percent.neutral = TRUE,
                         plot.percents = FALSE, text.size = 3,
                         text.color = "black",
                         centered = TRUE,
                         legend = "Respuestas",
                         legend.position = "bottom",
                         panel.strip.color = "#F0F0F0",
                         xlab="Porcentaje"))




  print(plot(Result,
             type = 'heat',
             low.color = "white",
             high.color = "coral",
             text.color = "black",
             text.size = 3,
             wrap = 50)  +
          theme(legend.position = 'none'))

  #Densidad
  plot(Result,
       type="density",
       facet = TRUE,
       bw = 0.5,
       main="",
       col = mis.colores(netiq))

}
