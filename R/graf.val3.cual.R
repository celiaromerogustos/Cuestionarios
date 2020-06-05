#' @import likert
#' @import dplyr
#' @title Visualización gráfica variables de valoración (Escala Likert) para bloques de tres con
#'  las mismas etiquetas en función de una variable cualitativa
#' @description Función que genera un diagrama de barras y gráfico de densidad para una fácil
#'  visualización y comparación de 3 variables de tipo ordinal en función de otra variable cualitativa
#' @param datos data.frame con los datos a analizar.
#' @param X1 vector de respuestas de la variable de valoración (tamaño n)
#' @param X2 vector de respuestas de la variable de valoración (tamaño n)
#' @param X3 vector de respuestas de la variable de valoración (tamaño n)
#' @param X4 vector de respuestas de la variable cualitativa (tamaño n)
#' @param Etiquetas vector de cadena de caracteres con las posibles respuestas  de las variables de valoración
#' @param Etiquetasx4 vector de cadena de caracteres con las posibles respuestas no numéricas de la variable 4
#' @param Preguntas vector de cadenas de caracteres con las preguntas o nombres en orden de x1, x2 y x3
#' @param Nombrex4 cadena de caracteres indicando la pregunta realizada para la variable x4
#' @export


graf.val3.cual=function(datos,x1,x2,x3,x4, etiquetas,
                        etiquetasx4, preguntas, nombrex4) {
  datos <- mutate(datos, factor1 = factor(x1,labels = etiquetas))
  datos <- mutate(datos, factor2 = factor(x2,labels = etiquetas))
  datos <- mutate(datos, factor3 = factor(x3,labels = etiquetas))
  datos1<- mutate(datos, factor4 = factor(x4, labels = etiquetasx4))
  data=cbind.data.frame(datos$factor1,datos$factor2, datos$factor3)
  names(data)=preguntas
  Result = likert(data, grouping = datos1$factor4)

  #Grafico de barras
  print( likert.bar.plot(Result, xlab="Hola", low.color = "coral",
                         high.color = "lightblue", neutral.color = "azure",
                         neutral.color.ramp = "white", plot.percent.low = TRUE,
                         plot.percent.high = TRUE, plot.percent.neutral = TRUE,
                         plot.percents = FALSE, text.size = 3,
                         text.color = "black",
                         centered = TRUE,
                         legend = "Respuestas",
                         legend.position = "bottom",
                         panel.strip.color = "#F0F0F0"))

  #Densidad
  plot(Result,
       type="density",
       facet = TRUE,
       bw = 0.5,
       main="")


}
