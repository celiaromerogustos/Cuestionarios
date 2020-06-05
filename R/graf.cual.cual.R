#' @import ggplot2
#' @import dplyr
#' @title Visualización gráfica de dos variables cualitativas.
#' @description Esta funcion devuelve un gráfico de barras para
#'  observar la relación que existe entre dos variables cualitativas.
#' @param Datos data.frame con los datos a analizar.
#' @param X1 vector de respuestas de la variable cualitativa (tamaño n)
#' @param X2 vector de respuestas de la variable cualitativa (tamaño n)
#' @param Etiquetasx1 vector de cadena de caracteres con las posibles respuestas no numéricas de la variable 1
#' @param Etiquetasx2 vector de cadena de caracteres con las posibles respuestas no numéricas de la variable 2
#' @param Nombrex1 cadena de caracteres indicando la pregunta realizada para la variable x1
#' @param Nombrex2 cadena de caracteres indicando la pregunta realizada para la variable x2
#' @param Título cadena de caracteres indicando el nombre que se le quiere dar a los gráficos
#' @export

graf.cual.cual=function(datos,x1,x2, etiquetasx1,
                         etiquetasx2, nombrex1, nombrex2, título) {
  #Transformo como factores las variables originales x1 y x2
  datos <- mutate(datos, factor1 = factor(x1,labels = etiquetasx1))
  datos <- mutate(datos, factor2 = factor(x2,labels = etiquetasx2))

  mis.colores <- colorRampPalette(c("coral", "lightblue"))
  n2=length(etiquetasx2)
  n1=length(etiquetasx1)

  datos3 <- datos %>%
    filter(factor2 %in% etiquetasx2 )%>%
    group_by(factor1, factor2) %>%
    summarise(Frecuencia = n())

  #Gráfico de barras
  print(ggplot(datos3, aes(x = factor1, y= Frecuencia )) +
          geom_bar(aes(color = factor2, fill = factor2),
                   stat = "identity", position = position_dodge(0.8),
                   width = 0.7) +
          labs(title = título, x = nombrex1)+
          scale_color_manual(name= nombrex2,values = mis.colores(n2))+
          scale_fill_manual(name= nombrex2,values = mis.colores(n2))+
          geom_text( aes(label = Frecuencia, group = factor2),
                     position = position_dodge(0.8),
                     vjust = -0.3, size = 3.5))


}
