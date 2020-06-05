#' @import ggplot2
#' @import dplyr
#' @title Visualización gráfica para variables cualitativas
#' @description Función que genera un diagrama de barras y diagrama de sectores.
#' @param Datos data.frame donde se encuentra X
#' @param X vector de respuestas de la variable cualitativas a analizar (tamaño n)
#' @param Etiquetas vector de cadena de caracteres con las posibles respuestas no numéricas
#' @param Nombre cadena de caracteres indicando la pregunta realizada o nombre de X
#' @param Título cadena de caracteres indicando el nombre que se le quiere dar a la salida
#' @return Diagrama de barras y de sectores
#' @export

graf.cual= function(datos, x , etiquetas, nombre, título) {

  #Creamos una nueva columna como factores de la variable X
  datos <- mutate(datos, factor = factor(x, labels = etiquetas))

  netiq= length(etiquetas)

  #Uso esta paleta de color.
  mis.colores <- colorRampPalette(c("coral", "lightblue"))

  #Diagrama de barras

  print(ggplot(datos, aes(x = factor)) +
          geom_bar(width = 0.4,
                   aes(y = (..count..)/sum(..count..)),
                   fill = mis.colores(1)) +
          scale_x_discrete(nombre) +
          scale_y_continuous("Porcentaje", labels=scales::percent) +
          labs(title = título))

  #Diagrama de sectores

  pie(table(datos$factor), main=nombre, border="white",
      radius=0.75,
      cex=0.7, col = mis.colores(netiq))
  legend("right", etiquetas, fill=mis.colores(netiq), cex=1)

}
