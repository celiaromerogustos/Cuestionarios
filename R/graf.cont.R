#' @import ggplot2
#' @title Visualización gráfica de variables continuas.
#' @description Función que genera un diagrama de caja, histograma con media e
#'  histograma más densidad de la variable X.
#' @param Datos data.frame donde se encuentra X
#' @param X vector de respuestas de la variable continua  (tamaño n)
#' @param Nombre cadena de caracteres indicando la pregunta realizada o nombre de X
#' @param Título cadena de caracteres indicando el nombre que se le quiere dar a la salida
#' @return Diagrama de caja, histograma con media e histograma más densidad
#' @export

graf.cont=function(datos, X, nombre, título){
  mis.colores <- colorRampPalette(c("coral", "lightblue"))

  #Diagrama de caja
  boxplot(X, xlab=nombre, main = título, border= "cyan4")

  #Densidad
  print(ggplot(datos, aes(x = X)) +
          geom_density() +
          scale_x_continuous(nombre)+
          scale_y_continuous("densidad")+
          labs(title = título,
               subtitle = nombre))

  #Histograma con media
  print(ggplot(datos, aes(x=X)) +
          geom_histogram(bins=30, color=mis.colores(1), fill="white") +
          scale_x_continuous(nombre) +
          scale_y_continuous("frecuencia")+
          geom_vline(aes(xintercept=mean(X)),
                     color=mis.colores(1), linetype="dashed", size=1))

  #Histograma + Densidad

  print(ggplot(datos, aes(x=X)) +
          geom_histogram(bins=30,aes(y=..density..),
                         colour="black", fill="white")+
          scale_x_continuous(nombre) +
          scale_y_continuous("densidad")+
          geom_density(alpha=.2, fill=mis.colores(1)) )

}
