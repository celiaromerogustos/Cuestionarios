#' @import ggplot2
#' @title Visualización gráfica de variables discretas
#' @description Función que genera un diagrama de caja y gráfico de densidad de la variable X
#' @param Datos data.frame donde se encuentra X
#' @param X vector de respuestas de la variable discreta a analizar (tamaño n)
#' @param Nombre cadena de caracteres indicando la pregunta realizada o nombre de X
#' @param Título cadena de caracteres indicando el nombre que se le quiere dar a la salida
#' @return Diagrama de cajas y de densidad
#' @export

graf.disc=function(datos, X, nombre, título) {

  #Diagrama de caja
  boxplot(X, xlab= nombre, main = título ,
          border= "cyan4")

  #Densidad
  print(ggplot(datos, aes(x = X)) +
          geom_density() +
          scale_x_continuous(nombre) +
          scale_y_continuous("densidad") +
          labs(title = título))  }
