#' @import ggplot2
#' @title Visualización gráfica de una variable continua respecto de otra continua
#' @description La siguiente función muestra el gráfico de dispersión entre dos variables continuas dadas.
#' @param datos data.frame con los datos a analizar.
#' @param X1 vector de respuestas de la variable continua 1 (tamaño n)
#' @param X2 vector de respuestas de la variable continua 2 (tamaño n)
#' @param Nombrex1 cadena de caracteres indicando la pregunta realizada para la variable x1
#' @param Nombrex2 cadena de caracteres indicando la pregunta realizada para la variable x2
#' @param Título cadena de caracteres indicando el nombre que se le quiere dar al gráfico
#' @export


graf.cuan.cuan=function(X1, X2, nombrex1 ,nombrex2, título) {

  print(ggplot(datos,aes(x=X1, y=X2)) +
          geom_point() +
          theme_minimal() +
          labs(title = título)+
          geom_smooth(method = "lm", se = F))

}
