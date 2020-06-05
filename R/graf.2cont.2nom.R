#' @import ggplot2
#' @import dplyr
#' @title Visualización gráfica de dos variables continuas clasificadas por dos variables nominales
#' @description Esta función relaciona dos variables continuas según dos variables nominales , cada una por separado
#' @param Datos data.frame con los datos a analizar.
#' @param X1 vector de respuestas de la variable cuantitativa 1 (columnas) (tamaño n)
#' @param X2 vector de respuestas de la variable cuantitativa 2 (filas) (tamaño n)
#' @param X3 vector de respuestas de la variable cualitativa 3 (tamaño n)
#' @param X4 vector de respuestas de la variable cualitativa 4 (tamaño n)
#' @param Etiquetasx3 vector de caracteres con las posibles respuestas de la variable 3
#' @param Etiquetasx4 vector de caracteres con las posibles respuestas de la variable 4
#' @param Nombre1 cadena de caracteres con el nombre de la variable 1
#' @param Nombre2 cadena de caracteres con el nombre de la variable 2
#' @param Nombre3 cadena de caracteres con el nombre de la variable 3
#' @param Nombre4 cadena de caracteres con el nombre de la variable 4
#' @param Título cadena de caracteres indicando el nombre que se le quiere dar al gráfico
#' @emport

graf.2cont.2nom= function(datos, X1, X2, X3, X4, nombre1, nombre2,
                           nombre3, nombre4, etiquetas1, etiquetas2, título) {

  datos <- mutate(datos, factor1= factor(X3, labels = etiquetas1))
  datos <- mutate(datos, factor2= factor(X4, labels = etiquetas2))

  mis.colores <- colorRampPalette(c("coral", "lightblue"))
  netiq=length(etiquetas)

  #Gráfico de dispersión
  ggplot(data = datos, aes(x = X1, y = X2)) +
    geom_point(aes(color = factor1), size = 1, alpha = 0.7) +
    geom_smooth(aes(color = factor1)) +
    facet_grid(factor1~factor2, scales = 'free') +
    scale_color_manual(name= nombre3 ,values = mis.colores(netiq))+
    xlab(nombre1) +
    ylab(nombre2) +
    ggtitle(título) +
    theme_minimal()


}
