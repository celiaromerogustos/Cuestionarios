#' @import dplyr
#' @import ggplot2
#' @title Visualización gráfica Relación entre dos variables continuas clasificada por una nominal.
#' @description Esta función relaciona dos variables continuas según una variable nominal,
#' muestra los gráficos de dispersión por separado.
#' @param datos data.frame con los datos a analizar.
#' @param X1 vector de respuestas de la variable continua1 (columnas) (tamaño n)
#' @param X2 vector de respuestas de la variable continua 2 (filas) (tamaño n)
#' @param X3 vector de respuestas de la variable nominal (tamaño n)
#' @param Etiquetas vector de caracteres con las posibles respuestas de la variable 3
#' @param Nombre1 cadena de caracteres con el nombre de la variable 1
#' @param Nombre2 cadena de caracteres con el nombre de la variable 2
#' @param nombre3 cadena de caracteres con el nombre de la variable 3
#' @param Título cadena de caracteres indicando el nombre que se le quiere dar al gráfico
#' @emport

graf.2cont.nom= function(datos, X1, X2, X3, nombre1, nombre2,
                          nombre3, etiquetas, título) {

  datos <- mutate(datos, factor= factor(X3, labels = etiquetas))


  mis.colores <- colorRampPalette(c("coral", "lightblue"))
  netiq=length(etiquetas)

  #Gráfico de dispersión
  ggplot(data = datos, aes(x = X1, y = X2)) +
    geom_point(aes(color = factor), size = 1, alpha = 0.7) +
    geom_smooth(aes(color = factor)) +
    facet_grid(factor~., scales = 'free') +
    scale_color_manual(name= nombre3 ,values = mis.colores(netiq))+
    xlab(nombre1) +
    ylab(nombre2) +
    ggtitle(título) +
    theme_minimal()


}
