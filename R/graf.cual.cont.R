#' @import ggplot2
#' @import dplyr
#' @title Visualización gráfica variable continua en función de una cualitativa.
#' @description Esta función genera el histograma, gráfico de densidad ,
#' caja y bigote y tipo jitter de la variable continua en función de una cualitativa
#' @param Datos data.frame con los datos a analizar.
#' @param X1 vector de respuestas de la variable cualitativa (tamaño n)
#' @param X2 vector de respuestas de la variable continua (tamaño n)
#' @param EtiquetasX1 vector de cadena de caracteres con las posibles respuestas de la variable 1
#' @param Nombrex1 cadena de caracteres indicando la pregunta realizada para la variable x1
#' @param Nombrex2 cadena de caracteres indicando la pregunta realizada para la variable x2
#' @param Título cadena de caracteres indicando el nombre que se le quiere dar a los gráficos
#' @export


graf.cont.cual=function(X1, X2, etiquetasx1, nombrex1,nombrex2, título) {

  datos <- mutate(datos, factor1= factor(X1,labels = etiquetasx1))

  mis.colores <- colorRampPalette(c("coral", "lightblue"))
  netiq=length(etiquetas)


  #Histograma
  print(ggplot(datos, aes(x=X2,color=factor1, fill=factor1)) +
          geom_histogram(binwidth = 0.01, fill="white") +
          labs(x = nombre2) +
          theme_minimal() +
          facet_grid(factor1 ~.)+
          scale_color_manual(name=nombre1,
                             values=mis.colores(netiq),labels=etiquetasx1)+
          scale_fill_manual(name=nombrex1,
                            values=mis.colores(netiq),labels=etiquetasx1)+
          labs(title = título, x = nombrex2, y = "frecuencia")+
          theme_minimal())


  #Densidades por categorías en un mismo gráfico
  print(ggplot(datos, aes(x=X2, color=factor1)) +
          geom_density()+
          scale_color_manual(name=nombrex1,
                             values=mis.colores(netiq),labels=etiquetasx1))

  # Densidades de la variable continua para cada
  # categoría en gráficos separados
  print(ggplot(datos, aes(x=X2, color=factor1)) +
          geom_line(stat="density") +
          facet_grid(factor1 ~.)+
          scale_color_manual(name=nombrex1,
                             values=mis.colores(netiq),labels=etiquetasx1))

  # Gráfico de caja y bigote por categorías
  print(ggplot(datos, aes(factor1,X2)) +
          geom_boxplot(aes(color = factor1 ))+
          scale_color_manual(name=nombrex1, values = mis.colores(netiq))+
          labs(title = título,
               x = nombrex1, y = nombrex2)+
          theme_minimal())

  #Gráfico tipo jitter
  print(ggplot(datos, aes(x=factor1, y= X2)) +
          geom_jitter(aes(colour = factor1))+
          scale_color_manual(name=nombrex1,
                             values=mis.colores(netiq),labels=etiquetasx1)+
          labs(title = título, x = nombrex1, y = nombrex2) )

  #Mezcla gráfico caja y bigote y tipo jitter.
  print(ggplot(datos, aes(x = factor1, y = X2)) +
          geom_jitter(aes(color = factor1), size = 1, alpha = 0.7) +
          geom_boxplot(aes(color = factor1), alpha = 0.7) +
          scale_color_manual(name=nombrex1,
                             values=mis.colores(netiq),labels=etiquetasx1)+
          xlab(nombrex1) +
          ylab(nombrex2) +
          ggtitle(título) +
          theme_minimal())


}
