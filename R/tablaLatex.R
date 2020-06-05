#' @title Tablas en Latex
#' @description Función para imprimir tablas en latex
#' @param Tabla tabla en R que se desea escribir en Latex
#' @param Caption título de la tabla, por defecto nulo
#' @references  \url{http://gauss.inf.um.es/8jornadasR/filestaller/T1_descriptivos.html}
#' @export

tablaLatex <- function( tabla, caption = NULL ){
  cat( '\\begin{table}[!ht] \\centering\n' )
  if( !is.null( caption ) ) cat( paste0( '\\caption{', caption, '}\n'))
  latex( tabla )
  cat( '\\end{table}' )  }
