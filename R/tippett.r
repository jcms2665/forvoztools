#' Generar múltiples gráficas Tippett (versión original)
#'
#' Genera una cuadrícula de gráficas Tippett para diferentes valores
#' de diferencia (0 a 3) usando la variable especificada.
#'
#' @param llr_var Nombre de la columna con valores de LLR
#' @return No retorna valor, muestra las gráficas en una cuadrícula
#' @export
#'
#' @examples
#' \dontrun{
#' # Asume que existe un data frame 'todo_balanceado' en el entorno global
#' generar_graficos_tippett("llr_40")
#' }
generar_graficos_tippett <- function(llr_var) {
  # Verificar que el paquete gridExtra está instalado
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("El paquete 'gridExtra' es necesario para esta función. Por favor instálalo con install.packages('gridExtra')")
  }
  
  plots <- list()
  
  for (i in 0:3) {
    df_filtrado <- todo_balanceado %>% filter(diferencia == i)
    titulo <- paste0("diferencia = ", i)
    p <- T_ajustada(df_filtrado, llr_var, "misma_persona", title = titulo, c(-5, 5))
    if (i < 4) {
      p <- p + theme(legend.position = "none")
    }
    plots[[i + 1]] <- p
  }
  
  # Agregar etiqueta en la posición 6
  plots[[6]] <- grid::textGrob("#")
  
  # Mostrar las gráficas (5 gráficos + etiqueta)
  gridExtra::grid.arrange(
    grobs = plots,
    ncol = 2,
    widths = c(1, 1)
  )
}

#' Generar múltiples gráficas Tippett (versión 3 diferencias)
#'
#' Genera una cuadrícula de 2x2 con gráficas Tippett para diferencias 0 a 3.
#'
#' @param llr_var Nombre de la columna con valores de LLR
#' @return No retorna valor, muestra las gráficas en una cuadrícula
#' @export
#'
#' @examples
#' \dontrun{
#' generar_graficos_tippett_con3("llr_40")
#' }
generar_graficos_tippett_con3 <- function(llr_var) {
  # Verificar que el paquete gridExtra está instalado
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("El paquete 'gridExtra' es necesario para esta función. Por favor instálalo con install.packages('gridExtra')")
  }
  
  plots <- list()
  
  # Generar gráficos para diferencias 0, 1, 2, 3
  for (i in 0:3) {
    df_filtrado <- todo_balanceado %>% filter(diferencia == i)
    titulo <- paste0("diferencia = ", i)
    p <- T_ajustada(df_filtrado, llr_var, "misma_persona", title = titulo, c(-5, 5))
    # Eliminar leyenda para todos los gráficos (ahora son 4)
    p <- p + theme(legend.position = "none")
    plots[[i + 1]] <- p
  }
  
  # Convertir todos los ggplot a grobs
  plots_grobs <- lapply(plots, ggplot2::ggplotGrob)
  
  # Mostrar las gráficas en una cuadrícula de 2x2
  gridExtra::grid.arrange(
    grobs = plots_grobs,
    ncol = 2,
    nrow = 2,
    widths = c(1, 1)
  )
}

#' Generar múltiples gráficas Tippett para LLR calibrado
#'
#' Genera una cuadrícula de 2x2 con gráficas Tippett para LLR calibrado,
#' con zoom en el rango [-0.5, 0.5].
#'
#' @param llr_var Nombre de la columna con valores de LLR calibrado
#' @return No retorna valor, muestra las gráficas en una cuadrícula
#' @export
#'
#' @examples
#' \dontrun{
#' generar_graficos_tippett_calibrado_con3("llr_calibrado")
#' }
generar_graficos_tippett_calibrado_con3 <- function(llr_var) {
  # Verificar que el paquete gridExtra está instalado
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("El paquete 'gridExtra' es necesario para esta función. Por favor instálalo con install.packages('gridExtra')")
  }
  
  plots <- list()
  
  # Generar gráficos para diferencias 0, 1, 2, 3
  for (i in 0:3) {
    df_filtrado <- todo_balanceado %>% filter(diferencia == i)
    titulo <- paste0("diferencia = ", i)
    p <- T_ajustada(df_filtrado, llr_var, "misma_persona", title = titulo, c(-.5, .5))
    p <- p + theme(legend.position = "none")  # Quitar leyenda de todos
    plots[[i + 1]] <- p
  }
  
  # Convertir todos los ggplot a grobs
  plots_grobs <- lapply(plots, ggplot2::ggplotGrob)
  
  # Como tenemos 4 gráficos, los mostramos en 2x2
  gridExtra::grid.arrange(
    grobs = plots_grobs,
    ncol = 2,
    nrow = 2,
    widths = c(1, 1)
  )
}