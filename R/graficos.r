#' Gráfica de densidad de LLR
#'
#' Genera un gráfico de densidad que muestra la distribución de LLR
#' para las hipótesis de misma y diferente persona.
#'
#' @param base Data frame con los datos
#' @param var_llr Nombre de la columna con valores de LLR
#' @param var_categoria Nombre de la columna con categorías
#' @param nombre_base Texto adicional para el título
#' @param x_range Vector de dos elementos para limitar el eje x (opcional)
#' @return Objeto ggplot
#' @export
#'
#' @examples
#' \dontrun{
#' graficar_densidad_llr(datos, "llr_valores", "categoria", "Mi análisis")
#' }
graficar_densidad_llr <- function(base, var_llr, var_categoria, nombre_base = "", x_range = NULL) {
  var_llr_sym <- rlang::ensym(var_llr)
  var_cat_sym <- rlang::ensym(var_categoria)
  
  # Extraer valores de LLR y categoría
  llr_vals <- base[[as.character(var_llr_sym)]]
  cat_vals <- base[[as.character(var_cat_sym)]]
  
  # Calcular densidad para ambas categorías
  dens_misma <- density(llr_vals[cat_vals == "Misma persona"])
  dens_diferente <- density(llr_vals[cat_vals == "Diferente persona"])
  
  pico_misma <- dens_misma$x[which.max(dens_misma$y)]
  pico_diferente <- dens_diferente$x[which.max(dens_diferente$y)]
  
  # Crear gráfico
  ggplot(base, aes(x = !!var_llr_sym, fill = !!var_cat_sym, color = !!var_cat_sym)) +
    geom_density(alpha = 0.4, linewidth = 0.2) +
    geom_vline(xintercept = pico_misma, linetype = "dashed", color = "#0B3D62", linewidth = 0.5) +
    geom_vline(xintercept = pico_diferente, linetype = "dashed", color = "#3498DB", linewidth = 0.5) +
    annotate("text", x = pico_misma + 0.2, y = max(dens_misma$y) * 1.05, 
             label = paste("LLR ≈", round(pico_misma, 2)), color = "#0B3D62", size = 3, hjust = 0) +
    annotate("text", x = pico_diferente + 0.2, y = max(dens_diferente$y) * 1.05, 
             label = paste("LLR ≈", round(pico_diferente, 2)), color = "#0B3D62", size = 3, hjust = 1) +
    scale_fill_manual(values = c("Misma persona" = "#0B3D62", "Diferente persona" = "#3498DB")) +
    scale_color_manual(values = c("Misma persona" = "#0B3D62", "Diferente persona" = "#3498DB")) +
    labs(
      x = "LLR",
      y = "Densidad de probabilidad",
      title = paste("", nombre_base),
      fill = "Hipótesis",
      color = "Hipótesis"
    ) +
    coord_cartesian(xlim = x_range) +
    theme_minimal(base_size = 10) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_blank(),
      axis.text = element_text(color = "#2c3e50"),
      axis.title = element_text(color = "#2c3e50"),
      plot.title = element_text(color = "#2c3e50", face = "bold", hjust = 0.5),
      legend.title = element_text(color = "#2c3e50"),
      legend.text = element_text(color = "#2c3e50"),
      legend.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(10, 10, 10, 10)
    )
}

#' Gráfica de violín de LLR
#'
#' Genera un gráfico de violín que muestra la distribución de LLR
#' para las hipótesis de misma y diferente persona.
#'
#' @param base Data frame con los datos
#' @param var_llr Nombre de la columna con valores de LLR
#' @param var_categoria Nombre de la columna con categorías
#' @param nombre_base Texto adicional para el título
#' @return Objeto ggplot
#' @export
#'
#' @examples
#' \dontrun{
#' graficar_violin_llr(datos, "llr_valores", "categoria", "Mi análisis")
#' }
graficar_violin_llr <- function(base, var_llr, var_categoria, nombre_base = "") {
  # Verificamos que existan las columnas especificadas
  if (!all(c(var_llr, var_categoria) %in% names(base))) {
    stop("Las columnas especificadas no existen en la base de datos.")
  }
  
  # Crear el gráfico
  ggplot(base, aes(x = .data[[var_categoria]], y = .data[[var_llr]], 
                   fill = .data[[var_categoria]], color = .data[[var_categoria]])) +
    geom_violin(trim = FALSE, alpha = 0.4, linewidth = 0.2) +
    geom_hline(yintercept = 0, color = "#bdc3c7", linewidth = 0.3, linetype = "solid") +
    scale_fill_manual(values = c("Misma persona" = "#0B3D62", "Diferente persona" = "#3498DB")) +
    scale_color_manual(values = c("Misma persona" = "#0B3D62", "Diferente persona" = "#3498DB")) +
    labs(
      x = "Hipótesis",
      y = "LLR",
      title = paste("Distribución de LLR por hipótesis", nombre_base),
      fill = "Hipótesis",
      color = "Hipótesis"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "#bdc3c7", size = 0.05, linetype = "dotted"),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "#2c3e50"),
      axis.title = element_text(color = "#2c3e50"),
      plot.title = element_text(color = "#2c3e50", face = "bold", hjust = 0.5),
      legend.title = element_text(color = "#2c3e50"),
      legend.text = element_text(color = "#2c3e50"),
      legend.background = element_rect(fill = "white", color = NA),
      legend.position = "right",
      plot.margin = margin(10, 10, 10, 10)
    )
}

#' Gráfica Tippett
#'
#' Genera una gráfica Tippett que muestra las proporciones acumuladas
#' de LLR para las hipótesis de misma y diferente persona.
#'
#' @param base Data frame con los datos
#' @param var_llr Nombre de la columna con valores de LLR
#' @param var_categoria Nombre de la columna con categorías
#' @param title Título del gráfico
#' @param zoom Vector de dos elementos para limitar el eje x (opcional)
#' @return Objeto ggplot
#' @export
#'
#' @examples
#' \dontrun{
#' T_ajustada(datos, "llr_valores", "categoria", "Mi Tippett", c(-5, 5))
#' }
T_ajustada <- function(base, var_llr, var_categoria, title = "Gráfica Tippett", zoom = NULL) {
  # Verificamos que existan las columnas especificadas
  if (!all(c(var_llr, var_categoria) %in% names(base))) {
    stop("Las columnas especificadas no existen en la base de datos.")
  }
  
  # Extraer LLRs y convertirlos a log10
  llr_H1 <- base[[var_llr]][base[[var_categoria]] == "Misma persona"]
  llr_H2 <- base[[var_llr]][base[[var_categoria]] == "Diferente persona"]
  
  # Conversión a log10
  ss_log10 <- llr_H1 / log(10)
  ds_log10 <- llr_H2 / log(10)
  
  # Preparar datos para el gráfico
  plot_data <- rbind(
    data.frame(
      LRs = sort(ss_log10),
      Proportion = seq(from = 0, to = 1, length = length(ss_log10)),
      Type = factor("SS log10 LRs", levels = c("SS log10 LRs", "DS log10 LRs"))
    ),
    data.frame(
      LRs = sort(ds_log10),
      Proportion = seq(from = 1, to = 0, length = length(ds_log10)),
      Type = factor("DS log10 LRs", levels = c("SS log10 LRs", "DS log10 LRs"))
    )
  )
  
  # Límites del gráfico
  x.min <- min(-8, floor(min(ss_log10, ds_log10)))
  x.max <- max(8, ceiling(max(ss_log10, ds_log10)))
  
  # Crear el gráfico
  p <- ggplot(plot_data, aes(x = LRs, y = Proportion, color = Type, linetype = Type)) +
    geom_line(linewidth = 0.8) +
    geom_vline(xintercept = 0, color = "black", linewidth = 0.3) +
    scale_color_manual(
      values = c("SS log10 LRs" = "#0B3D62", "DS log10 LRs" = "#3498DB"),
      labels = c(expression("SS"~log[10](LR)), expression("DS"~log[10](LR))),
      breaks = c("SS log10 LRs", "DS log10 LRs")
    ) +
    scale_linetype_manual(
      values = c("SS log10 LRs" = "solid", "DS log10 LRs" = "dashed"),
      labels = c(expression("SS"~log[10](LR)), expression("DS"~log[10](LR))),
      breaks = c("SS log10 LRs", "DS log10 LRs")
    ) +
    labs(
      x = expression(log[10](LR)),
      y = "Proporción acumulada",
      title = title
    ) +
    coord_cartesian(xlim = c(x.min, x.max), ylim = c(0, 1)) +
    guides(
      color = guide_legend(order = 1),
      linetype = guide_legend(order = 1)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      plot.title = element_text(face = "bold", hjust = 0.5, color = "#0B3D62"),
      legend.position = "right",
      legend.title = element_blank(),
      legend.text = element_text(color = "black", size = 10),
      legend.key.width = unit(1.5, "cm"),
      legend.spacing.x = unit(0.2, "cm")
    )
  
  # Aplicar zoom si se especifica
  if (!is.null(zoom)) {
    if (length(zoom) != 2 || !is.numeric(zoom)) {
      stop("El argumento 'zoom' debe ser un vector numérico de longitud 2, por ejemplo, c(-5, 5)")
    }
    p <- p + coord_cartesian(xlim = zoom)
  }
  
  return(p)
}

#' Comparación LLR original vs calibrado
#'
#' Genera un gráfico de densidad comparando LLR original, calibrado y fusionado.
#'
#' @param base Data frame con los datos
#' @param var_original Nombre de la columna con LLR original
#' @param var_calibrado Nombre de la columna con LLR calibrado
#' @param var_tercera Nombre de la columna con LLR fusionado
#' @param nombre_base Texto adicional para el título
#' @return Objeto ggplot
#' @export
#'
#' @examples
#' \dontrun{
#' graficar_llr_vs_calibrado(datos, "llr_original", "llr_calibrado", "llr_fusion")
#' }
graficar_llr_vs_calibrado <- function(base, var_original, var_calibrado, var_tercera, nombre_base = "") {
  # Verificamos que existan todas las columnas
  if (!all(c(var_original, var_calibrado, var_tercera) %in% names(base))) {
    stop("Las columnas especificadas no existen en la base de datos.")
  }
  
  # Crear tabla en formato largo
  base_long <- tibble(
    valor = c(base[[var_original]], base[[var_calibrado]], base[[var_tercera]]),
    tipo = rep(c("LLR original", "LLR calibrado", "LLR fusion"), 
               each = nrow(base))
  )
  
  # Gráfico
  ggplot(base_long, aes(x = valor, fill = tipo, color = tipo)) +
    geom_density(alpha = 0.4, linewidth = 0.4) +
    scale_fill_manual(values = c("LLR original" = "#3498DB", 
                                 "LLR calibrado" = "#0B3D62",
                                 "LLR fusion" = "#D59F0F")) +
    scale_color_manual(values = c("LLR original" = "#3498DB", 
                                  "LLR calibrado" = "#0B3D62",
                                  "LLR fusion" = "#D59F0F")) +
    labs(
      title = paste(nombre_base),
      x = "Valor del LLR",
      y = "Densidad",
      fill = "",
      color = ""
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom"
    )
}