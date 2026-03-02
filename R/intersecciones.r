#' Encontrar punto de cruce en gráfica Tippett
#'
#' Encuentra el valor de x donde se intersectan las curvas SS y DS
#' en una gráfica Tippett.
#'
#' @param base Data frame con los datos
#' @param var_llr Nombre de la columna con valores de LLR
#' @param var_categoria Nombre de la columna con categorías
#' @return Valor numérico del punto de cruce en escala log10
#' @export
#'
#' @examples
#' \dontrun{
#' cruce <- encontrar_cruce(datos, "llr_valores", "categoria")
#' }
encontrar_cruce <- function(base, var_llr, var_categoria) {
  # Verificamos que existan las columnas especificadas
  if (!all(c(var_llr, var_categoria) %in% names(base))) {
    stop("Las columnas especificadas no existen en la base de datos.")
  }
  
  # Extraer LLRs y convertirlos a log10 (igual que en tu función original)
  llr_H1 <- base[[var_llr]][base[[var_categoria]] == "Misma persona"]
  llr_H2 <- base[[var_llr]][base[[var_categoria]] == "Diferente persona"]
  
  # Conversión a log10
  ss_log10 <- llr_H1 / log(10)
  ds_log10 <- llr_H2 / log(10)
  
  # Crear funciones de distribución empírica
  ecdf_ss <- ecdf(ss_log10)
  ecdf_ds <- ecdf(ds_log10)
  
  # Definir el rango de búsqueda
  x_range <- seq(
    from = min(ss_log10, ds_log10),
    to = max(ss_log10, ds_log10),
    length.out = 1000
  )
  
  # Calcular las diferencias entre las funciones de distribución
  diferencias <- ecdf_ss(x_range) - (1 - ecdf_ds(x_range))
  
  # Encontrar donde la diferencia cambia de signo (cruce)
  sign_changes <- which(diff(sign(diferencias)) != 0)
  
  if (length(sign_changes) == 0) {
    warning("No se encontró un punto de cruce en el rango de datos.")
    return(NULL)
  }
  
  # Tomar el primer cruce (usualmente el más significativo)
  idx <- sign_changes[1]
  
  # Interpolación lineal para encontrar el punto exacto de cruce
  x1 <- x_range[idx]
  x2 <- x_range[idx + 1]
  y1 <- diferencias[idx]
  y2 <- diferencias[idx + 1]
  
  # Encontrar x donde y = 0 (interpolación lineal)
  x_cruce <- x1 - y1 * (x2 - x1) / (y2 - y1)
  
  return(x_cruce)
}

#' Encontrar intersecciones por filtro
#'
#' Calcula los puntos de intersección en gráficas Tippett
#' para diferentes niveles de un filtro (ej. diferencia).
#'
#' @param base Data frame con los datos
#' @param var_llr Nombre de la columna con valores de LLR
#' @param var_categoria Nombre de la columna con categorías
#' @param filtro Nombre de la columna usada como filtro
#' @return Data frame con columnas: diferencia, interseccion
#' @export
#'
#' @examples
#' \dontrun{
#' intersecciones <- find_intersections_tippett(datos, "llr_40", "misma_persona")
#' }
find_intersections_tippett <- function(base, var_llr, var_categoria, filtro = "diferencia") {
  resultados <- data.frame(diferencia = 0:3, interseccion = NA_real_)
  
  for (i in 0:3) {
    df_filtrado <- base[base[[filtro]] == i, ]
    
    # Extracción directa por nombres de categoría
    llr_H1 <- df_filtrado[[var_llr]][df_filtrado[[var_categoria]] == "Misma persona"]
    llr_H2 <- df_filtrado[[var_llr]][df_filtrado[[var_categoria]] == "Diferente persona"]
    
    if (length(llr_H1) >= 2 && length(llr_H2) >= 2) {
      ss_log10 <- llr_H1 / log(10)
      ds_log10 <- llr_H2 / log(10)
      
      ss_ecdf <- ecdf(ss_log10)
      ds_ecdf <- ecdf(ds_log10)
      
      tryCatch({
        resultados$interseccion[resultados$diferencia == i] <- uniroot(
          function(x) (1 - ss_ecdf(x)) - ds_ecdf(x),
          range(c(ss_log10, ds_log10)),
          extendInt = "yes"
        )$root
      }, error = function(e) NULL)
    }
  }
  
  return(resultados)
}

#' Encontrar intersecciones (versión avanzada)
#'
#' Versión mejorada que permite múltiples filtros y devuelve
#' información detallada sobre las intersecciones.
#'
#' @param base Data frame con los datos
#' @param var_llr Nombre de la columna con valores de LLR
#' @param var_categoria Nombre de la columna con categorías
#' @param filtro Nombre de la columna usada como filtro principal
#' @param niveles_filtro Vector con niveles del filtro a analizar
#' @param etiqueta_H1 Etiqueta para hipótesis misma persona
#' @param etiqueta_H2 Etiqueta para hipótesis diferente persona
#' @param llr_es_log10 Indica si los LLR ya están en log10
#' @param grid_points Número de puntos para la rejilla de búsqueda
#' @param var_n Nombre de la columna con tamaño de muestra
#' @param niveles_n Vector con niveles de n a analizar
#' @return Data frame con intersecciones detalladas
#' @export
#'
#' @examples
#' \dontrun{
#' resultados <- find_intersections_tippett2(datos, "llr_40", "misma_persona",
#'                                           niveles_n = c(10, 20, 30))
#' }
find_intersections_tippett2 <- function(
    base,
    var_llr,
    var_categoria,
    filtro         = "diferencia",
    niveles_filtro = 0:4,
    etiqueta_H1    = "Misma persona",
    etiqueta_H2    = "Diferente persona",
    llr_es_log10   = FALSE,
    grid_points    = 2000,
    var_n          = "n",
    niveles_n      = c(2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
){
  stopifnot(is.data.frame(base))
  
  # Auxiliar para encontrar raíces por rejilla
  find_roots <- function(f, rng, n = 2000){
    xs <- seq(rng[1], rng[2], length.out = n)
    ys <- f(xs)
    sgn <- sign(ys)
    idx <- which(diff(sgn) != 0 & is.finite(ys[-1]) & is.finite(ys[-length(ys)]))
    roots <- numeric(0)
    for (k in idx){
      a <- xs[k]; b <- xs[k+1]
      rr <- tryCatch(uniroot(f, c(a, b), extendInt = "yes")$root, error = function(e) NA_real_)
      roots <- c(roots, rr)
    }
    unique(roots[is.finite(roots)])
  }
  
  # Data.frame acumulador
  out_all <- data.frame(
    n                      = integer(0),
    diferencia             = integer(0),
    interseccion_x_log10   = character(0),
    interseccion_LR        = character(0),
    interseccion_y         = character(0),
    n_H1                   = integer(0),
    n_H2                   = integer(0),
    stringsAsFactors       = FALSE
  )
  
  # Bucle por cada n solicitado
  for (n_val in niveles_n){
    base_n <- base[base[[var_n]] == n_val, , drop = FALSE]
    
    # Estructura de salida para este n
    out <- data.frame(
      n                    = n_val,
      diferencia           = niveles_filtro,
      interseccion_x_log10 = NA_character_,
      interseccion_LR      = NA_character_,
      interseccion_y       = NA_character_,
      n_H1                 = NA_integer_,
      n_H2                 = NA_integer_,
      stringsAsFactors     = FALSE
    )
    
    for (i in seq_along(niveles_filtro)){
      val_f <- niveles_filtro[i]
      df_i <- base_n[base_n[[filtro]] == val_f, , drop = FALSE]
      if (nrow(df_i) == 0) next
      
      x_H1 <- df_i[[var_llr]][df_i[[var_categoria]] == etiqueta_H1]
      x_H2 <- df_i[[var_llr]][df_i[[var_categoria]] == etiqueta_H2]
      x_H1 <- x_H1[is.finite(x_H1)]
      x_H2 <- x_H2[is.finite(x_H2)]
      
      out$n_H1[i] <- length(x_H1)
      out$n_H2[i] <- length(x_H2)
      if (length(x_H1) < 2 || length(x_H2) < 2) next
      
      ss_log10 <- if (llr_es_log10) x_H1 else x_H1 / log(10)
      ds_log10 <- if (llr_es_log10) x_H2 else x_H2 / log(10)
      
      ss_ecdf <- ecdf(ss_log10)
      ds_ecdf <- ecdf(ds_log10)
      
      f <- function(x) (1 - ss_ecdf(x)) - ds_ecdf(x)
      rango <- range(c(ss_log10, ds_log10), finite = TRUE)
      
      raiz_directa <- tryCatch(uniroot(f, rango, extendInt = "yes")$root, error = function(e) NA_real_)
      roots <- if (is.finite(raiz_directa)) raiz_directa else find_roots(f, rango, n = grid_points)
      
      if (length(roots) == 0){
        xs <- seq(rango[1], rango[2], length.out = grid_points)
        ys <- abs(f(xs))
        x_star <- xs[which.min(ys)]
      } else {
        x_star <- roots[which.min(abs(roots))]
      }
      
      y_star  <- ds_ecdf(x_star)   # = 1 - ss_ecdf(x_star)
      LR_star <- 10^x_star
      
      # 10 decimales, sin notación científica
      out$interseccion_x_log10[i] <- formatC(x_star,  format = "f", digits = 10)
      out$interseccion_LR[i]      <- formatC(LR_star, format = "f", digits = 10)
      out$interseccion_y[i]       <- formatC(y_star,  format = "f", digits = 10)
    }
    
    # Pegar resultados
    out_all <- rbind(out_all, out)
  }
  
  rownames(out_all) <- NULL
  out_all
}

#' Graficar intersecciones
#'
#' Genera un gráfico de puntos que muestra las intersecciones
#' para diferentes modelos y niveles de diferencia.
#'
#' @param resultados_combinados Data frame con resultados de intersecciones
#' @return Objeto ggplot
#' @export
#'
#' @examples
#' \dontrun{
#' graficar_intersecciones(mis_resultados)
#' }
graficar_intersecciones <- function(resultados_combinados) {
  # Verificar que los paquetes necesarios estén instalados
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("El paquete 'ggplot2' es requerido pero no está instalado.")
  }
  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    stop("El paquete 'ggrepel' es requerido pero no está instalado.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("El paquete 'dplyr' es requerido pero no está instalado.")
  }
  
  # Cargar los paquetes
  library(ggplot2)
  library(ggrepel)
  library(dplyr)
  
  # Preparamos los datos en formato largo para ggplot
  datos_grafico <- resultados_combinados %>%
    pivot_longer(cols = -diferencia, names_to = "modelo", values_to = "interseccion")
  
  # Crear el gráfico
  grafico <- ggplot(datos_grafico, aes(x = interseccion, y = diferencia, color = modelo, shape = modelo)) +
    geom_vline(xintercept = 0, color = "black", linewidth = 0.5, linetype = "solid") +
    geom_point(size = 2.5) +
    geom_point(data = filter(datos_grafico, diferencia == 2 & modelo == "llr_40"), 
               color = "#D59F0F", size = 2.5) +
    geom_text_repel(aes(label = sprintf("%.2f", interseccion)), 
                    size = 2.5, 
                    color = "#5D6E73", 
                    min.segment.length = 0, 
                    box.padding = 0.5, 
                    max.overlaps = Inf,
                    segment.linetype = "dashed",
                    segment.size = 0.05) +
    scale_y_reverse(breaks = 0:4) +
    scale_color_manual(
      values = c("llr_10" = "#EAEDEF", "llr_20" = "#7297A6", 
                 "llr_30" = "#35637D", "llr_40" = "#D59F0F"),
      labels = c("10 audios", "20 audios", "30 audios", "40 audios")
    ) +
    scale_shape_manual(
      values = c("llr_10" = 15, "llr_20" = 16, 
                 "llr_30" = 18, "llr_40" = 17),
      labels = c("10 audios", "20 audios", "30 audios", "40 audios")
    ) +
    labs(
      title = "Puntos de intersección por número de diferencia",
      subtitle = "Valores más cercanos a cero indican mejor equilibrio",
      x = "Valor de intersección (log10)",
      y = "Diferencias",
      color = NULL,
      shape = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "#ecf0f1", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "#2c3e50"),
      axis.title = element_text(color = "#2c3e50"),
      axis.title.x = element_text(margin = margin(t = 10)),
      plot.title = element_text(color = "#2c3e50", face = "bold", hjust = 0.5),
      plot.subtitle = element_text(color = "#7f8c8d", hjust = 0.5, size = 10),
      legend.position = "bottom",
      legend.justification = "center",
      legend.margin = margin(t = 10),
      legend.box = "horizontal",
      legend.spacing.x = unit(0.5, "cm"),
      plot.margin = margin(10, 10, 40, 10)
    ) +
    annotate("text", 
             x = mean(range(datos_grafico$interseccion)), 
             y = -0.5,
             label = "", 
             color = "#2c3e50",
             size = 4,
             vjust = 1) +
    guides(
      color = guide_legend(
        title = NULL,
        override.aes = list(shape = c(15, 16, 18, 17), size = 3.5)
      ),
      shape = "none"
    )
  
  return(grafico)
}

#' Procesar por n
#'
#' Procesa datos para un valor específico de n, generando gráficas
#' Tippett y calculando intersecciones.
#'
#' @param k Valor de n a procesar
#' @return Data frame con intersecciones para ese n
#' @export
#'
#' @examples
#' \dontrun{
#' resultados_n10 <- procesar_por_n(10)
#' }
procesar_por_n <- function(k) {
  base_k <- todo_balanceado %>%
    filter(n == k) %>%
    mutate(
      misma_persona = if_else(misma_persona_binaria == 1, "Misma persona", "Diferente persona"),
      misma_persona = factor(misma_persona, levels = c("Misma persona", "Diferente persona"))
    )
  
  try({
    generar_graficos_tippett(data = base_k, var_llr = "llr_40")
  }, silent = TRUE)
  
  res_k <- find_intersections_tippett(
    base = base_k,
    var_llr = "calibrado",
    var_categoria = "misma_persona"
  )
  res_k <- as.data.frame(res_k)
  res_k <- tibble::as_tibble(res_k) %>% mutate(n = k, .before = 1)
  return(res_k)
}

#' Procesar por n y duración
#'
#' Procesa datos para combinaciones de n y duración,
#' generando gráficas Tippett y calculando intersecciones.
#'
#' @param duracion_val Valor de duración a filtrar
#' @return Data frame con intersecciones para cada n en esa duración
#' @export
#'
#' @examples
#' \dontrun{
#' resultados_duracion5 <- procesar_por_n_duracion(5)
#' }
procesar_por_n_duracion <- function(duracion_val) {
  # Filtrar por duración
  base_duracion <- todo_balanceado %>%
    filter(duracion == duracion_val)
  
  # Procesar cada n para esta duración
  resultados_duracion <- map_dfr(n_vals, function(k) {
    base_k <- base_duracion %>%
      filter(n == k) %>%
      mutate(
        misma_persona = if_else(misma_persona_binaria == 1, "Misma persona", "Diferente persona"),
        misma_persona = factor(misma_persona, levels = c("Misma persona", "Diferente persona"))
      )
    
    # (1) Gráfico de Tippett para este n y duración
    try({
      generar_graficos_tippett(data = base_k, var_llr = "llr_40")
    }, silent = TRUE)
    
    # (2) Intersecciones para este n y duración
    res_k <- find_intersections_tippett(
      base = base_k,
      var_llr = "calibrado",
      var_categoria = "misma_persona"
    )
    
    # Convertir a tibble y agregar columnas n y duracion
    res_k <- as.data.frame(res_k)
    res_k <- tibble::as_tibble(res_k) %>% 
      mutate(n = k, duracion = duracion_val, .before = 1)
    
    return(res_k)
  })
  
  return(resultados_duracion)
}

#' Transformar tabla de intersecciones
#'
#' Convierte una tabla de intersecciones a formato ancho
#' y aplica valor absoluto a los valores.
#'
#' @param tabla_intersecciones Data frame con intersecciones
#' @return Data frame transformado
#' @export
#'
#' @examples
#' \dontrun{
#' tabla_ancha <- transformar_tabla_intersecciones(mis_intersecciones)
#' }
transformar_tabla_intersecciones <- function(tabla_intersecciones) {
  require(dplyr)
  require(tidyr)
  
  tabla_final <- tabla_intersecciones %>%
    mutate(diferencia = as.numeric(diferencia)) %>%
    select(n, diferencia, interseccion) %>%
    pivot_wider(
      names_from = n,
      values_from = interseccion,
      names_sort = TRUE
    ) %>%
    rename(diferencia = diferencia) %>%
    arrange(diferencia) %>%
    # Aplicar valor absoluto a todas las columnas numéricas excepto 'diferencia'
    mutate(across(where(is.numeric) & !diferencia, abs))
  
  return(tabla_final)
}