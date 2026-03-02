#' Generar Excel con resultados completos
#'
#' Genera un archivo Excel con dos hojas: intersecciones y Cllr
#' para diferentes valores de n.
#'
#' @param base_datos Data frame con los datos
#' @param columna_llr Nombre de la columna con valores de LLR
#' @param columna_grupo Nombre de la columna con grupos
#' @param valores_n Vector con valores de n a analizar
#' @param nombre_archivo Nombre del archivo Excel a generar
#' @param directorio Directorio donde guardar el archivo
#' @return Lista invisible con los data frames generados
#' @export
#'
#' @examples
#' \dontrun{
#' generar_excel_resultados(mis_datos, "llr_40", "misma_persona",
#'                          valores_n = seq(5, 35, by = 5))
#' }
generar_excel_resultados <- function(
    base_datos, 
    columna_llr = "llr_40", 
    columna_grupo = "misma_persona",
    valores_n = seq(5, 35, by = 5),
    nombre_archivo = "Resultados-completos.xlsx",
    directorio = resultados_analisis) {
  
  # Cargar librería requerida
  if (!require(openxlsx)) {
    install.packages("openxlsx")
    library(openxlsx)
  }
  
  # Función para procesar intersecciones
  procesar_intersecciones <- function(n_val) {
    base_datos %>% 
      filter(n == n_val) %>%
      find_intersections_tippett(columna_llr, columna_grupo) %>%
      pull(2)
  }
  
  # Función para procesar Cllr
  procesar_Cllr <- function(n_val) {
    calculate_Cllr(
      base_datos %>% filter(n == n_val), 
      columna_llr, 
      columna_grupo
    )
  }
  
  # Aplicar ambas funciones para cada valor de n
  resultados_intersecciones <- lapply(valores_n, procesar_intersecciones)
  resultados_Cllr <- lapply(valores_n, procesar_Cllr)
  
  # Hoja 1: Resultados de intersecciones
  general_intersecciones <- data.frame(
    diferencia = find_intersections_tippett(
      base_datos %>% filter(n == valores_n[1]), 
      columna_llr, columna_grupo
    )[,1],
    setNames(as.data.frame(resultados_intersecciones), paste0("n",valores_n,"-audios"))
  )
  
  # Hoja 2: Resultados de Cllr
  if (is.list(resultados_Cllr[[1]]) && !is.data.frame(resultados_Cllr[[1]])) {
    general_Cllr <- data.frame(
      n = valores_n,
      Cllr = sapply(resultados_Cllr, function(x) ifelse(is.numeric(x), x, NA))
    )
  } else if (is.data.frame(resultados_Cllr[[1]])) {
    general_Cllr <- do.call(rbind, resultados_Cllr)
    general_Cllr$n <- rep(valores_n, sapply(resultados_Cllr, nrow))
  } else {
    general_Cllr <- data.frame(
      n = valores_n,
      Cllr = unlist(resultados_Cllr)
    )
  }
  
  # Configurar directorio
  if (!missing(directorio)) {
    setwd(directorio)
  }
  
  # Crear y guardar el archivo Excel
  wb <- createWorkbook()
  
  # Hoja 1: Intersecciones
  addWorksheet(wb, "Intersecciones")
  writeData(wb, sheet = "Intersecciones", x = general_intersecciones)
  
  # Hoja 2: Cllr
  addWorksheet(wb, "Cllr")
  writeData(wb, sheet = "Cllr", x = general_Cllr)
  
  # Guardar
  saveWorkbook(wb, nombre_archivo, overwrite = TRUE)
  
  # Mensaje de confirmación
  cat("Archivo guardado:", file.path(directorio, nombre_archivo), "\n")
  
  # Retornar los resultados invisibles por si se quieren usar
  invisible(list(
    intersecciones = general_intersecciones,
    Cllr = general_Cllr
  ))
}

#' Generar Excel con resultados (versión 15 minutos)
#'
#' Versión especializada que incluye categorías de duración promedio.
#'
#' @param base_datos Data frame con los datos
#' @param columna_llr Nombre de la columna con valores de LLR
#' @param columna_grupo Nombre de la columna con grupos
#' @param valores_n Vector con valores de n a analizar
#' @param nombre_archivo Nombre del archivo Excel a generar
#' @param directorio Directorio donde guardar el archivo
#' @return Lista invisible con los data frames generados
#' @export
#'
#' @examples
#' \dontrun{
#' generar_excel_resultados_15_min(mis_datos, "llr_40", "misma_persona")
#' }
generar_excel_resultados_15_min <- function(
    base_datos, 
    columna_llr = "llr_40", 
    columna_grupo = "misma_persona",
    valores_n = seq(5, 35, by = 5),
    nombre_archivo = "Resultados-completos.xlsx",
    directorio = resultados_analisis) {
  
  # Cargar librería requerida
  if (!require(openxlsx)) {
    install.packages("openxlsx")
    library(openxlsx)
  }
  
  # Función para procesar intersecciones por categoría
  procesar_intersecciones_categoria <- function(categoria, n_val) {
    base_datos %>% 
      filter(n == n_val, promedio_duracion_cat == categoria) %>%
      find_intersections_tippett(columna_llr, columna_grupo) %>%
      pull(2)
  }
  
  # Función para procesar Cllr por categoría
  procesar_Cllr_categoria <- function(categoria, n_val) {
    calculate_Cllr(
      base_datos %>% filter(n == n_val, promedio_duracion_cat == categoria), 
      columna_llr, 
      columna_grupo
    )
  }
  
  # Procesar todas las categorías y valores de n
  categorias <- 1:5
  
  # Preparar hoja 1: Intersecciones
  general_intersecciones <- data.frame()
  
  for (cat in categorias) {
    # Obtener la estructura base de diferencias
    estructura_base <- find_intersections_tippett(
      base_datos %>% filter(n == valores_n[1], promedio_duracion_cat == cat), 
      columna_llr, columna_grupo
    )[,1]
    
    # Crear data frame temporal para esta categoría
    temp_df <- data.frame(
      promedio_duracion_cat = cat,
      diferencia = estructura_base
    )
    
    # Agregar columnas para cada valor de n
    for (n_val in valores_n) {
      intersecciones_val <- procesar_intersecciones_categoria(cat, n_val)
      nombre_col <- paste0("n", n_val, ".audios")
      temp_df[[nombre_col]] <- intersecciones_val
    }
    
    # Combinar con el data frame principal
    general_intersecciones <- rbind(general_intersecciones, temp_df)
  }
  
  # Preparar hoja 2: Resultados de Cllr
  general_Cllr <- data.frame()
  
  for (cat in categorias) {
    for (n_val in valores_n) {
      resultado_Cllr <- procesar_Cllr_categoria(cat, n_val)
      
      if (is.list(resultado_Cllr) && !is.data.frame(resultado_Cllr)) {
        valor_Cllr <- ifelse(is.numeric(resultado_Cllr), resultado_Cllr, NA)
      } else if (is.data.frame(resultado_Cllr)) {
        columna_numerica <- sapply(resultado_Cllr, is.numeric)
        if (any(columna_numerica)) {
          valor_Cllr <- resultado_Cllr[, which(columna_numerica)[1], drop = TRUE]
        } else {
          valor_Cllr <- NA
        }
      } else {
        valor_Cllr <- unlist(resultado_Cllr)
      }
      
      # Crear fila para esta combinación categoría-n
      fila_Cllr <- data.frame(
        promedio_duracion_cat = cat,
        n = n_val,
        Cllr = valor_Cllr
      )
      
      general_Cllr <- rbind(general_Cllr, fila_Cllr)
    }
  }
  
  # Ordenar las hojas para mejor presentación
  general_intersecciones <- general_intersecciones[order(general_intersecciones$promedio_duracion_cat, general_intersecciones$diferencia), ]
  general_Cllr <- general_Cllr[order(general_Cllr$promedio_duracion_cat, general_Cllr$n), ]
  
  # Configurar directorio
  if (!missing(directorio)) {
    setwd(directorio)
  }
  
  # Crear y guardar el archivo Excel
  wb <- createWorkbook()
  
  # Hoja 1: Intersecciones
  addWorksheet(wb, "Intersecciones")
  writeData(wb, sheet = "Intersecciones", x = general_intersecciones)
  
  # Hoja 2: Cllr
  addWorksheet(wb, "Cllr")
  writeData(wb, sheet = "Cllr", x = general_Cllr)
  
  # Guardar
  saveWorkbook(wb, nombre_archivo, overwrite = TRUE)
  
  # Mensaje de confirmación
  cat("Archivo guardado:", file.path(directorio, nombre_archivo), "\n")
  
  # Retornar los resultados invisibles
  invisible(list(
    intersecciones = general_intersecciones,
    Cllr = general_Cllr
  ))
}