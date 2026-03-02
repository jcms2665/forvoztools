#' Verificar e instalar paquetes requeridos
#'
#' Esta función verifica si los paquetes necesarios están instalados y los carga.
#' Si algún paquete no está instalado, lo instala automáticamente.
#'
#' @param paquetes Vector de nombres de paquetes a verificar e instalar
#' @return No retorna valor, solo carga los paquetes
#' @export
#'
#' @examples
#' \dontrun{
#' paquetes <- c("ggplot2", "dplyr", "tidyr")
#' verificar_paquetes(paquetes)
#' }
verificar_paquetes <- function(paquetes) {
  for (i in paquetes) { 
    if (!require(i, character.only = TRUE)) { 
      install.packages(i)
      library(i, character.only = TRUE) 
    } else { 
      library(i, character.only = TRUE) 
    } 
  }
}

#' Extraer nombre completo de archivos de audio
#'
#' Extrae el nombre base eliminando sufijos comunes en archivos de audio forense.
#'
#' @param texto Vector de caracteres con nombres de archivo
#' @return Vector de caracteres con nombres limpios
#' @export
#'
#' @examples
#' extraer_nombre_completo("audio_HESP_T2.wav")
extraer_nombre_completo <- function(texto) {
  if(is.na(texto) || texto == "") return(NA_character_)
  
  # Lista completa de palabras que indican el inicio de sufijos
  sufijos <- c(
    "HESP", "LEC", "TEC",           # Tipos de audio
    "ia",                            # Sufijo ia
    "T2", "T3", "T4",                # Números de test
    "\\d+",                          # Cualquier número
    "[A-Z]",                         # Cualquier letra mayúscula sola
    "_"                               # Doble underscore
  )
  
  # Buscar el primer sufijo conocido
  patron <- paste0("_(?:", paste(sufijos, collapse = "|"), ")")
  
  # Encontrar posición
  pos <- str_locate(texto, patron)[1, "start"]
  
  if(!is.na(pos)) {
    return(str_sub(texto, 1, pos - 1))
  } else {
    return(texto)
  }
}