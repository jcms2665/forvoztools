#' Calcular Cllr (Cost of Log Likelihood Ratio)
#'
#' Calcula el costo de los ratios de verosimilitud logarítmicos (Cllr),
#' una medida de rendimiento para sistemas de reconocimiento forense.
#'
#' @param base Data frame que contiene los datos
#' @param var_llr Nombre de la columna con valores de LLR
#' @param var_categoria Nombre de la columna con categorías ("Misma persona"/"Diferente persona")
#' @return Valor numérico del Cllr redondeado a 4 decimales
#' @export
#'
#' @examples
#' \dontrun{
#' calculate_Cllr(mis_datos, "llr_valores", "categoria")
#' }
calculate_Cllr <- function(base, var_llr, var_categoria) {
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
  
  # Convertir de log10 a lineal
  ss.LRs <- 10^ss_log10
  ds.LRs <- 10^ds_log10
  
  # Función interna para cálculo de sumas
  calculate_sum <- function(LRs, type = "ss") {
    sum_result <- 0
    for (i in seq_along(LRs)) {
      if (type == "ss") {
        sum_result <- sum_result + log2(1 + (1 / LRs[i]))
      } else {
        sum_result <- sum_result + log2(1 + LRs[i])
      }
    }
    return(sum_result / length(LRs))
  }
  
  # Calcular componentes del Cllr
  ss.cllr <- calculate_sum(ss.LRs, "ss")
  ds.cllr <- calculate_sum(ds.LRs, "ds")
  
  # Calcular Cllr final
  Cllr <- (ss.cllr + ds.cllr) / 2
  
  return(round(Cllr, digits = 4))
}

#' Calibrar LRs mediante regresión logística
#'
#' Aplica calibración a los ratios de verosimilitud (LRs) usando
#' regresión logística para mejorar su rendimiento.
#'
#' @param data Data frame con los datos
#' @param LR_col Nombre de la columna con los LRs
#' @param binary_col Nombre de la columna binaria (1 = misma persona, 0 = diferente)
#' @return Lista con coeficientes, prior odds, modelo ajustado y LRs calibrados
#' @export
#'
#' @examples
#' \dontrun{
#' resultado <- calibrate_LR(mis_datos, "LR", "misma_persona_binaria")
#' lrs_calibrados <- resultado$calibrated_LRs
#' }
calibrate_LR <- function(data, LR_col = "LR", binary_col = "misma_persona_binaria") {
  LR.ss <- data[[LR_col]][data[[binary_col]] == 1]
  LR.ds <- data[[LR_col]][data[[binary_col]] == 0]
  
  LR.ss.dataframe <- data.frame(lr = LR.ss, post = 1)
  LR.ds.dataframe <- data.frame(lr = LR.ds, post = 0)
  combined_data <- rbind(LR.ss.dataframe, LR.ds.dataframe)
  
  # Calculate sample sizes and prior odds
  n.ss <- length(LR.ss)
  n.ds <- length(LR.ds)
  n <- n.ss + n.ds
  prior.odds <- n.ss/n.ds
  
  # Calculate log10 of LR values
  combined_data$loglr <- log10(combined_data$lr)
  
  # Fit logistic regression model
  fit <- glm(post ~ loglr, data = combined_data, family = binomial(link = "logit"))
  
  # Check if the slope is negative and issue warning if necessary
  if (fit$coefficients[2] < 0) {
    warning("The logistic regression is decreasing. Check that the LR values are correctly ordered (should be larger for same source) or that your model is sufficiently performant")
  }
  
  # Extract coefficients
  coef <- coefficients(fit)
  
  # Calculate calibrated posterior LRs for all input LRs
  predictors <- log10(data[[LR_col]])
  calibrated.posterior.ratio <- exp(predictors * coef[2] + coef[1])
  calibrated.posterior.probabilities <- calibrated.posterior.ratio / (calibrated.posterior.ratio + 1)
  calibrated.posterior.LRs <- (calibrated.posterior.probabilities / (1 - calibrated.posterior.probabilities)) / prior.odds
  
  # Return results as a list
  out <- list(
    coefficients = coef,
    prior.odds = prior.odds,
    fit = fit,
    calibrated_LRs = calibrated.posterior.LRs
  )
  
  return(out)
}

#' Fusionar múltiples LRs
#'
#' Combina múltiples variables de LLR mediante regresión logística
#' para producir un valor fusionado.
#'
#' @param data Data frame con los datos
#' @param target_var Variable objetivo binaria (1 = misma persona, 0 = diferente)
#' @param predictor_vars Vector con nombres de las variables predictoras
#' @return Vector con valores fusionados (log10 del LR calibrado)
#' @export
#'
#' @examples
#' \dontrun{
#' fusionados <- fusion_LR(datos, "misma_persona_binaria", c("llr_10", "llr_20", "llr_30"))
#' }
fusion_LR <- function(data, target_var, predictor_vars){
  # Calcular n.ss, n.ds, n y prior.odds
  n.ss <- sum(data[[target_var]] == 1, na.rm = TRUE)
  n.ds <- sum(data[[target_var]] == 0, na.rm = TRUE)
  n <- n.ss + n.ds
  prior.odds <- n.ss / n.ds
  
  # Construir la fórmula para el modelo glm
  formula_str <- paste(target_var, "~", paste(predictor_vars, collapse = " + "))
  formula <- as.formula(formula_str)
  
  # Ajustar el modelo de regresión logística
  fit <- glm(formula, data = data, family = binomial(link = "logit"))
  coef <- coef(fit)
  
  # Calcular el predictor lineal
  predictor_lineal <- coef[1]  # Intercept
  
  # Sumar cada coeficiente multiplicado por su variable correspondiente
  for (i in seq_along(predictor_vars)) {
    predictor_lineal <- predictor_lineal + coef[i+1] * data[[predictor_vars[i]]]
  }
  
  # Calcular las probabilidades calibradas
  calibrated.posterior.ratio <- exp(predictor_lineal)
  calibrated.posterior.probabilities <- calibrated.posterior.ratio / (calibrated.posterior.ratio + 1)
  
  # Calcular el valor fusionado (log10 del cociente de verosimilitudes calibrado)
  fusion_values <- log10((calibrated.posterior.probabilities / (1 - calibrated.posterior.probabilities)) / prior.odds)
  
  return(fusion_values)
}

#' Calcular área de solapamiento para densidades
#'
#' Calcula el área de solapamiento entre dos curvas de densidad
#' correspondientes a las hipótesis de misma y diferente persona.
#'
#' @param base Data frame con los datos
#' @param var_llr Nombre de la columna con valores de LLR
#' @param var_categoria Nombre de la columna con categorías
#' @return Área de solapamiento calculada con la regla del trapecio
#' @export
#'
#' @examples
#' \dontrun{
#' overlap <- calcular_overlap_llr(mis_datos, "llr_valores", "categoria")
#' }
calcular_overlap_llr <- function(base, var_llr, var_categoria) {
  var_llr_sym <- rlang::ensym(var_llr)
  var_cat_sym <- rlang::ensym(var_categoria)
  
  llr_vals <- base[[as.character(var_llr_sym)]]
  cat_vals <- base[[as.character(var_cat_sym)]]
  
  # Asegurar que haya exactamente dos categorías
  categorias <- unique(cat_vals)
  if (length(categorias) != 2) {
    stop("Se requieren exactamente dos categorías para calcular el área de solapamiento.")
  }
  
  # Estimar densidades
  dens1 <- density(llr_vals[cat_vals == categorias[1]])
  dens2 <- density(llr_vals[cat_vals == categorias[2]])
  
  # Interpolar sobre un mismo eje común
  x_common <- seq(max(min(dens1$x), min(dens2$x)),
                  min(max(dens1$x), max(dens2$x)),
                  length.out = 1000)
  
  y1_interp <- approx(dens1$x, dens1$y, xout = x_common)$y
  y2_interp <- approx(dens2$x, dens2$y, xout = x_common)$y
  
  # Área bajo la curva del mínimo
  area_overlap <- trapz(x_common, pmin(y1_interp, y2_interp))
  return(area_overlap)
}

#' Calcular área de solapamiento para gráficas Tippett
#'
#' Calcula el área de solapamiento entre las curvas acumuladas
#' de las hipótesis en una gráfica Tippett.
#'
#' @param base Data frame con los datos
#' @param var_llr Nombre de la columna con valores de LLR
#' @param var_categoria Nombre de la columna con categorías
#' @return Área de solapamiento
#' @export
#'
#' @examples
#' \dontrun{
#' overlap_tip <- calcular_overlap_tippett(mis_datos, "llr_valores", "categoria")
#' }
calcular_overlap_tippett <- function(base, var_llr, var_categoria) {
  # Verificar que existan las columnas
  if (!all(c(var_llr, var_categoria) %in% names(base))) {
    stop("Las columnas especificadas no existen en la base de datos.")
  }
  
  # Extraer y convertir LLRs a log10 como en T_ajustada()
  llr_H1 <- base[[var_llr]][base[[var_categoria]] == "Misma persona"]
  llr_H2 <- base[[var_llr]][base[[var_categoria]] == "Diferente persona"]
  
  ss_log10 <- llr_H1 / log(10)
  ds_log10 <- llr_H2 / log(10)
  
  # Crear las curvas acumulativas como en el gráfico Tippett
  ecdf_ss <- ecdf(ss_log10)
  ecdf_ds <- ecdf(ds_log10)
  
  # Rango común para evaluar
  x_vals <- seq(min(c(ss_log10, ds_log10)), 
                max(c(ss_log10, ds_log10)), 
                length.out = 1000)
  
  # Calcular las proporciones acumuladas
  y_ss <- 1 - ecdf_ss(x_vals)  # Proporción SS >= x
  y_ds <- ecdf_ds(x_vals)      # Proporción DS <= x
  
  # El área de solapamiento es la integral del mínimo entre las dos curvas
  overlap_area <- trapz(x_vals, pmin(y_ss, y_ds))
  
  return(overlap_area)
}