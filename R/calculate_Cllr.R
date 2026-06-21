#' Calculate Empirical Cross-Entropy (Cllr) for Likelihood Ratios
#'
#' @description
#' Computes the Empirical Cross-Entropy (Cllr) measure for forensic likelihood 
#' ratios. Cllr is a performance metric that evaluates the quality of a 
#' likelihood ratio system, measuring the average information loss when 
#' comparing same-source and different-source pairs.
#'
#' @details
#' The Cllr (log-likelihood-ratio cost) is calculated as:
#' \deqn{Cllr = \frac{1}{2}(Cllr_{ss} + Cllr_{ds})}
#' where:
#' \deqn{Cllr_{ss} = \frac{1}{n_{ss}} \sum_{i=1}^{n_{ss}} \log_2(1 + 1/LR_i)}
#' \deqn{Cllr_{ds} = \frac{1}{n_{ds}} \sum_{i=1}^{n_{ds}} \log_2(1 + LR_i)}
#' 
#' Lower Cllr values indicate better performance, with Cllr = 0 representing 
#' perfect discrimination and Cllr = 1 representing chance-level performance.
#'
#' @param base A data frame containing the likelihood ratios and category labels.
#' @param var_llr Character string specifying the column name with likelihood 
#'        ratios. The values should be in natural scale (not log-transformed).
#' @param var_categoria Character string specifying the column name with 
#'        categories. Must contain "Misma persona" (same-source) and 
#'        "Diferente persona" (different-source).
#'
#' @return A numeric value representing the Cllr (Empirical Cross-Entropy) 
#'         rounded to 4 decimal places.
#'
#' @references
#' Brümmer, N., & du Preez, J. (2006). Application-independent evaluation of 
#' speaker detection. \emph{Computer Speech & Language}, 20(2-3), 230-275.
#' \doi{10.1016/j.csl.2005.08.001}
#'
#' Ramos, D., & Gonzalez-Rodriguez, J. (2007). Cross-entropy analysis of the 
#' information in forensic speaker recognition. In \emph{2007 IEEE Workshop on 
#' Automatic Identification Advanced Technologies} (pp. 216-221).
#' \doi{10.1109/AUTOID.2007.380620}
#'
#' @seealso
#' \code{\link{ece_plot}} for visualizing the Empirical Cross-Entropy curve.
#'
#' @examples
#' # Create example data
#' set.seed(123)
#' base <- data.frame(
#'   LR = c(rlnorm(50, meanlog = 2, sdlog = 0.5),   # Same-source LRs
#'          rlnorm(50, meanlog = -1, sdlog = 0.3)), # Different-source LRs
#'   categoria = c(rep("Misma persona", 50), rep("Diferente persona", 50))
#' )
#'
#' # Calculate Cllr
#' cllr_value <- calculate_Cllr(base, var_llr = "LR", var_categoria = "categoria")
#' print(cllr_value)
#'
#' @export

calculate_Cllr <- function(base, var_llr, var_categoria) {
  
  # ============================================================
  # 1. INPUT VALIDATION
  # ============================================================
  
  # Check if base is a data frame
  if (!is.data.frame(base)) {
    stop("'base' must be a data frame", call. = FALSE)
  }
  
  # Check if base has at least 2 rows
  if (nrow(base) < 2) {
    stop("'base' must have at least 2 rows", call. = FALSE)
  }
  
  # Check if columns exist
  if (!all(c(var_llr, var_categoria) %in% names(base))) {
    stop(sprintf("Columns '%s' and/or '%s' not found in the data frame", 
                 var_llr, var_categoria), 
         call. = FALSE)
  }
  
  # Check if var_llr is numeric
  if (!is.numeric(base[[var_llr]])) {
    stop(sprintf("Column '%s' must be numeric", var_llr), call. = FALSE)
  }
  
  # Check for NA values
  if (anyNA(base[[var_llr]])) {
    warning("NA values found in likelihood ratio column. These rows will be removed.", 
            call. = FALSE)
    base <- base[!is.na(base[[var_llr]]), ]
  }
  
  if (anyNA(base[[var_categoria]])) {
    warning("NA values found in category column. These rows will be removed.", 
            call. = FALSE)
    base <- base[!is.na(base[[var_categoria]]), ]
  }
  
  # Check if there are rows after removing NAs
  if (nrow(base) == 0) {
    stop("No valid rows remaining after removing NA values", call. = FALSE)
  }
  
  # Check categories
  categories <- unique(base[[var_categoria]])
  required_cats <- c("Misma persona", "Diferente persona")
  
  if (!all(required_cats %in% categories)) {
    stop(sprintf("Categories must include '%s' and '%s'", 
                 required_cats[1], required_cats[2]), 
         call. = FALSE)
  }
  
  # ============================================================
  # 2. EXTRACT AND PREPARE DATA
  # ============================================================
  
  # Extract likelihood ratios by category
  llr_H1 <- base[[var_llr]][base[[var_categoria]] == "Misma persona"]
  llr_H2 <- base[[var_llr]][base[[var_categoria]] == "Diferente persona"]
  
  # Check if both categories have data
  if (length(llr_H1) == 0) {
    stop("No 'Misma persona' cases found", call. = FALSE)
  }
  
  if (length(llr_H2) == 0) {
    stop("No 'Diferente persona' cases found", call. = FALSE)
  }
  
  # Check for zero or negative LR values
  if (any(llr_H1 <= 0, na.rm = TRUE)) {
    warning("LR values <= 0 found in same-source comparisons. These will be replaced with 1e-10", 
            call. = FALSE)
    llr_H1[llr_H1 <= 0] <- 1e-10
  }
  
  if (any(llr_H2 <= 0, na.rm = TRUE)) {
    warning("LR values <= 0 found in different-source comparisons. These will be replaced with 1e-10", 
            call. = FALSE)
    llr_H2[llr_H2 <= 0] <- 1e-10
  }
  
  # Check for infinite values
  if (any(is.infinite(llr_H1))) {
    warning("Infinite LR values found in same-source comparisons. These will be replaced with 1e10", 
            call. = FALSE)
    llr_H1[is.infinite(llr_H1)] <- 1e10
  }
  
  if (any(is.infinite(llr_H2))) {
    warning("Infinite LR values found in different-source comparisons. These will be replaced with 1e10", 
            call. = FALSE)
    llr_H2[is.infinite(llr_H2)] <- 1e10
  }
  
  # ============================================================
  # 3. CONVERT TO LOG10 SCALE
  # ============================================================
  
  # Convert to log10 (note: ln(LR) / ln(10) = log10(LR))
  ss_log10 <- log10(llr_H1)
  ds_log10 <- log10(llr_H2)
  
  # Convert back to natural scale (this is essentially the same as original)
  # but ensures consistency with the method
  ss.LRs <- 10^ss_log10
  ds.LRs <- 10^ds_log10
  
  # ============================================================
  # 4. CALCULATE Cllr COMPONENTS
  # ============================================================
  
  # Internal function to calculate sums for each category
  calculate_sum <- function(LRs, type = "ss") {
    # Input validation for internal function
    if (!is.numeric(LRs)) {
      stop("LRs must be numeric", call. = FALSE)
    }
    
    if (length(LRs) == 0) {
      stop("LRs vector cannot be empty", call. = FALSE)
    }
    
    # Initialize sum
    sum_result <- 0
    
    # Calculate based on type
    for (i in seq_along(LRs)) {
      if (type == "ss") {
        # For same-source: log2(1 + 1/LR)
        sum_result <- sum_result + log2(1 + (1 / LRs[i]))
      } else {
        # For different-source: log2(1 + LR)
        sum_result <- sum_result + log2(1 + LRs[i])
      }
    }
    
    # Return average
    return(sum_result / length(LRs))
  }
  
  # Calculate components
  ss.cllr <- calculate_sum(ss.LRs, type = "ss")
  ds.cllr <- calculate_sum(ds.LRs, type = "ds")
  
  # ============================================================
  # 5. CALCULATE FINAL Cllr
  # ============================================================
  
  # Cllr is the average of both components
  Cllr <- (ss.cllr + ds.cllr) / 2
  
  # Round to 4 decimal places
  Cllr <- round(Cllr, digits = 4)
  
  # ============================================================
  # 6. RETURN RESULT
  # ============================================================
  
  return(Cllr)
}

# ============================================================
# 7. ADDITIONAL HELPER FUNCTIONS (Optional)
# ============================================================

#' Calculate Cllr and related statistics
#'
#' @description
#' Extended version of \code{\link{calculate_Cllr}} that returns additional 
#' statistics including the separate components for same-source and 
#' different-source comparisons.
#'
#' @param base A data frame containing the likelihood ratios and category labels.
#' @param var_llr Character string specifying the column name with likelihood ratios.
#' @param var_categoria Character string specifying the column name with categories.
#'
#' @return A list with components:
#' \item{Cllr}{Overall Cllr value.}
#' \item{Cllr_ss}{Cllr component for same-source comparisons.}
#' \item{Cllr_ds}{Cllr component for different-source comparisons.}
#' \item{n_ss}{Number of same-source comparisons.}
#' \item{n_ds}{Number of different-source comparisons.}
#' \item{Cllr_min}{Theoretical minimum Cllr (0 for perfect discrimination).}
#' \item{Cllr_max}{Theoretical maximum Cllr (1 for chance-level performance).}
#'
#' @examples
#' set.seed(123)
#' base <- data.frame(
#'   LR = c(rlnorm(50, meanlog = 2, sdlog = 0.5),
#'          rlnorm(50, meanlog = -1, sdlog = 0.3)),
#'   categoria = c(rep("Misma persona", 50), rep("Diferente persona", 50))
#' )
#'
#' result <- calculate_Cllr_extended(base, "LR", "categoria")
#' print(result)
#'
#' @export

calculate_Cllr_extended <- function(base, var_llr, var_categoria) {
  
  # Use the main function to get basic validation and extraction
  # We'll replicate the logic to get all components
  
  # Basic validation
  if (!is.data.frame(base)) {
    stop("'base' must be a data frame", call. = FALSE)
  }
  
  if (!all(c(var_llr, var_categoria) %in% names(base))) {
    stop(sprintf("Columns '%s' and/or '%s' not found", var_llr, var_categoria), 
         call. = FALSE)
  }
  
  # Extract data
  llr_H1 <- base[[var_llr]][base[[var_categoria]] == "Misma persona"]
  llr_H2 <- base[[var_llr]][base[[var_categoria]] == "Diferente persona"]
  
  # Remove NAs
  llr_H1 <- llr_H1[!is.na(llr_H1)]
  llr_H2 <- llr_H2[!is.na(llr_H2)]
  
  # Handle problematic values
  llr_H1[llr_H1 <= 0] <- 1e-10
  llr_H2[llr_H2 <= 0] <- 1e-10
  llr_H1[is.infinite(llr_H1)] <- 1e10
  llr_H2[is.infinite(llr_H2)] <- 1e10
  
  # Calculate Cllr components
  n_ss <- length(llr_H1)
  n_ds <- length(llr_H2)
  
  Cllr_ss <- mean(log2(1 + 1/llr_H1))
  Cllr_ds <- mean(log2(1 + llr_H2))
  
  Cllr <- (Cllr_ss + Cllr_ds) / 2
  
  # Return extended results
  list(
    Cllr = round(Cllr, 4),
    Cllr_ss = round(Cllr_ss, 4),
    Cllr_ds = round(Cllr_ds, 4),
    n_ss = n_ss,
    n_ds = n_ds,
    Cllr_min = 0,
    Cllr_max = 1
  )
}