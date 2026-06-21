#' Calibrate Likelihood Ratios Using Logistic Regression
#'
#' @description
#' Calibrates likelihood ratios (LRs) using logistic regression to obtain 
#' calibrated posterior likelihood ratios. This function implements the 
#' method described in the forensic statistics literature for calibrating 
#' likelihood ratio systems.
#'
#' @details
#' The calibration process follows these steps:
#' \enumerate{
#'   \item The likelihood ratios are log10-transformed.
#'   \item A logistic regression model is fitted: 
#'         \eqn{logit(P(post = 1)) = \beta_0 + \beta_1 \cdot \log_{10}(LR)}
#'         where \code{post = 1} indicates same-source comparisons.
#'   \item The fitted model is used to compute calibrated posterior probabilities.
#'   \item These probabilities are converted to calibrated likelihood ratios 
#'         using the prior odds: \eqn{LR_{cal} = posterior\_odds / prior\_odds}
#' }
#'
#' The prior odds are computed as \eqn{n_{ss}/n_{ds}}, where \eqn{n_{ss}} and 
#' \eqn{n_{ds}} are the number of same-source and different-source comparisons, 
#' respectively.
#'
#' @param data A data frame containing the likelihood ratios and binary 
#'        classification. Must have at least 2 rows.
#' @param LR_col Character string specifying the column name with likelihood 
#'        ratios. Default: \code{"LR"}. The column must be numeric and contain 
#'        positive values.
#' @param binary_col Character string specifying the column name with binary 
#'        classification. Default: \code{"misma_persona_binaria"}. Must contain 
#'        \code{1} for same-source comparisons and \code{0} for different-source 
#'        comparisons.
#'
#' @return An object of class \code{"calibrated_LR"} containing:
#' \item{coefficients}{A numeric vector of length 2 with the intercept and 
#'        slope coefficients from the logistic regression.}
#' \item{prior.odds}{The prior odds computed as \eqn{n_{ss}/n_{ds}}.}
#' \item{fit}{The fitted \code{\link[stats]{glm}} object.}
#' \item{calibrated_LRs}{A numeric vector of calibrated likelihood ratios 
#'        for each input LR.}
#' \item{data}{A data frame with the original data and calibrated LRs.}
#' \item{call}{The matched call.}
#'
#' @note
#' A warning is issued if the logistic regression slope is negative, as this 
#' indicates potential issues with the data or the LR system's performance 
#' (i.e., larger LRs should correspond to same-source comparisons).
#'
#' @references
#' Puch-Solis, R., Roberts, P., & Pope, S. (2012). Assessing the weight of 
#' forensic evidence using logistic regression. \emph{Journal of the Royal 
#' Statistical Society: Series C (Applied Statistics)}, 61(5), 869-885.
#' \doi{10.1111/j.1467-9876.2012.01047.x}
#'
#' Ramos, D., & Gonzalez-Rodriguez, J. (2013). Reliable support: Measuring 
#' calibration of likelihood ratios. \emph{Forensic Science International}, 
#' 230(1-3), 156-163.
#'
#' @seealso
#' \code{\link[stats]{glm}} for logistic regression details.
#' \code{\link{print.calibrated_LR}} for printing method.
#' \code{\link{summary.calibrated_LR}} for summary method.
#' \code{\link{plot.calibrated_LR}} for plotting method.
#'
#' @examples
#' \donttest{
#' # Create example data
#' set.seed(123)
#' n_same <- 50
#' n_diff <- 50
#' 
#' example_data <- data.frame(
#'   LR = c(rlnorm(n_same, meanlog = 2, sdlog = 0.5),
#'          rlnorm(n_diff, meanlog = -1, sdlog = 0.3)),
#'   misma_persona_binaria = c(rep(1, n_same), rep(0, n_diff))
#' )
#' 
#' # Calibrate the LRs
#' result <- calibrate_LR(example_data)
#' 
#' # View results
#' print(result)
#' summary(result)
#' 
#' # Extract calibrated LRs
#' head(result$calibrated_LRs)
#' 
#' # Access model coefficients
#' result$coefficients
#' }
#'
#' @importFrom stats glm binomial coefficients
#' @importFrom utils head
#'
#' @export

calibrate_LR <- function(data, LR_col = "LR", binary_col = "misma_persona_binaria") {
  
  # ============================================================
  # 1. INPUT VALIDATION
  # ============================================================
  
  # Check if data is a data frame
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame", call. = FALSE)
  }
  
  # Check if data has at least 2 rows
  if (nrow(data) < 2) {
    stop("'data' must have at least 2 rows", call. = FALSE)
  }
  
  # Check if columns exist
  if (!LR_col %in% names(data)) {
    stop(sprintf("Column '%s' not found in data", LR_col), call. = FALSE)
  }
  
  if (!binary_col %in% names(data)) {
    stop(sprintf("Column '%s' not found in data", binary_col), call. = FALSE)
  }
  
  # Check if LR column is numeric
  if (!is.numeric(data[[LR_col]])) {
    stop(sprintf("Column '%s' must be numeric", LR_col), call. = FALSE)
  }
  
  # Check if binary column is numeric
  if (!is.numeric(data[[binary_col]])) {
    stop(sprintf("Column '%s' must be numeric", binary_col), call. = FALSE)
  }
  
  # Check if binary column contains only 0 and 1
  unique_values <- unique(na.omit(data[[binary_col]]))
  if (!all(unique_values %in% c(0, 1))) {
    stop(sprintf("Column '%s' must contain only 0 and 1 values", binary_col), 
         call. = FALSE)
  }
  
  # Check for NA values
  if (anyNA(data[[LR_col]])) {
    warning("NA values found in LR column. These rows will be removed.", 
            call. = FALSE)
    data <- data[!is.na(data[[LR_col]]), ]
  }
  
  if (anyNA(data[[binary_col]])) {
    warning("NA values found in binary column. These rows will be removed.", 
            call. = FALSE)
    data <- data[!is.na(data[[binary_col]]), ]
  }
  
  # Check if there are rows after removing NAs
  if (nrow(data) == 0) {
    stop("No valid rows remaining after removing NA values", call. = FALSE)
  }
  
  # ============================================================
  # 2. EXTRACT AND PREPARE DATA
  # ============================================================
  
  # Extract LR values by category
  LR.ss <- data[[LR_col]][data[[binary_col]] == 1]
  LR.ds <- data[[LR_col]][data[[binary_col]] == 0]
  
  # Check if both categories are present
  if (length(LR.ss) == 0) {
    stop("No same-source comparisons found (binary_col = 1)", call. = FALSE)
  }
  
  if (length(LR.ds) == 0) {
    stop("No different-source comparisons found (binary_col = 0)", call. = FALSE)
  }
  
  # Check for zero or negative LR values
  if (any(data[[LR_col]] <= 0, na.rm = TRUE)) {
    warning("LR values <= 0 found. These will be replaced with a small positive value (1e-10)", 
            call. = FALSE)
    data[[LR_col]][data[[LR_col]] <= 0] <- 1e-10
  }
  
  # Check for infinite values
  if (any(is.infinite(data[[LR_col]]))) {
    warning("Infinite LR values found. These will be replaced with 1e10", 
            call. = FALSE)
    data[[LR_col]][is.infinite(data[[LR_col]])] <- 1e10
  }
  
  # ============================================================
  # 3. PREPARE DATA FOR MODEL
  # ============================================================
  
  # Create data frame for modeling
  LR.ss.dataframe <- data.frame(lr = LR.ss, post = 1)
  LR.ds.dataframe <- data.frame(lr = LR.ds, post = 0)
  combined_data <- rbind(LR.ss.dataframe, LR.ds.dataframe)
  
  # Calculate sample sizes and prior odds
  n.ss <- length(LR.ss)
  n.ds <- length(LR.ds)
  prior.odds <- n.ss / n.ds
  
  # Calculate log10 of LR values
  combined_data$loglr <- log10(combined_data$lr)
  
  # Check for infinite log values
  if (any(is.infinite(combined_data$loglr))) {
    warning("Infinite log10 values detected. Check for extreme LR values.", 
            call. = FALSE)
  }
  
  # ============================================================
  # 4. FIT LOGISTIC REGRESSION
  # ============================================================
  
  # Try to fit the model
  fit <- tryCatch(
    stats::glm(post ~ loglr, 
               data = combined_data, 
               family = binomial(link = "logit")),
    error = function(e) {
      stop("Failed to fit logistic regression: ", e$message, call. = FALSE)
    }
  )
  
  # Extract coefficients
  coef <- stats::coefficients(fit)
  
  # Check slope direction
  if (coef[2] < 0) {
    warning("The logistic regression slope is negative. Check that LR values ",
            "are correctly ordered (larger for same-source comparisons) or ",
            "that your model is sufficiently performant",
            call. = FALSE)
  }
  
  # ============================================================
  # 5. CALCULATE CALIBRATED LRs
  # ============================================================
  
  # Calculate calibrated posterior LRs for all input LRs
  predictors <- log10(data[[LR_col]])
  
  # Handle potential numerical issues
  if (any(is.infinite(predictors))) {
    warning("Infinite values in predictors. Replacing with finite values.", 
            call. = FALSE)
    predictors[is.infinite(predictors) & predictors > 0] <- 308  # Max finite log10
    predictors[is.infinite(predictors) & predictors < 0] <- -308
  }
  
  # Calculate calibrated posterior ratio
  calibrated.posterior.ratio <- exp(predictors * coef[2] + coef[1])
  
  # Handle extreme values
  calibrated.posterior.ratio[calibrated.posterior.ratio == Inf] <- 1e308
  calibrated.posterior.ratio[calibrated.posterior.ratio == 0] <- 1e-308
  
  # Calculate posterior probabilities
  calibrated.posterior.probabilities <- calibrated.posterior.ratio / 
    (calibrated.posterior.ratio + 1)
  
  # Calculate calibrated posterior LRs
  calibrated.posterior.LRs <- (calibrated.posterior.probabilities / 
                                 (1 - calibrated.posterior.probabilities)) / prior.odds
  
  # Handle NA and infinite values in output
  calibrated.posterior.LRs[is.na(calibrated.posterior.LRs)] <- 1
  calibrated.posterior.LRs[!is.finite(calibrated.posterior.LRs)] <- 1e10
  
  # ============================================================
  # 6. PREPARE OUTPUT
  # ============================================================
  
  # Create output data frame
  output_data <- data.frame(
    original_LR = data[[LR_col]],
    binary = data[[binary_col]],
    calibrated_LR = calibrated.posterior.LRs,
    stringsAsFactors = FALSE
  )
  
  # Create output list
  out <- list(
    coefficients = coef,
    prior.odds = prior.odds,
    fit = fit,
    calibrated_LRs = calibrated.posterior.LRs,
    data = output_data,
    call = match.call()
  )
  
  # Assign class
  class(out) <- "calibrated_LR"
  
  return(out)
}

# ============================================================
# 7. S3 METHODS FOR CLASS "calibrated_LR"
# ============================================================

#' Print method for calibrated_LR objects
#'
#' @param x An object of class \code{"calibrated_LR"}.
#' @param ... Additional arguments passed to \code{\link{print}}.
#'
#' @return Invisibly returns the object \code{x}.
#' @export
#' @method print calibrated_LR

print.calibrated_LR <- function(x, ...) {
  cat("Calibrated Likelihood Ratios\n")
  cat("============================\n\n")
  cat("Call:", deparse(x$call), "\n\n")
  cat("Coefficients:\n")
  print(x$coefficients)
  cat("\nPrior odds (n_ss/n_ds):", round(x$prior.odds, 4), "\n")
  cat("Number of calibrated LRs:", length(x$calibrated_LRs), "\n")
  cat("\nSummary of calibrated LRs:\n")
  print(summary(x$calibrated_LRs))
  invisible(x)
}

#' Summary method for calibrated_LR objects
#'
#' @param object An object of class \code{"calibrated_LR"}.
#' @param ... Additional arguments passed to \code{\link{summary}}.
#'
#' @return Invisibly returns the object \code{object}.
#' @export
#' @method summary calibrated_LR

summary.calibrated_LR <- function(object, ...) {
  cat("Summary of Calibrated Likelihood Ratios\n")
  cat("======================================\n\n")
  cat("Model Fit:\n")
  print(summary(object$fit, ...))
  cat("\nCalibrated LRs:\n")
  cat("  Min:", min(object$calibrated_LRs), "\n")
  cat("  1st Qu:", quantile(object$calibrated_LRs, 0.25), "\n")
  cat("  Median:", median(object$calibrated_LRs), "\n")
  cat("  Mean:", mean(object$calibrated_LRs), "\n")
  cat("  3rd Qu:", quantile(object$calibrated_LRs, 0.75), "\n")
  cat("  Max:", max(object$calibrated_LRs), "\n")
  invisible(object)
}

#' Plot method for calibrated_LR objects
#'
#' @description
#' Creates diagnostic plots for calibrated likelihood ratios:
#' \itemize{
#'   \item Histogram of log10-transformed calibrated LRs
#'   \item Scatter plot of original vs calibrated LRs (log10 scale)
#' }
#'
#' @param x An object of class \code{"calibrated_LR"}.
#' @param ... Additional arguments passed to \code{\link{hist}} and 
#'        \code{\link{plot}}.
#'
#' @return Invisibly returns \code{NULL}.
#' @export
#' @method plot calibrated_LR

plot.calibrated_LR <- function(x, ...) {
  # Check if graphics are available
  if (!interactive() && !capabilities("X11") && .Platform$OS.type != "windows") {
    warning("No graphical display available", call. = FALSE)
    return(invisible(NULL))
  }
  
  # Set up plotting
  old_par <- graphics::par(mfrow = c(1, 2))
  on.exit(graphics::par(old_par))
  
  # Histogram of calibrated LRs (log10 scale)
  log10_cal_LR <- log10(x$calibrated_LRs)
  log10_cal_LR[!is.finite(log10_cal_LR)] <- NA
  
  if (all(is.na(log10_cal_LR))) {
    warning("No valid calibrated LRs for plotting", call. = FALSE)
    return(invisible(NULL))
  }
  
  graphics::hist(log10_cal_LR, 
                 main = "Distribution of Calibrated LRs",
                 xlab = expression(log[10]~"(Calibrated LR)"),
                 col = "lightblue",
                 border = "white",
                 ...)
  
  # Scatter plot of original vs calibrated LRs (log10 scale)
  log10_orig <- log10(x$data$original_LR)
  log10_cal <- log10(x$data$calibrated_LR)
  
  # Handle infinite values
  log10_orig[!is.finite(log10_orig)] <- NA
  log10_cal[!is.finite(log10_cal)] <- NA
  
  # Remove NAs for plotting
  valid_idx <- !is.na(log10_orig) & !is.na(log10_cal)
  
  if (sum(valid_idx) < 2) {
    warning("Not enough valid points for scatter plot", call. = FALSE)
    return(invisible(NULL))
  }
  
  graphics::plot(log10_orig[valid_idx], 
                 log10_cal[valid_idx],
                 main = "Original vs Calibrated LRs",
                 xlab = expression(log[10]~"(Original LR)"),
                 ylab = expression(log[10]~"(Calibrated LR)"),
                 pch = 20,
                 col = "darkblue",
                 ...)
  
  # Add identity line (y = x)
  graphics::abline(a = 0, b = 1, col = "red", lty = 2)
  
  invisible(NULL)
}