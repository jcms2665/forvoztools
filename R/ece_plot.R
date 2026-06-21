#' ECE (Empirical Cross-Entropy) Plot with Cllr, Cllr_min and Cllr_cal
#'
#' Creates an Empirical Cross-Entropy plot for forensic likelihood ratios,
#' including Cllr, Cllr_min, and Cllr_cal calculations with customizable label positions.
#'
#' @param LR.ss Numeric vector of likelihood ratios for same-source comparisons
#' @param LR.ds Numeric vector of likelihood ratios for different-source comparisons
#' @param LR.cal.ss_1 Numeric vector of calibrated likelihood ratios (log10) for same-source
#' @param LR.cal.ds_1 Numeric vector of calibrated likelihood ratios (log10) for different-source
#' @param prior Numeric vector of prior probabilities. Default: seq(0.01, 0.99, length = 99)
#' @param title Character string for the plot title. Default: "Gráfica ECE"
#' @param zoom Numeric vector of length 2 for y-axis zoom limits. Default: NULL
#' @param pos_cllr Numeric vector of length 2 specifying (x, y) position for Cllr text.
#'        Default: NULL (uses automatic positioning)
#' @param pos_cllr_min Numeric vector of length 2 specifying (x, y) position for Cllr_min text.
#'        Default: NULL (uses automatic positioning)
#' @param pos_cllr_cal Numeric vector of length 2 specifying (x, y) position for Cllr_cal text.
#'        Default: NULL (uses automatic positioning)
#' @param grid_fondo Logical indicating whether to show background grid. Default: TRUE
#' @param y_label Character string for y-axis label. Default: "ECE"
#'
#' @return A list containing:
#'   \item{grafico}{A ggplot2 object}
#'   \item{tabla}{A data frame with Cllr, Cllr_min, and Cllr_cal values}
#'   \item{cllr}{Cllr value at prior = 0.5}
#'   \item{cllr_min}{Minimum Cllr value}
#'   \item{cllr_cal}{Calibrated Cllr value}
#'   \item{data}{The complete ECE data used for plotting}
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_line geom_hline geom_point annotate scale_color_manual
#' @importFrom ggplot2 scale_linetype_manual labs coord_cartesian theme_minimal theme
#' @importFrom ggplot2 element_text element_blank element_line

ece_plot <- function(LR.ss, LR.ds,
                     LR.cal.ss_1 = NULL,
                     LR.cal.ds_1 = NULL,
                     prior = seq(from = 0.01, to = 0.99, length = 99),
                     title = "Gráfica ECE",
                     zoom = NULL,
                     pos_cllr = NULL,
                     pos_cllr_min = NULL,
                     pos_cllr_cal = NULL,
                     grid_fondo = TRUE,
                     y_label = "ECE") {
  
  # ============================================================
  # 1. INPUT VALIDATION
  # ============================================================
  if (!is.numeric(LR.ss) || !is.numeric(LR.ds)) {
    stop("LR.ss and LR.ds must be numeric vectors")
  }
  
  if (length(LR.ss) == 0 || length(LR.ds) == 0) {
    stop("LR vectors cannot be empty")
  }
  
  n.ss <- length(LR.ss)
  n.ds <- length(LR.ds)
  n.prior <- length(prior)
  
  odds <- prior / (1 - prior)
  prior_log_odds <- log(prior / (1 - prior))
  
  # Null system (LR = 1)
  LR.null.ss <- rep(1, n.ss)
  LR.null.ds <- rep(1, n.ds)
  
  # Initialize
  ECE <- numeric(n.prior)
  ECE.null <- numeric(n.prior)
  ECE.cal <- rep(NA_real_, n.prior)
  
  # Convert calibrated LR from log10 to natural scale if not NULL
  if (!is.null(LR.cal.ss_1) && !is.null(LR.cal.ds_1)) {
    if (length(LR.cal.ss_1) != n.ss || length(LR.cal.ds_1) != n.ds) {
      stop("Calibrated LR vectors must have same length as original LR vectors")
    }
    LR.cal.ss <- 10^(LR.cal.ss_1)
    LR.cal.ds <- 10^(LR.cal.ds_1)
    has_calibrated <- TRUE
  } else {
    LR.cal.ss <- NULL
    LR.cal.ds <- NULL
    has_calibrated <- FALSE
  }
  
  # ============================================================
  # 2. CALCULATE ECE FOR EACH PRIOR
  # ============================================================
  for (ctr in seq_len(n.prior)) {
    
    bit.1 <- prior[ctr] / n.ss
    bit.3 <- (1 - prior[ctr]) / n.ds
    
    # --- Original LRs ---
    bit.2a <- log2(1 + (1 / (LR.ss * odds[ctr])))
    bit.4a <- log2(1 + (LR.ds * odds[ctr]))
    
    # --- Null LRs (LR = 1) ---
    bit.2b <- log2(1 + (1 / (LR.null.ss * odds[ctr])))
    bit.4b <- log2(1 + (LR.null.ds * odds[ctr]))
    
    # --- Calibrated LRs (if available) ---
    if (has_calibrated) {
      bit.2c <- log2(1 + (1 / (LR.cal.ss * odds[ctr])))
      bit.4c <- log2(1 + (LR.cal.ds * odds[ctr]))
      ECE.cal[ctr] <- (bit.1 * sum(bit.2c)) + (bit.3 * sum(bit.4c))
    }
    
    # --- ECE ---
    ECE[ctr] <- (bit.1 * sum(bit.2a)) + (bit.3 * sum(bit.4a))
    ECE.null[ctr] <- (bit.1 * sum(bit.2b)) + (bit.3 * sum(bit.4b))
  }
  
  # ============================================================
  # 3. CALCULATE Cllr VALUES (at prior = 0.5, log odds = 0)
  # ============================================================
  idx_cero <- which.min(abs(prior - 0.5))
  cllr <- ECE[idx_cero]
  cllr_min <- if (has_calibrated) ECE.cal[idx_cero] else NA_real_
  cllr_cal <- cllr - cllr_min
  
  # ============================================================
  # 4. PREPARE DATA FOR PLOTTING
  # ============================================================
  plot_data <- data.frame(
    prior = prior,
    prior_log_odds = prior_log_odds,
    ECE = ECE,
    ECE_null = ECE.null,
    ECE_cal = ECE.cal
  )
  
  # Calculate y-axis limits
  y_max <- max(c(ECE, ECE.null, ECE.cal), na.rm = TRUE)
  if (is.null(zoom)) {
    y_lim <- c(0, y_max * 1.1)
  } else {
    y_lim <- zoom
  }
  
  # ============================================================
  # 5. CREATE PLOT
  # ============================================================
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = prior_log_odds)) +
    
    # --- Lines ---
    # Null model (reference)
    ggplot2::geom_line(
      ggplot2::aes(y = ECE_null, color = "Null", linetype = "Null"),
      linewidth = 1.2
    ) +
    # Original ECE
    ggplot2::geom_line(
      ggplot2::aes(y = ECE, color = "Original", linetype = "Original"),
      linewidth = 1.2
    ) +
    # Calibrated ECE (if available)
    {if (has_calibrated) ggplot2::geom_line(
      ggplot2::aes(y = ECE_cal, color = "Calibrated", linetype = "Calibrated"),
      linewidth = 1.2,
      na.rm = TRUE
    )} +
    
    # --- Horizontal line at y = 0 ---
    #ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
    
    # --- Vertical line at log odds = 0 (prior = 0.5) ---
    ggplot2::geom_vline(xintercept = 0, color = "black", linewidth = 0.3, linetype = "dotted") +
    {if (grid_fondo) ggplot2::geom_vline(
      xintercept = 0,
      color = "black",
      linewidth = 0.3,
      linetype = "dotted"
    )} +
    
    # --- Points for Cllr and Cllr_min ---
    ggplot2::geom_point(
      ggplot2::aes(x = 0, y = cllr),
      color = "#0B3D62", size = 1
    ) +
    {if (has_calibrated && !is.na(cllr_min)) ggplot2::geom_point(
      ggplot2::aes(x = 0, y = cllr_min),
      color = "#3498DB", size = 1
    )} +
    
    # --- Color and linetype scales ---
    ggplot2::scale_color_manual(
      name = NULL,
      values = c(
        "Null" = "#7F9297",
        "Original" = "#0B3D62",
        "Calibrated" = "#3498DB"
      ),
      labels = c(
        "Null" = expression(bold("Null")),
        "Original" = expression(bold("Original")),
        "Calibrated" = expression(bold("Calibrated"))
      )
    ) +
    ggplot2::scale_linetype_manual(
      name = NULL,
      values = c(
        "Null" = "dashed",
        "Original" = "solid",
        "Calibrated" = "solid"
      ),
      labels = c(
        "Null" = expression(bold("Null")),
        "Original" = expression(bold("Original")),
        "Calibrated" = expression(bold("Calibrated"))
      )
    ) +
    
    # --- Labels ---
    ggplot2::labs(
      x = expression(bold(log)~bold(odds)),
      y = y_label,
      title = title
    )
  
  # ============================================================
  # 6. ADD ANNOTATIONS WITH CUSTOMIZABLE POSITIONS
  # ============================================================
  
  # Get x-range for positioning
  x_range <- range(prior_log_odds)
  x_min <- x_range[1]
  x_max <- x_range[2]
  
  # --- Cllr annotation ---
  if (is.null(pos_cllr)) {
    # Automatic positioning: to the right of the point
    pos_cllr <- c(0.3, cllr + y_max * 0.02)
  }
  
  # Validate pos_cllr
  if (!is.numeric(pos_cllr) || length(pos_cllr) != 2) {
    warning("'pos_cllr' must be a numeric vector of length 2. Using default.")
    pos_cllr <- c(0.3, cllr + y_max * 0.02)
  }
  
  p <- p + ggplot2::annotate(
    "text",
    x = pos_cllr[1],
    y = pos_cllr[2],
    label = sprintf("bold(Cllr) == %.3f", cllr),
    parse = TRUE,
    hjust = 0,
    size = 3.5,
    color = "#0B3D62"
  ) +
    # Horizontal line from annotation to vertical line at x=0
    ggplot2::annotate(
      "segment",
      x = pos_cllr[1] - 0.05,
      xend = 0,
      y = pos_cllr[2],
      yend = cllr,
      color = "#0B3D62",
      linewidth = 0.3,
      linetype = "dashed"
    )
  
  # --- Cllr_min and Cllr_cal annotations (if calibrated data available) ---
  if (has_calibrated && !is.na(cllr_min)) {
    
    # Cllr_min annotation
    if (is.null(pos_cllr_min)) {
      # Automatic positioning: to the right of the point
      pos_cllr_min <- c(0.3, cllr_min - y_max * 0.02)
    }
    
    # Validate pos_cllr_min
    if (!is.numeric(pos_cllr_min) || length(pos_cllr_min) != 2) {
      warning("'pos_cllr_min' must be a numeric vector of length 2. Using default.")
      pos_cllr_min <- c(0.3, cllr_min - y_max * 0.02)
    }
    
    p <- p + ggplot2::annotate(
      "text",
      x = pos_cllr_min[1],
      y = pos_cllr_min[2],
      label = sprintf("bold(Cllr[min]) == %.3f", cllr_min),
      parse = TRUE,
      hjust = 0,
      size = 3.5,
      color = "#3498DB"
    ) +
      # Horizontal line from annotation to vertical line at x=0
      ggplot2::annotate(
        "segment",
        x = pos_cllr_min[1] - 0.05,
        xend = 0,
        y = pos_cllr_min[2],
        yend = cllr_min,
        color = "#3498DB",
        linewidth = 0.3,
        linetype = "dashed"
      ) +
      
      # Vertical line with two horizontal caps (no arrows)
      # Bottom cap
      #ggplot2::annotate(
      #  "segment",
      #  x = 0.15 - 0.03,
      #  xend = 0.15 + 0.03,
      #  y = cllr_min,
      #  yend = cllr_min,
      #  color = "#7F9297",
      #  linewidth = 0.5
      #) +
      # Top cap
      #ggplot2::annotate(
      #  "segment",
      #  x = 0.15 - 0.03,
      #  xend = 0.15 + 0.03,
      #  y = cllr,
      #  yend = cllr,
      #  color = "#7F9297",
      #  linewidth = 0.5
      #) +
      # Vertical line connecting caps
      #ggplot2::annotate(
      #  "segment",
      #  x = 0.15,
      #  xend = 0.15,
      #  y = cllr_min,
      #  yend = cllr,
      #  color = "#7F9297",
      #  linewidth = 0.5
      #)
    
    # Cllr_cal annotation (positioned between Cllr and Cllr_min)
    if (is.null(pos_cllr_cal)) {
      # Automatic positioning: to the right of the vertical line
      pos_cllr_cal <- c(0.3, (cllr + cllr_min) / 2)
    }
    
    # Validate pos_cllr_cal
    if (!is.numeric(pos_cllr_cal) || length(pos_cllr_cal) != 2) {
      warning("'pos_cllr_cal' must be a numeric vector of length 2. Using default.")
      pos_cllr_cal <- c(0.3, (cllr + cllr_min) / 2)
    }
    
    p <- p + ggplot2::annotate(
      "text",
      x = pos_cllr_cal[1],
      y = pos_cllr_cal[2],
      label = sprintf("bold(Cllr[cal]) == %.3f", cllr_cal),
      parse = TRUE,
      hjust = 0,
      size = 3.5,
      color = "#D59F0F"
    ) #+
      # Horizontal line from Cllr_cal annotation to vertical line
      #ggplot2::annotate(
      #  "segment",
      #  x = pos_cllr_cal[1] - 0.05,
      #  xend = 0.15,
      #  y = pos_cllr_cal[2],
      #  yend = pos_cllr_cal[2],
      #  color = "#7F9297",
      #  linewidth = 0.3,
      #  linetype = "dashed"
      #)
  }
  
  # ============================================================
  # 7. COORDINATES AND ZOOM
  # ============================================================
  p <- p + ggplot2::coord_cartesian(
    xlim = range(prior_log_odds),
    ylim = y_lim
  )
  
  # ============================================================
  # 8. THEME
  # ============================================================
  p <- p + ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        hjust = 0.5,
        color = "#0B3D62"
      ),
      legend.text = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      legend.box = "horizontal",
      axis.title.x = ggplot2::element_text(face = "bold"),
      axis.title.y = ggplot2::element_text(face = "bold")
    )
  
  # --- Background grid option ---
  if (!grid_fondo) {
    p <- p + ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "#7F9297", linewidth = 0.2, linetype = "dashed")
    )
  }
  
  # ============================================================
  # 9. RESULTS TABLE
  # ============================================================
  tabla_resultados <- data.frame(
    Cllr = round(cllr, 4),
    Cllr_min = round(cllr_min, 4),
    Cllr_cal = round(cllr_cal, 4),
    check.names = FALSE
  )
  
  # ============================================================
  # 10. RETURN
  # ============================================================
  return(list(
    grafico = p,
    tabla = tabla_resultados,
    cllr = cllr,
    cllr_min = cllr_min,
    cllr_cal = cllr_cal,
    data = plot_data
  ))
}


#ec_r <- ece_plot(
#  LR.ss       = datos_fusion$LR_fusion_bal[datos_fusion$clase_binaria == 1],
#  LR.ds       = datos_fusion$LR_fusion_bal[datos_fusion$clase_binaria == 0],
#  LR.cal.ss_1 = log10(datos_fusion$LR_fusion_calibrado[datos_fusion$clase_binaria == 1]),     
#  LR.cal.ds_1 = log10(datos_fusion$LR_fusion_calibrado[datos_fusion$clase_binaria == 0]),
#  title = "Análisis de Entropía Cruzada",
#  pos_cllr = c(2.5, 2),      # Posición de Cllr
#  pos_cllr_min = c(.5, 0.25),  # Posición de Cllr_min
#  pos_cllr_cal = c(.6, 1.3), # Posición de Cllr_cal
#  grid_fondo = FALSE,
#  y_label = "Empirical C Entropy"
#)

#ec_r$grafico
#ec_r$tabla

