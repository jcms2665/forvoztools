#' Plot Empirical Cross-Entropy (ECE) Curve
#'
#' @description
#' Creates a visualization of the empirical cross-entropy curve, showing the
#' performance of likelihood ratio systems across different prior probabilities.
#'
#' @param x An object of class 'ece' returned by the \code{ece_funcional} function.
#' @param cllr Numeric, optional. Custom Cllr value to display. If NULL, uses
#'   value from x.
#' @param cllr_min Numeric, optional. Custom minimum Cllr value to display. If
#'   NULL, uses value from x.
#' @param titulo Character. Title for the plot. Default is "Gráfico de entropía
#'   cruzada (ECE)".
#' @param ... Additional arguments passed to ggplot2.
#'
#' @return A ggplot object.
#'
#' @import ggplot2
#' @export
plot_ece_funcional <- function(x, cllr = NULL, cllr_min = NULL, 
                               titulo = "Gráfico de entropía cruzada (ECE)", ...) {
  
  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting. Please install it.")
  }
  
  #--------------------------------------------------
  # 1. Extract data
  #--------------------------------------------------
  prior_vals    <- x$prior
  ece_orig_vals <- x$ece
  ece_cal_vals  <- x$ece.cal
  ece_ref_vals  <- x$ece.null
  
  prior_log_odds <- log10(prior_vals / (1 - prior_vals))
  
  #--------------------------------------------------
  # 2. Calculate Cllr and Cllr_min
  #--------------------------------------------------
  idx_cero <- which.min(abs(prior_vals - 0.5))
  
  if (is.null(cllr)) {
    cllr <- ece_orig_vals[idx_cero]
  }
  
  if (is.null(cllr_min)) {
    cllr_min <- if (!is.null(ece_cal_vals) && !all(is.na(ece_cal_vals))) {
      ece_cal_vals[idx_cero]
    } else {
      NA_real_
    }
  }
  
  #--------------------------------------------------
  # 3. Data frame
  #--------------------------------------------------
  df_ece <- data.frame(
    prior_log_odds = prior_log_odds,
    ece_orig = ece_orig_vals,
    ece_cal  = ece_cal_vals,
    ece_ref  = ece_ref_vals
  )
  
  #--------------------------------------------------
  # 4. Limits
  #--------------------------------------------------
  y_max <- max(c(df_ece$ece_orig, df_ece$ece_cal, df_ece$ece_ref), na.rm = TRUE)
  y_lim <- max(1, ceiling(y_max * 10) / 10)
  
  #--------------------------------------------------
  # 5. Base plot
  #--------------------------------------------------
  p <- ggplot2::ggplot(df_ece, ggplot2::aes(x = prior_log_odds)) +
    
    # Curves
    ggplot2::geom_line(ggplot2::aes(y = ece_orig, color = "LR original"),
                       linewidth = 1.2) +
    
    ggplot2::geom_line(ggplot2::aes(y = ece_cal, color = "LR calibrado"),
                       linewidth = 1.2, linetype = "dashed", na.rm = TRUE) +
    
    ggplot2::geom_line(ggplot2::aes(y = ece_ref, color = "Referencia"),
                       linewidth = 0.9, linetype = "dotted") +
    
    # Vertical line at 0
    ggplot2::geom_vline(xintercept = 0, linetype = "dotdash", color = "black") +
    
    #--------------------------------------------------
  # 6. Cllr labels with arrows
  #--------------------------------------------------
  # Cllr label
  ggplot2::annotate("text",
                    x = min(df_ece$prior_log_odds) * 0.8,
                    y = cllr,
                    label = sprintf("Cllr = %.2f", cllr),
                    hjust = 0,
                    size = 3.5,
                    color = "gray30") +
    
    # Arrow from Cllr label
    ggplot2::annotate("segment",
                      x = min(df_ece$prior_log_odds) * 0.5,
                      xend = 0,
                      y = cllr,
                      yend = cllr,
                      arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "cm"), type = "closed"),
                      color = "#7F9297",
                      linewidth = 0.4) +
    
    # Cllr[min] label
    ggplot2::annotate("text",
                      x = min(df_ece$prior_log_odds) * 0.95,
                      y = cllr_min,
                      label = sprintf("Cllr[%s] = %.2f", "min", cllr_min),
                      hjust = 0,
                      size = 3.5,
                      color = "gray30") +
    
    # Arrow from Cllr[min] label
    ggplot2::annotate("segment",
                      x = min(df_ece$prior_log_odds) * 0.95 + 0.75,
                      xend = 0,
                      y = cllr_min,
                      yend = cllr_min,
                      arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "cm"), type = "closed"),
                      color = "#7F9297",
                      linewidth = 0.4) +
    
    #--------------------------------------------------
  # 7. Scales
  #--------------------------------------------------
  ggplot2::scale_color_manual(
    values = c(
      "LR original"  = "#0B3D62",
      "LR calibrado" = "#D59F0F",
      "Referencia"   = "#7F9297"
    )
  ) +
    
    ggplot2::scale_y_continuous(
      limits = c(0, y_lim),
      breaks = seq(0, y_lim, 0.1)
    ) +
    
    ggplot2::labs(
      title = titulo,
      x = expression("Prior log"[10]*"(odds)"),
      y = "Empirical cross-entropy",
      color = NULL
    ) +
    
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = "gray80", linetype = "dotted"),
      panel.grid.minor = ggplot2::element_line(color = "gray90", linetype = "dotted"),
      legend.position = c(0.85, 0.8),
      legend.background = ggplot2::element_rect(fill = "white", color = "black"),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )
  
  return(p)
}



