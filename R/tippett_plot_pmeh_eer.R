#' Tippett Plot with PMEH and EER
#'
#' Creates a Tippett plot (cumulative proportion plot) for forensic likelihood ratios,
#' including Pooled Misclassification Error Equivalent (PMEH) and Equal Error Rate (EER)
#' calculations.
#'
#' @param base A data frame containing the data
#' @param var_llr Character string with the name of the column containing likelihood ratios
#' @param var_categoria Character string with the name of the column containing categories
#'        ("Misma persona" and "Diferente persona")
#' @param title Character string for the plot title. Default: "Gráfica Tippett"
#' @param zoom Numeric vector of length 2 for x-axis zoom limits. Default: NULL
#' @param x_pos Numeric vector of length 2 specifying (x, y) position for PMEH text. 
#'        Default: c(0.05, 0) - uses x_pos[1] for x and x_pos[2] for y offset
#' @param x_pos_eer Numeric vector of length 2 specifying (x, y) position for EER text.
#'        If NULL, uses eer_log10 for x and eer for y. Default: NULL
#' @param grid_fondo Logical indicating whether to show background grid. Default: TRUE
#' @param y_label Character string for y-axis label. Default: "Proporción acumulada"
#'
#' @return A list containing:
#'   \item{grafico}{A ggplot2 object}
#'   \item{tabla}{A data frame with PMEH1, PMEH0 and EER in percentages}
#'   \item{PMEH1}{Pooled Misclassification Error for same-source comparisons}
#'   \item{PMEH0}{Pooled Misclassification Error for different-source comparisons}
#'   \item{EER}{Equal Error Rate}
#'
#' @export

tippett_plot_pmeh_eer <- function(base, 
                                  var_llr, 
                                  var_categoria, 
                                  title = "Gráfica Tippett", 
                                  zoom = NULL,
                                  x_pos = c(0.05, 0),
                                  x_pos_eer = NULL,
                                  grid_fondo = TRUE,
                                  y_label = "Proporción acumulada") {
  
  if (!is.data.frame(base)) {
    stop("'base' must be a data frame")
  }
  
  if (!all(c(var_llr, var_categoria) %in% names(base))) {
    stop("The specified columns do not exist in the database")
  }
  
  if (!is.numeric(x_pos) || length(x_pos) != 2) {
    warning("'x_pos' must be a numeric vector of length 2. Using default c(0.05, 0)")
    x_pos <- c(0.05, 0)
  }
  
  categories <- unique(base[[var_categoria]])
  required_cats <- c("Misma persona", "Diferente persona")
  if (!all(required_cats %in% categories)) {
    stop("Categories must include 'Misma persona' and 'Diferente persona'")
  }
  
  llr_same <- base[[var_llr]][base[[var_categoria]] == "Misma persona"]
  llr_diff <- base[[var_llr]][base[[var_categoria]] == "Diferente persona"]
  
  llr_same <- llr_same[!is.na(llr_same)]
  llr_diff <- llr_diff[!is.na(llr_diff)]
  
  if (length(llr_same) == 0 || length(llr_diff) == 0) {
    stop("Insufficient data after removing NA values")
  }
  
  ss_log10 <- sort(llr_same / log(10))
  ds_log10 <- sort(llr_diff / log(10))
  
  prop_ss <- seq(0, 1, length.out = length(ss_log10))
  prop_ds <- seq(1, 0, length.out = length(ds_log10))
  
  interpolate_at_zero <- function(x, y) {
    if (length(x) == 0 || length(y) == 0 || length(x) != length(y)) {
      return(NA_real_)
    }
    
    if (any(x == 0)) {
      return(y[x == 0][1])
    }
    
    idx_neg <- which(x < 0)
    idx_pos <- which(x > 0)
    
    if (length(idx_neg) == 0 || length(idx_pos) == 0) {
      warning("No points around 0 for interpolation")
      return(NA_real_)
    }
    
    x1 <- x[max(idx_neg)]
    x2 <- x[min(idx_pos)]
    y1 <- y[max(idx_neg)]
    y2 <- y[min(idx_pos)]
    
    y0 <- stats::approx(c(x1, x2), c(y1, y2), xout = 0)$y
    return(y0)
  }
  
  PMEH1 <- interpolate_at_zero(ss_log10, prop_ss)
  PMEH0 <- interpolate_at_zero(ds_log10, prop_ds)
  
  all_scores <- c(llr_same, llr_diff)
  thresholds <- unique(sort(all_scores))
  
  far <- vapply(thresholds, function(u) {
    mean(llr_diff >= u, na.rm = TRUE)
  }, numeric(1))
  
  frr <- vapply(thresholds, function(u) {
    mean(llr_same < u, na.rm = TRUE)
  }, numeric(1))
  
  idx_eer <- which.min(abs(far - frr))
  eer <- mean(c(far[idx_eer], frr[idx_eer]))
  umbral_eer <- thresholds[idx_eer]
  eer_log10 <- umbral_eer / log(10)
  
  if (is.null(x_pos_eer)) {
    x_pos_eer <- c(eer_log10, eer)
  } else if (!is.numeric(x_pos_eer) || length(x_pos_eer) != 2) {
    warning("'x_pos_eer' must be a numeric vector of length 2. Using default position")
    x_pos_eer <- c(eer_log10, eer)
  }
  
  plot_data <- rbind(
    data.frame(
      LRs = ss_log10,
      Proportion = prop_ss,
      Type = "SS log10 (LR)"
    ),
    data.frame(
      LRs = ds_log10,
      Proportion = prop_ds,
      Type = "DS log10 (LR)"
    )
  )
  
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = LRs, y = Proportion, color = Type, linetype = Type)
  ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_vline(xintercept = 0, color = "black", linewidth = 0.3) +
    
    ggplot2::geom_point(
      ggplot2::aes(x = 0, y = PMEH1),
      color = "#0B3D62", size = 2
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = 0, y = PMEH0),
      color = "#3498DB", size = 2
    ) +
    
    ggplot2::annotate(
      "text",
      x = x_pos[1],
      y = PMEH1 + x_pos[2],
      label = sprintf("bold('PME(H'[1]*')') == %.2f*'%%'", PMEH1 * 100),
      parse = TRUE,
      size = 3,
      hjust = 0,
      color = "#0B3D62"
    ) +
    ggplot2::annotate(
      "text",
      x = x_pos[1],
      y = PMEH0 + x_pos[2],
      label = sprintf("bold('PME(H'[0]*')') == %.2f*'%%'", PMEH0 * 100),
      parse = TRUE,
      size = 3,
      hjust = 0,
      color = "#3498DB"
    ) +
    
    ggplot2::geom_point(
      ggplot2::aes(x = eer_log10, y = eer),
      color = "#D59F0F", size = 3
    ) +
    ggplot2::annotate(
      "text",
      x = x_pos_eer[1],
      y = x_pos_eer[2],
      label = sprintf("bold(EER) == %.2f*'%%'", eer * 100),
      parse = TRUE,
      hjust = 0.5,
      vjust = 0.5,
      size = 3.5,
      color = "#D59F0F"
    ) +
    
    ggplot2::scale_color_manual(
      name = NULL,
      values = c(
        "SS log10 (LR)" = "#0B3D62",
        "DS log10 (LR)" = "#3498DB"
      ),
      labels = c(
        "SS log10 (LR)" = expression(bold(SS)~bold(log[10])~bold("(LR)")),
        "DS log10 (LR)" = expression(bold(DS)~bold(log[10])~bold("(LR)"))
      )
    ) +
    ggplot2::scale_linetype_manual(
      name = NULL,
      values = c(
        "SS log10 (LR)" = "solid",
        "DS log10 (LR)" = "dashed"
      ),
      labels = c(
        "SS log10 (LR)" = expression(bold(SS)~bold(log[10])~bold("(LR)")),
        "DS log10 (LR)" = expression(bold(DS)~bold(log[10])~bold("(LR)"))
      )
    ) +
    
    ggplot2::labs(
      x = expression(log[10](LR)),
      y = y_label,
      title = title
    ) +
    
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold", 
        hjust = 0.5, 
        color = "#0B3D62"
      ),
      legend.text = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      legend.box = "horizontal"
    )
  
  if (!is.null(zoom)) {
    if (!is.numeric(zoom) || length(zoom) != 2) {
      warning("'zoom' must be a numeric vector of length 2. Ignoring.")
    } else {
      p <- p + ggplot2::coord_cartesian(xlim = zoom, ylim = c(0, 1))
    }
  }
  
  if (!grid_fondo) {
    p <- p + ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "#7F9297", linewidth = 0.2, linetype = "dashed")
    )
  }
  
  tabla_resultados <- data.frame(
    `PME(H1)` = round(PMEH1 * 100, 1),
    `PME(H0)` = round(PMEH0 * 100, 1),
    EER      = round(eer * 100, 1),
    check.names = FALSE
  )

  return(list(
    grafico = p,
    tabla   = tabla_resultados,
    PMEH1   = PMEH1,
    PMEH0   = PMEH0,
    EER     = eer
  ))
}

#datos_fusion=original_1

#r=tippett_plot_pmeh_eer(
#  base          = datos_fusion[original_1$etiqueta_label == "Ambos IA", ],
#  title = "Tippett Ambos IA", 
#  var_llr       = "LLR_wavlm",
#  var_categoria = "misma_persona",
#  zoom          = c(-0.2, 0.2),
#  x_pos = c(0.05, 0.02),  
#  x_pos_eer = c(-0.05, 0.25),  
#  grid_fondo = FALSE,
#  y_label = "Cumulative proportion"
#)

#r$grafico
#r$tabla
