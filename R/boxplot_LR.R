#' Boxplot for Likelihood Ratios by Category
#'
#' @description
#' Creates a boxplot visualization for likelihood ratios comparing same-source 
#' and different-source categories. The plot displays the distribution of 
#' log10-transformed likelihood ratios for both categories, with customizable 
#' aesthetics matching the Tippett plot style.
#'
#' @details
#' The function creates a boxplot with:
#' \itemize{
#'   \item Log10-transformed likelihood ratios on the y-axis
#'   \item Two categories: "Misma persona" (same-source) and "Diferente persona" (different-source)
#'   \item Points showing individual observations (jittered)
#'   \item Median values displayed with labels
#'   \item Consistent color scheme with Tippett plot
#' }
#'
#' @param base A data frame containing the data
#' @param var_llr Character string with the name of the column containing likelihood ratios
#' @param var_categoria Character string with the name of the column containing categories
#'        ("Misma persona" and "Diferente persona")
#' @param title Character string for the plot title. Default: "Distribución de LRs por Categoría"
#' @param show_points Logical indicating whether to show individual points. Default: TRUE
#' @param show_median Logical indicating whether to show median labels. Default: TRUE
#' @param grid_fondo Logical indicating whether to show background grid. Default: TRUE
#' @param y_label Character string for y-axis label. Default: expression(log[10](LR))
#' @param x_label Character string for x-axis label. Default: "Categoría"
#' @param point_alpha Numeric value for point transparency (0-1). Default: 0.5
#' @param point_size Numeric value for point size. Default: 1.5
#' @param notch Logical indicating whether to show notches on boxplots. Default: FALSE
#'
#' @return A ggplot2 object
#'
#' @examples
#' \donttest{
#' # Create example data
#' set.seed(123)
#' base <- data.frame(
#'   LR = c(rlnorm(50, meanlog = 2, sdlog = 0.5),
#'          rlnorm(50, meanlog = -1, sdlog = 0.3)),
#'   categoria = c(rep("Misma persona", 50), rep("Diferente persona", 50))
#' )
#' 
#' # Create boxplot
#' boxplot_LR(base, var_llr = "LR", var_categoria = "categoria")
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_jitter geom_point
#' @importFrom ggplot2 stat_summary scale_y_continuous scale_fill_manual
#' @importFrom ggplot2 labs theme_minimal theme element_text element_blank
#' @importFrom ggplot2 element_line coord_cartesian

boxplot_LR <- function(base, 
                       var_llr, 
                       var_categoria, 
                       title = "Distribución de LRs por Categoría",
                       show_points = TRUE,
                       show_median = TRUE,
                       grid_fondo = TRUE,
                       y_label = expression(log[10](LR)),
                       x_label = "Categoría",
                       point_alpha = 0.5,
                       point_size = 1.5,
                       notch = FALSE) {
  
  # ============================================================
  # 1. INPUT VALIDATION
  # ============================================================
  
  # Check if base is a data frame
  if (!is.data.frame(base)) {
    stop("'base' must be a data frame", call. = FALSE)
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
  
  # Check categories
  categories <- unique(base[[var_categoria]])
  required_cats <- c("Misma persona", "Diferente persona")
  if (!all(required_cats %in% categories)) {
    stop(sprintf("Categories must include '%s' and '%s'", 
                 required_cats[1], required_cats[2]), 
         call. = FALSE)
  }
  
  # ============================================================
  # 2. PREPARE DATA
  # ============================================================
  
  # Create a copy of the data with log10 transformation
  plot_data <- base[!is.na(base[[var_llr]]), ]
  plot_data <- plot_data[!is.na(plot_data[[var_categoria]]), ]
  
  if (nrow(plot_data) == 0) {
    stop("No valid data after removing NA values", call. = FALSE)
  }
  
  # Create log10 transformed column
  plot_data$log10_LR <- log10(plot_data[[var_llr]])
  
  # Check for infinite values
  if (any(is.infinite(plot_data$log10_LR))) {
    warning("Infinite log10 values detected. Check for zero or negative LR values.", 
            call. = FALSE)
    # Replace infinite values with finite values
    plot_data$log10_LR[is.infinite(plot_data$log10_LR) & plot_data$log10_LR > 0] <- 10
    plot_data$log10_LR[is.infinite(plot_data$log10_LR) & plot_data$log10_LR < 0] <- -10
  }
  
  # Ensure categories are factors with proper order
  plot_data[[var_categoria]] <- factor(plot_data[[var_categoria]], 
                                       levels = c("Diferente persona", "Misma persona"))
  
  # ============================================================
  # 3. CREATE BOXPLOT
  # ============================================================
  
  p <- ggplot2::ggplot(plot_data, 
                       ggplot2::aes(x = .data[[var_categoria]], 
                                    y = log10_LR, 
                                    fill = .data[[var_categoria]])) +
    
    # Boxplots
    ggplot2::geom_boxplot(notch = notch,
                          linewidth = 0.8,
                          outlier.shape = NA,
                          alpha = 0.7,
                          width = 0.6) +
    
    # Individual points (if requested)
    {if (show_points) ggplot2::geom_jitter(
      width = 0.2,
      height = 0,
      size = point_size,
      alpha = point_alpha,
      color = "gray30"
    )} +
    
    # Points for outliers (if points are shown)
    {if (!show_points) ggplot2::geom_point(
      data = function(d) d,
      position = ggplot2::position_jitter(width = 0.2, height = 0),
      size = point_size,
      alpha = point_alpha,
      color = "gray30"
    )} +
    
    # Median points and labels (if requested)
    {if (show_median) ggplot2::stat_summary(
      fun = median,
      geom = "point",
      size = 3,
      color = "#0B3D62",
      shape = 18
    )} +
    
    {if (show_median) ggplot2::stat_summary(
      fun = median,
      geom = "text",
      ggplot2::aes(label = sprintf("Mediana = %.2f", ..y..)),
      vjust = -0.5,
      size = 3.5,
      color = "#0B3D62"
    )} +
    
    # Color scales
    ggplot2::scale_fill_manual(
      name = NULL,
      values = c(
        "Misma persona" = "#0B3D62",
        "Diferente persona" = "#3498DB"
      ),
      labels = c(
        "Misma persona" = expression(bold("Misma persona")),
        "Diferente persona" = expression(bold("Diferente persona"))
      )
    ) +
    
    # Labels
    ggplot2::labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    
    # Theme
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        hjust = 0.5,
        color = "#0B3D62"
      ),
      legend.position = "none",
      axis.title.x = ggplot2::element_text(face = "bold"),
      axis.title.y = ggplot2::element_text(face = "bold"),
      axis.text = ggplot2::element_text(face = "bold")
    )
  
  # Background grid option
  if (!grid_fondo) {
    p <- p + ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "#7F9297", linewidth = 0.2, linetype = "dashed")
    )
  } else {
    p <- p + ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_line(color = "gray95", linewidth = 0.2)
    )
  }
  
  return(p)
}

# ============================================================
# 4. ADDITIONAL HELPER FUNCTION: Boxplot with Statistics
# ============================================================

#' Boxplot with Statistical Summary for Likelihood Ratios
#'
#' @description
#' Extended version of \code{\link{boxplot_LR}} that includes statistical 
#' summary information displayed on the plot.
#'
#' @param base A data frame containing the data
#' @param var_llr Character string with the name of the column containing likelihood ratios
#' @param var_categoria Character string with the name of the column containing categories
#' @param title Character string for the plot title. Default: "Distribución de LRs con Estadísticos"
#' @param show_stats Logical indicating whether to show statistical annotations. Default: TRUE
#' @param ... Additional arguments passed to \code{\link{boxplot_LR}}
#'
#' @return A ggplot2 object
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' base <- data.frame(
#'   LR = c(rlnorm(50, meanlog = 2, sdlog = 0.5),
#'          rlnorm(50, meanlog = -1, sdlog = 0.3)),
#'   categoria = c(rep("Misma persona", 50), rep("Diferente persona", 50))
#' )
#' 
#' boxplot_LR_stats(base, "LR", "categoria")
#' }
#'
#' @export

boxplot_LR_stats <- function(base, 
                             var_llr, 
                             var_categoria, 
                             title = "Distribución de LRs con Estadísticos",
                             show_stats = TRUE,
                             ...) {
  
  # Create base boxplot
  p <- boxplot_LR(base, var_llr, var_categoria, title = title, ...)
  
  if (show_stats) {
    # Prepare data for statistics
    plot_data <- base[!is.na(base[[var_llr]]), ]
    plot_data <- plot_data[!is.na(plot_data[[var_categoria]]), ]
    plot_data$log10_LR <- log10(plot_data[[var_llr]])
    
    # Calculate statistics for each category
    stats_data <- aggregate(log10_LR ~ plot_data[[var_categoria]], 
                            data = plot_data, 
                            FUN = function(x) {
                              c(mean = mean(x, na.rm = TRUE),
                                median = median(x, na.rm = TRUE),
                                sd = sd(x, na.rm = TRUE),
                                n = length(x))
                            })
    
    # Convert to data frame
    stats_df <- data.frame(
      categoria = stats_data[,1],
      mean = stats_data[,2][,1],
      median = stats_data[,2][,2],
      sd = stats_data[,2][,3],
      n = stats_data[,2][,4]
    )
    
    # Add statistical annotations
    for (i in 1:nrow(stats_df)) {
      cat <- stats_df$categoria[i]
      
      # Find position for annotation (slightly above the boxplot)
      y_pos <- max(plot_data$log10_LR[plot_data[[var_categoria]] == cat], na.rm = TRUE) * 1.05
      
      p <- p + ggplot2::annotate(
        "text",
        x = i,
        y = y_pos,
        label = sprintf("n = %d\nMedia = %.2f\nSD = %.2f", 
                        stats_df$n[i], 
                        stats_df$mean[i], 
                        stats_df$sd[i]),
        size = 3,
        color = ifelse(cat == "Misma persona", "#0B3D62", "#3498DB"),
        hjust = 0.5,
        vjust = 1
      )
    }
  }
  
  return(p)
}