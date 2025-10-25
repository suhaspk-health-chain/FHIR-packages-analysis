library(ggplot2)
theme_healthchain <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", color = "#f26d21", hjust = 0.5, size = base_size + 2),
      axis.title = element_text(color = "#ffffff"),
      axis.text  = element_text(color = "#dce6f1"),
      panel.background = element_rect(fill = "#0c223f", color = NA),
      plot.background  = element_rect(fill = "#0c223f", color = NA),
      legend.background = element_rect(fill = "#0c223f", color = NA),
      legend.text = element_text(color = "#dce6f1"),
      legend.title = element_text(color = "#ffffff", face = "bold"),
      strip.background = element_rect(fill = "#123358", color = NA),
      strip.text = element_text(color = "#ffffff", face = "bold"),
      panel.grid.major = element_line(color = "#1d3557", linewidth = 0.3),
      panel.grid.minor = element_blank()
    )
}
theme_set(theme_healthchain())
