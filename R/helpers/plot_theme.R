

theme_retail <- function(){
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 11),
      legend.position = "bottom"
    )
}