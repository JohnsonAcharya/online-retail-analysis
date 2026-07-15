

save_plot <- function(plot, filename) {
  
  if (!dir.exists(PATH_PLOTS)) {
    dir.create(PATH_PLOTS, recursive = TRUE)
  }
  
  ggsave(
    filename = file.path(PATH_PLOTS, filename),
    plot = plot,
    width = 10,
    height = 6,
    dpi = 300
  )
}


save_table <- function(data, filename) {
  readr::write_csv(
    data,
    file.path(PATH_TABLES, filename)
  )
}