
# Output Helper Functions

create_output_directories <- function() {
  directories <- c(
    PATH_TABLES,
    PATH_PLOTS,
    here::here("outputs", "logs")
  )
  
  purrr::walk(
    directories,
    ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
  )
}


# Save Plots

save_plot <- function(
    plot, 
    filename,
    width = 10,
    height = 6,
    dpi = 300
    ) 
  {
  
  ggsave(
    filename = file.path(PATH_PLOTS, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi
  )
  
  message("✓ Plot saved: ", filename)
  
}


# Save Table

save_table <- function(data, filename) {
  readr::write_csv(
    data,
    file = file.path(PATH_TABLES, filename)
  )
  
  message("✓ Table saved: ", filename)
  
}



# Save R Objects

save_rds <- function(object, filename) {
  saveRDS(
    object,
    file = file.path(PATH_PROCESSED, filename)
  )
  
}



# Print Section Headers

print_section <- function(title) {
  
  cat("\n")
  
  cat(
    "=========================================\n"
  )
  
  cat(title)
  
  cat(
    "\n=========================================\n"
  )
  
}

