# ============================================================
# Project : Online Retail Analysis
# Script  : load_data.R
# Purpose : Load Feature Dataset
# Author  : Johnson
# ============================================================

retail_clean_data <- readRDS(
  file = here::here("data", 
                    "processed", 
                    "retail_clean.rds" 
  )
)


retail_features <- readRDS(
  file = here::here("data", 
                    "features", 
                    "retail_features.rds" 
                    )
)