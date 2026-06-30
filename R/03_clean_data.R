# ============================================================
# Project : Online Retail Analysis
# Script  : 03_clean_data.R
# Purpose : Clean retail dataset
# Author  : Johnson
# ============================================================

# Load packages 

library(tidyverse)
library(janitor)
library(here)


# Import data

retail_raw <- readRDS(
  here("data", "rds", "retail_raw.rds")
)

# Create a working copy

retail_clean <- retail_raw


# Step 1 - Standardize column names

retail_clean <- retail_clean |> 
  clean_names()

names(retail_clean)


# Step 2 - Remove exact duplicate rows

row_before <- nrow(retail_clean)


retail_clean <- retail_clean |> 
  distinct()

row_after <- nrow(retail_clean)


cat(
  "Duplicate rows removed:",
    row_before - row_after,
    "\n"
  )


# Step 3 - Validate data types

## Check structure
glimpse(retail_clean)


# Step 4 - Save cleaned dataset

saveRDS(
  retail_clean,
  here("data", "processed", "retail_clean.rds")
)


## Completion message

cat("\n===================================\n")
cat("Cleaning Completed\n")
cat("\n===================================\n")

cat("Rows:", nrow(retail_clean), "\n")
cat("Cols:", ncol(retail_clean), "\n")

message("✓ Clean dataset saved successfully.")