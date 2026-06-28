# =====================================================
# Project : Online Retail Analysis
# Script  : 01_import.R
# Purpose : Import and validate raw retail dataset
# Author  : Johnson
# Date    : 2026-06-29
# =====================================================


# =====================================================
# Load Required Packages
# =====================================================
library(tidyverse)
library(readxl)
library(janitor)
library(here)
library(skimr)

# =====================================================
# Define File Path
# =====================================================

data_path <- here::here(
  "data",
  "raw",
  "Online_Retail.xlsx"
)


# =====================================================
# Check File Exists
# =====================================================

if(!file.exists(data_path)) {
  stop(
    "Dataset not found.\n",
    "Expected location: data/raw/Online_Retail.xlsx")
}


# =====================================================
# Import Dataset
# =====================================================

retail_raw <- read_excel(data_path)


# =====================================================
# Initial Data Validation
# =====================================================

# Dimensions

cat("\n--- Dataset Dimensions ---\n")
cat("Number of rows    :", nrow(retail_raw), "\n")
cat("Number of columns :", ncol(retail_raw), "\n")


# Column names

cat("\nColumn Names:\n")
print(names(retail_raw))


# Check duplicate column names

anyDuplicated(names(retail_raw))


# Data Preview

cat("First Six Rows:\n")
print(head(retail_raw))

cat("Last Six Rows:\n")
print(tail(retail_raw))


# 3. Structure check (data types)

cat("\n---Dataset Structure---\n")
glimpse(retail_raw)

cat("Base R Structure\n")
str(retail_raw)


# Missing Values

cat("Missing Values by Column\n")
missing_values <- colSums(is.na(retail_raw))

print(missing_values)

# Duplicate Rows

cat("Duplicate Rows\n")
duplicate_rows <- sum(duplicated(retail_raw))

cat("Duplicate Rows :", duplicate_rows, "\n")


# Summary Statistics

cat("Summary Statistics\n")
print(summary(retail_raw))

# Data Profiling

cat("Data Profiling using skimr\n")

skim(retail_raw)


# Memory Usage

cat("Memory Usage\n")

print(format(object.size(retail_raw), units = "MB"))


# Save Imported Dataset

if(!dir.exists(here("data", "rds"))) {
  dir.create(here("data", "rds"))
}


saveRDS(
  retail_raw,
  here("data", "rds", "retail_raw.rds")
)


# Completion Message

cat("Import Completed Successfully\n")
cat("Rows:", nrow(retail_raw), "\n")
cat("Columns:", ncol(retail_raw), "\n")
cat("RDS File :", here("data", "rds", "retail_raw.rds"), "\n")

message("\n✓ Raw dataset imported and validated successfully.")
