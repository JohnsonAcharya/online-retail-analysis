# ============================================================
# Project : Online Retail Analysis
# Script  : 05_feature_engineering.R
# Purpose : Create derived variables for analysis
# Author  : Johnson
# ============================================================


# 1. Load Required Packages

library(tidyverse)
library(here)
library(lubridate)


# 2. Load Cleaned Dataset

retail_clean <- readRDS(
  here("data", "processed", "retail_clean.rds")
)


# 3. Create Working Copy

retail_features <- retail_clean


# 4. Feature Engineering

retail_features <- retail_features |> 
  mutate(
    
    # Revenue Feature
    
    sales_amount = quantity * unit_price,
    
    # Date Features
    
    invoice_year = year(invoice_date),
    invoice_month = month(invoice_date, label = TRUE),
    invoice_month_number =  month(invoice_date),
    invoice_quarter = quarter(invoice_date),
    invoice_weekday = wday(invoice_date, label = TRUE),
    invoice_hour = hour(invoice_date),
    
    # Transaction Flags: Cancellation, Return, Sales
    
    is_cancelled = str_starts(invoice_no, "C"),
    is_return = quantity < 0,
    is_sale = quantity > 0
    
  )


# 5 Validate Feature Engineering

glimpse(retail_features)

# Confirm row count has not changed
stopifnot(
  nrow(retail_features) == nrow(retail_clean)
)

# Confirm required columns exist
required_columns <- c(
  "sales_amount",
  "invoice_year",
  "invoice_month",
  "invoice_month_number",
  "invoice_quarter",
  "invoice_weekday",
  "invoice_hour",
  "is_cancelled",
  "is_return",
  "is_sale"
)

stopifnot(
  all(required_columns %in% names(retail_features))
)


message("✓ Feature engineering validation passed.")


# 6. Create Output Directory

features_path <- here("data", "features")

if(!dir.exists(features_path)) {
  dir.create(features_path, recursive = TRUE)
}

  
# 7. Save Feature Engineered Dataset

saveRDS(
  retail_features,
 file = here("data", "features", "retail_features.rds")
)

message("✓ Feature engineered dataset saved successfully.")


