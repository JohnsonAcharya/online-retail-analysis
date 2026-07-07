# ============================================================
# Project : Online Retail Analysis
# Script  : 04_data_validation.R
# Purpose : Validate the quality of the cleaned dataset before moving
# to feature engineering and analysis.
# Author  : Johnson
# ============================================================


# 1. Load Required Packages

library(tidyverse)
library(here)


# 2. Load Cleaned Dataset

retail_clean <- readRDS(
  here("data", "processed", "retail_clean.rds")
)

# 3. Create a validation summary

# This table will store the results of each validation check.

validation_summary <- tibble(
  Check = character(),
  Result = character()
)


# 4. Validate required columns

# Define the expected schema

expected_columns <- c(
  "invoice_no",
  "stock_code",
  "description",
  "quantity",
  "invoice_date",
  "unit_price",
  "customer_id",
  "country"
)


# Check whether the dataset contains exactly the expected columns.

column_check <- setequal(
  names(retail_clean),
  expected_columns
)

print(column_check)

validation_summary <- validation_summary |> 
  add_row(
    Check = "Required Columns",
    Result = ifelse(column_check, "PASS", "FAIL")
  )


# 5. Validate missing values

missing_values <- colSums(is.na(retail_clean))

print(missing_values)

validation_summary <- validation_summary |> 
  add_row(
    Check = "Missing Values",
    Result = "Reviewed"
  )


# 6. Validate duplicate rows

duplicate_rows <- sum(duplicated(retail_clean))

print(duplicate_rows)

validation_summary <- validation_summary |> 
  add_row(
    Check = "Duplicate Rows",
    Result = as.character(duplicate_rows)
  )


# 7. Validate quantities

quantity_summary  <- summary(retail_clean$quantity)

print(quantity_summary)

negative_quantity <- sum(retail_clean$quantity < 0)

zero_quantity <- sum(retail_clean$quantity == 0)

validation_summary <- validation_summary |> 
  add_row(
    Check = "Negative Quantity",
    Result = as.character(negative_quantity)
    ) |>
  add_row(
    Check = "Zero Quantity",
    Result = as.character(zero_quantity)
    )


# 8. Validate prices

price_summary <- summary(retail_clean$unit_price)

print(price_summary)

negative_price <- sum(retail_clean$unit_price < 0)

zero_price <- sum(retail_clean$unit_price == 0)

validation_summary <- validation_summary |> 
  add_row(
    Check = "Negative Price",
    Result = as.character(negative_price)
  ) |> 
  add_row(
    Check = "Zero Price",
    Result = as.character(zero_price)
  )


# 9. Validate dates

date_summary <- summary(retail_clean$invoice_date)

date_range <- range(retail_clean$invoice_date)

print(date_summary)

validation_summary <- validation_summary |> 
  add_row(
    Check = "Invoice Date Range",
    Result = paste(date_range[1], "to", date_range[2])
  )


# 10. Validate countries

country_counts <- retail_clean |> 
  count(country, sort = TRUE) 

print(country_counts)

validation_summary <- validation_summary |> 
  add_row(
    Check = "Country Values",
    Result = "Reviewed"
  )

# 11. Display Validation Summary

print(validation_summary)


# 12. Save a validation report

write.csv(
  validation_summary,
  here("outputs", "validation_summary.csv"),
  row.names = FALSE
)

# 13. Validation Complete

cat("\n")
cat("=====================================\n")
cat(" Business Data Validation Complete\n")
cat(" Validation report saved to:\n")
cat(" outputs/validation_summary.csv\n")
cat("=====================================\n")