# ==============================================================================
# Project : Online Retail Analysis
# Script  : 06_01_sales_overview.R
# Purpose : Exploratory Data Analysis (EDA) - Sales Overview
# Author  : Johnson
# ==============================================================================


# 1. Load Packages

library(tidyverse)
library(here)
library(scales)


# 2. Load Feature-Engineered Data

retail_features <- readRDS(
  here("data", "features", "retail_features.rds")
)

# Prepare Sales Dataset
# Exclude cancelled orders and returns to analyze completed sales only.

sales_data <- retail_features |> 
  filter(
  !is_cancelled, 
  quantity > 0
  )


# 3. Overall Sales KPIs

sales_overview <- tibble(
  Matric = c(
    "Total Revenue",
    "Total Order",
    "Total Customer",
    "Total Product Sold"
  ),
  Value = c(
    sum(sales_data$sales_amount, na.rm = TRUE),
    n_distinct(sales_data$invoice_no),
    n_distinct(sales_data$customer_id),
    sum(sales_data$quantity, na.rm = TRUE)
  )
)

# Display raw KPI table
print(sales_overview)


# 4. # Format KPIs for Presentation

sales_overview_formatted <- sales_overview |> 
  mutate(
    Value = case_when(
      Matric ==  "Total Revenue" ~ dollar(Value),
      TRUE ~ comma(Value)
    )
  )

print(sales_overview_formatted)


# 5. Save KPI Summary

tables_dir <- here("outputs", "tables")

if(!dir.exists(tables_dir)) {
  dir.create(tables_dir, 
             recursive = TRUE)
}


write.csv(
  sales_overview,
  file = here("Outputs", "tables", "sales_overview.csv"),
  row.names = FALSE
)


# 6. Create your first chart

# Monthly revenue:
monthly_sales <- sales_data |> 
  group_by(invoice_year, invoice_month_number) |> 
  summarise(
    revenue = sum(sales_amount, na.rm = TRUE),
    .groups = "drop"
  )

# Monthly Revenue Plot:
monthly_sales_plot <- ggplot(
  monthly_sales,
  aes(
    x = invoice_month_number,
    y = revenue
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly Revenue",
    x = "Month",
    y = "Revenue"
  )

# Display Plot  
print(monthly_sales_plot)


# 7. Save the chart

plots_dir <- here("outputs", "plots")

if(!dir.exists(plots_dir)) {
  dir.create(plots_dir, recursive = TRUE)
}

# Save the figure:
ggsave(
  filename = here(
    "outputs",
    "plots",
    "monthly_revenue.png"
  ),
  plot = monthly_sales_plot,
  width = 8,
  height = 5,
  dpi = 300
)

