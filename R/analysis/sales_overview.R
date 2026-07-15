# =============================================================================
# Project : Online Retail Analysis
# Script  : sales_overview.R
# Purpose: Perform exploratory data analysis (EDA) by generating
#          sales overview KPIs and visualizations.
# Author  : Johnson
# ==============================================================================


# 1. Load Project Dependencies

source(here::here("R", "helpers", "load_packages.R"))
source(here::here("R", "helpers", "project_paths.R"))
source(here::here("R", "helpers", "load_data.R"))
source(here::here("R", "helpers", "plot_theme.R"))
source(here::here("R", "helpers", "helper_functions.R"))
source(here::here("R", "helpers", "save_outputs.R"))


# Create Analysis Dataset

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
      Matric ==  "Total Revenue" ~ format_currency(Value),
      TRUE ~ format_number(Value)
    )
  )

print(sales_overview_formatted)


# 5. Save KPI Summary


if(!dir.exists(PATH_TABLES)) {
  dir.create(PATH_TABLES, 
             recursive = TRUE)
}


write.csv(
  sales_overview,
  file.path(PATH_TABLES, "sales_overview.csv"),
  row.names = FALSE
)


# 6. Create your first chart

# Monthly revenue:
monthly_sales <- sales_data |> 
  group_by(invoice_year, invoice_month_number, invoice_month) |> 
  summarise(
    revenue = sum(sales_amount, na.rm = TRUE),
    .groups = "drop"
  )

print(monthly_sales)

# Monthly Revenue Plot:
monthly_sales_plot <- ggplot(
  monthly_sales,
  aes(
    x = invoice_month,
    y = revenue,
    group = invoice_year 
  )
) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Monthly Revenue",
    subtitle = "Completed Sales Onlyrr",
    x = "Month",
    y = "Revenue"
  ) +
  theme_retail()

# Display Plot  
print(monthly_sales_plot)


# 7. Save the Plot

save_plot(monthly_sales_plot, "monthly_revenue.png")


# ============================================================
# Completion Message
# ============================================================

message(
  "\n✓ Sales overview analysis completed successfully."
)
