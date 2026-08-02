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


# 2. Create Analysis Dataset

# Prepare Sales Dataset
# Exclude cancelled orders and returns.
# Analyze completed sales only.

sales_data <- retail_features |> 
  filter(
    !is_cancelled, 
    quantity > 0
  )


# 3. Calculate Overall Sales KPIs

sales_overview <- tibble(
  Metric = c(
    "Total Revenue",
    "Total Orders",
    "Total Customers",
    "Total Products Sold"
  ),
  Value = c(
    sum(sales_data$sales_amount, na.rm = TRUE),
    n_distinct(sales_data$invoice_no),
    n_distinct(sales_data$customer_id),
    sum(sales_data$quantity, na.rm = TRUE)
  )
)

print(sales_overview)



# 4. Format Sales KPIs

sales_overview_formatted <- sales_overview |> 
  mutate(
    Value = case_when(
      Metric ==  "Total Revenue" ~ format_currency(Value),
      TRUE ~ format_number(Value)
    )
  )

print(sales_overview_formatted)



# 5. Monthly Revenue Summary

monthly_sales <- sales_data |> 
  group_by(invoice_year, invoice_month_number, invoice_month) |> 
  summarise(
    revenue = sum(sales_amount, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  arrange(invoice_year, invoice_month_number)

print(monthly_sales)



# 6. Monthly Revenue Plot

monthly_sales_plot <- ggplot(
  monthly_sales,
  aes(
    x = invoice_month,
    y = revenue,
    group = invoice_year,
    colour = factor(invoice_year)
  )
) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Monthly Revenue",
    subtitle = "Completed Sales Only",
    x = "Month",
    y = "Revenue"
  ) +
  theme_retail()

print(monthly_sales_plot)



# 7. Save Outputs

print_section("Saving Outputs")


create_output_directories()

# Save tables
save_table(
  sales_overview,
  "sales_overview.csv"
  )

# Save plots
save_plot(
  monthly_sales_plot,
  "monthly_revenue.png"
  )


# ============================================================
# Completion Message
# ============================================================

message(
  "\n✓ Sales overview analysis completed successfully."
)
