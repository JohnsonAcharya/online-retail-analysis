# ============================================================
# Project : Online Retail Analysis
# Script  : cancellation_analysis.R
# Purpose : Analyze cancelled/returned transactions
# Author  : Johnson
# ============================================================



# Load Project Dependencies

source(here::here("R", "helpers", "load_packages.R"))
source(here::here("R", "helpers", "project_paths.R"))
source(here::here("R", "helpers", "load_data.R"))
source(here::here("R", "helpers", "helper_functions.R"))
source(here::here("R", "helpers", "plot_theme.R"))
source(here::here("R", "helpers", "save_outputs.R"))
source(here::here("R", "helpers", "non_merchandise_codes.R"))

print_section("Cancellation Analysis")

create_output_directories()



# Understand the cancellation data

# Check the number of cancelled and non-cancelled records

retail_features |> 
  count(is_cancelled)


# Inspect cancelled records

retail_features |> 
  filter(is_cancelled) |> 
  select(
    invoice_no,
    stock_code,
    description,
    quantity,
    unit_price,
    sales_amount,
    customer_id,
    country
  ) |> 
  head(20)


## Define cancellations dataset

# Keep cancelled merchandise transactions for cancellation analysis.
# Exclude non-merchandise items from cancellation metrics.

cancellation_data <-
  retail_features |> 
  filter(is_cancelled) |> 
  mutate(
    stock_code = str_to_upper(stock_code)
  ) |> 
  anti_join(
    non_merchandise,
    by = "stock_code"
  )
  


# cancellations Summary

cancellations_summary <-
  tibble(
    total_cancelled_orders = n_distinct(cancellation_data$invoice_no),
    total_cancelled_lines = nrow(cancellation_data),
    total_units_cancelled = sum(abs(cancellation_data$quantity), na.rm = TRUE),
    total_cancelled_value = sum(abs(cancellation_data$sales_amount), na.rm = TRUE)
  )

print(cancellations_summary)


## Define Valid Sales Data

# Keep valid product sales for calculating cancellation rates.
# Exclude cancellations, invalid quantities/prices, cancellation pairs,
# and non-merchandise transactions.

sales_data <-
  retail_features |> 
  filter(
    !is_cancelled,
    quantity > 0,
    unit_price > 0,
    !is_cancelled_pair
  ) |> 
  mutate(
    stock_code = str_to_upper(stock_code)
  ) |> 
  anti_join(
    non_merchandise,
    by = "stock_code"
  )


# Count completed sales orders

total_orders <-
  n_distinct(sales_data$invoice_no)

# Count cancelled orders

cancelled_orders <-
  n_distinct(cancellation_data$invoice_no)


total_orders_all <-
  total_orders + cancelled_orders


# Calculate the proportion of all orders that were cancelled.

order_cancellation_rate  <-
  cancelled_orders / total_orders_all



# Cancellations KPI Table

cancellation_kpis <-
  tibble(
    metric = c(
      "Cancelled Orders",
      "Completed Sales Orders",
      "Total Orders",
      "Order Cancellation Rate",
      "Units Cancelled",
      "Cancelled Sales Value"
    ),
    value = c(
      cancelled_orders,
      total_orders,
      total_orders_all,
      order_cancellation_rate ,
      cancellations_summary$total_units_cancelled,
      cancellations_summary$total_cancelled_value
    )
  )

print(cancellation_kpis)


## Cancellation Analysis by Product

product_cancellations <-
  cancellation_data |> 
  group_by(
    stock_code,
    description
  ) |> 
  summarise(
    units_cancelled = sum(abs(quantity), na.rm = TRUE),
    cancelled_value = sum(abs(sales_amount), na.rm = TRUE),
    cancelled_orders = n_distinct(invoice_no),
    .groups = "drop"
  ) |> 
  arrange(desc(units_cancelled)
  )
 
 
print(product_cancellations)


## Top 10 cancelled Products

top_cancelled_products <-
  product_cancellations |> 
  slice_head(n = 10)


print(top_cancelled_products)


## Plot Top cancelled Products

top_cancelled_products_plot <-
  ggplot(
    top_cancelled_products,
    aes(
      x = reorder(description, units_cancelled),
      y = units_cancelled 
    )
  ) + 
  geom_col() +
  coord_flip() + 
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Top 10 Products by Cancelled Quantity",
    subtitle = "Based on transactions identified as cancellations",
    x = NULL,
    y = "Units Cancelled"
  ) + 
  theme_retail()


print(top_cancelled_products_plot)



## Cancellation Analysis by Country

country_cancellations <-
  cancellation_data |> 
  group_by(country) |> 
  summarise(
    units_cancelled = sum(abs(quantity), na.rm = TRUE),
    cancelled_value = sum(abs(sales_amount), na.rm = TRUE),
    cancelled_orders = n_distinct(invoice_no),
    .groups = "drop"
  ) |> 
  arrange(desc(units_cancelled)
  )


print(country_cancellations)



## Top 10 Countries by Cancelled Quantity

top_cancellations_countries <-
  country_cancellations |> 
  slice_head(n = 10)


print(top_cancellations_countries)



# Plot cancellations by Country

country_cancellations_plot <-
  ggplot(
    top_cancellations_countries,
    aes(
      x = reorder(country, units_cancelled),
      y = units_cancelled
    )
  ) + 
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Top 10 Countries by Cancelled Quantity",
    subtitle = "Based on cancelled transactions",
    x = NULL,
    y = "Units Cancelled"
  ) + 
  theme_retail()


print(country_cancellations_plot)



# Cancellations Over Time

monthly_cancellations <-
  cancellation_data |> 
  mutate(
    month_date = lubridate::floor_date(
      invoice_date, 
      unit = "month"
    )
  ) |> 
  group_by(month_date) |> 
  summarise(
    units_cancelled = sum(abs(quantity), na.rm = TRUE),
    cancelled_value  = sum(abs(sales_amount), na.rm = TRUE),
    cancelled_orders = n_distinct(invoice_no),
    .groups = "drop"
  ) |> 
  arrange(month_date)


print(monthly_cancellations)



# Plot Monthly cancellations Value

monthly_cancellations_plot <-
  ggplot(
    monthly_cancellations,
    aes(
      x = month_date,
      y = cancelled_value
    )
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) + 
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b '%y"
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Monthly Cancelled Transaction Value",
    subtitle = "Total value associated with cancelled transactions",
    x = NULL,
    y = "Cancelled Value",
  ) +
  theme_retail()
  


print(monthly_cancellations_plot)



# Product cancellations Rate

product_sales <-
  sales_data |>
  group_by(stock_code, description) |> 
  summarise(
    units_sold = sum(quantity, na.rm = TRUE),
    sales_value = sum(sales_amount, na.rm = TRUE),
    .groups = "drop"
  )
  


product_cancellations_rate <-
  cancellation_data |> 
  group_by(stock_code, description) |> 
  summarise(
    units_cancelled  = sum(abs(quantity), na.rm = TRUE),
    cancelled_value = sum(abs(sales_amount), na.rm = TRUE),
    cancelled_orders = n_distinct(invoice_no),
    .groups = "drop"
  )


## Combine Product Sales and Cancellation Data

product_cancellation_analysis <-
  product_sales |> 
  left_join(
    product_cancellations_rate,
    by = c("stock_code", "description")
    ) |> 
  mutate(
    units_cancelled = replace_na(units_cancelled, 0),
    cancelled_value = replace_na(cancelled_value, 0),
    cancelled_orders = replace_na(cancelled_orders, 0),
    cancellation_rate =
      units_cancelled / (units_sold + units_cancelled)
  )


print(product_cancellation_analysis, width = Inf)



# Save Outputs

print_section("Saving Outputs")


# Save Tables

save_table(
  cancellation_kpis,
  "cancellation_kpis.csv"
)


save_table(
  product_cancellations,
  "product_cancellations.csv"
)


save_table(
  top_cancelled_products,
  "top_cancelled_products.csv"
)


save_table(
  country_cancellations,
  "country_cancellations.csv"
)


save_table(
  top_cancellations_countries,
  "top_cancellations_countries.csv"
)


save_table(
  monthly_cancellations,
  "monthly_cancellations.csv"
)


save_table(
  product_cancellation_analysis,
  "product_cancellation_analysis.csv"
)



# Save Plots

save_plot(
  top_cancelled_products_plot,
  "top_cancelled_products.png"
)

save_plot(
  country_cancellations_plot,
  "country_cancellations.png"
)


save_plot(
  monthly_cancellations_plot,
  "monthly_cancellations.png"
)


# Analysis Complete

print_section(
  "Cancellation analysis completed successfully."
  )