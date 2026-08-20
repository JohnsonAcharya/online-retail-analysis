# =============================================================================
# Project : Online Retail Analysis
# Script  : customer_analysis.R
# Purpose : Analyze customer behavior by looking at customer purchases,
#           spending patterns, order frequency, and identifying valuable
#           customers. This script creates customer-level summaries and
#           visual reports to help understand customer value and buying habits.
# Author  : Johnson
# ==============================================================================


# Load Project Dependencies

source(here::here("R", "helpers", "load_packages.R"))
source(here::here("R", "helpers", "project_paths.R"))
source(here::here("R", "helpers", "load_data.R"))
source(here::here("R", "helpers", "plot_theme.R"))
source(here::here("R", "helpers", "helper_functions.R"))
source(here::here("R", "helpers", "save_outputs.R"))


# Create Customer Dataset

customer_data <- 
  retail_features |>
  filter(
    !is_cancelled,
    quantity > 0,
    !is.na(customer_id)
  )



# Create Customer Summary

customer_summary <- 
  customer_data |> 
  group_by(customer_id) |> 
  summarise(
    
    total_revenue = sum(sales_amount, na.rm = TRUE),
    total_orders = n_distinct(invoice_no),
    total_units = sum(quantity, na.rm = TRUE),
    average_order_value = total_revenue/total_orders,
    first_purchase = min(invoice_date),
    last_purchase = max(invoice_date),
    .groups = "drop"
    
  )

# Inspect Results

glimpse(customer_summary)

summary(customer_summary)



# Overall Customer Statistics

customer_statistics <- 
  tibble(
    Metric = c(
      "Unique Customers",
      "Average Revenue",
      "Average Orders",
      "Average Units",
      "Average Order Value",
      "One-Time Customers",
      "Repeat Customers"
    ),
    
    Value = c(
      n_distinct(customer_summary$customer_id),
      mean(customer_summary$total_revenue),
      mean(customer_summary$total_orders),
      mean(customer_summary$total_units),
      mean(customer_summary$average_order_value),
      sum(customer_summary$total_orders == 1),
      sum(customer_summary$total_orders > 1)
    )
  )

print(customer_statistics)



# Top Customers by Revenue

top_customers_revenue <- 
  customer_summary |> 
  arrange(
    desc(total_revenue)
  ) |> 
  slice_head(n = 10)



# Plot Revenue

top_customers_revenue_plot <- 
  ggplot(
    top_customers_revenue,
    aes(
    x = reorder(
      factor(customer_id),
      total_revenue
    ),
    y = total_revenue
  )
  ) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Top Customer By Revenue",
    x = "Customer",
    y = "Revenue"
    ) +
  theme_retail()



# Top Customers by Orders

top_customers_orders <- 
  customer_summary |> 
  arrange(
    desc(total_orders)
  ) |> 
slice_head(n = 10)



# Orders Plot

top_customers_orders_plot <- 
  ggplot(
    top_customers_orders,
    aes(
      x = reorder(
        factor(customer_id),
        total_orders
      ),
      y = total_orders
    )
  ) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Top Customers by Orders",
    x = "Customer",
    y = "Orders"
  ) +
  theme_retail()



# Customer Order Distribution - Orders per Customer

customer_order_distribution_plot <- 
  ggplot(
    customer_summary,
    aes(total_orders)
    ) +
  geom_histogram(bins = 30) +
  labs( title = "Orders per Customer") +
  theme_retail()



# Save Outputs

print_section("Saving Outputs")


create_output_directories()



# Save Plots

save_plot(
  top_customers_revenue_plot,
  "top_customers_revenue.png"
)


save_plot(
  top_customers_orders_plot,
  "top_customers_orders.png"
)


save_plot(
  customer_order_distribution_plot,
  "customer_order_distribution.png"
)



# Save Tables

save_table(
  customer_summary,
  "customer_summary.csv"
)


save_table(
  customer_statistics,
  "customer_statistics.csv"
)


save_table(
  top_customers_revenue,
  "top_customers_revenue.csv"
)


save_table(
  top_customers_orders,
  "top_customers_orders.csv"
)



# Analysis Complete

message("======================================")
message("Customer analysis completed successfully.")
message("======================================")