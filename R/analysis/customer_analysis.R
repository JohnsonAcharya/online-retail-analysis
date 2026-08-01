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
    total_products = sum(quantity, na.rm = TRUE),
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
      "Average Products",
      "Average Order Value",
      "One-Time Customers",
      "Repeat Customers"
    ),
    
    Value = c(
      n_distinct(customer_summary$customer_id),
      mean(customer_summary$total_revenue),
      mean(customer_summary$total_orders),
      mean(customer_summary$total_products),
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


ggsave(
  filename = file.path(
    PATH_PLOTS,
    "top_customers_revenue_plot.png"
  ),
  plot = top_customers_revenue_plot,
  width = 10,
  height = 6,
  dpi = 300
)


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
    title = "Top Customer By Orders",
    x = "Customer",
    y = "Orders"
  ) +
  theme_retail()


ggsave(
  filename = file.path(
    PATH_PLOTS,
    "top_customers_orders_plot.png"
  ),
  plot = top_customers_orders_plot,
  width = 10,
  height = 6,
  dpi = 300
)



# Customer Order Distribution - Orders per Customer

customer_order_distribution_plot <- 
  ggplot(
    customer_summary,
    aes(total_orders)
    ) +
  geom_histogram(bins = 30) +
  labs( title = "Orders per Customer") +
  theme_retail()


ggsave(
  filename = file.path(
    PATH_PLOTS,
    "customer_order_distribution_plot.png"
  ),
  plot = customer_order_distribution_plot,
  width = 10,
  height = 6,
  dpi = 300
)


# Save Tables

write.csv(
  customer_summary,
  file.path(
    PATH_TABLES,
    "customer_summary.csv"
  ),
  row.names = FALSE
)


write.csv(
  customer_statistics,
  file.path(
    PATH_TABLES,
    "customer_statistics.csv"
  ),
  row.names = FALSE
)

write.csv(
  top_customers_revenue,
  file.path(
    PATH_TABLES,
    "top_customers_revenue.csv"
  ),
  row.names = FALSE
)


write.csv(
  top_customers_orders,
  file.path(
    PATH_TABLES,
    "top_customers_orders.csv"
  ),
  row.names = FALSE
)



# Analysis Complete

message("======================================")
message("Customer analysis completed successfully.")
message("Tables saved to: outputs/tables/")
message("Plots saved to: outputs/plots/")
message("======================================")