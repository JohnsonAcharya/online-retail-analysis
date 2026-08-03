# =============================================================================
# Project : Online Retail Analysis
# Script  : country_analysis.R
# Purpose : Perform country-level sales analysis and generate business insights.
# Author  : Johnson
# ==============================================================================


# Load Project Dependencies

source(here::here("R", "helpers", "load_packages.R"))
source(here::here("R", "helpers", "project_paths.R"))
source(here::here("R", "helpers", "load_data.R"))
source(here::here("R", "helpers", "plot_theme.R"))
source(here::here("R", "helpers", "helper_functions.R"))
source(here::here("R", "helpers", "save_outputs.R"))



# Create analysis dataset

country_data <- 
  retail_features |> 
  filter(
    !is_cancelled,
    quantity > 0
  )


# Build Country Summary

country_summary <- 
  country_data |> 
  
  group_by(country) |> 
  summarise(
    total_revenue = sum(sales_amount, na.rm = TRUE),
    total_orders = n_distinct(invoice_no),
    total_customers = n_distinct(customer_id),
    total_quantity = sum(quantity, na.rm = TRUE),
    average_order_value = total_revenue/total_quantity,
    
    .groups = "drop"
    
  )

glimpse(country_summary)

summary(country_summary)



# Business Statistics

country_statistic <- 
  tibble(
    Metric = c(
    "Countries",
    "Average Revenue",
    "Average Orders"
    ),
    Value = c(
      nrow(country_summary),
      mean(country_summary$total_revenue),
      mean(country_summary$total_orders)
    )
  )

print(country_statistic)



# Top Countries by Revenue

top_countries_revenue <- 
  country_summary |> 
  arrange(
    desc(total_revenue)
    ) |> 
  slice_head(n = 10)

print(top_countries_revenue)



# Plot Revenue

country_revenue_plot <- 
  ggplot(
    top_countries_revenue,
    aes(
      reorder(country, total_revenue),
      total_revenue
    )
  ) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(
    labels = scales::comma
  ) +
  labs(
    title = "Top Countries by Revenue",
    x = NULL,
    y = "Revenue"
  ) +
  theme_retail()



# Top Countries by Customers

top_countries_customers <- 
  country_summary |> 
  arrange(
    desc(total_customers)
  ) |>  
  slice_head(n = 10)



# Plot Customers

country_customer_plot <- 
  ggplot(
    top_countries_customers,
    aes(
      reorder(country, total_customers),
      total_customers
    )
  ) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(
    labels = scales::comma
  ) +
  labs(
    title = "Top Countries by Customer",
    x = NULL,
    y = "Customer"
  ) +
  theme_retail()



# Average Order Value

country_average_order_value <- 
  country_summary |> 
  filter(total_orders >= 50) |> 
  arrange(
    desc(average_order_value)
  )


# Top Countries by Average order value
top_countries_aov <-
  country_average_order_value |> 
  slice_head(n = 10)



# Avg Orders Plot

country_average_order_value_plot <- 
  ggplot(
    top_countries_aov,
    aes(
      x = reorder(
        country,
        average_order_value 
      ),
      y = average_order_value
    )
  ) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Top 10 Countries by Average Order Value",
    x = NULL,
    y = "Average Order Value"
  ) +
  theme_retail()



# Save Outputs

print_section("Saving Outputs")

create_output_directories()


# Save Tables

save_table(
  country_summary,
  "country_summary.csv"
)


save_table(
  country_statistic,
  "country_statistic.csv"
)

save_table(
  top_countries_revenue,
  "top_countries_revenue.csv"
)


save_table(
  top_countries_customers,
  "top_countries_customers.csv"
)


save_plot(
  country_revenue_plot,
  "country_revenue.png"
)


save_plot(
  country_customer_plot,
  "country_customer.png"
)

save_plot(
  country_average_order_value_plot,
  "country_average_order_value.png"
)


# Analysis Complete

message("======================================")
message("Country analysis completed successfully.")
message("======================================")