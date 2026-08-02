# =============================================================================
# Project : Online Retail Analysis
# Script  : product_analysisR
# Purpose: Perform exploratory data analysis (EDA) by generating
#          sales overview KPIs and visualizations.
# Author  : Johnson
# ==============================================================================


# Load Project Dependencies

source(here::here("R", "helpers", "load_packages.R"))
source(here::here("R", "helpers", "project_paths.R"))
source(here::here("R", "helpers", "load_data.R"))
source(here::here("R", "helpers", "plot_theme.R"))
source(here::here("R", "helpers", "helper_functions.R"))
source(here::here("R", "helpers", "save_outputs.R"))


# Prepare Analysis Dataset

## Exclude returns and cancellations.

product_data <- 
  retail_features |> 
  filter(
    !is_cancelled,
    quantity > 0,
    unit_price > 0
    )

# Product Summary

product_summary <- product_data |> 
  group_by(
    stock_code,
    description
  ) |> 
  summarise(
    total_revenue = sum(sales_amount, na.rm = TRUE),
    total_quantity = sum(quantity, na.rm = TRUE),
    total_orders = n_distinct(invoice_no),
    average_price = mean(unit_price, na.rm = TRUE),
    .groups = "drop"
  )


glimpse(product_summary)
summary(product_summary)


# Top 10 Products by Revenue

top_products_revenue <- 
  product_summary |> 
  arrange(desc(total_revenue)) |> 
  slice_head(n = 10)

print(top_products_revenue)



# Plot Revenue

top_products_revenue_plot <- 
  ggplot(top_products_revenue,
         aes(
           x = reorder(description, total_revenue),
           y = total_revenue
          )
  ) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Top Products by Revenue",
    x = NULL,
     y = "Revenue"
    ) +
  theme_retail()

print(top_products_revenue_plot)



# Top 10 Products by Quantity

top_products_quantity <- 
  product_summary |> 
  arrange(desc(total_quantity)) |> 
  slice_head(n = 10)

print(top_products_quantity)



# Plot Quantity

top_products_quantity_plot <- 
  ggplot(top_products_quantity,
         aes(
           x = reorder(description, total_quantity ),
           y = total_quantity 
         )
  ) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Top Products by Quantity",
    x = NULL,
    y = "Quantity"
  ) +
  theme_retail()

print(top_products_quantity_plot)



# Save Outputs

print_section("Saving Outputs")


create_output_directories()


# Save tables

save_table(
  top_products_revenue,
  "top_products_revenue.csv"
)

save_table(
  top_products_quantity,
  "top_products_quantity.csv"
)

save_table(
  product_summary,
  "product_summary.csv"
)


# Save plots

save_plot(
  top_products_revenue_plot,
  "top_products_revenue.png"
)


save_plot(
  top_products_quantity_plot,
  "top_products_quantity.png"
)

# ============================================================
# Completion Message
# ============================================================

message(
  "\n✓ Products analysis completed successfully."
)


