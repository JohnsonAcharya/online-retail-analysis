# ============================================================
# Project : Online Retail Analysis
# Script  : time_analysis.R
# Purpose : Analyze sales trends, seasonality and purchasing time
# Author  : Johnson
# ============================================================


# Load Project Dependencies

source(here::here("R", "helpers", "load_packages.R"))
source(here::here("R", "helpers", "project_paths.R"))
source(here::here("R", "helpers", "load_data.R"))
source(here::here("R", "helpers", "helper_functions.R"))
source(here::here("R", "helpers", "plot_theme.R"))
source(here::here("R", "helpers", "save_outputs.R"))

print_section("Time Analysis")

create_output_directories()


# Create Analysis Dataset

time_data <- 
  retail_features |> 
  filter(
    !is_cancelled,
    quantity > 0
  )


# Monthly Revenue Analysis

monthly_sales <-
  time_data |> 
  mutate(
    month_date = floor_date(
      invoice_date,
      unit = "month"
    )
  ) |> 
  group_by(
    month_date
    ) |> 
  summarise(
    revenue = sum(sales_amount, na.rm = TRUE),
    orders = n_distinct(invoice_no),
    customer = n_distinct(customer_id),
    .groups = "drop"
  ) |> 
  arrange(
    month_date
  )

print(monthly_sales)



# Monthly Revenue Plot

monthly_revenue_plot <-
  ggplot(
    monthly_sales,
    aes(
      x = month_date,
      y = revenue
    )
  )+
  geom_line(linewidth = 1)+
  geom_point(size = 2)+
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b '%y"
  )+
  scale_y_continuous(labels = scales::comma)+
  labs(
    title = "Montly Revenue Trend",
    subtitle = "Revenue from completed sales",
    x = NULL,
    y = "Revenue"
  )+
  theme_retail()


print(monthly_revenue_plot)



# Revenue by Month

calendar_month_sales <- 
  time_data |> 
  group_by(
    invoice_month_number, 
    invoice_month
  ) |> 
  summarise(
    revenue = sum(sales_amount, na.rm = TRUE),
    orders = n_distinct(invoice_no),
    .groups = "drop"
  ) |> 
  arrange(
    invoice_month_number
  )

print(calendar_month_sales)


# Plot Seasonality - Calendar Month Sales


calendar_month_sales_plot <-
  ggplot(
    calendar_month_sales,
      aes(
        x = reorder(
          invoice_month,
          invoice_month_number
        ),
        y = revenue
      )
  )+
  geom_col()+
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Revenue by Calender Month",
       subtitle = "Seasonal revenue patterns across the year",
       x = "Month",
       y = "Revenue") +
  theme_retail()


print(calendar_month_sales_plot)


# Weekday Analysis

weekday_sales <-
  time_data |> 
  group_by(
    invoice_weekday
  ) |> 
  summarise(
    revenue = sum(sales_amount, na.rm = TRUE),
    orders = n_distinct(invoice_no),
    .groups = "drop"
  )


print(weekday_sales)



weekday_revenue_plot <-
  ggplot(
    weekday_sales,
    aes(
      x = invoice_weekday,
      y = revenue)
  ) +
  geom_col() + 
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Revenue by Day of Week",
    x = "Day",
    y = "Revenue"
  ) +
  theme_retail()


print(weekday_revenue_plot)
  


# Hourly Analysis

hourly_sales <-
  time_data |> 
  group_by(
  invoice_hour  
  ) |> 
  summarise(
    revenue = sum(sales_amount, na.rm = TRUE),
    orders = n_distinct(invoice_no),
    .groups = "drop"
  ) |> 
  arrange(invoice_hour)


print(hourly_sales)


# Hourlu Plot

hourly_revenue_plot <- 
  ggplot(
    hourly_sales,
    aes(
      x = invoice_hour,
      y = revenue
    )
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(
      min(hourly_sales$invoice_hour, na.rm = TRUE),
      max(hourly_sales$invoice_hour, na.rm = TRUE)
    )
  ) +
  scale_y_continuous(
    labels = scales::comma
    ) +
  labs(
    title = "Revenue by Hour of Day",
    x = "Hour",
    y ="Revenue"
  ) +
  theme_retail()


print(hourly_revenue_plot)



# Add a Year-over-Year View

annual_sales <-
  time_data |> 
  group_by(invoice_year) |> 
  summarise(
    revenue =  sum(sales_amount, na.rm = TRUE),
    orders= n_distinct(invoice_no),
    customer =  n_distinct(customer_id),
    .groups = "drop"
  ) |> 
  arrange(invoice_year)

print(annual_sales)


annual_sales <- 
  annual_sales |> 
  mutate(
    revenue_growth = (revenue / lag(revenue)) - 1
  )

print(annual_sales)



# Save Outputs

print_section("Saving Outputs")

create_output_directories()


# Save Tables

save_table(
  monthly_sales,
  "monthly_sales.csv"
)

save_table(
  calendar_month_sales,
  "calendar_month_sales.csv"
)

save_table(
  weekday_sales,
  "weekday_sales.csv"
)

save_table(
  hourly_sales,
  "hourly_sales.csv"
)

save_table(
  annual_sales,
  "annual_sales.csv"
)

# Save Plots

save_plot(
  monthly_revenue_plot,
  "monthly_revenue.png"
)

save_plot(
  calendar_month_sales_plot,
  "calendar_month_sales.png"
)

save_plot(
  weekday_revenue_plot,
  "weekday_revenue.png"
)

save_plot(
  hourly_revenue_plot,
  "hourly_revenue.png"
)


# Analysis Complete

message("======================================")
message("Time analysis completed successfully.")
message("======================================")