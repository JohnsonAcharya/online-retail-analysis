# ============================================================
# Module: Sales Performance
# Phase 5.3
# ============================================================


# ------------------------------------------------------------
# Sales UI
# ------------------------------------------------------------



sales_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      
      box(
        title = "Total Revenue",
        width = 3,
        status = "primary",
        solidHeader = TRUE,
        
        h3(
          textOutput(
            ns("total_revenue")
          )
        )
      ),
      
      
      box(
        title = "Total Orders",
        width = 3,
        status = "primary",
        solidHeader = TRUE,
        
        h3(
          textOutput(
            ns("total_orders")
          )
        )
      ),
      
      
      box(
        title = "Average Order Value",
        width = 3,
        status = "primary",
        solidHeader = TRUE,
        
        h3(
          textOutput(
            ns("average_order_value")
          )
        )
      ),
      
      
      box(
        title = "Active Customers",
        width = 3,
        status = "primary",
        solidHeader = TRUE,
        
        h3(
          textOutput(
            ns("active_customers")
          )
        )
      )
    ),
    
    
    fluidRow(
      
      box(
        title = "Monthly Revenue Trend",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        
        h3(
          plotOutput(
            ns("monthly_revenue"),
            height = "400px"
          )
        )
      )
    ),
    
    fluidRow(
      
      box(
        title = "Monthly Orders Trend",
        width = 6,
        status = "primary",
        solidHeader = TRUE,
        
        h3(
          plotOutput(
            ns("monthly_orders"),
            height = "350px"
          )
        )
      ),
      
      
      box(
        title = "Revenue by Weekday",
        width = 6,
        status = "primary",
        solidHeader = TRUE,
        
        h3(
          plotOutput(
            ns("weekday_revenue"),
            height = "350px"
          )
        )
      )
    ),
    
    fluidRow(
      
      box(
        title = "Revenue by Hour",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        
        h3(
          plotOutput(
            ns("hourly_revenue"),
            height = "400px"
          )
        )
      )
    )
    
  )
}




# ------------------------------------------------------------
# Sales Server
# ------------------------------------------------------------

sales_server <- function(id, data) {
  
  moduleServer(id, function(input, output, session) {
    
    # Merchandise Sales Data
    
    merchandise_sales_data <- reactive({
      
      data |> 
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
     
    })
    
    
    # Total Revenue
    
    output$total_revenue <- renderText({
      
      revenue <- merchandise_sales_data() |> 
        summarise(
          total_revenue = sum(sales_amount, na.rm = TRUE)
        ) |> 
        pull(total_revenue)
      
      paste0(
        "£",
        format(
          round(revenue, 0),
          big.mark = ",",
          scientific = FALSE
        )
      )
      
      })
    
    # Total Orders
    
    output$total_orders <- renderText({
      
      orders <- merchandise_sales_data() |> 
        summarise(
          total_orders = n_distinct(invoice_no)
        ) |> 
        pull(total_orders)
      
      format(
        orders,
        big.mark = ","
        )
      
      })
    
    
    # Average Order Value
    
    output$average_order_value <- renderText({
      
      summary <- merchandise_sales_data() |>
        summarise(
          total_revenue = sum(
            sales_amount,
            na.rm = TRUE
          ),
          total_orders = n_distinct(invoice_no)
        )
      
      average_order_value <-
        summary$total_revenue /
        summary$total_orders
      
      paste0(
        "£",
        format(
          round(average_order_value, 2),
          big.mark = ",",
          scientific = FALSE
        )
      )
      
    })
    
    
    # Active Customers
    
    output$active_customers <- renderText({
      
      customers <- merchandise_sales_data() |>
        summarise(
          active_customers = n_distinct(
            customer_id,
            na.rm = TRUE
          )
        ) |>
        pull(active_customers)
      
      format(
        customers,
        big.mark = ","
      )
      
    })
    
    
    # Monthly Revenue
    
    output$monthly_revenue <- renderPlot({
      
      monthly_revenue <- merchandise_sales_data() |> 
        mutate(
          month = floor_date(invoice_date, unit = "month")
          ) |>
        group_by(month) |> 
        summarise(
          revenue = sum(sales_amount, na.rm = TRUE),
          .groups = "drop"
        )
      
      ggplot(
        monthly_revenue,
          aes(
            x = month,
            y = revenue
          )
      ) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        scale_y_continuous(labels = scales::label_currency(
          prefix = "£",
          big.mark = ","
        )) +
        scale_x_date(
          date_breaks = "1 month",
          date_labels = "%b %Y"
        ) +
        labs(
          title = "Monthly Revenue Trend",
          x = NULL,
          y = "Revenue"
        ) +
        theme_retail()
    })
    
    
    # Monthly Order
    
    output$monthly_orders <- renderPlot({
      
      monthly_orders <- merchandise_sales_data() |> 
        mutate(
          month = floor_date(invoice_date, unit = "month")
        ) |>
        group_by(month) |> 
        summarise(
          orders = n_distinct(invoice_no),
          .groups = "drop"
        ) 
      
      ggplot(
        monthly_orders,
        aes(
          x = month,
          y = orders
        )
      ) +
        
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        scale_y_continuous(labels = scales::label_comma()) +
        scale_x_date(date_breaks = "1 month",
                     date_labels = "%b %Y") +
        labs(title = "Monthly Order Trend",
             x = NULL,
             y = "Orders") +
        theme_retail()
    })
    
    
    # Weekday Sales
    
    output$weekday_revenue <- renderPlot({
      
      weekday_revenue <- merchandise_sales_data() |> 
        mutate(
          weekday = wday(invoice_date, label = TRUE, abbr = FALSE, week_start =1)
        ) |> 
        group_by(weekday) |> 
        summarise(
          revenue = sum(sales_amount, na.rm = TRUE),
          .groups = "drop"
        )
      
      
      ggplot(
        weekday_revenue,
        aes(
          x = weekday,
          y = revenue
        )
      ) +
        
        geom_col() +
        scale_y_continuous(labels = scales::label_currency(
          prefix = "£",
          big.mark = ","
          )
        ) +
        labs(
          title = "Revenue by Weekday",
          x = NULL,
          y = "Revenue"
        ) +
        theme_retail()
    })
    
    
    # Hourly Sales
    
    output$hourly_revenue <- renderPlot({
      
      hourly_revenue <- merchandise_sales_data() |>
        mutate(
          hour = hour(invoice_date)
        ) |>
        group_by(hour) |>
        summarise(
          revenue = sum(sales_amount, na.rm = TRUE),
          .groups = "drop"
        )
      
      
      ggplot(
        hourly_revenue,
        aes(
          x = hour,
          y = revenue
        )
      ) +
        geom_col() +
        scale_x_continuous(
          breaks = 0:23
        ) +
        scale_y_continuous(
          labels = scales::label_currency(
            prefix = "£",
            big.mark = ","
          )
        ) +
        labs(
          title = "Revenue by Hour",
          x = "Hour of Day",
          y = "Revenue"
        ) +
        theme_minimal()
    })
    }
  )
}