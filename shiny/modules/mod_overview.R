# ============================================================
# Module: Executive Overview
# Phase 5.2
# ============================================================


# ------------------------------------------------------------
# Overview UI
# ------------------------------------------------------------



overview_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      
      box(
        title = "Total Revenue",
        width = 3,
        status = "primary",
        solidHeader = TRUE,
        h3(textOutput(ns("total_revenue")))
      ),
      
      box(
        title = "Total Orders",
        width = 3,
        status = "primary",
        solidHeader = TRUE,
        h3(textOutput(ns("total_orders")))
      ),
      
      box(
        title = "Unique Customers",
        width = 3,
        status = "primary",
        solidHeader = TRUE,
        h3(textOutput(ns("unique_customers")))
      ),
      
      box(
        title = "Cancellation Rate",
        width = 3,
        status = "primary",
        solidHeader = TRUE,
        h3(textOutput(ns("cancellation_rate")))
      )
    ),
    
    fluidRow(
      
      box(
        title = "Monthly Revenue Trend",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        plotOutput(
          ns("monthly_revenue"),
          height = "400px"
          )
      )
    ),
    
    fluidRow(
      
      box(
        title = "Top 10 Countries by Revenue",
        width = 6,
        status = "primary",
        solidHeader = TRUE,
        
        plotOutput(
          ns("top_countries"),
          height = "400px"
        )
      ),
      
      box(
        title = "Top 10 Products by Revenue",
        width = 6,
        status = "primary",
        solidHeader = TRUE,
        
        plotOutput(
          ns("top_products"),
          height = "400px"
        )
      )
    )
  )
}


# ------------------------------------------------------------
# Overview Server
# ------------------------------------------------------------


overview_server <- function(id, data) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # Sales Data
      
      sales_data <- reactive({
        
       data |> 
          filter(is_sale)
      })
      
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
        
        revenue <- sales_data() |> 
          summarise(
            revenue = sum(
              sales_amount,
              na.rm = TRUE)
          ) |> 
          pull(revenue)
        
        paste0(
          "£",
          format(
            round(revenue, 0),
            big.mark = ",",
            scientific = FALSE
          )
        )
          
      })
      
      # Total Orders'
      
      output$total_orders <- renderText({
        
        orders <- sales_data() |> 
          summarise(
            orders = n_distinct(invoice_no)
          ) |> 
          pull(orders)
        
        format(
          orders,
          big.mark = ","
        )
      })
      
      # Unique Customers
      
      output$unique_customers <- renderText({
        
        customers <- sales_data() |> 
          summarise(
            customers = n_distinct(customer_id)
          ) |> 
          pull(customers)
        
        format(
          customers,
          big.mark = ","
        )
          
      })
      
      # Cancellation Rate
      
      output$cancellation_rate <- renderText({
        
        total_orders <- data |> 
          summarise(
            orders = n_distinct(invoice_no)
          ) |> 
          pull(orders)
        
        cancelled_orders <- data |> 
          filter(is_cancelled) |> 
          summarise(
            orders =  n_distinct(invoice_no)
          ) |> 
          pull(orders)
        
        rate <- cancelled_orders / total_orders
        
        paste0(
          round(rate * 100, 1),
          "%"
        )
        
      })
      
      
      # Monthly Revenue
      
      output$monthly_revenue <- renderPlot({
        
        monthly_data <- sales_data() |> 
          mutate(
            month = floor_date(
              invoice_date,
              unit = "month"
            )
          ) |> 
          group_by(month) |> 
          summarise(
            revenue = sum(
              sales_amount,
              na.rm = TRUE
            ),
            .groups = "drop"
          )
        
        ggplot(
          monthly_data,
          aes(
            x = month,
            y = revenue
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
            x = NULL,
            y = "Revenue",
            title = NULL
          ) +
          theme_retail()
      })
      
      
      # Top 10 Countries by Revenue
      
      output$top_countries <- renderPlot({
        
        country_data <- merchandise_sales_data() |> 
          group_by(country) |> 
          summarise(
            revenue = sum(
              sales_amount, 
              na.rm = TRUE
              ),
            .groups = "drop"
          ) |> 
          arrange(desc(revenue)) |> 
          slice_head(n = 10)
        
        
        ggplot(
          country_data,
          aes(
            x = reorder(country, revenue),
            y = revenue
          )
        ) +
          geom_col() +
          coord_flip() + 
          scale_y_continuous(
            labels = scales::label_number(
              prefix = "£",
              big.mark = ","
            )
          ) +
          labs(
            x = NULL,
            y = "Revenue"
          ) + 
          theme_retail()
      })
      
      
      # Top 10 Products by Revenue
      
      output$top_products <- renderPlot({
        top_products_data <- merchandise_sales_data() |> 
          group_by(
            stock_code, 
            description
            ) |> 
          summarise(
            revenue = sum(
              sales_amount, 
              na.rm = TRUE),
            .groups = "drop"
          ) |> 
          arrange(desc(revenue)) |> 
          slice_head(n = 10)
        
        
        ggplot(
          top_products_data,
          aes(
            x = reorder(description, revenue),
            y = revenue
          )
        )+
          geom_col() +
          coord_flip() +
          scale_y_continuous(
            labels = scales::label_number(
              prefix = "£",
              big.mark = ","
            )
          ) +
          labs(
            x = NULL,
            y = "Revenue"
          ) +
          theme_retail()
      })
      
      
    }
  )
}

