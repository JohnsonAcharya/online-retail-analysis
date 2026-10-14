
# ============================================================
# Product Performance Dashboard
# File: shiny/modules/mod_products.R
# ============================================================


# ------------------------------------------------------------
# Product Dashboard UI
# ------------------------------------------------------------

products_ui <- function(id) {

  ns <- NS(id)

  tagList(


    fluidRow(
      valueBoxOutput(ns("unique_products"), width = 3),
      valueBoxOutput(ns("units_sold"), width = 3),
      valueBoxOutput(ns("product_revenue"), width = 3),
      valueBoxOutput(ns("average_unit_price"), width = 3)
    ),


    # Top Products by Revenue

    fluidRow(
      box(
        title = "Top 10 Products by Revenue",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        plotOutput(
          ns("top_products_revenue"),
          height = "450px"
        )
      )
    ),


    # Top Products by Quantity

    fluidRow(
      box(
        title = "Top 10 Products by Quantity",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        plotOutput(
          ns("top_products_quantity"),
          height = "450px"
        )
      )
    ),


    # Product Comparison Chart

    fluidRow(
      box(
        title = "Product Revenue vs Units Sold",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        plotOutput(
          ns("product_comparison"),
          height = "450px"
        )
      )
    ),

    # Product Performance Table

    fluidRow(
      box(
        title = "Top 10 Products by Revenue — Details",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        tableOutput(
          ns("product_performance_table")
        )
      )
    ),

    # Key Product Insights

    fluidRow(
      box(
        title = "Key Product Insights",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        uiOutput(
          ns("product_insights")
        )
      )
    )

  )
}


# ------------------------------------------------------------
# Product Dashboard Server
# ------------------------------------------------------------

products_server <- function(id, data) {

  moduleServer(
    id,
    function(input, output, session) {

      # --------------------------------------------------
      # Product Sales Data
      # --------------------------------------------------

      merchandise_sales_data <- reactive({

        data |>
          filter(
            !is_cancelled,
            quantity > 0,
            unit_price > 0,
            !is_cancelled_pair
          ) |>
          mutate(
            stock_code = stringr::str_to_upper(stock_code)
          ) |>
          anti_join(
            non_merchandise,
            by = "stock_code"
          )

      })



      # --------------------------------------------------
      # Product Performance Summary
      # --------------------------------------------------

      product_summary_data <- reactive({

        merchandise_sales_data() |>
          group_by(stock_code, description) |>
          summarise(
            units_sold = sum(quantity, na.rm = TRUE),
            revenue = sum(sales_amount, na.rm = TRUE),
            .groups = "drop"
          ) |>
          mutate(
            average_unit_price = if_else(
              units_sold > 0,
              revenue / units_sold,
              0
            )
          )

      })


      # --------------------------------------------------
      # KPI 1: Unique Products
      # --------------------------------------------------

      output$unique_products <- renderValueBox({

        total_products <- merchandise_sales_data() |>
          summarise(
            total = n_distinct(stock_code)
          ) |>
          pull(total)

        valueBox(
          value = format(
            total_products,
            big.mark = ","
          ),
          subtitle = "Unique Products",
          icon = icon("shopping-bag"),
          color = "blue"
        )

      })


      # --------------------------------------------------
      # KPI 2: Units Sold
      # --------------------------------------------------

      output$units_sold <- renderValueBox({

        total_units <- merchandise_sales_data() |>
          summarise(
            total = sum(quantity, na.rm = TRUE)
          ) |>
          pull(total)

        valueBox(
          value = format(
            total_units,
            big.mark = ",",
            scientific = FALSE
          ),
          subtitle = "Units Sold",
          icon = icon("boxes-stacked"),
          color = "green"
        )

      })


      # --------------------------------------------------
      # KPI 3: Product Revenue
      # --------------------------------------------------

      output$product_revenue <- renderValueBox({

        total_revenue <- merchandise_sales_data() |>
          summarise(
            total = sum(sales_amount, na.rm = TRUE)
          ) |>
          pull(total)

        valueBox(
          value = paste0(
            "£",
            format(
              round(total_revenue, 0),
              big.mark = ",",
              scientific = FALSE
            )
          ),
          subtitle = "Product Revenue",
          icon = icon("sterling-sign"),
          color = "purple"
        )

      })


      # --------------------------------------------------
      # KPI 4: Average Unit Price
      # --------------------------------------------------

      output$average_unit_price <- renderValueBox({

        summary <- merchandise_sales_data() |>
          summarise(
            total_revenue = sum(sales_amount, na.rm = TRUE),
            total_units = sum(quantity, na.rm = TRUE)
          )

        average_price <- if (summary$total_units > 0) {
          summary$total_revenue / summary$total_units
        } else {
          0
        }

        valueBox(
          value = paste0(
            "£",
            format(
              round(average_price, 2),
              big.mark = ",",
              scientific = FALSE
            )
          ),
          subtitle = "Average Unit Price",
          icon = icon("tag"),
          color = "yellow"
        )

      })


      # --------------------------------------------------
      # Top 10 Products by Revenue
      # --------------------------------------------------

      output$top_products_revenue <- renderPlot({

        top_products <- merchandise_sales_data() |>
          group_by(stock_code, description) |>
          summarise(
            revenue = sum(sales_amount, na.rm = TRUE),
            units_sold = sum(quantity, na.rm = TRUE),
            .groups = "drop"
          ) |>
          arrange(desc(revenue)) |>
          slice_head(n = 10) |>
          mutate(
            product_label = paste0(
              stock_code,
              " - ",
              stringr::str_trunc(description, 35)
            ),
            product_label = forcats::fct_reorder(
              product_label,
              revenue
            )
          )

        ggplot(
          top_products,
          aes(
            x = revenue,
            y = product_label
          )
        ) +
          geom_col() +
          scale_x_continuous(
            labels = scales::label_currency(
              prefix = "£",
              big.mark = ","
            )
          ) +
          labs(
            title = "Top 10 Products by Revenue",
            x = "Revenue",
            y = NULL
          ) +
          theme_minimal() +
          theme(
            axis.text.y = element_text(size = 9)
          )

      })


      # --------------------------------------------------
      # Top 10 Products by Quantity
      # --------------------------------------------------

      output$top_products_quantity <- renderPlot({

        top_products <- merchandise_sales_data() |>
          group_by(stock_code, description) |>
          summarise(
            units_sold = sum(quantity, na.rm = TRUE),
            revenue = sum(sales_amount, na.rm = TRUE),
            .groups = "drop"
          ) |>
          arrange(desc(units_sold)) |>
          slice_head(n = 10) |>
          mutate(
            product_label = paste0(
              stock_code,
              " - ",
              stringr::str_trunc(description, 35)
            ),
            product_label = forcats::fct_reorder(
              product_label,
              units_sold
            )
          )

        ggplot(
          top_products,
          aes(
            x = units_sold,
            y = product_label
          )
        ) +
          geom_col() +
          scale_x_continuous(
            labels = scales::label_comma()
          ) +
          labs(
            title = "Top 10 Products by Quantity Sold",
            x = "Units Sold",
            y = NULL
          ) +
          theme_minimal() +
          theme(
            axis.text.y = element_text(size = 9)
          )

      })


      # --------------------------------------------------
      # Product Revenue vs Units Sold
      # --------------------------------------------------

      output$product_comparison <- renderPlot({

        comparison_data <- product_summary_data()

        validate(
          need(
            nrow(comparison_data) > 0,
            "No product sales data is available."
          )
        )

        ggplot(
          comparison_data,
          aes(
            x = units_sold,
            y = revenue
          )
        ) +
          geom_point(alpha = 0.6, size = 2) +
          scale_x_continuous(
            labels = scales::label_comma()
          ) +
          scale_y_continuous(
            labels = scales::label_currency(
              prefix = "£",
              big.mark = ","
            )
          ) +
          labs(
            title = "Product Revenue vs Units Sold",
            subtitle = "Each point represents one merchandise product",
            x = "Units Sold",
            y = "Revenue"
          ) +
          theme_minimal()

      })


      # --------------------------------------------------
      # Top 10 Product Performance Table
      # --------------------------------------------------

      output$product_performance_table <- renderTable({

        product_summary_data() |>
          arrange(desc(revenue)) |>
          slice_head(n = 10) |>
          transmute(
            `Stock Code` = stock_code,
            `Product` = description,
            `Units Sold` = units_sold,
            `Revenue (£)` = round(revenue, 2),
            `Average Unit Price (£)` = round(
              average_unit_price,
              2
            )
          )

      }, striped = TRUE, bordered = TRUE, hover = TRUE)


      # --------------------------------------------------
      # Key Product Insights
      # --------------------------------------------------

      output$product_insights <- renderUI({

        summary_data <- product_summary_data()

        validate(
          need(
            nrow(summary_data) > 0,
            "No product sales data is available."
          )
        )

        highest_revenue_product <- summary_data |>
          slice_max(
            revenue,
            n = 1,
            with_ties = FALSE
          )

        highest_volume_product <- summary_data |>
          slice_max(
            units_sold,
            n = 1,
            with_ties = FALSE
          )

        tagList(

          tags$p(
            tags$strong("Highest Revenue: "),
            highest_revenue_product$description,
            " (",
            scales::dollar(
              highest_revenue_product$revenue,
              prefix = "£",
              big.mark = ","
            ),
            ")"
          ),

          tags$p(
            tags$strong("Highest Volume: "),
            highest_volume_product$description,
            " (",
            scales::comma(
              highest_volume_product$units_sold
            ),
            " units)"
          ),

          tags$p(
            tags$strong("Products Analysed: "),
            scales::comma(nrow(summary_data))
          )

        )

      })


    }
  )
}
