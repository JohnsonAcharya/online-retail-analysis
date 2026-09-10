# ============================================================
# Online Retail Analytics Dashboard
# Shiny Application Foundation
# ============================================================


# Load Packages
#--------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(here)


# Load Helpers

source(here::here("R", "helpers", "plot_theme.R"))
source(here::here("R", "helpers", "non_merchandise_codes.R"))


# Load Dashboard Modules
#--------------------------------------------------------------
source(here::here("shiny", "modules", "mod_overview.R"))
source(here::here("shiny", "modules", "mod_sales.R"))



# Load feature Data

retail_features <- readRDS(
  here("data", "features", "retail_features.rds")
)



# UI

library(shiny)

ui <- dashboardPage(
  
  dashboardHeader(
    title = "Online Retail Analytics"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Overview",
        tabName = "overview",
        icon = icon("dashboard")
      ),
      
      menuItem(
        "Sales",
        tabName = "sales",
        icon = icon("chart-line")
      ),
      
      menuItem(
        "Products",
        tabName = "products",
        icon = icon("box")
      ),
      
      menuItem(
        "Customers",
        tabName = "customers",
        icon = icon("users")
      ),
      
      menuItem(
        "Countries",
        tabName = "countries",
        icon = icon("globe")
      ),
      
      menuItem(
        "Cancellations",
        tabName = "cancellations",
        icon = icon("ban")
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # Overview
      
      tabItem(
        tabName = "overview",
        h2("Executive Overview"),
        p(
          "Interactive dashboard for Online Retail sales,
          customer,product, country and cancellation analysis."
        ),
        overview_ui("overview")
      ),
      
      # Sales
      
      tabItem(
        tabName = "sales",
        h2("Sales Performance"),
        p("Explore sales performance, revenue trends, and order activity"),
        sales_ui("sales")
      ),
      
      # Products
      
      tabItem(
        tabName = "products",
        h2("Products Performance"),
        p("Products analysis will be added")
      ),
      
      # Customers
      
      tabItem(
        tabName = "customers",
        h2("Customers Performance"),
        p("Customers analysis will be added")
      ),
      
      # Countries
      
      tabItem(
        tabName = "countries",
        h2("Countries Performance"),
        p("Countries analysis will be added")
      ),
      
      # Cancellations
      
      tabItem(
        tabName = "cancellations",
        h2("Cancellations Performance"),
        p("Cancellations analysis will be added")
      )
    )
  )
  
)


# Server

server <- function(input, output, session) {
  
  overview_server(
    id = "overview",
    data = retail_features
  )
  
  sales_server(
    id = "sales",
    data = retail_features
  )
  
}

shinyApp(ui = ui, server = server)