
# ============================================================
# Project : Online Retail Analysis
# Script  : 02_data_understanding.R
# Purpose : Understand the dataset and define cleaning rules
# ============================================================


library(tidyverse)
library(here)


retail_raw <- readRDS(
  here("data", "rds", "retail_raw.rds")
)


#Dataset Snapshot

cat("DATASET SNAPSHOT\n")

cat("Rows:", nrow(retail_raw), "\n")
cat("Cols:", ncol(retail_raw), "\n")


# Business KPIs
cat("BUSINESS OVERVIEW\n")

# Number of unique invoice numbers

cat("Unique Invoices :",
    n_distinct(retail_raw$InvoiceNo), 
    "\n"
    )


# Number of unique customers

cat("Unique Customers :",
    n_distinct(retail_raw$CustomerID),
    "\n"
    )


# Number of unique products

cat("Unique Products :",
    n_distinct(retail_raw$StockCode),
    "\n"
    )


# Number of unique countries

cat("Unique Countries :",
    n_distinct(retail_raw$Country),
    "\n"
    )


# Date Range

cat("DATE RANGE\n")
cat("Start Date :", as.character(min(retail_raw$InvoiceDate)), "\n",
    "End Date   :", as.character(max(retail_raw$InvoiceDate)), "\n"
   )


# Number of unique countries

country_summary <- retail_raw |> 
  count(Country, sort = TRUE)

print(country_summary)


# Top Products

product_summary <- retail_raw |> 
  count(Description, sort = TRUE)

head(product_summary, 20)


# Invoice Types - 

cancelled_invoice <-
  retail_raw %>%
  filter(str_detect(InvoiceNo, "^C"))

nrow(cancelled_invoice)


# Missing Customer IDs

retail_raw |> 
  summarise(
    MissingCustomerID =
      sum(is.na(CustomerID))
  )


# Negative Quantity

retail_raw |> 
  filter(Quantity < 0)


retail_raw |> 
  summarise(
    NegativeQuantity  = 
      sum(Quantity < 0 )
  )


# Zero Price

retail_raw |> 
  filter(UnitPrice == 0)


# Summary of quantity and Unit price

summary(retail_raw$Quantity)
summary(retail_raw$UnitPrice)


# Data Quality Summary (KEY CLEANING INPUTS)

data_quality_summary <- retail_raw |> 
  summarise(
    TotalRows = n(),
    MissingCustomerID = sum(is.na(CustomerID)),
    MissingDescription = sum(is.na(Description)),
    NegativeQuantity = sum(Quantity < 0),
    ZeroUnitPrice = sum(UnitPrice == 0),
    CancelledInvoice = sum(str_detect(InvoiceNo, "^C")) 
  )|> 
  mutate(
    DuplicateRows = sum(duplicated(retail_raw))
  )

print(data_quality_summary)


# Data Quality Percentage Summary (Percentage View)

data_quality_rate <- retail_raw %>%
  summarise(
    MissingCustomerID_pct = mean(is.na(CustomerID)) * 100,
    MissingDescription_pct = mean(is.na(Description)) * 100,
    NegativeQuantity_pct = mean(Quantity < 0) * 100,
    ZeroUnitPrice_pct = mean(UnitPrice == 0) * 100
  )

print(data_quality_rate)
