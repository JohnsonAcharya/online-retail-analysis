# ============================================================
# Non-Merchandise Transaction Classification
# ============================================================
#
# Purpose: Define stock codes that represent non-merchandise,
#          operational, financial, or adjustment transactions.
#
#         These codes are excluded from product-performance analysis.
#
# ============================================================


# Non-merchandise transaction classification

non_merchandise <- tibble(
  stock_code = c(
    "DOT",
    "POST",
    "M",
    "C2",
    "AMAZONFEE",
    "B",
    "BANK CHARGES",
    "S"
  ),
  
  category = c(
    "postage",
    "postage",
    "manual_adjustment",
    "carriage",
    "fee",
    "financial_adjustment",
    "bank_charge",
    "sample"
  )
)


# Review classification table 

print(non_merchandise)
