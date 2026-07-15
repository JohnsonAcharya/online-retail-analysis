# ============================================================
# Formatting Helpers
# ============================================================

format_currency <- function(x) {
  scales::dollar(x)
}


format_number <- function(x) {
  scales::comma(x)
}