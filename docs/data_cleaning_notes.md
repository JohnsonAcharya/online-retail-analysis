# Data Cleaning Notes

## Dataset

- **Dataset:** Online Retail
- **Period Covered:** 2010-12-01 to 2011-12-09
- **Rows:** 541,909
- **Columns:** 8

## Data Quality Observations

|Observation | Decision | Reason |
|-------------|----------|--------|
| Missing CustomerID | Keep for now | Sales are still valid. |
| Missing Description | Investigate | Product cannot be identified. |
| Negative Quantity | Keep for now | Likely product returns. |
| Zero UnitPrice | Investigate | Could represent promotional items. |
| Invoice starts with 'C' | Keep | Indicates cancellations, not necessarily bad data.
| Duplicate rows | Investigate | Remove only if confirmed to be exact duplicates. |


## Business Understanding

- Each row represents one product purchased within an invoice.
- One invoice may contain multiple products
- A customer can have multiple invoices.
- Products are identified by `StockCode`.
- Some invoices represent **returns or cancellations**
- Negative quantities indicate **product returns**
- Sales occured across multiple countries, with the majority from the United Kingdom.


## Data Quality Observations

### Missing Data
- `CustomerID` is missing in several records
- `Description` missing in small subset of data

### Quantity Issues
- Negative values exist (returns)
- Needs separation between sales and returns analysis

### Price Issues
- Some unit prices are zero
- May represent promotions or data entry issues


## Duplicate Analysis

- Duplicate invoices are expected (multi-item orders)
- Exact duplicate rows must be investigated before removal
- Line-item duplicates require validation


## Cleaning Rules

### Keep

- Cancelled invoices (`InvoiceNo` starts with `C`)
- Negative quantitiess (returns)
- Missing `CustomerID` (until customer analysis)


### Investigate

- Missing `CustomerID`
- Missing `Description`
- Zero `UnitPrice`
- Duplicate records


### Remove Later (Analysis-Specific)

For Sales analysis only:

- Cancelled invoices
- Negative quantities
- Zero-price items (if confirmed not to represent valid sales)
- Invalid or confirmed duplicate rows


## Key Understanding

- Dataset contains both **sales and returns**
- Not all “negative values” are errors
- Business logic is essential for cleaning decisions
- Cleaning depends on analysis goal (sales vs customer vs returns)

## Next Step

Create a cleaned dataset using the defined rules and prepare it for Exploratory Data Analysis (EDA).




