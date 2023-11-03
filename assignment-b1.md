Assignment B-1: Making a function
================
Irvin Ng
2023-11-03

``` r
suppressPackageStartupMessages(library(tidyverse))
```

    ## Warning: package 'tidyverse' was built under R version 4.2.3

    ## Warning: package 'tibble' was built under R version 4.2.3

    ## Warning: package 'tidyr' was built under R version 4.2.2

    ## Warning: package 'readr' was built under R version 4.2.2

    ## Warning: package 'purrr' was built under R version 4.2.2

    ## Warning: package 'dplyr' was built under R version 4.2.3

    ## Warning: package 'stringr' was built under R version 4.2.2

    ## Warning: package 'lubridate' was built under R version 4.2.2

``` r
suppressPackageStartupMessages(library(gapminder))
```

    ## Warning: package 'gapminder' was built under R version 4.2.3

``` r
suppressPackageStartupMessages(library(testthat))
```

    ## Warning: package 'testthat' was built under R version 4.2.3

# Function Definition

I wanted to make a function which gives me relevant summary data. In
particular, I wanted the function to give me the counts, mean, median,
and SD (where applicable). Even if the data was not numeric, it would at
least give me a count of all the non-numeric data (eg. the amount of
countries in a df).

``` r
#' @Title Generate summary statistics
#'
#' This function calculates the different summary statistics (eg.count, mean, median, and standard deviation) of numeric values
#' in the specified column of a data frame. In the case of non-numeric values, it would at least get counts.
#'
#' @param data A data frame containing the column to be summarized (e.g., gapminder dataset). I named this data because it seems to be a 
#' very common name for the object even for other functions in other packages.
#' @param column_name The name of the column in the data frame to be summarized.
#' @return The count, mean, median, and standard deviation of numeric values.

numeric_summary <- function(data, column_name) {
  # Check if the input data is a data frame
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  
  # Check if the input column exists in the data frame
  if (!column_name %in% names(data)) {
    stop("Specified column does not exist in the data frame.")
  }
  
  # Extract numeric values from the specified column, excluding NA values
  numeric_values <- na.omit(data[[column_name]])
  
  # Check if the column contains numeric data
  if (length(numeric_values) == 0) {
    stop("Specified column does not contain numeric data.")
  }
  
  # Calculate summary statistics with NA values removed
  summary_stats <- list(
    Count = length(numeric_values),
    Mean = mean(numeric_values, na.rm = TRUE),
    Median = median(numeric_values, na.rm = TRUE),
    Standard_Deviation = sd(numeric_values, na.rm = TRUE)
  )
  
  # Return the summary statistics
  return(summary_stats)
}

gapminder
```

    ## # A tibble: 1,704 × 6
    ##    country     continent  year lifeExp      pop gdpPercap
    ##    <fct>       <fct>     <int>   <dbl>    <int>     <dbl>
    ##  1 Afghanistan Asia       1952    28.8  8425333      779.
    ##  2 Afghanistan Asia       1957    30.3  9240934      821.
    ##  3 Afghanistan Asia       1962    32.0 10267083      853.
    ##  4 Afghanistan Asia       1967    34.0 11537966      836.
    ##  5 Afghanistan Asia       1972    36.1 13079460      740.
    ##  6 Afghanistan Asia       1977    38.4 14880372      786.
    ##  7 Afghanistan Asia       1982    39.9 12881816      978.
    ##  8 Afghanistan Asia       1987    40.8 13867957      852.
    ##  9 Afghanistan Asia       1992    41.7 16317921      649.
    ## 10 Afghanistan Asia       1997    41.8 22227415      635.
    ## # ℹ 1,694 more rows

# Examples

I will be using the gapminder dataset to demonstrate my function. Here,
we will look at the population, GDP per capita, and the Country
(non-numeric)

## Example 1: Generate summary for the ‘pop’ column in the gapminder dataset

``` r
numeric_summary(gapminder, "pop")
```

    ## $Count
    ## [1] 1704
    ## 
    ## $Mean
    ## [1] 29601212
    ## 
    ## $Median
    ## [1] 7023596
    ## 
    ## $Standard_Deviation
    ## [1] 106157897

## Example 2: Generate summary for the ‘gdpPercap’ column in the gapminder dataset

``` r
numeric_summary(gapminder, "gdpPercap")
```

    ## $Count
    ## [1] 1704
    ## 
    ## $Mean
    ## [1] 7215.327
    ## 
    ## $Median
    ## [1] 3531.847
    ## 
    ## $Standard_Deviation
    ## [1] 9857.455

## Example 3: Generate a summary for a non-numeric column (we’ll see errors)

``` r
numeric_summary(gapminder, "country")
```

    ## Warning in mean.default(numeric_values, na.rm = TRUE): argument is not numeric
    ## or logical: returning NA

    ## Error in median.default(numeric_values, na.rm = TRUE): need numeric data

# Error Testing

``` r
# Make a custom df with NA values to do the second test:
NA_df <- gapminder
set.seed(1) # Set seed for reproducibility
NA_df$gdpPercap[sample(1:nrow(NA_df), 10)] <- NA # Add NA values to 10 random rows


test_that("numeric_summary function works as expected", {
  # Test 1: Check if the function works with a numeric column without NA values
  expect_equal(numeric_summary(gapminder, "pop")$Count, 1704)
  
  # Test 2: Check if the function works with a numeric column with NA values
  expect_true(!is.na(numeric_summary(NA_df, "gdpPercap")$Mean))
  
  # Test 3: Check if the function handles non-numeric columns and throws an error
  expect_error(numeric_summary(gapminder, "country"))
})
```

    ## ── Warning: numeric_summary function works as expected ─────────────────────────
    ## argument is not numeric or logical: returning NA
    ## Backtrace:
    ##     ▆
    ##  1. ├─testthat::expect_error(numeric_summary(gapminder, "country"))
    ##  2. │ └─testthat:::quasi_capture(...)
    ##  3. │   ├─testthat (local) .capture(...)
    ##  4. │   │ └─base::withCallingHandlers(...)
    ##  5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
    ##  6. └─global numeric_summary(gapminder, "country")
    ##  7.   ├─base::mean(numeric_values, na.rm = TRUE)
    ##  8.   └─base::mean.default(numeric_values, na.rm = TRUE)
