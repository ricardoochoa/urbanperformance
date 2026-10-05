#' Housing Affordability Ratio
#'
#' This function calculates the Price-to-Income Ratio (PIR) to measure housing stress
#' and classifies housing markets according to UN-Habitat and World Bank benchmarks.
#'
#' METHOD:
#' The Price-to-Income Ratio evaluates the relationship between median housing costs
#' and median annual household income:
#' \deqn{PIR = \frac{HousingCost_{median}}{Income_{annual}}}
#' When monthly income figures are supplied, they are annualized (\eqn{Income_{annual} = Income_{monthly} \times 12}).
#' Markets are classified into four internationally recognized affordability tiers:
#' \itemize{
#'   \item \bold{Affordable}: PIR <= 3.0
#'   \item \bold{Moderately Unaffordable}: 3.1 <= PIR <= 4.0
#'   \item \bold{Seriously Unaffordable}: 4.1 <= PIR <= 5.0
#'   \item \bold{Severely Unaffordable}: PIR >= 5.1
#' }
#'
#' @param housing_cost Numeric. Median or average housing unit cost (e.g. from `housing_costs$value`).
#' @param income Numeric scalar (median household income) or `RasterLayer` containing income data.
#' @param income_period Character. Timeframe of provided income: `"monthly"` (default) or `"annual"`.
#' @param decile_data Optional `data.frame` containing decile breakdowns with columns `decile`
#'   and `monthly_median_income_usd` (or `income`).
#'
#' @return A tidy `data.frame` containing the Price-to-Income Ratio and the UN-Habitat affordability tier.
#' @export
#'
#' @examples
#' housing_affordability(housing_cost = 65000, income = 1200, income_period = "monthly")
housing_affordability <- function(housing_cost,
                                  income,
                                  income_period = c("monthly", "annual"),
                                  decile_data = NULL) {
  income_period <- match.arg(income_period)

  # Extract scalar housing cost
  if (is.data.frame(housing_cost)) {
    val_col <- intersect(c("value", "housing_cost", "cost"), names(housing_cost))[1]
    if (is.na(val_col)) stop("Could not find numeric value column in housing_cost data.frame.")
    hc_val <- as.numeric(housing_cost[[val_col]][1])
  } else if (is.numeric(housing_cost)) {
    hc_val <- as.numeric(housing_cost[1])
  } else {
    stop("Input 'housing_cost' must be a numeric value or a data.frame from housing_costs().")
  }

  # Extract scalar income
  if (inherits(income, "RasterLayer")) {
    inc_val <- raster::cellStats(income, stat = "median", na.rm = TRUE)
  } else if (is.data.frame(income)) {
    val_col <- intersect(c("monthly_median_income_usd", "income", "value"), names(income))[1]
    inc_val <- stats::median(as.numeric(income[[val_col]]), na.rm = TRUE)
  } else if (is.numeric(income)) {
    inc_val <- stats::median(income, na.rm = TRUE)
  } else {
    stop("Input 'income' must be a numeric value, data frame, or RasterLayer.")
  }

  if (is.na(inc_val) || inc_val <= 0) {
    stop("Household income must be greater than zero.")
  }

  # Annualize income if monthly
  inc_annual <- if (income_period == "monthly") inc_val * 12 else inc_val

  pir <- hc_val / inc_annual

  # UN-Habitat / World Bank tier categorization
  tier <- if (pir <= 3.0) {
    "Affordable"
  } else if (pir <= 4.0) {
    "Moderately Unaffordable"
  } else if (pir <= 5.0) {
    "Seriously Unaffordable"
  } else {
    "Severely Unaffordable"
  }

  res <- data.frame(
    indicator = rep("Housing affordability", 2),
    fclass = c("price_to_income_ratio", "affordability_tier"),
    value = c(round(pir, 2), NA_real_),
    units = c("ratio", tier),
    stringsAsFactors = FALSE
  )

  # Decile breakdown if provided
  if (!is.null(decile_data) && is.data.frame(decile_data)) {
    inc_col <- intersect(c("monthly_median_income_usd", "income", "monthly_income"), names(decile_data))[1]
    dec_col <- intersect(c("decile", "decil"), names(decile_data))[1]

    if (!is.na(inc_col) && !is.na(dec_col)) {
      dec_inc <- as.numeric(decile_data[[inc_col]])
      dec_annual <- if (income_period == "monthly") dec_inc * 12 else dec_inc
      dec_pir <- hc_val / dec_annual

      dec_tiers <- vapply(dec_pir, function(p) {
        if (p <= 3.0) "Affordable"
        else if (p <= 4.0) "Moderately Unaffordable"
        else if (p <= 5.0) "Seriously Unaffordable"
        else "Severely Unaffordable"
      }, character(1))

      attr(res, "decile_analysis") <- data.frame(
        decile = decile_data[[dec_col]],
        annual_income = dec_annual,
        housing_cost = hc_val,
        pir = round(dec_pir, 2),
        classification = dec_tiers,
        stringsAsFactors = FALSE
      )
    }
  }

  res
}
