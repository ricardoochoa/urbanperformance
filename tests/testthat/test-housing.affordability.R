test_that("housing_affordability computes PIR and applies UN tiers correctly", {
  # Monthly income: $1,000 -> Annual = $12,000. Housing cost = $36,000 -> PIR = 3.0 (Affordable)
  res_afford <- housing_affordability(housing_cost = 36000, income = 1000, income_period = "monthly")
  expect_equal(res_afford$value[1], 3.0)
  expect_equal(res_afford$units[2], "Affordable")

  # Moderately Unaffordable (PIR 3.5)
  res_mod <- housing_affordability(housing_cost = 42000, income = 1000, income_period = "monthly")
  expect_equal(res_mod$value[1], 3.5)
  expect_equal(res_mod$units[2], "Moderately Unaffordable")

  # Seriously Unaffordable (PIR 4.5)
  res_ser <- housing_affordability(housing_cost = 54000, income = 1000, income_period = "monthly")
  expect_equal(res_ser$value[1], 4.5)
  expect_equal(res_ser$units[2], "Seriously Unaffordable")

  # Severely Unaffordable (PIR 6.0)
  res_sev <- housing_affordability(housing_cost = 72000, income = 1000, income_period = "monthly")
  expect_equal(res_sev$value[1], 6.0)
  expect_equal(res_sev$units[2], "Severely Unaffordable")
})

test_that("housing_affordability handles decile breakdowns", {
  deciles <- data.frame(
    decile = 1:3,
    monthly_median_income_usd = c(300, 600, 1200)
  )
  res <- housing_affordability(housing_cost = 36000, income = 600, income_period = "monthly", decile_data = deciles)

  expect_true(!is.null(attr(res, "decile_analysis")))
  df_dec <- attr(res, "decile_analysis")
  expect_equal(nrow(df_dec), 3)
  expect_equal(df_dec$pir[1], 10.0) # 36000 / (300*12) = 10
  expect_equal(df_dec$pir[3], 2.5)  # 36000 / (1200*12) = 2.5
})
