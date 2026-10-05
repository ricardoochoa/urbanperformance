test_that("transit_utilization calculates nominal and crush utilization accurately", {
  routes <- data.frame(
    transit_mode = c("trunk_brt", "standard_bus"),
    nominal_vehicle_capacity = c(100, 45),
    crush_vehicle_capacity = c(150, 70),
    daily_trips = c(120, 80),
    daily_riders = c(9800, 3200)
  )

  res <- transit_utilization(routes, capacity_type = "both")

  expect_s3_class(res, "data.frame")

  tot_riders <- res$value[res$fclass == "total_ridership"]
  expect_equal(tot_riders, 13000)

  nom_cap <- res$value[res$fclass == "nominal_capacity"]
  expect_equal(nom_cap, 15600) # (120*100) + (80*45) = 12000 + 3600 = 15600

  crush_cap <- res$value[res$fclass == "crush_capacity"]
  expect_equal(crush_cap, 23600) # (120*150) + (80*70) = 18000 + 5600 = 23600

  nom_util <- res$value[grepl("nominal_utilization", res$fclass)]
  expect_equal(nom_util, round((13000 / 15600) * 100, 2))

  crush_util <- res$value[grepl("crush_utilization", res$fclass)]
  expect_equal(crush_util, round((13000 / 23600) * 100, 2))

  # Check route breakdown attribute
  expect_true(!is.null(attr(res, "route_breakdown")))
  df_break <- attr(res, "route_breakdown")
  expect_equal(nrow(df_break), 2)
  expect_equal(df_break$nominal_status[1], "Optimal")
  expect_equal(df_break$nominal_status[2], "Overcrowded")
})
