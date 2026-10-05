test_that("housing_costs computes weighted unit costs correctly", {
  library(sf)
  library(raster)

  # Create a small decile raster
  r <- raster(extent(0, 200, 0, 200), res = 100, crs = CRS("+init=epsg:3857"))
  values(r) <- c(1, 2, 3, 4)

  # Create residential blocks
  p1 <- st_polygon(list(matrix(c(0,0, 100,0, 100,100, 0,100, 0,0), ncol=2, byrow=TRUE)))
  p2 <- st_polygon(list(matrix(c(100,100, 200,100, 200,200, 100,200, 100,100), ncol=2, byrow=TRUE)))
  blocks <- st_sf(id = 1:2, num_units = c(10, 20), geometry = st_sfc(p1, p2), crs = 3857)

  cost_params <- data.frame(
    decile = 1:10,
    land_cost_sqm = seq(50, 500, length.out = 10),
    constr_cost_sqm = seq(300, 1200, length.out = 10),
    avg_unit_sqm = seq(40, 150, length.out = 10)
  )

  res <- housing_costs(blocks, r, cost_params, num_units_col = "num_units")

  expect_s3_class(res, "data.frame")
  expect_equal(res$indicator, "Housing cost")
  expect_equal(res$units, "USD/unit")
  expect_true(res$value > 0)

  # Test spatial return
  spatial_res <- housing_costs(blocks, r, cost_params, spatial = TRUE)
  expect_s3_class(spatial_res, "sf")
  expect_true("housing_cost_unit" %in% names(spatial_res))
})
