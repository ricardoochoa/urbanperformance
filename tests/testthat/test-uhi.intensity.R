test_that("uhi_intensity calculates temperature differential excluding water", {
  library(raster)

  # Create small raster
  lc <- raster(extent(0, 400, 0, 400), res = 100, crs = CRS("+init=epsg:3857"))
  values(lc) <- 7 # Forest
  lc[2:3, 2:3] <- 2 # Urban center
  lc[1, 1] <- 3 # Water body in corner

  lst <- raster(extent(0, 400, 0, 400), res = 100, crs = CRS("+init=epsg:3857"))
  values(lst) <- 26 # Rural forest
  lst[2:3, 2:3] <- 32 # Urban
  lst[1, 1] <- 20 # Cool water

  res <- uhi_intensity(lst, lc, rural_buffer_km = 0, water_class = 3, urban_class = 2)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3)

  t_urb <- res$value[res$fclass == "urban_mean_temperature"]
  t_rur <- res$value[res$fclass == "rural_reference_temperature"]
  uhi <- res$value[res$fclass == "uhi_differential"]

  expect_equal(t_urb, 32)
  expect_equal(t_rur, 26) # Water (20) must be excluded, leaving forest (26)
  expect_equal(uhi, 6)
})
