test_that("anthropogenic_hazard calculates exposure without double counting", {
  library(sf)
  library(raster)

  # Create population raster
  r_pop <- raster(extent(0, 1000, 0, 1000), res = 100, crs = CRS("+init=epsg:3857"))
  values(r_pop) <- 10

  # Overlapping pollution sources
  pt1 <- st_point(c(300, 300))
  pt2 <- st_point(c(350, 300))
  pts <- st_sf(
    fclass = c("water_contamination", "water_contamination"),
    geometry = st_sfc(pt1, pt2),
    crs = 3857
  )

  lookup <- data.frame(
    fclass = "water_contamination",
    value = 200,
    description = "Test buffer",
    stringsAsFactors = FALSE
  )

  res <- anthropogenic_hazard(pts, r_pop, p_distances = lookup)

  expect_s3_class(res, "data.frame")
  expect_true(any(res$fclass == "water_contamination"))
  expect_true(any(res$units == "inhabitants"))
  expect_true(any(res$units == "%"))

  # Percentage must be <= 100%
  pct_val <- res$value[res$units == "%" & res$fclass == "water_contamination"]
  expect_true(pct_val <= 100)
  expect_true(pct_val > 0)
})
