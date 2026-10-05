test_that("vacant_housing calculates rate with raster inputs", {
  library(raster)

  r_tot <- raster(extent(0, 100, 0, 100), res = 10, crs = CRS("+init=epsg:3857"))
  values(r_tot) <- 20
  r_vac <- raster(extent(0, 100, 0, 100), res = 10, crs = CRS("+init=epsg:3857"))
  values(r_vac) <- 1 # 5% vacancy

  res <- vacant_housing(r_tot, r_vac)

  expect_s3_class(res, "data.frame")
  expect_equal(res$indicator, c("Vacant housing", "Vacant housing"))
  expect_equal(res$units, c("units", "%"))
  expect_equal(res$value[res$units == "%"], 5)
})

test_that("vacant_housing calculates rate with sf polygon inputs", {
  library(sf)

  p1 <- st_polygon(list(matrix(c(0,0, 10,0, 10,10, 0,10, 0,0), ncol=2, byrow=TRUE)))
  blocks <- st_sf(total_units = 100, vacant_units = 4, geometry = st_sfc(p1))

  res <- vacant_housing(blocks)

  expect_equal(res$value[res$units == "%"], 4)
  expect_equal(res$value[res$units == "units"], 4)
})
