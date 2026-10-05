test_that("walkability_index computes composite z-score over spatial grid", {
  library(sf)

  r1 <- st_linestring(matrix(c(0, 500, 2000, 500), ncol=2, byrow=TRUE))
  r2 <- st_linestring(matrix(c(500, 0, 500, 2000), ncol=2, byrow=TRUE))
  roads <- st_sf(geometry = st_sfc(r1, r2), crs = 3857)

  s1 <- st_point(c(500, 500))
  transit <- st_sf(geometry = st_sfc(s1), crs = 3857)

  fp <- st_polygon(list(matrix(c(0,0, 2000,0, 2000,2000, 0,2000, 0,0), ncol=2, byrow=TRUE)))
  footprint_sf <- st_sf(geometry = st_sfc(fp), crs = 3857)

  amenities <- st_sf(
    fclass = c("school", "grocery_store"),
    geometry = st_sfc(st_point(c(400, 400)), st_point(c(600, 600))),
    crs = 3857
  )

  res <- walkability_index(
    roads = roads,
    transit_stops = transit,
    footprint = footprint_sf,
    amenities = amenities,
    grid_size_m = 500,
    weights = c(0.33, 0.33, 0.34)
  )

  expect_s3_class(res, "data.frame")
  expect_equal(res$indicator, c("Walkability Index", "Walkability Index"))
  expect_equal(res$units, c("z-score", "%"))

  # Test spatial return
  spatial_res <- walkability_index(
    roads = roads,
    transit_stops = transit,
    footprint = footprint_sf,
    amenities = amenities,
    grid_size_m = 500,
    spatial = TRUE
  )
  expect_s3_class(spatial_res, "sf")
  expect_true("walkability_score" %in% names(spatial_res))
})
