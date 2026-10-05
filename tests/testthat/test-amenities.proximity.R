test_that("amenities_proximity processes spatial data and respects deprecation", {
    library(raster)
    library(sf)

    # Create small pop raster
    m_pop <- matrix(10, nrow = 10, ncol = 10)
    r_pop <- raster(m_pop, xmn = 0, xmx = 1000, ymn = 0, ymx = 1000, crs = "+init=EPSG:3857")

    # Create an sf point representing an amenity
    pts <- st_sf(
        fclass = c("school", "hospital"),
        geometry = st_sfc(
            st_point(c(200, 200)),
            st_point(c(800, 800))
        ),
        crs = 3857
    )

    # Dummy parameters matching p.distances structure
    dummy_params <- data.frame(
        fclass = c("school", "hospital"),
        value = c(500, 1000)
    )

    # Should produce a deprecation warning about saving to global env
    expect_warning(
        result <- amenities_proximity(pts, pop = r_pop, parameters = dummy_params, save = TRUE),
        "The 'save' argument is deprecated"
    )

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 4) # 2 rows for each category (inhabitants and %)
    expect_true(all(result$indicator == "Amenities proximity"))
    expect_true(all(result$fclass %in% c("school", "hospital")))
})
