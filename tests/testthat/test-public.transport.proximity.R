test_that("public_transport_proximity calculates correct access stats", {
    library(raster)
    library(sf)

    # Create small pop raster
    m_pop <- matrix(10, nrow = 10, ncol = 10)
    r_pop <- raster(
        m_pop,
        xmn = 0,
        xmx = 1000,
        ymn = 0,
        ymx = 1000,
        crs = "+init=EPSG:3857"
    )

    # Create an sf point representing transit
    pts <- st_sf(
        fclass = c("bus_stop"),
        geometry = st_sfc(
            st_point(c(500, 500))
        ),
        crs = 3857
    )

    # Dummy parameters
    dummy_params <- data.frame(
        fclass = c("bus_stop"),
        value = c(500)
    )

    result <- public_transport_proximity(
        pts,
        pop = r_pop,
        parameters = dummy_params
    )

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 2)
    expect_equal(result$indicator[1], "Public transport proximity")
    expect_equal(result$units, c("inhabitants", "%"))
})
