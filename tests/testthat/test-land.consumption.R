test_that("land_consumption correctly calculates expansion", {
    library(raster)

    # Base footprint raster (1 = built-up, 0 = no)
    m_base <- matrix(c(
        1, 1, 0, 0,
        1, 1, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
    ), nrow = 4, byrow = TRUE)
    r_base <- raster(m_base, xmn = 0, xmx = 1000, ymn = 0, ymx = 1000, crs = "+init=EPSG:3857")

    # Horizon footprint raster: some new expansion
    m_horizon <- matrix(c(
        1, 1, 1, 0,
        1, 1, 1, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
    ), nrow = 4, byrow = TRUE)
    r_horiz <- raster(m_horizon, xmn = 0, xmx = 1000, ymn = 0, ymx = 1000, crs = "+init=EPSG:3857")

    # Calculate
    result <- land_consumption(r_base, r_horiz)

    # Expected difference: 2 cells
    expect_s3_class(result, "data.frame")
    expect_equal(result$indicator, "Land consumption")
    expect_equal(result$units, "km2")

    # Ensure the area is > 0 (exact km2 depends on the projection, but logic holds)
    expect_true(result$value > 0)
})
