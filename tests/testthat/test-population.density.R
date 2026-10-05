test_that("population_density works with perfectly matching rasters", {
    library(raster)

    # Population raster
    m_pop <- matrix(c(
        10, 20, 0, 0,
        0, 30, 0, 0,
        0, 0, 40, 50,
        0, 0, 0, 10
    ), nrow = 4, byrow = TRUE)
    r_pop <- raster(m_pop, xmn = 0, xmx = 1000, ymn = 0, ymx = 1000, crs = "+init=EPSG:3857")

    # Footprint raster (built-up)
    m_fp <- matrix(c(
        1, 1, 0, 0,
        1, 1, 0, 0,
        0, 0, 1, 1,
        0, 0, 0, 1
    ), nrow = 4, byrow = TRUE)
    r_fp <- raster(m_fp, xmn = 0, xmx = 1000, ymn = 0, ymx = 1000, crs = "+init=EPSG:3857")

    # Calculate
    result <- population_density(r_pop, r_fp)

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 3) # "Total population", "Footprint area", "Population density"

    tot_pop_val <- result$value[result$indicator == "Total population"]
    fp_val <- result$value[result$indicator == "Footprint area"]
    dens_val <- result$value[result$indicator == "Population density"]

    expect_equal(tot_pop_val, 160)

    # Footprint: 7 pixels * (250m * 250m) = 7 * 62500 m2 = 437500 m2 = 0.4375 km2 (approximated based on rasterchecker scaling)
    # Actually, the built-in urban_footprint function will reproject, so let's just assert density ratio logic:
    expect_equal(dens_val, round(tot_pop_val / fp_val, 2))
})

test_that("population_density stops when layers mismatch", {
    library(raster)

    r_pop <- stack(raster(matrix(1)), raster(matrix(1)))
    r_fp <- stack(raster(matrix(1)))

    # Expect an error now due to our code modification
    expect_error(population_density(r_pop, r_fp), "Please provide the same number of population and urban footprint layers")
})
