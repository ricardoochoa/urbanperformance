test_that("tot_pop calculations are correct with dummy rasters", {
    library(raster)

    # Create a small dummy raster
    m <- matrix(c(
        10, 20, 0, 0,
        0, 30, 0, 0,
        0, 0, 40, 50,
        0, 0, 0, 10
    ), nrow = 4, byrow = TRUE)
    r1 <- raster(m, xmn = 0, xmx = 10, ymn = 0, ymx = 10, crs = "+init=EPSG:3857")

    # Calculate total population
    result <- tot_pop(r1)

    # Expectations
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 1)
    expect_equal(result$indicator, "Total population")
    expect_equal(result$units, "inhabitants")

    # Total sum should be 160
    expect_equal(as.numeric(result$value), 160)
})

test_that("tot_pop works with raster stacks", {
    library(raster)

    m1 <- matrix(1:16, nrow = 4)
    r1 <- raster(m1)
    names(r1) <- "Year1"

    m2 <- matrix(rep(10, 16), nrow = 4)
    r2 <- raster(m2)
    names(r2) <- "Year2"

    s <- stack(r1, r2)

    result <- tot_pop(s)

    expect_equal(nrow(result), 2)
    expect_equal(result$fclass, c("Year1", "Year2"))
    expect_equal(as.numeric(result$value[1]), sum(1:16))
    expect_equal(as.numeric(result$value[2]), 160)
})
