test_that("tree_canopy_carbon projects canopy and carbon correctly", {
  params <- data.frame(
    species = c("Chacáh", "Jabín"),
    canopy_sqm = c(50.0, 60.0),
    foliar_co2seq_kg_yr = c(20.0, 25.0),
    annual_mortality_rate = c(0.02, 0.02),
    years_to_maturity = c(10, 10),
    stringsAsFactors = FALSE
  )

  # Test without base trees
  res <- tree_canopy_carbon(
    trees_base = NULL,
    pop = 100000,
    params = params,
    horizon_years = 5,
    annual_forestation = 1000,
    public_space_ratio = 0.5
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$indicator, c("Tree canopy percapita", "Carbon sequestration"))
  expect_equal(res$units, c("m2/inh", "tCO2/yr"))
  expect_true(res$value[1] > 0)
  expect_true(res$value[2] > 0)
})

test_that("tree_canopy_carbon handles existing tree inventory and spatial flag", {
  params <- data.frame(
    species = "Ceiba",
    canopy_sqm = 100,
    foliar_co2seq = 40,
    mortality_rate = 0.01,
    maturity_age = 15,
    stringsAsFactors = FALSE
  )

  pt <- sf::st_sfc(sf::st_point(c(0, 0)))
  trees_base <- sf::st_sf(species = "Ceiba", is_public = TRUE, geometry = pt)

  res <- tree_canopy_carbon(
    trees_base = trees_base,
    pop = 1000,
    params = params,
    horizon_years = 1,
    annual_forestation = 0,
    spatial = TRUE
  )

  expect_true(!is.null(attr(res, "species_breakdown")))
  expect_equal(as.character(attr(res, "species_breakdown")$species), "Ceiba")
})
