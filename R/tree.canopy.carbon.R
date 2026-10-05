#' Tree Canopy Cover and Carbon Sequestration
#'
#' This function projects tree population dynamics, calculating public tree
#' canopy cover per capita and annual carbon sequestration across a simulated
#' policy horizon.
#'
#' METHOD:
#' The algorithm accounts for the survival of the existing tree inventory and
#' compounding annual municipal tree planting cohorts. To avoid overestimating
#' early-horizon canopy, newly planted cohorts are subject to an age-dependent
#' maturity discount factor based on species-specific years to maturity.
#' Foliar CO2 sequestration is calculated for all trees (public and private)
#' and converted from kilograms to metric tonnes per year.
#'
#' @param trees_base `sf` object (points) of current urban trees with attributes
#'   `species` (matching `params$species`) and `is_public` (logical). Can also be
#'   NULL or empty `sf` object if simulating from a baseline of zero trees.
#' @param pop `RasterLayer` of population distribution or numeric value with total population.
#' @param params `data.frame` containing species-specific forestry parameters with columns:
#'   \describe{
#'     \item{species}{Common or scientific species name}
#'     \item{canopy_sqm}{Average mature crown projection area in square meters}
#'     \item{foliar_co2seq}{Annual CO2 capture in kg/year (or `foliar_co2seq_kg_yr`)}
#'     \item{mortality_rate}{Annual mortality rate (0 to 1, or `annual_mortality_rate`)}
#'     \item{maturity_age}{Years required to reach mature crown size (or `years_to_maturity`)}
#'   }
#' @param horizon_years Numeric. Simulated time horizon in years (default: 10).
#' @param annual_forestation Numeric. Number of trees planted per year (default: 3500).
#' @param public_space_ratio Numeric. Proportion of planted trees located in public space (default: 0.45).
#' @param planting_share Numeric vector or named vector with planting allocation across species.
#'   If NULL, trees are allocated equally among all species in `params`.
#' @param spatial Logical. If TRUE, includes per-species summary details in attributes. Default FALSE.
#'
#' @return A data frame containing tree canopy per capita (m2/inh) and annual carbon
#'   sequestration (tCO2/yr).
#' @export
#'
#' @examples
#' \dontrun{
#' params <- data.frame(
#'   species = c("Chacáh", "Jabín", "Chechén", "Ceiba"),
#'   canopy_sqm = c(50.2, 60.0, 45.0, 175.0),
#'   foliar_co2seq = c(22.5, 25.5, 21.0, 45.0),
#'   mortality_rate = c(0.025, 0.020, 0.018, 0.015),
#'   maturity_age = c(8, 12, 14, 15)
#' )
#' tree_canopy_carbon(trees_base = NULL, pop = 900000, params = params, horizon_years = 10)
#' }
tree_canopy_carbon <- function(trees_base = NULL,
                               pop,
                               params,
                               horizon_years = 10,
                               annual_forestation = 3500,
                               public_space_ratio = 0.45,
                               planting_share = NULL,
                               spatial = FALSE) {
  # Validate population input
  if (is.numeric(pop)) {
    t_pop <- pop
  } else if (inherits(pop, "RasterLayer")) {
    t_pop <- as.numeric(tot_pop(pop)$value)
  } else {
    stop("Input 'pop' must be a numeric value or a RasterLayer.")
  }

  if (t_pop <= 0) {
    stop("Total population must be greater than zero.")
  }

  # Normalize params column names
  col_map <- list(
    species = c("species", "species_common", "species_scientific"),
    canopy_sqm = c("canopy_sqm", "crown_sqm", "canopy"),
    foliar_co2seq = c("foliar_co2seq", "foliar_co2seq_kg_yr", "co2_seq_kg"),
    mortality_rate = c("mortality_rate", "annual_mortality_rate", "mortality"),
    maturity_age = c("maturity_age", "years_to_maturity", "maturity")
  )

  params_clean <- params
  for (std_name in names(col_map)) {
    found <- intersect(col_map[[std_name]], names(params_clean))
    if (length(found) > 0) {
      names(params_clean)[names(params_clean) == found[1]] <- std_name
    } else if (!std_name %in% names(params_clean)) {
      stop(paste0("Required column '", std_name, "' not found in 'params' data frame."))
    }
  }

  n_species <- nrow(params_clean)
  if (n_species == 0) {
    stop("'params' must contain at least one species.")
  }

  # Normalize planting shares
  if (is.null(planting_share)) {
    shares <- rep(1 / n_species, n_species)
    names(shares) <- params_clean$species
  } else {
    if (is.null(names(planting_share))) {
      if (length(planting_share) != n_species) {
        stop("Length of unnamed 'planting_share' must match the number of species in 'params'.")
      }
      shares <- planting_share / sum(planting_share)
      names(shares) <- params_clean$species
    } else {
      shares <- numeric(n_species)
      names(shares) <- params_clean$species
      for (sp in params_clean$species) {
        if (sp %in% names(planting_share)) {
          shares[sp] <- planting_share[sp]
        }
      }
      if (sum(shares) == 0) {
        shares <- rep(1 / n_species, n_species)
        names(shares) <- params_clean$species
      } else {
        shares <- shares / sum(shares)
      }
    }
  }

  # Tally existing trees
  base_counts <- numeric(n_species)
  names(base_counts) <- params_clean$species
  base_public_counts <- numeric(n_species)
  names(base_public_counts) <- params_clean$species

  if (!is.null(trees_base) && inherits(trees_base, "sf") && nrow(trees_base) > 0) {
    species_col <- intersect(c("species", "species_common", "species_scientific"), names(trees_base))[1]
    if (is.na(species_col)) {
      stop("trees_base sf object must have a 'species' column.")
    }
    public_col <- intersect(c("is_public", "public"), names(trees_base))[1]

    for (i in seq_len(n_species)) {
      sp_name <- params_clean$species[i]
      match_rows <- trees_base[[species_col]] == sp_name
      base_counts[i] <- sum(match_rows, na.rm = TRUE)
      if (!is.na(public_col)) {
        base_public_counts[i] <- sum(match_rows & as.logical(trees_base[[public_col]]), na.rm = TRUE)
      } else {
        base_public_counts[i] <- base_counts[i] * public_space_ratio
      }
    }
  }

  # Simulation calculations per species
  tot_public_canopy_sqm <- 0
  tot_co2_kg <- 0

  species_results <- vector("list", n_species)

  for (i in seq_len(n_species)) {
    sp <- params_clean$species[i]
    m_rate <- params_clean$mortality_rate[i]
    canopy_mature <- params_clean$canopy_sqm[i]
    co2_kg_annual <- params_clean$foliar_co2seq[i]
    mat_years <- max(1, params_clean$maturity_age[i])
    sp_share <- shares[sp]

    # 1. Base population survival
    surv_rate_horizon <- (1 - m_rate)^horizon_years
    surv_base_total <- base_counts[sp] * surv_rate_horizon
    surv_base_public <- base_public_counts[sp] * surv_rate_horizon

    # Base trees are assumed mature
    canopy_from_base_public <- surv_base_public * canopy_mature

    # 2. Planted cohorts survival and canopy (cohorts year 1 to horizon_years)
    annual_n_planted <- annual_forestation * sp_share
    surv_planted_total <- 0
    canopy_from_planted_total <- 0

    if (horizon_years > 0 && annual_n_planted > 0) {
      for (yr in seq_len(horizon_years)) {
        # Cohort planted in year yr:
        # Years alive at horizon end = horizon_years - yr + 1
        age <- horizon_years - yr + 1
        surviving_cohort <- annual_n_planted * ((1 - m_rate)^(horizon_years - yr))
        maturity_factor <- min(1.0, age / mat_years)

        surv_planted_total <- surv_planted_total + surviving_cohort
        canopy_from_planted_total <- canopy_from_planted_total +
          (surviving_cohort * canopy_mature * maturity_factor)
      }
    }

    # Public portion of planted canopy
    canopy_from_planted_public <- canopy_from_planted_total * public_space_ratio

    # Total canopy and CO2 for this species
    sp_public_canopy <- canopy_from_base_public + canopy_from_planted_public
    sp_total_trees <- surv_base_total + surv_planted_total
    sp_co2_kg <- sp_total_trees * co2_kg_annual

    tot_public_canopy_sqm <- tot_public_canopy_sqm + sp_public_canopy
    tot_co2_kg <- tot_co2_kg + sp_co2_kg

    species_results[[i]] <- data.frame(
      species = sp,
      surviving_base_trees = round(surv_base_total, 1),
      surviving_planted_trees = round(surv_planted_total, 1),
      total_trees = round(sp_total_trees, 1),
      public_canopy_sqm = round(sp_public_canopy, 1),
      co2_kg_yr = round(sp_co2_kg, 1)
    )
  }

  canopy_pcapita <- tot_public_canopy_sqm / t_pop
  co2_tonnes_yr <- tot_co2_kg / 1000

  res <- data.frame(
    indicator = c("Tree canopy percapita", "Carbon sequestration"),
    fclass = c("public_canopy", "total_foliar"),
    value = c(round(canopy_pcapita, 2), round(co2_tonnes_yr, 2)),
    units = c("m2/inh", "tCO2/yr"),
    stringsAsFactors = FALSE
  )

  if (spatial) {
    attr(res, "species_breakdown") <- do.call(rbind, species_results)
  }

  res
}
