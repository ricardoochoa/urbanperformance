test_that("init_project creates standardized directory structure and metadata", {
  tmp_dir <- file.path(tempdir(), paste0("test_proj_", as.numeric(Sys.time())))
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  res <- init_project(tmp_dir, project_name = "Test_Project")

  expect_true(dir.exists(tmp_dir))
  expect_true(dir.exists(file.path(tmp_dir, "data", "raw")))
  expect_true(dir.exists(file.path(tmp_dir, "data", "processed")))
  expect_true(dir.exists(file.path(tmp_dir, "exports")))
  expect_true(dir.exists(file.path(tmp_dir, "metadata")))

  meta_file <- file.path(tmp_dir, "metadata", "project_state.json")
  expect_true(file.exists(meta_file))

  # Test loading project state
  state <- load_project_state(tmp_dir)
  expect_equal(state$project_name, "Test_Project")
  expect_true(is.list(state$parameters))

  # Test saving project state
  state$parameters$transit_buffer_m <- 950
  save_project_state(tmp_dir, state)

  reloaded <- load_project_state(tmp_dir)
  expect_equal(reloaded$parameters$transit_buffer_m, 950)
})

test_that("indicator_dependencies schema is valid", {
  schema_path <- system.file("schema", "indicator_dependencies.json", package = "urbanperformance")
  if (!file.exists(schema_path)) {
    schema_path <- file.path("..", "..", "inst", "schema", "indicator_dependencies.json")
  }
  expect_true(file.exists(schema_path))

  schema <- jsonlite::fromJSON(schema_path, simplifyVector = FALSE)
  expect_true(length(schema$layers) >= 15)
  expect_equal(length(schema$indicators), 27)
})

test_that("create_demo_project initializes Cancun demo workspace", {
  demo_dir <- file.path(tempdir(), paste0("test_cancun_", as.numeric(Sys.time())))
  on.exit(unlink(demo_dir, recursive = TRUE), add = TRUE)

  res <- create_demo_project(demo_dir)
  expect_true(dir.exists(demo_dir))

  state <- load_project_state(demo_dir)
  expect_equal(state$project_name, "Cancun_Urban_Assessment")
  expect_true(length(state$layers) > 0)
  expect_true("pop_base" %in% names(state$layers))
})
