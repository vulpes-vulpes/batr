# Tests for .read_file_guano and workflow functions using mocked bindings

library(testthat)
library(batr)

context("new_importer read & import flows")

# Helper: fabricate a stubbed read.guano returning metadata per file
stub_read_guano <- function(path) {
  fn <- basename(path)
  # Vary fields by filename to exercise branches
  if (fn == "bad.wav") stop("corrupt file")
  # Location field name will be provided by tests via site_col
  loc_val <- if (grepl("A", fn)) "LocA" else "LocB"
  # Alternate between lower/upper loc.position fields and types
  lat <- if (grepl("1", fn)) list(name = "Loc Position latitude", value = "51.5000") else list(name = "Loc Position Lat", value = 52.25)
  lon <- if (grepl("1", fn)) list(name = "Loc Position Lon", value = -0.12) else list(name = "Loc Position longitude", value = "-0.2001")
  # Species: sometimes manual missing
  sp_manual <- if (grepl("2", fn)) NA_character_ else "MYLU"
  sp_auto <- if (is.na(sp_manual)) "EPFU" else "EPFU"
  # Model: make one Swift to test standardization
  model <- if (grepl("A", fn)) "Swift" else "Other"
  # Compose list with GUANO-like names (spaces)
  res <- list(
    Timestamp = "2023-01-01 13:00:00",
    `File Name` = fn,
    `File Path` = path,
    Model = model,
    `Species Manual ID` = sp_manual,
    `Species Auto ID` = sp_auto
  )
  res[[lat$name]] <- lat$value
  res[[lon$name]] <- lon$value
  # Add location field name dynamic; use "Site" key
  res[["Site"]] <- loc_val
  res
}

# Build a file_list data.frame for paths
make_file_list_df <- function(paths) {
  data.frame(
    File.Name = basename(paths),
    Full.Path = paths,
    File.Modified = as.numeric(Sys.time()),
    stringsAsFactors = FALSE
  )
}

# ----------------------------------------------------------------------------
# .read_file_guano with stubbed read.guano
# ----------------------------------------------------------------------------

test_that(".read_file_guano processes metadata, types, and filtering", {
  td <- tempfile(pattern = "wavdir_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  paths <- file.path(td, c("recA1.wav", "recB2.wav", "bad.wav"))
  for (p in paths) writeBin(raw(), p)
  fl <- make_file_list_df(paths)

  testthat::with_mocked_bindings(
    {
      out <- suppressWarnings(batr:::`.read_file_guano`(fl, site_col = "Site", timezone = "UTC", fast_import = FALSE))
      # bad.wav should be excluded due to read error
      expect_true(all(out$File.Name %in% basename(paths[basename(paths) != "bad.wav"])))
      expect_true(all(c("Timestamp", "Species", "Location", "Latitude", "Longitude", "Night") %in% names(out)))
      expect_type(out$Latitude, "double")
      expect_type(out$Longitude, "double")
      # Species prefers manual when present, else auto
      a_row <- out[out$File.Name == "recA1.wav", ]
      b_row <- out[out$File.Name == "recB2.wav", ]
      expect_equal(a_row$Species, "MYLU")
      expect_equal(b_row$Species, "EPFU")
      # Night computed as date
      expect_s3_class(out$Night, "Date")
      # Location rename from Site
      expect_true(all(out$Location %in% c("LocA", "LocB")))
    },
    read.guano = stub_read_guano
  )
})

# ----------------------------------------------------------------------------
# Workflow functions (.new_observations, .add_observations, .update_observations)
# ----------------------------------------------------------------------------

test_that(".new/.add/.update_observations work end-to-end with mocks", {
  # Prepare two directories representing two import waves
  d1 <- tempfile("wave1_")
  dir.create(d1)
  d2 <- tempfile("wave2_")
  dir.create(d2)
  on.exit(unlink(c(d1, d2), recursive = TRUE), add = TRUE)
  f1 <- file.path(d1, c("recA1.wav", "recB2.wav"))
  for (p in f1) writeBin(raw(), p)
  f2 <- file.path(d2, c("recA1.wav", "recC3.wav"))
  for (p in f2) writeBin(raw(), p)

  # Data path for RData
  rdata <- tempfile(fileext = ".RData")

  # Mock .save_to_rdata to actually save object, and rely on real .get_file_list (standard mode)
  # Also mock read.guano
  saver <- function(observations, data_path) save(observations, file = data_path)

  # Helper: call .get_file_list with fast_import = FALSE for stability
  testthat::with_mocked_bindings(
    {
      # NEW
      expect_message(batr:::`.new_observations`(d1, site_col = "Site", timezone = "UTC", data_path = rdata, fast_import = FALSE))
      expect_true(file.exists(rdata))
      env <- new.env()
      load(rdata, envir = env)
      expect_true(exists("observations", envir = env))
      expect_equal(nrow(env$observations), 2)

      # ADD (should add only the new file recC3.wav)
      expect_message(batr:::`.add_observations`(d2, site_col = "Site", timezone = "UTC", data_path = rdata, fast_import = FALSE))
      env2 <- new.env()
      load(rdata, envir = env2)
      expect_equal(nrow(env2$observations), 3)

      # UPDATE: simulate that recA1.wav got a newer modified time
      # We mock .get_file_list to control mtimes and to support list=TRUE calls
    },
    read.guano = stub_read_guano,
    .save_to_rdata = saver
  )

  # Custom get_file_list mock for update test
  now <- as.numeric(Sys.time())
  old_df <- make_file_list_df(f1)
  new_df <- make_file_list_df(f2)
  # Make recA1.wav in wave2 newer
  new_df$File.Modified[new_df$File.Name == "recA1.wav"] <- now + 100
  get_file_list_mock <- function(input_path, fast_import = TRUE, list = FALSE) {
    if (isTRUE(list)) {
      # input_path is a vector of paths to update; return rows from new_df
      fps <- as.character(input_path)
      sub <- new_df[new_df$Full.Path %in% fps, ]
      if (nrow(sub) == 0) {
        # Fallback: match by filename
        sub <- new_df[new_df$File.Name %in% basename(fps), ]
      }
      return(sub)
    }
    # If directory matches d1 or d2, return corresponding df
    if (identical(normalizePath(input_path), normalizePath(d1))) {
      return(old_df)
    }
    if (identical(normalizePath(input_path), normalizePath(d2))) {
      return(new_df)
    }
    stop("Unexpected input_path to mock")
  }

  # Run update with mocks for read.guano, .save_to_RDATA, .get_file_list
  testthat::with_mocked_bindings(
    {
      expect_message(batr:::`.update_observations`(d2, site_col = "Site", timezone = "UTC", data_path = rdata, fast_import = FALSE))
      env3 <- new.env()
      load(rdata, envir = env3)
      # After update, still 3 rows, but recA1.wav metadata re-read; at least confirm presence
      expect_setequal(env3$observations$File.Name, c("recA1.wav", "recB2.wav", "recC3.wav"))
    },
    read.guano = stub_read_guano,
    .save_to_rdata = saver,
    .get_file_list = get_file_list_mock
  )
})

# ----------------------------------------------------------------------------
# import_guano main function tests
# ----------------------------------------------------------------------------

test_that("import_guano validates action parameter", {
  expect_error(
    import_guano(
      action = "Invalid", input_path = tempdir(),
      site_col = "Site", timezone = "UTC"
    ),
    "Invalid action given"
  )
})

test_that("import_guano New action creates RData file with mocked components", {
  skip_on_cran()

  td <- tempfile(pattern = "wavdir_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  paths <- file.path(td, c("recA1.wav", "recB2.wav"))
  for (p in paths) writeBin(raw(), p)

  rdata <- tempfile(fileext = ".RData")
  on.exit(unlink(rdata), add = TRUE)

  # Mock components
  testthat::with_mocked_bindings(
    {
      # Suppress interactive prompts by providing data_path
      result <- import_guano(
        action = "New",
        input_path = td,
        site_col = "Site",
        timezone = "UTC",
        data_path = rdata,
        fast_import = FALSE
      )

      # Check that RData file was created
      expect_true(file.exists(rdata))

      # Load and verify contents
      env <- new.env()
      load(rdata, envir = env)
      expect_true("observations" %in% ls(env))
      expect_s3_class(env$observations, "data.frame")
      expect_true(nrow(env$observations) > 0)
    },
    read.guano = stub_read_guano
  )
})
