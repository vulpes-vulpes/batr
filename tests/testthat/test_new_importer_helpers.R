# Tests for helper functions in R/new_importer.R

library(testthat)
library(batr)

context("new_importer helpers")

# ----------------------------------------------------------------------------
# .harmonize_location_columns
# ----------------------------------------------------------------------------

test_that(".harmonize_location_columns merges and coerces lat/lon variants", {
    df <- data.frame(
        `Loc.Position.Lat` = c(51.5, NA, 40.0),
        `Loc.Position.latitude` = c(NA, "52.1234", "40.0000"),
        `Loc.Position.Lon` = c(-0.12, NA, -74.0),
        `Loc.Position.longitude` = c(NA, "-0.2001", "-74.0060"),
        check.names = TRUE,
        stringsAsFactors = FALSE
    )
    out <- batr:::`.harmonize_location_columns`(df)
    expect_true(all(c("Latitude", "Longitude") %in% names(out)))
    expect_type(out$Latitude, "double")
    expect_type(out$Longitude, "double")
    # Row-wise expectations after coalesce and coercion
    expect_equal(out$Latitude[1], 51.5)
    expect_equal(out$Longitude[1], -0.12)
    expect_equal(out$Latitude[2], 52.1234)
    expect_equal(out$Longitude[2], -0.2001)
    expect_equal(out$Latitude[3], 40.0) # prefer capitalized when both present
    expect_equal(out$Longitude[3], -74.0) # prefer capitalized when both present
})

# ----------------------------------------------------------------------------
# .create_species_column
# ----------------------------------------------------------------------------

test_that(".create_species_column prefers manual and falls back to auto", {
    df <- data.frame(
        Species.Manual.ID = c("MYLU", NA, NA),
        Species.Auto.ID = c("EPFU", "MYSE", NA),
        stringsAsFactors = FALSE
    )
    out <- batr:::`.create_species_column`(df)
    expect_true("Species" %in% names(out))
    expect_equal(out$Species[1], "MYLU")
    expect_equal(out$Species[2], "MYSE")
    expect_true(is.na(out$Species[3]))
})

# ----------------------------------------------------------------------------
# .standardize_swift_locations
# ----------------------------------------------------------------------------

test_that(".standardize_swift_locations fixes coords per location for Swift", {
    df <- data.frame(
        Model = c("Swift", "Swift", "Other"),
        Location = c("A", "A", "B"),
        Latitude = c(10, 11, 20),
        Longitude = c(30, 31, 40),
        stringsAsFactors = FALSE
    )
    out <- batr:::`.standardize_swift_locations`(df)
    a_rows <- out[out$Model == "Swift" & out$Location == "A", ]
    expect_equal(unique(a_rows$Latitude), 10)
    expect_equal(unique(a_rows$Longitude), 30)
    # Non-swift unchanged
    b_row <- out[out$Location == "B", ]
    expect_equal(b_row$Latitude, 20)
    expect_equal(b_row$Longitude, 40)
})

# ----------------------------------------------------------------------------
# Validators
# ----------------------------------------------------------------------------

test_that(".validate_timezone accepts valid and rejects invalid timezones", {
    expect_silent(batr:::`.validate_timezone`("UTC"))
    expect_error(batr:::`.validate_timezone`("Not/AZone"))
})

test_that(".validate_input_path checks existence", {
    expect_silent(batr:::`.validate_input_path`(tempdir()))
    nonexistent <- file.path(tempdir(), "definitely_missing_dir_zzz")
    on.exit(unlink(nonexistent, recursive = TRUE), add = TRUE)
    if (dir.exists(nonexistent)) unlink(nonexistent, recursive = TRUE)
    expect_error(batr:::`.validate_input_path`(nonexistent))
})

# ----------------------------------------------------------------------------
# .missing_data_checker (non-interactive path)
# ----------------------------------------------------------------------------

test_that(".missing_data_checker flags missing fields (non-interactive)", {
    obs <- data.frame(
        File.Name = c("a.wav", "b.wav", "c.wav"),
        Site = c("A", NA, "C"),
        Latitude = c(51, 52, NA),
        Longitude = c(-1, NA, -3),
        stringsAsFactors = FALSE
    )

    # Mock interactive() to return FALSE so function doesn't prompt
    withr::local_options(list(batr.non_interactive = TRUE))

    # Or use testthat::local_mocked_bindings to mock readline
    testthat::local_mocked_bindings(
        readline = function(prompt) "y", # Default answer "no"
        .package = "base"
    )

    miss <- batr:::`.missing_data_checker`(obs, site_col = "Site")
    expect_s3_class(miss, "data.frame")
    expect_equal(sort(miss$File.Name), sort(c("b.wav", "c.wav")))

    # When site_col missing entirely, all files flagged
    obs2 <- obs
    obs2$Site <- NULL
    miss2 <- batr:::`.missing_data_checker`(obs2, site_col = "Site")
    expect_equal(nrow(miss2), nrow(obs2))
})
