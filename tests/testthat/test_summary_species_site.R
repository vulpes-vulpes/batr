# Tests for summary_table and helper functions in R/species_site_summary.R

library(testthat)
library(batr)

context("summary_table and helpers")

# ----------------------------------------------------------------------------
# .load_and_validate_summary_data
# ----------------------------------------------------------------------------

test_that(".load_and_validate_summary_data validates inputs correctly", {
    # Create test data
    observations <- data.frame(
        Species = c("Epfu", "Labo"),
        Location = c("Site1", "Site2")
    )
    temp_rdata <- tempfile(fileext = ".RData")
    save(observations, file = temp_rdata)

    # Valid inputs
    expect_silent(batr:::`.load_and_validate_summary_data`(temp_rdata, NULL, NULL))
    expect_silent(batr:::`.load_and_validate_summary_data`(temp_rdata, c("Epfu"), NULL))
    expect_silent(batr:::`.load_and_validate_summary_data`(temp_rdata, NULL, c("Site1")))

    # Invalid species_list
    expect_error(
        batr:::`.load_and_validate_summary_data`(temp_rdata, character(0), NULL),
        "species_list must be a character vector with at least one element"
    )
    expect_error(
        batr:::`.load_and_validate_summary_data`(temp_rdata, 123, NULL),
        "species_list must be a character vector"
    )

    # Invalid location_list
    expect_error(
        batr:::`.load_and_validate_summary_data`(temp_rdata, NULL, character(0)),
        "location_list must be a character vector with at least one element"
    )
    expect_error(
        batr:::`.load_and_validate_summary_data`(temp_rdata, NULL, 123),
        "location_list must be a character vector"
    )

    unlink(temp_rdata)
})

test_that(".load_and_validate_summary_data checks for observations object", {
    # Create RData without observations (using explicit envir to avoid pollution)
    temp_env <- new.env()
    temp_env$other_data <- data.frame(x = 1:5)
    temp_rdata <- tempfile(fileext = ".RData")
    save(list = "other_data", file = temp_rdata, envir = temp_env)

    expect_error(
        batr:::`.load_and_validate_summary_data`(temp_rdata, NULL, NULL),
        "No 'observations' object found"
    )

    unlink(temp_rdata)
})

# ----------------------------------------------------------------------------
# .clean_summary_dataset
# ----------------------------------------------------------------------------

test_that(".clean_summary_dataset removes NA species and locations", {
    dataset <- data.frame(
        Species = c("Epfu", NA, "Labo", NA, "Epfu"),
        Location = c("Site1", "Site2", NA, NA, "Site3")
    )

    expect_message(
        result <- batr:::`.clean_summary_dataset`(dataset),
        "Removing 3 observation\\(s\\) with missing data"
    )

    expect_equal(nrow(result), 2)
    expect_true(all(!is.na(result$Species)))
    expect_true(all(!is.na(result$Location)))
})

test_that(".clean_summary_dataset reports NA counts correctly", {
    dataset <- data.frame(
        Species = c("Epfu", NA, "Labo", NA),
        Location = c("Site1", "Site2", NA, NA)
    )

    expect_message(
        batr:::`.clean_summary_dataset`(dataset),
        "1 with NA species"
    )
    expect_message(
        batr:::`.clean_summary_dataset`(dataset),
        "1 with NA location"
    )
    expect_message(
        batr:::`.clean_summary_dataset`(dataset),
        "1 with both NA"
    )
})

test_that(".clean_summary_dataset errors on all NA data", {
    dataset <- data.frame(
        Species = c(NA, NA),
        Location = c(NA, NA)
    )

    expect_error(
        batr:::`.clean_summary_dataset`(dataset),
        "No valid observations found after removing NA values"
    )
})

test_that(".clean_summary_dataset handles clean data silently", {
    dataset <- data.frame(
        Species = c("Epfu", "Labo"),
        Location = c("Site1", "Site2")
    )

    expect_silent(result <- batr:::`.clean_summary_dataset`(dataset))
    expect_equal(nrow(result), 2)
})

# ----------------------------------------------------------------------------
# .filter_summary_dataset
# ----------------------------------------------------------------------------

test_that(".filter_summary_dataset filters by location_list", {
    dataset <- data.frame(
        Species = c("Epfu", "Labo", "Epfu", "Laci"),
        Location = c("Site1", "Site1", "Site2", "Site3")
    )

    result <- batr:::`.filter_summary_dataset`(dataset, NULL, c("Site1", "Site2"))
    expect_equal(nrow(result), 3)
    expect_true(all(result$Location %in% c("Site1", "Site2")))
})

test_that(".filter_summary_dataset filters by species_list", {
    dataset <- data.frame(
        Species = c("Epfu", "Labo", "Epfu", "Laci"),
        Location = c("Site1", "Site1", "Site2", "Site3")
    )

    result <- batr:::`.filter_summary_dataset`(dataset, c("Epfu", "Labo"), NULL)
    expect_equal(nrow(result), 3)
    expect_true(all(result$Species %in% c("Epfu", "Labo")))
})

test_that(".filter_summary_dataset warns about missing species", {
    dataset <- data.frame(
        Species = c("Epfu", "Labo"),
        Location = c("Site1", "Site2")
    )

    expect_message(
        batr:::`.filter_summary_dataset`(dataset, c("Epfu", "Mylu"), NULL),
        "1 species not found in data: Mylu"
    )
})

test_that(".filter_summary_dataset warns about missing locations", {
    dataset <- data.frame(
        Species = c("Epfu", "Labo"),
        Location = c("Site1", "Site2")
    )

    expect_message(
        batr:::`.filter_summary_dataset`(dataset, NULL, c("Site1", "Site3")),
        "1 location\\(s\\) not found in data: Site3"
    )
})

test_that(".filter_summary_dataset returns empty table when no matches", {
    dataset <- data.frame(
        Species = c("Epfu", "Labo"),
        Location = c("Site1", "Site2")
    )

    expect_warning(
        result <- batr:::`.filter_summary_dataset`(dataset, c("Mylu"), NULL),
        "No observations match the specified species filter"
    )
    expect_equal(nrow(result), 0)
    expect_true("Mylu" %in% names(result))
})

# ----------------------------------------------------------------------------
# .build_summary_table
# ----------------------------------------------------------------------------

test_that(".build_summary_table creates correct count table", {
    dataset <- data.frame(
        Species = c("Epfu", "Epfu", "Labo", "Epfu"),
        Location = c("Site1", "Site1", "Site1", "Site2")
    )

    result <- batr:::`.build_summary_table`(dataset, NULL, NULL, FALSE)

    expect_equal(nrow(result), 2)
    expect_equal(result$Location, c("Site1", "Site2"))
    expect_equal(result$Epfu, c(2, 1))
    expect_equal(result$Labo, c(1, 0))
})

test_that(".build_summary_table respects species_list order", {
    dataset <- data.frame(
        Species = c("Epfu", "Labo", "Laci"),
        Location = c("Site1", "Site1", "Site1")
    )

    result <- batr:::`.build_summary_table`(dataset, c("Laci", "Epfu", "Labo"), NULL, FALSE)

    expect_equal(names(result), c("Location", "Laci", "Epfu", "Labo"))
})

test_that(".build_summary_table respects location_list order", {
    dataset <- data.frame(
        Species = c("Epfu", "Epfu", "Epfu"),
        Location = c("Site1", "Site2", "Site3")
    )

    result <- batr:::`.build_summary_table`(dataset, NULL, c("Site3", "Site1", "Site2"), FALSE)

    expect_equal(result$Location, c("Site3", "Site1", "Site2"))
})

test_that(".build_summary_table adds missing species columns with zeros", {
    dataset <- data.frame(
        Species = c("Epfu", "Epfu"),
        Location = c("Site1", "Site2")
    )

    result <- batr:::`.build_summary_table`(dataset, c("Epfu", "Labo", "Mylu"), NULL, FALSE)

    expect_true("Labo" %in% names(result))
    expect_true("Mylu" %in% names(result))
    expect_equal(result$Labo, c(0, 0))
    expect_equal(result$Mylu, c(0, 0))
})

test_that(".build_summary_table adds missing location rows with zeros", {
    dataset <- data.frame(
        Species = c("Epfu", "Labo"),
        Location = c("Site1", "Site1")
    )

    result <- batr:::`.build_summary_table`(dataset, NULL, c("Site1", "Site2", "Site3"), FALSE)

    expect_equal(nrow(result), 3)
    expect_equal(result$Location, c("Site1", "Site2", "Site3"))
    expect_equal(result$Epfu[2], 0)
    expect_equal(result$Labo[3], 0)
})

test_that(".build_summary_table sorts alphabetically when sort=TRUE", {
    dataset <- data.frame(
        Species = c("Mylu", "Epfu", "Labo"),
        Location = c("Site3", "Site1", "Site2")
    )

    result <- batr:::`.build_summary_table`(dataset, NULL, NULL, TRUE)

    expect_equal(result$Location, c("Site1", "Site2", "Site3"))
    expect_equal(names(result), c("Location", "Epfu", "Labo", "Mylu"))
})

test_that(".build_summary_table handles empty dataset", {
    dataset <- data.frame(
        Species = character(0),
        Location = character(0)
    )

    result <- batr:::`.build_summary_table`(dataset, NULL, NULL, FALSE)
    expect_equal(nrow(result), 0)
})

# ----------------------------------------------------------------------------
# .add_totals_row
# ----------------------------------------------------------------------------

test_that(".add_totals_row adds correct totals", {
    summary_table <- data.frame(
        Location = c("Site1", "Site2"),
        Epfu = c(5, 3),
        Labo = c(2, 4)
    )

    result <- batr:::`.add_totals_row`(summary_table)

    expect_equal(nrow(result), 3)
    expect_equal(result$Location[3], "Totals")
    expect_equal(result$Epfu[3], 8)
    expect_equal(result$Labo[3], 6)
})

test_that(".add_totals_row handles empty table", {
    summary_table <- data.frame(Location = character(0))

    result <- batr:::`.add_totals_row`(summary_table)
    expect_equal(nrow(result), 0)
})

test_that(".add_totals_row handles NA values", {
    summary_table <- data.frame(
        Location = c("Site1", "Site2"),
        Epfu = c(5, NA),
        Labo = c(2, 4)
    )

    result <- batr:::`.add_totals_row`(summary_table)
    expect_equal(result$Epfu[3], 5) # NA ignored in sum
    expect_equal(result$Labo[3], 6)
})

# ----------------------------------------------------------------------------
# .create_empty_summary_table
# ----------------------------------------------------------------------------

test_that(".create_empty_summary_table creates correct structure", {
    result <- batr:::`.create_empty_summary_table`(c("Epfu", "Labo", "Mylu"))

    expect_equal(nrow(result), 0)
    expect_equal(names(result), c("Location", "Epfu", "Labo", "Mylu"))
    expect_type(result$Epfu, "integer")
})

test_that(".create_empty_summary_table handles NULL species_list", {
    result <- batr:::`.create_empty_summary_table`(NULL)

    expect_equal(nrow(result), 0)
    expect_equal(names(result), "Location")
})

# ----------------------------------------------------------------------------
# Integration tests for summary_table
# ----------------------------------------------------------------------------

test_that("summary_table works end-to-end with default parameters", {
    skip_on_cran()

    observations <- data.frame(
        Species = c("Epfu", "Epfu", "Labo", "Labo", "Epfu"),
        Location = c("Site1", "Site1", "Site1", "Site2", "Site2"),
        stringsAsFactors = FALSE
    )

    temp_rdata <- tempfile(fileext = ".RData")
    save(observations, file = temp_rdata)

    result <- summary_table(temp_rdata)

    expect_equal(nrow(result), 3) # 2 sites + totals
    expect_equal(result$Location, c("Site1", "Site2", "Totals"))
    expect_equal(result$Epfu, c(2, 1, 3))
    expect_equal(result$Labo, c(1, 1, 2))

    unlink(temp_rdata)
})

test_that("summary_table filters by species_list", {
    skip_on_cran()

    observations <- data.frame(
        Species = c("Epfu", "Labo", "Mylu", "Epfu"),
        Location = c("Site1", "Site1", "Site1", "Site2"),
        stringsAsFactors = FALSE
    )

    temp_rdata <- tempfile(fileext = ".RData")
    save(observations, file = temp_rdata)

    result <- summary_table(temp_rdata, species_list = c("Epfu", "Labo"))

    expect_true("Epfu" %in% names(result))
    expect_true("Labo" %in% names(result))
    expect_false("Mylu" %in% names(result))

    unlink(temp_rdata)
})

test_that("summary_table applies species_names labels", {
    skip_on_cran()

    observations <- data.frame(
        Species = c("Epfu", "Labo", "Epfu"),
        Location = c("Site1", "Site1", "Site2"),
        stringsAsFactors = FALSE
    )

    temp_rdata <- tempfile(fileext = ".RData")
    save(observations, file = temp_rdata)

    species_labels <- c("Epfu" = "Big Brown Bat", "Labo" = "Hoary Bat")
    result <- summary_table(temp_rdata, species_names = species_labels)

    expect_true("Big Brown Bat" %in% names(result))
    expect_true("Hoary Bat" %in% names(result))
    expect_false("Epfu" %in% names(result))
    expect_false("Labo" %in% names(result))

    unlink(temp_rdata)
})

test_that("summary_table filters by location_list", {
    skip_on_cran()

    observations <- data.frame(
        Species = c("Epfu", "Epfu", "Epfu"),
        Location = c("Site1", "Site2", "Site3"),
        stringsAsFactors = FALSE
    )

    temp_rdata <- tempfile(fileext = ".RData")
    save(observations, file = temp_rdata)

    result <- summary_table(temp_rdata, location_list = c("Site1", "Site3"))

    expect_equal(nrow(result), 3) # 2 sites + totals
    expect_true(all(result$Location[1:2] %in% c("Site1", "Site3")))

    unlink(temp_rdata)
})

test_that("summary_table respects include_totals=FALSE", {
    skip_on_cran()

    observations <- data.frame(
        Species = c("Epfu", "Labo"),
        Location = c("Site1", "Site2"),
        stringsAsFactors = FALSE
    )

    temp_rdata <- tempfile(fileext = ".RData")
    save(observations, file = temp_rdata)

    result <- summary_table(temp_rdata, include_totals = FALSE)

    expect_equal(nrow(result), 2) # No totals row
    expect_false("Totals" %in% result$Location)

    unlink(temp_rdata)
})

test_that("summary_table sorts alphabetically when sort=TRUE", {
    skip_on_cran()

    observations <- data.frame(
        Species = c("Mylu", "Epfu", "Labo", "Epfu"),
        Location = c("Site3", "Site1", "Site2", "Site1"),
        stringsAsFactors = FALSE
    )

    temp_rdata <- tempfile(fileext = ".RData")
    save(observations, file = temp_rdata)

    result <- summary_table(temp_rdata, sort = TRUE)

    # Check locations sorted (excluding Totals)
    expect_equal(result$Location[1:3], c("Site1", "Site2", "Site3"))

    # Check species sorted
    expect_equal(names(result), c("Location", "Epfu", "Labo", "Mylu"))

    unlink(temp_rdata)
})

test_that("summary_table handles NA values in data", {
    skip_on_cran()

    observations <- data.frame(
        Species = c("Epfu", NA, "Labo", "Epfu"),
        Location = c("Site1", "Site2", NA, "Site2"),
        stringsAsFactors = FALSE
    )

    temp_rdata <- tempfile(fileext = ".RData")
    save(observations, file = temp_rdata)

    expect_message(
        result <- summary_table(temp_rdata),
        "Removing 2 observation\\(s\\) with missing data"
    )

    expect_equal(nrow(result), 3) # 2 valid rows + totals

    unlink(temp_rdata)
})

test_that("summary_table adds missing species/locations with zeros", {
    skip_on_cran()

    observations <- data.frame(
        Species = c("Epfu", "Epfu"),
        Location = c("Site1", "Site1"),
        stringsAsFactors = FALSE
    )

    temp_rdata <- tempfile(fileext = ".RData")
    save(observations, file = temp_rdata)

    expect_message(
        result <- summary_table(temp_rdata,
            species_list = c("Epfu", "Labo", "Mylu"),
            location_list = c("Site1", "Site2")
        ),
        "2 species not found in data"
    )
    expect_message(
        result <- summary_table(temp_rdata,
            species_list = c("Epfu", "Labo", "Mylu"),
            location_list = c("Site1", "Site2")
        ),
        "1 location\\(s\\) not found in data"
    )

    expect_true("Labo" %in% names(result))
    expect_true("Mylu" %in% names(result))
    expect_equal(result$Location[1:2], c("Site1", "Site2"))
    expect_equal(result$Labo, c(0, 0, 0)) # All zeros including totals

    unlink(temp_rdata)
})

test_that("summary_table returns empty table when no data matches filters", {
    skip_on_cran()

    observations <- data.frame(
        Species = c("Epfu", "Labo"),
        Location = c("Site1", "Site2"),
        stringsAsFactors = FALSE
    )

    temp_rdata <- tempfile(fileext = ".RData")
    save(observations, file = temp_rdata)

    expect_warning(
        result <- summary_table(temp_rdata, species_list = c("Mylu")),
        "No observations match the specified species filter"
    )

    expect_equal(nrow(result), 0)
    expect_true("Mylu" %in% names(result))

    unlink(temp_rdata)
})

test_that("summary_table works with both filters and custom ordering", {
    skip_on_cran()

    observations <- data.frame(
        Species = c("Epfu", "Labo", "Mylu", "Epfu", "Labo", "Laci"),
        Location = c("Site1", "Site1", "Site2", "Site2", "Site3", "Site3"),
        stringsAsFactors = FALSE
    )

    temp_rdata <- tempfile(fileext = ".RData")
    save(observations, file = temp_rdata)

    result <- summary_table(temp_rdata,
        species_list = c("Mylu", "Epfu", "Labo"),
        location_list = c("Site3", "Site1", "Site2")
    )

    # Check custom ordering preserved
    expect_equal(names(result), c("Location", "Mylu", "Epfu", "Labo"))
    expect_equal(result$Location[1:3], c("Site3", "Site1", "Site2"))

    unlink(temp_rdata)
})
