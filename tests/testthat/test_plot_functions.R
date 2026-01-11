# Unit tests for batr plotting functions

test_that("species_daily_site_plot returns a ggplot object and handles all/NULL", {
    skip_if_not(file.exists("testdata/test_obs.RData"))
    p1 <- species_daily_site_plot("testdata/test_obs.RData", species = "Epfu")
    expect_s3_class(p1, "ggplot")
    p2 <- species_daily_site_plot("testdata/test_obs.RData", species = "all")
    expect_s3_class(p2, "ggplot")
})

test_that("species_activity_facet_plot returns a ggplot object", {
    skip_if_not(file.exists("testdata/test_obs.RData"))
    p1 <- species_activity_facet_plot("testdata/test_obs.RData", species = c("Epfu", "Mylu"))
    expect_s3_class(p1, "ggplot")
    # Test with NULL species (should include all)
    p2 <- species_activity_facet_plot("testdata/test_obs.RData", species = NULL)
    expect_s3_class(p2, "ggplot")
})

test_that("location_map_plot returns a ggplot object", {
    skip_if_not(file.exists("testdata/test_obs.RData"))
    p <- location_map_plot("testdata/test_obs.RData")
    expect_s3_class(p, "ggplot")
    # Test with specific location
    p2 <- location_map_plot("testdata/test_obs.RData", location_list = "Site1")
    expect_s3_class(p2, "ggplot")
})

test_that("location_map_plot errors without coordinates", {
    skip_if_not(file.exists("testdata/test_obs.RData"))
    # Should error if no Latitude/Longitude columns exist
    # This assumes test data has coordinates - if it doesn't, this will pass
    expect_error(
        location_map_plot("testdata/no_coords.RData"),
        "Latitude and Longitude"
    )
})

test_that("monitoring_effort_plot returns a ggplot object and handles missing gaps", {
    skip_if_not(file.exists("testdata/test_obs.RData"))
    expect_s3_class(
        monitoring_effort_plot("testdata/test_obs.RData"),
        "ggplot"
    )
    # Should not error if log data missing
    expect_warning(
        monitoring_effort_plot("testdata/nonexistent.RData"),
        regexp = "Failed to load gap data"
    )
})

test_that("first_observations_plot returns a ggplot object and handles no data", {
    skip_if_not(file.exists("testdata/test_obs.RData"))
    p <- first_observations_plot(
        "testdata/test_obs.RData",
        species = "Epfu",
        timezone = "UTC"
    )
    expect_s3_class(p, "ggplot")
    # Should warn and return empty plot if no evening obs
    expect_warning(
        first_observations_plot(
            "testdata/test_obs.RData",
            species = "none",
            timezone = "UTC"
        ),
        regexp = "No evening observations"
    )
})

test_that(".adaptive_date_breaks returns expected structure", {
    out <- .adaptive_date_breaks(Sys.Date(), Sys.Date() + 30)
    expect_type(out, "list")
    expect_true(all(c("breaks", "labels") %in% names(out)))
})

test_that(".clean_location_label replaces underscores", {
    expect_equal(.clean_location_label("Site_1"), "Site 1")
    expect_equal(.clean_location_label("A_B_C"), "A B C")
})
