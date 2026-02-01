# Unit tests for batr plotting functions

# Helper to create test data
create_test_data <- function() {
  temp_file <- tempfile(fileext = ".RData")
  observations <- data.frame(
    Species = rep(c("Epfu", "Mylu", "Lano"), each = 10),
    Location = rep(c("Site1", "Site2"), times = 15),
    Night = rep(as.Date("2024-01-01") + 0:9, times = 3),
    Latitude = rep(c(43.65, 43.70), times = 15),
    Longitude = rep(c(-79.38, -79.42), times = 15),
    Timestamp = as.POSIXct("2024-01-01 21:30:00", tz = "UTC") + sample(0:3600, 30, replace = TRUE),
    Sunset = as.POSIXct("2024-01-01 20:00:00", tz = "UTC"),
    File.Name = paste0("file_", 1:30, ".wav")
  )
  # Create active_dates with proper structure (Date, Location, Log_Count)
  # Log_Count > 0 means recorder was active, 0 means gap
  active_dates <- data.frame(
    Date = rep(as.Date("2024-01-01") + 0:14, times = 2),
    Location = rep(c("Site1", "Site2"), each = 15),
    Log_Count = rep(c(10, 12, 8, 11, 9, 10, 13, 11, 10, 12, 8, 0, 0, 11, 10), times = 2)
  )
  save(observations, active_dates, file = temp_file)
  temp_file
}

test_that("species_daily_site_plot returns a ggplot object and handles all/NULL", {
  test_file <- create_test_data()
  on.exit(unlink(test_file))
  
  p1 <- species_daily_site_plot(test_file, species = "Epfu")
  expect_s3_class(p1, "ggplot")
  p2 <- species_daily_site_plot(test_file, species = "all")
  expect_s3_class(p2, "ggplot")
})

test_that("species_activity_facet_plot returns a ggplot object", {
  test_file <- create_test_data()
  on.exit(unlink(test_file))
  
  p1 <- species_activity_facet_plot(test_file, species = c("Epfu", "Mylu"))
  expect_s3_class(p1, "ggplot")
  # Test with NULL species (should include all)
  p2 <- species_activity_facet_plot(test_file, species = NULL)
  expect_s3_class(p2, "ggplot")
})

test_that("location_map_plot returns a ggplot object", {
  test_file <- create_test_data()
  on.exit(unlink(test_file))
  
  p <- location_map_plot(test_file)
  expect_s3_class(p, "ggplot")
  # Test with specific location
  p2 <- location_map_plot(test_file, location_list = "Site1")
  expect_s3_class(p2, "ggplot")
})

test_that("location_map_plot errors without coordinates", {
  temp_file <- tempfile(fileext = ".RData")
  observations <- data.frame(
    Species = "Epfu",
    Location = "Site1",
    Night = as.Date("2024-01-01")
    # No Latitude/Longitude
  )
  save(observations, file = temp_file)
  on.exit(unlink(temp_file))
  
  expect_error(
    location_map_plot(temp_file),
    "Latitude and Longitude"
  )
})

test_that("monitoring_effort_plot returns a ggplot object", {
  test_file <- create_test_data()
  on.exit(unlink(test_file))
  
  # Should return a plot even if gap calculation has issues with minimal data
  plot <- monitoring_effort_plot(test_file)
  expect_s3_class(plot, "ggplot")
})

test_that("monitoring_effort_plot warns without log data", {
  temp_file <- tempfile(fileext = ".RData")
  observations <- data.frame(
    Species = "Epfu",
    Location = "Site1",
    Night = as.Date("2024-01-01")
  )
  save(observations, file = temp_file)
  
  expect_warning(
    monitoring_effort_plot(temp_file),
    "Failed to load gap data"
  )
  
  unlink(temp_file)
})

test_that("first_observations_plot returns a ggplot object and handles no data", {
  test_file <- create_test_data()
  on.exit(unlink(test_file))
  
  p <- first_observations_plot(
    test_file,
    species = "Epfu",
    timezone = "UTC"
  )
  expect_s3_class(p, "ggplot")
  
  # Test that function handles no observations gracefully
  # (Should error or warn when trying to load data for nonexistent species)
  expect_error(
    first_observations_plot(
      test_file,
      species = "NonexistentSpecies",
      timezone = "UTC"
    )
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

test_that("monthly_activity_plot returns a ggplot object", {
    # Create minimal test data
    species_night_site <- data.frame(
        Night = as.Date(c("2023-01-15", "2023-02-15", "2023-03-15")),
        Location = c("Site1", "Site1", "Site1"),
        Species = c("Epfu", "Epfu", "Epfu"),
        Count = c(10, 15, 20)
    )
    monthly_active_nights <- data.frame(
        Year = 2023,
        Location = "Site1",
        `1` = 5,
        `2` = 6,
        `3` = 7,
        Total_Nights = 18,
        check.names = FALSE
    )

    p <- monthly_activity_plot(species_night_site, monthly_active_nights)
    expect_s3_class(p, "ggplot")

    # Test with exclusion
    p2 <- monthly_activity_plot(species_night_site, monthly_active_nights,
        exclude_species = "Mylu"
    )
    expect_s3_class(p2, "ggplot")
})

test_that("pipe operator %>% works", {
    # Simple test that the pipe is available
    result <- mtcars %>%
        head(5) %>%
        nrow()
    expect_equal(result, 5)
})
