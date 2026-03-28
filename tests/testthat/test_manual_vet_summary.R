# Tests for manual_vet_summary and helper functions in R/manual_vet_summary.R

library(testthat)
library(batr)

context("manual_vet_summary and helpers")

# ----------------------------------------------------------------------------
# .validate_vet_summary_data
# ----------------------------------------------------------------------------

test_that(".validate_vet_summary_data validates inputs correctly", {
  # Create test data with required columns
  observations <- data.frame(
    Species = c("Epfu", "Labo"),
    Location_label = c("Site1", "Site2"),
    Species.Auto.ID = c("Epfu", "Labo"),
    Species.Manual.ID = c("Epfu", "Labo"),
    User.Manually.Vetted = c("Y", NA)
  )
  temp_rdata <- tempfile(fileext = ".RData")
  save(observations, file = temp_rdata)

  # Valid inputs
  expect_silent(batr:::`.validate_vet_summary_data`(temp_rdata, NULL, NULL, "species"))
  expect_silent(batr:::`.validate_vet_summary_data`(temp_rdata, c("Epfu"), NULL, "species"))
  expect_silent(batr:::`.validate_vet_summary_data`(temp_rdata, NULL, c("Site1"), "species_and_site"))

  # Invalid species_list
  expect_error(
    batr:::`.validate_vet_summary_data`(temp_rdata, character(0), NULL, "species"),
    "species_list must be a character vector with at least one element"
  )
  expect_error(
    batr:::`.validate_vet_summary_data`(temp_rdata, 123, NULL, "species"),
    "species_list must be a character vector"
  )

  # Invalid location_list
  expect_error(
    batr:::`.validate_vet_summary_data`(temp_rdata, NULL, character(0), "species"),
    "location_list must be a character vector with at least one element"
  )

  unlink(temp_rdata)
})

test_that(".validate_vet_summary_data checks for required columns", {
  # Create data missing required columns
  observations <- data.frame(
    Species = c("Epfu", "Labo"),
    Location_label = c("Site1", "Site2")
  )
  temp_rdata <- tempfile(fileext = ".RData")
  save(observations, file = temp_rdata)

  expect_error(
    batr:::`.validate_vet_summary_data`(temp_rdata, NULL, NULL, "species"),
    "Dataset is missing required columns"
  )

  unlink(temp_rdata)
})

test_that(".validate_vet_summary_data allows missing Location_label for species stratification", {
  # Create data without Location_label
  observations <- data.frame(
    Species = c("Epfu", "Labo"),
    Species.Auto.ID = c("Epfu", "Labo"),
    Species.Manual.ID = c("Epfu", "Labo"),
    User.Manually.Vetted = c("Y", NA)
  )
  temp_rdata <- tempfile(fileext = ".RData")
  save(observations, file = temp_rdata)

  # Should work fine for species stratification
  expect_silent(batr:::`.validate_vet_summary_data`(temp_rdata, NULL, NULL, "species"))

  unlink(temp_rdata)
})

# ----------------------------------------------------------------------------
# .check_manually_vetted_field
# ----------------------------------------------------------------------------

test_that(".check_manually_vetted_field errors when field is missing", {
  dataset <- data.frame(
    Species = c("Epfu", "Labo"),
    Location_label = c("Site1", "Site2")
  )

  expect_error(
    batr:::`.check_manually_vetted_field`(dataset),
    "Column 'User.Manually.Vetted' not found in dataset"
  )
})

test_that(".check_manually_vetted_field warns when no vetted files", {
  dataset <- data.frame(
    Species = c("Epfu", "Labo"),
    Location_label = c("Site1", "Site2"),
    User.Manually.Vetted = c(NA, NA)
  )

  expect_warning(
    batr:::`.check_manually_vetted_field`(dataset),
    "No files marked with 'Y' in User.Manually.Vetted field"
  )
})

test_that(".check_manually_vetted_field is silent when vetted files exist", {
  dataset <- data.frame(
    Species = c("Epfu", "Labo"),
    Location_label = c("Site1", "Site2"),
    User.Manually.Vetted = c("Y", NA)
  )

  expect_silent(batr:::`.check_manually_vetted_field`(dataset))
})

# ----------------------------------------------------------------------------
# .check_species_auto_id
# ----------------------------------------------------------------------------

test_that(".check_species_auto_id warns when vetted files have NA Auto.ID", {
  dataset <- data.frame(
    Species = c("Epfu", "Labo", "Epfu"),
    Species.Auto.ID = c(NA, "Labo", "Epfu"),
    User.Manually.Vetted = c("Y", "Y", "Y")
  )

  expect_warning(
    batr:::`.check_species_auto_id`(dataset),
    "1 vetted file\\(s\\) have NA in Species.Auto.ID"
  )
})

test_that(".check_species_auto_id is silent when Auto.ID is complete", {
  dataset <- data.frame(
    Species = c("Epfu", "Labo"),
    Species.Auto.ID = c("Epfu", "Labo"),
    User.Manually.Vetted = c("Y", "Y")
  )

  expect_silent(batr:::`.check_species_auto_id`(dataset))
})

# ----------------------------------------------------------------------------
# .filter_vetted_dataset
# ----------------------------------------------------------------------------

test_that(".filter_vetted_dataset filters by species_list", {
  dataset <- data.frame(
    Species = c("Epfu", "Labo", "Epfu", "Laci"),
    Species.Auto.ID = c("Epfu", "Labo", "Epfu", "Laci"),
    Location_label = c("Site1", "Site1", "Site2", "Site3")
  )

  result <- batr:::`.filter_vetted_dataset`(dataset, c("Epfu", "Labo"), NULL)
  expect_equal(nrow(result), 3)
  expect_true(all(result$Species.Auto.ID %in% c("Epfu", "Labo")))
})

test_that(".filter_vetted_dataset filters by location_list", {
  dataset <- data.frame(
    Species = c("Epfu", "Labo", "Epfu", "Laci"),
    Species.Auto.ID = c("Epfu", "Labo", "Epfu", "Laci"),
    Location_label = c("Site1", "Site1", "Site2", "Site3")
  )

  result <- batr:::`.filter_vetted_dataset`(dataset, NULL, c("Site1", "Site2"))
  expect_equal(nrow(result), 3)
  expect_true(all(result$Location_label %in% c("Site1", "Site2")))
})

test_that(".filter_vetted_dataset errors when no data remains", {
  dataset <- data.frame(
    Species = c("Epfu", "Labo"),
    Species.Auto.ID = c("Epfu", "Labo"),
    Location_label = c("Site1", "Site2")
  )

  expect_error(
    batr:::`.filter_vetted_dataset`(dataset, c("Mylu"), NULL),
    "No data remaining after applying filters"
  )
})

# ----------------------------------------------------------------------------
# .ensure_location_label
# ----------------------------------------------------------------------------

test_that(".ensure_location_label returns dataset unchanged if Location_label exists", {
  dataset <- data.frame(
    Species = c("Epfu", "Labo"),
    Location_label = c("Site 1", "Site 2"),
    stringsAsFactors = FALSE
  )

  result <- batr:::`.ensure_location_label`(dataset)

  expect_identical(result, dataset)
})

test_that(".ensure_location_label creates Location_label from Location", {
  dataset <- data.frame(
    Species = c("Epfu", "Labo"),
    Location = c("Site_1", "Site_2"),
    stringsAsFactors = FALSE
  )

  expect_message(
    result <- batr:::`.ensure_location_label`(dataset),
    "Creating Location_label from Location column"
  )

  expect_true("Location_label" %in% colnames(result))
  expect_equal(result$Location_label, c("Site 1", "Site 2"))
})

test_that(".ensure_location_label errors when neither Location_label nor Location exists", {
  dataset <- data.frame(
    Species = c("Epfu", "Labo"),
    stringsAsFactors = FALSE
  )

  expect_error(
    batr:::`.ensure_location_label`(dataset),
    "requires either 'Location_label' or 'Location' column"
  )
})

# ----------------------------------------------------------------------------
# .calculate_species_vet_stats
# ----------------------------------------------------------------------------

test_that(".calculate_species_vet_stats calculates correctly", {
  dataset <- data.frame(
    Species = c("Epfu", "Epfu", "Epfu", "Epfu", "Labo", "Labo"),
    Species.Auto.ID = c("Epfu", "Epfu", "Epfu", "Epfu", "Labo", "Labo"),
    Species.Manual.ID = c("Epfu", "Mylu", NA, NA, "Labo", NA),
    User.Manually.Vetted = c("Y", "Y", "Y", NA, "Y", NA),
    stringsAsFactors = FALSE
  )

  result <- batr:::`.calculate_species_vet_stats`(dataset)

  # Check Epfu stats
  epfu_row <- result[result$Species == "Epfu", ]
  expect_equal(epfu_row$Total_Files, 4)
  expect_equal(epfu_row$Vetted_Count, 3)
  expect_equal(epfu_row$Percent_Vetted, 75.00)
  expect_equal(epfu_row$Match_Count, 1) # 1 match: Epfu==Epfu, 2 non-matches: Epfu!=Mylu and Epfu!=NA
  expect_equal(epfu_row$Percent_Match, 33.33) # 1 out of 3 vetted = 33.33%

  # Check Labo stats
  labo_row <- result[result$Species == "Labo", ]
  expect_equal(labo_row$Total_Files, 2)
  expect_equal(labo_row$Vetted_Count, 1)
  expect_equal(labo_row$Percent_Vetted, 50.00)
  expect_equal(labo_row$Match_Count, 1)
  expect_equal(labo_row$Percent_Match, 100.00)
})

test_that(".calculate_species_vet_stats handles no vetted files", {
  dataset <- data.frame(
    Species = c("Epfu", "Epfu"),
    Species.Auto.ID = c("Epfu", "Epfu"),
    Species.Manual.ID = c(NA, NA),
    User.Manually.Vetted = c(NA, NA),
    stringsAsFactors = FALSE
  )

  result <- batr:::`.calculate_species_vet_stats`(dataset)

  expect_equal(result$Total_Files, 2)
  expect_equal(result$Vetted_Count, 0)
  expect_equal(result$Percent_Vetted, 0)
  expect_equal(result$Match_Count, 0)
  expect_true(is.na(result$Percent_Match))
})

test_that(".calculate_species_vet_stats sorts species alphabetically", {
  dataset <- data.frame(
    Species = c("Mylu", "Epfu", "Labo"),
    Species.Auto.ID = c("Mylu", "Epfu", "Labo"),
    Species.Manual.ID = c("Mylu", "Epfu", "Labo"),
    User.Manually.Vetted = c("Y", "Y", "Y"),
    stringsAsFactors = FALSE
  )

  result <- batr:::`.calculate_species_vet_stats`(dataset)

  expect_equal(result$Species, c("Epfu", "Labo", "Mylu"))
})

# ----------------------------------------------------------------------------
# .calculate_species_site_vet_stats
# ----------------------------------------------------------------------------

test_that(".calculate_species_site_vet_stats calculates correctly", {
  dataset <- data.frame(
    Species = c("Epfu", "Epfu", "Epfu", "Labo", "Labo"),
    Location_label = c("Site1", "Site1", "Site2", "Site1", "Site2"),
    Species.Auto.ID = c("Epfu", "Epfu", "Epfu", "Labo", "Labo"),
    Species.Manual.ID = c("Epfu", "Mylu", "Epfu", "Labo", NA),
    User.Manually.Vetted = c("Y", "Y", "Y", "Y", NA),
    stringsAsFactors = FALSE
  )

  result <- batr:::`.calculate_species_site_vet_stats`(dataset)

  # Check Epfu-Site1
  epfu_site1 <- result[result$Species == "Epfu" & result$Location == "Site1", ]
  expect_equal(epfu_site1$Total_Files, 2)
  expect_equal(epfu_site1$Vetted_Count, 2)
  expect_equal(epfu_site1$Match_Count, 1)
  expect_equal(epfu_site1$Percent_Match, 50.00)

  # Check Labo-Site2
  labo_site2 <- result[result$Species == "Labo" & result$Location == "Site2", ]
  expect_equal(labo_site2$Total_Files, 1)
  expect_equal(labo_site2$Vetted_Count, 0)
  expect_equal(labo_site2$Match_Count, 0)
  expect_true(is.na(labo_site2$Percent_Match))
})

test_that(".calculate_species_site_vet_stats sorts correctly", {
  dataset <- data.frame(
    Species = c("Mylu", "Epfu", "Labo", "Epfu"),
    Location_label = c("Site2", "Site2", "Site1", "Site1"),
    Species.Auto.ID = c("Mylu", "Epfu", "Labo", "Epfu"),
    Species.Manual.ID = c("Mylu", "Epfu", "Labo", "Epfu"),
    User.Manually.Vetted = c("Y", "Y", "Y", "Y"),
    stringsAsFactors = FALSE
  )

  result <- batr:::`.calculate_species_site_vet_stats`(dataset)

  # Check sorting: Epfu-Site1, Epfu-Site2, Labo-Site1, Mylu-Site2
  expect_equal(result$Species[1], "Epfu")
  expect_equal(result$Location[1], "Site1")
  expect_equal(result$Species[2], "Epfu")
  expect_equal(result$Location[2], "Site2")
})

# ----------------------------------------------------------------------------
# .add_vet_totals_row
# ----------------------------------------------------------------------------

test_that(".add_vet_totals_row adds totals correctly for species stratification", {
  summary_table <- data.frame(
    Species = c("Epfu", "Labo"),
    Total_Files = c(100, 50),
    Vetted_Count = c(10, 5),
    Percent_Vetted = c(10.00, 10.00),
    Match_Count = c(8, 4),
    Percent_Match = c(80.00, 80.00),
    stringsAsFactors = FALSE
  )

  result <- batr:::`.add_vet_totals_row`(summary_table, "species")

  expect_equal(nrow(result), 3)
  totals <- result[result$Species == "Totals", ]
  expect_equal(totals$Total_Files, 150)
  expect_equal(totals$Vetted_Count, 15)
  expect_equal(totals$Percent_Vetted, 10.00)
  expect_equal(totals$Match_Count, 12)
  expect_equal(totals$Percent_Match, 80.00)
})

test_that(".add_vet_totals_row adds totals correctly for species_and_site", {
  summary_table <- data.frame(
    Species = c("Epfu", "Labo"),
    Location = c("Site1", "Site2"),
    Total_Files = c(100, 50),
    Vetted_Count = c(10, 5),
    Percent_Vetted = c(10.00, 10.00),
    Match_Count = c(8, 4),
    Percent_Match = c(80.00, 80.00),
    stringsAsFactors = FALSE
  )

  result <- batr:::`.add_vet_totals_row`(summary_table, "species_and_site")

  expect_equal(nrow(result), 3)
  totals <- result[result$Species == "Totals", ]
  expect_equal(totals$Location, "")
  expect_equal(totals$Total_Files, 150)
})

test_that(".add_vet_totals_row handles empty table", {
  summary_table <- data.frame(
    Species = character(0),
    Total_Files = numeric(0),
    Vetted_Count = numeric(0),
    Percent_Vetted = numeric(0),
    Match_Count = numeric(0),
    Percent_Match = numeric(0),
    stringsAsFactors = FALSE
  )

  result <- batr:::`.add_vet_totals_row`(summary_table, "species")
  expect_equal(nrow(result), 0)
})

# ----------------------------------------------------------------------------
# Integration tests for manual_vet_summary
# ----------------------------------------------------------------------------

test_that("manual_vet_summary works with species stratification", {
  skip_on_cran()

  observations <- data.frame(
    Species = c("Epfu", "Epfu", "Epfu", "Labo", "Labo"),
    Location_label = c("Site1", "Site1", "Site2", "Site1", "Site2"),
    Species.Auto.ID = c("Epfu", "Epfu", "Epfu", "Labo", "Labo"),
    Species.Manual.ID = c("Epfu", "Mylu", NA, "Labo", NA),
    User.Manually.Vetted = c("Y", "Y", NA, "Y", NA),
    stringsAsFactors = FALSE
  )

  temp_rdata <- tempfile(fileext = ".RData")
  save(observations, file = temp_rdata)

  result <- manual_vet_summary(temp_rdata, stratify_by = "species")

  expect_equal(nrow(result), 3) # 2 species + totals
  expect_true("Totals" %in% result$Species)
  expect_equal(ncol(result), 6)

  unlink(temp_rdata)
})

test_that("manual_vet_summary works with species_and_site stratification", {
  skip_on_cran()

  observations <- data.frame(
    Species = c("Epfu", "Epfu", "Labo"),
    Location_label = c("Site1", "Site2", "Site1"),
    Species.Auto.ID = c("Epfu", "Epfu", "Labo"),
    Species.Manual.ID = c("Epfu", "Epfu", "Labo"),
    User.Manually.Vetted = c("Y", "Y", "Y"),
    stringsAsFactors = FALSE
  )

  temp_rdata <- tempfile(fileext = ".RData")
  save(observations, file = temp_rdata)

  result <- manual_vet_summary(temp_rdata, stratify_by = "species_and_site")

  expect_equal(nrow(result), 4) # 3 combinations + totals
  expect_equal(ncol(result), 7) # includes Location column
  expect_true("Location" %in% colnames(result))

  unlink(temp_rdata)
})

test_that("manual_vet_summary respects species_list filter", {
  skip_on_cran()

  observations <- data.frame(
    Species = c("Epfu", "Epfu", "Labo", "Mylu"),
    Location_label = c("Site1", "Site2", "Site1", "Site1"),
    Species.Auto.ID = c("Epfu", "Epfu", "Labo", "Mylu"),
    Species.Manual.ID = c("Epfu", "Epfu", "Labo", "Mylu"),
    User.Manually.Vetted = c("Y", "Y", "Y", "Y"),
    stringsAsFactors = FALSE
  )

  temp_rdata <- tempfile(fileext = ".RData")
  save(observations, file = temp_rdata)

  result <- manual_vet_summary(
    temp_rdata,
    stratify_by = "species",
    species_list = c("Epfu", "Labo")
  )

  # Should have Epfu, Labo, and Totals (not Mylu)
  expect_equal(nrow(result), 3)
  expect_true(all(c("Epfu", "Labo", "Totals") %in% result$Species))
  expect_false("Mylu" %in% result$Species)

  unlink(temp_rdata)
})

test_that("manual_vet_summary errors when User.Manually.Vetted is missing", {
  skip_on_cran()

  observations <- data.frame(
    Species = c("Epfu", "Labo"),
    Location_label = c("Site1", "Site2"),
    Species.Auto.ID = c("Epfu", "Labo"),
    Species.Manual.ID = c("Epfu", "Labo"),
    stringsAsFactors = FALSE
  )

  temp_rdata <- tempfile(fileext = ".RData")
  save(observations, file = temp_rdata)

  expect_error(
    manual_vet_summary(temp_rdata),
    "Column 'User.Manually.Vetted' not found in dataset"
  )

  unlink(temp_rdata)
})

test_that("manual_vet_summary works without Location_label for species stratification", {
  skip_on_cran()

  observations <- data.frame(
    Species = c("Epfu", "Epfu", "Labo"),
    Species.Auto.ID = c("Epfu", "Epfu", "Labo"),
    Species.Manual.ID = c("Epfu", "Epfu", "Labo"),
    User.Manually.Vetted = c("Y", "Y", "Y"),
    stringsAsFactors = FALSE
  )

  temp_rdata <- tempfile(fileext = ".RData")
  save(observations, file = temp_rdata)

  # Should work fine with stratify_by = "species"
  result <- manual_vet_summary(temp_rdata, stratify_by = "species")

  expect_equal(nrow(result), 3) # 2 species + totals
  expect_equal(ncol(result), 6)
  expect_true("Totals" %in% result$Species)

  unlink(temp_rdata)
})

test_that("manual_vet_summary creates Location_label from Location for species_and_site", {
  skip_on_cran()

  observations <- data.frame(
    Species = c("Epfu", "Epfu", "Labo"),
    Location = c("Site_1", "Site_2", "Site_1"),
    Species.Auto.ID = c("Epfu", "Epfu", "Labo"),
    Species.Manual.ID = c("Epfu", "Epfu", "Labo"),
    User.Manually.Vetted = c("Y", "Y", "Y"),
    stringsAsFactors = FALSE
  )

  temp_rdata <- tempfile(fileext = ".RData")
  save(observations, file = temp_rdata)

  # Should work and create Location_label from Location
  expect_message(
    result <- manual_vet_summary(temp_rdata, stratify_by = "species_and_site"),
    "Creating Location_label from Location column"
  )

  expect_equal(nrow(result), 4) # 3 combinations + totals
  expect_equal(ncol(result), 7) # includes Location column
  expect_true("Location" %in% colnames(result))

  # Check that underscores were replaced
  expect_true("Site 1" %in% result$Location | "Site_1" %in% result$Location)

  unlink(temp_rdata)
})

test_that("manual_vet_summary errors without Location or Location_label for species_and_site", {
  skip_on_cran()

  observations <- data.frame(
    Species = c("Epfu", "Epfu", "Labo"),
    Species.Auto.ID = c("Epfu", "Epfu", "Labo"),
    Species.Manual.ID = c("Epfu", "Epfu", "Labo"),
    User.Manually.Vetted = c("Y", "Y", "Y"),
    stringsAsFactors = FALSE
  )

  temp_rdata <- tempfile(fileext = ".RData")
  save(observations, file = temp_rdata)

  # Should error when neither Location_label nor Location exists
  expect_error(
    manual_vet_summary(temp_rdata, stratify_by = "species_and_site"),
    "requires either 'Location_label' or 'Location' column"
  )

  unlink(temp_rdata)
})

test_that("manual_vet_summary cleans NA values like summary_table", {
  skip_on_cran()

  observations <- data.frame(
    Species = c("Epfu", "Epfu", NA, "Labo", "Mylu"),
    Location = c("Site1", "Site2", "Site3", NA, "Site1"),
    Species.Auto.ID = c("Epfu", "Epfu", NA, "Labo", "Mylu"),
    Species.Manual.ID = c("Epfu", NA, NA, "Labo", "Mylu"),
    User.Manually.Vetted = c("Y", "Y", "Y", "Y", "Y"),
    stringsAsFactors = FALSE
  )

  temp_rdata <- tempfile(fileext = ".RData")
  save(observations, file = temp_rdata)

  # Should remove 2 observations: one with NA Species, one with NA Location
  expect_message(
    result <- manual_vet_summary(temp_rdata, stratify_by = "species"),
    "Removing 2 observation\\(s\\) with missing data"
  )

  # Should have 2 species rows (Epfu, Mylu) + Totals = 3 rows
  # Labo was removed because it had NA Location
  expect_equal(nrow(result), 3)
  expect_true("Epfu" %in% result$Species)
  expect_true("Mylu" %in% result$Species)
  expect_true("Totals" %in% result$Species)

  # Epfu should have 2 total files (the one with NA Location was excluded)
  epfu_row <- result[result$Species == "Epfu", ]
  expect_equal(epfu_row$Total_Files, 2)

  # Totals should reflect only valid observations (3 total)
  totals_row <- result[result$Species == "Totals", ]
  expect_equal(totals_row$Total_Files, 3)

  unlink(temp_rdata)
})

test_that("manual_vet_summary groups by Species.Auto.ID not manual corrections", {
  skip_on_cran()

  # Create data where Manual.ID differs from Auto.ID
  # HiF only appears in Manual.ID, should NOT create a separate species row
  # Mysp is a manual correction of Myle, should count as Myle non-match
  observations <- data.frame(
    Species = c("Epfu", "Epfu", "Myle", "Myle"),
    Location = c("Site1", "Site1", "Site1", "Site2"),
    Species.Auto.ID = c("Epfu", "Epfu", "Myle", "Myle"),
    Species.Manual.ID = c("Epfu", "HiF", "Mysp", "Myle"),
    User.Manually.Vetted = c("Y", "Y", "Y", "Y"),
    stringsAsFactors = FALSE
  )

  temp_rdata <- tempfile(fileext = ".RData")
  save(observations, file = temp_rdata)

  result <- manual_vet_summary(temp_rdata, stratify_by = "species")

  # Should have 2 species rows (Epfu, Myle) + Totals = 3 rows
  # HiF should NOT appear as a separate species
  expect_equal(nrow(result), 3)
  expect_true("Epfu" %in% result$Species)
  expect_true("Myle" %in% result$Species)
  expect_false("HiF" %in% result$Species)
  expect_false("Mysp" %in% result$Species)

  # Check Epfu stats
  epfu_row <- result[result$Species == "Epfu", ]
  expect_equal(epfu_row$Total_Files, 2)
  expect_equal(epfu_row$Vetted_Count, 2)
  expect_equal(epfu_row$Match_Count, 1) # Only first one matches
  expect_equal(epfu_row$Percent_Match, 50.00) # 1 out of 2 = 50%

  # Check Myle stats
  myle_row <- result[result$Species == "Myle", ]
  expect_equal(myle_row$Total_Files, 2)
  expect_equal(myle_row$Vetted_Count, 2)
  expect_equal(myle_row$Match_Count, 1) # Only second one matches (Myle==Myle)
  expect_equal(myle_row$Percent_Match, 50.00) # 1 out of 2 = 50%

  unlink(temp_rdata)
})
