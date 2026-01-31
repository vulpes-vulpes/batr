context("Report Generation Functions")

# Setup test data
test_that("render_report validates template path", {
  result <- render_report(
    template_path = "/nonexistent/path/template.Rmd",
    output_file = "test.pdf",
    output_dir = tempdir(),
    params = list(data_path = "test.RData")
  )

  expect_false(result$success)
  expect_null(result$output_path)
  expect_match(result$error, "not found")
})

test_that("render_report validates parameters", {
  # Create a minimal valid template
  template_path <- file.path(tempdir(), "test_template.Rmd")
  writeLines("---\ntitle: Test\n---\n# Test", template_path)

  result <- render_report(
    template_path = template_path,
    output_file = "test.pdf",
    output_dir = tempdir(),
    params = list() # Empty params
  )

  expect_false(result$success)
  expect_match(result$error, "non-empty list")

  # Cleanup
  unlink(template_path)
})

test_that("render_report creates output directory if needed", {
  # Create a minimal valid template
  template_path <- file.path(tempdir(), "test_template.Rmd")
  writeLines("---\ntitle: Test\noutput: pdf_document\nparams:\n  test: !r NA_character_\n---\n# Test", template_path)

  new_dir <- file.path(tempdir(), "new_test_dir", "subdir")

  # Ensure it doesn't exist
  if (dir.exists(new_dir)) {
    unlink(new_dir, recursive = TRUE)
  }

  # Try to render (will likely fail due to params, but directory should be created)
  result <- render_report(
    template_path = template_path,
    output_file = "test.pdf",
    output_dir = new_dir,
    params = list(test = "value"),
    quiet = TRUE
  )

  # Directory should exist (created by render_report)
  expect_true(dir.exists(new_dir))

  # Cleanup
  unlink(template_path)
  unlink(file.path(tempdir(), "new_test_dir"), recursive = TRUE)
})

# Test single_site_report
test_that("single_site_report validates inputs", {
  # Create a temporary data file for validation tests
  temp_rdata <- file.path(tempdir(), "test.RData")
  observations <- data.frame(
    Species = "Epfu",
    Location = "Site1",
    Night = as.Date("2024-01-01"),
    Latitude = 43.6,
    Longitude = -79.3
  )
  active_dates <- data.frame(
    Location = "Site1",
    Date = as.Date("2024-01-01"),
    Log_Count = 1
  )
  save(observations, active_dates, file = temp_rdata)

  # Test missing data_path
  expect_error(
    single_site_report(
      data_path = "/nonexistent/data.RData",
      output_dir = tempdir(),
      site = "Site1",
      species = "Epfu"
    ),
    "does not exist"
  )

  # Test multiple sites (should fail - only single site allowed)
  expect_error(
    single_site_report(
      data_path = temp_rdata,
      output_dir = tempdir(),
      site = c("Site1", "Site2"),
      species = "Epfu"
    ),
    "single site"
  )

  # Test species auto-detection (NULL should work)
  expect_error(
    single_site_report(
      data_path = temp_rdata,
      output_dir = tempdir(),
      site = "Site1",
      species = NULL # Should auto-detect from data
    ),
    NA # Should not error
  )

  # Cleanup
  unlink(temp_rdata)
})

test_that("single_site_report looks for template", {
  # Create a temporary data file
  temp_rdata <- file.path(tempdir(), "test.RData")
  observations <- data.frame(
    Species = "Epfu",
    Location = "Site1",
    Night = as.Date("2024-01-01"),
    Latitude = 43.6,
    Longitude = -79.3
  )
  active_dates <- data.frame(
    Location = "Site1",
    Date = as.Date("2024-01-01"),
    Log_Count = 1
  )
  save(observations, active_dates, file = temp_rdata)

  # This should fail because template doesn't exist, but it will try to render
  result <- suppressWarnings(
    single_site_report(
      data_path = temp_rdata,
      output_dir = tempdir(),
      site = "Site1",
      species = "Epfu"
    )
  )

  # Should have returned a result (with success = FALSE likely)
  expect_is(result, "list")
  expect_true("success" %in% names(result))

  # Cleanup
  unlink(temp_rdata)
})

test_that("multi_site_report validates inputs", {
  expect_error(
    multi_site_report(
      data_path = "/nonexistent/data.RData",
      output_dir = tempdir(),
      sites = c("Site1", "Site2"),
      species = "Epfu"
    ),
    "does not exist"
  )
})

test_that("render_custom_report validates template path", {
  expect_error(
    render_custom_report(
      template_path = "/nonexistent/template.Rmd",
      output_dir = tempdir(),
      params = list(test = "value")
    ),
    "not found"
  )
})

test_that("render_custom_report validates parameters is list", {
  # Create a minimal valid template
  template_path <- file.path(tempdir(), "test_template.Rmd")
  writeLines("---\ntitle: Test\noutput: pdf_document\nparams:\n  test: !r NA_character_\n---\n# Test", template_path)

  expect_error(
    render_custom_report(
      template_path = template_path,
      output_dir = tempdir(),
      params = "not_a_list"
    ),
    "must be a list"
  )

  # Cleanup
  unlink(template_path)
})

test_that("render_custom_report handles relative paths", {
  # Create a minimal valid template
  template_path <- file.path(tempdir(), "test_template.Rmd")
  writeLines("---\ntitle: Test\noutput: pdf_document\nparams:\n  test: !r NA_character_\n---\n# Test", template_path)

  # Use relative path (will be converted to absolute)
  old_wd <- getwd()
  setwd(tempdir())

  result <- suppressWarnings(
    render_custom_report(
      template_path = "test_template.Rmd",
      output_dir = "test_output",
      params = list(test = "value")
    )
  )

  setwd(old_wd)

  # Should have attempted to process the template path
  expect_is(result, "list")

  # Cleanup
  unlink(template_path)
  unlink(file.path(tempdir(), "test_output"), recursive = TRUE)
})

test_that("render_report returns consistent structure", {
  # Create a minimal valid template
  template_path <- file.path(tempdir(), "test_template.Rmd")
  writeLines("---\ntitle: Test\noutput: pdf_document\nparams:\n  test: !r NA_character_\n---\n# Test", template_path)

  result <- render_report(
    template_path = template_path,
    output_file = "test.pdf",
    output_dir = tempdir(),
    params = list(test = "value"),
    quiet = TRUE
  )

  # Check structure
  expect_is(result, "list")
  expect_true(all(c("success", "output_path", "error") %in% names(result)))
  expect_is(result$success, "logical")
  expect_true(is.null(result$error) || is.character(result$error))
  expect_true(is.null(result$output_path) || is.character(result$output_path))

  # Cleanup
  unlink(template_path)
})
