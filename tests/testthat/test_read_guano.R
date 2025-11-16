# Unit tests for read.guano() function
# Tests focus on:
#   1. Proper parsing of GUANO metadata
#   2. Handling of NUL bytes and padding (the fix for the warning)
#   3. Edge cases (empty chunks, malformed files, etc.)
#   4. Data type coercion (timestamps, numeric values)
#   5. Special field handling (Loc Position parsing)

library(testthat)
library(batr)

# Helper function to create a minimal valid GUANO WAV file in memory
# Returns the raw bytes that would make up a valid WAV file with GUANO metadata
create_test_guano_bytes <- function(guano_metadata = "Timestamp: 2023-01-15T12:30:45Z\nSamplerate: 256000\n") {
  # Start with GUANO chunk
  guan_id <- charToRaw("guan")
  guan_data <- charToRaw(guano_metadata)
  # RIFF chunks must be padded to even length; add NUL padding if needed
  if (length(guan_data) %% 2 == 1) {
    guan_data <- c(guan_data, as.raw(0))
  }
  
  # Write guan_size as little-endian 32-bit integer
  guan_size_val <- length(guan_data)
  guan_size <- writeBin(guan_size_val, raw(), size = 4, endian = "little")
  
  # fmt subchunk (minimal, required)
  fmt_id <- charToRaw("fmt ")
  fmt_size_val <- 16L
  fmt_size <- writeBin(fmt_size_val, raw(), size = 4, endian = "little")
  # Format: PCM(1), 1 channel, 44100 Hz, 88200 B/s, 2 bytes/frame, 16 bits
  fmt_data <- c(
    writeBin(1L, raw(), size = 2, endian = "little"),       # PCM
    writeBin(1L, raw(), size = 2, endian = "little"),       # 1 channel
    writeBin(44100L, raw(), size = 4, endian = "little"),   # sample rate
    writeBin(88200L, raw(), size = 4, endian = "little"),   # byte rate
    writeBin(2L, raw(), size = 2, endian = "little"),       # block align
    writeBin(16L, raw(), size = 2, endian = "little")       # bits per sample
  )
  
  # Compute total file size
  # File structure: "RIFF" + size + "WAVE" + fmt chunk + guan chunk
  file_size_minus_8 <- 4L +  # "WAVE"
                      8L + length(fmt_data) +  # fmt header + data
                      8L + length(guan_data)   # guan header + data
  file_size <- writeBin(file_size_minus_8, raw(), size = 4, endian = "little")
  
  # Assemble the complete WAV file
  file_content <- c(
    charToRaw("RIFF"),
    file_size,
    charToRaw("WAVE"),
    fmt_id,
    fmt_size,
    fmt_data,
    guan_id,
    guan_size,
    guan_data
  )
  
  return(file_content)
}

# Helper to write bytes to a temporary file and read with read.guano()
read_guano_from_bytes <- function(bytes) {
  tmp_file <- tempfile(fileext = ".wav")
  writeBin(bytes, tmp_file)
  on.exit(unlink(tmp_file))
  result <- read.guano(tmp_file)
  return(result)
}

# ============================================================================
# TESTS
# ============================================================================

test_that("read.guano() correctly parses basic metadata", {
  metadata_str <- "Timestamp: 2023-01-15T12:30:45Z\nSamplerate: 256000\nTE: 1\n"
  bytes <- create_test_guano_bytes(metadata_str)
  result <- read_guano_from_bytes(bytes)
  
  expect_is(result, "list")
  expect_true("Timestamp" %in% names(result))
  expect_true("Samplerate" %in% names(result))
  expect_true("File Name" %in% names(result))
  expect_true("File Path" %in% names(result))
})

test_that("read.guano() coerces numeric fields correctly", {
  metadata_str <- "Samplerate: 256000\nHumidity: 65.5\nFilter HP: 15000.0\nFilter LP: 120000.5\n"
  bytes <- create_test_guano_bytes(metadata_str)
  result <- read_guano_from_bytes(bytes)
  
  expect_equal(result$Samplerate, 256000L)
  expect_equal(result$Humidity, 65.5)
  expect_equal(result$`Filter HP`, 15000.0)
  expect_equal(result$`Filter LP`, 120000.5)
})

test_that("read.guano() parses timestamps with UTC offset", {
  metadata_str <- "Timestamp: 2023-06-20T14:22:10-04:00\n"
  bytes <- create_test_guano_bytes(metadata_str)
  result <- read_guano_from_bytes(bytes)
  
  expect_is(result$Timestamp, "POSIXlt")
  # Verify the timestamp parsed without errors
  expect_false(is.na(result$Timestamp))
})

test_that("read.guano() parses timestamps with Z (UTC)", {
  metadata_str <- "Timestamp: 2023-01-15T12:30:45Z\n"
  bytes <- create_test_guano_bytes(metadata_str)
  result <- read_guano_from_bytes(bytes)
  
  expect_is(result$Timestamp, "POSIXlt")
  expect_equal(attr(result$Timestamp, "tzone"), "UTC")
})

test_that("read.guano() handles Loc Position parsing (lat/lon extraction)", {
  # Loc Position format: "latitude longitude" or with tabs
  metadata_str <- "Loc Position: 40.7128 -74.0060\n"
  bytes <- create_test_guano_bytes(metadata_str)
  result <- read_guano_from_bytes(bytes)
  
  # Loc Position should be split into Lat/Lon and removed
  expect_false("Loc Position" %in% names(result))
  expect_true("Loc Position Lat" %in% names(result))
  expect_true("Loc Position Lon" %in% names(result))
  expect_equal(result$`Loc Position Lat`, 40.7128)
  expect_equal(result$`Loc Position Lon`, -74.0060)
})

test_that("read.guano() handles tabs in Loc Position", {
  # GUANO spec allows tabs as field separators in Loc Position
  metadata_str <- "Loc Position: 40.7128\t-74.0060\n"
  bytes <- create_test_guano_bytes(metadata_str)
  result <- read_guano_from_bytes(bytes)
  
  expect_true("Loc Position Lat" %in% names(result))
  expect_true("Loc Position Lon" %in% names(result))
  expect_equal(result$`Loc Position Lat`, 40.7128)
  expect_equal(result$`Loc Position Lon`, -74.0060)
})

test_that("read.guano() handles NUL-terminated metadata without warning", {
  # This test verifies the fix: metadata followed by NUL bytes should not warn
  # Create metadata with explicit padding to ensure proper NUL handling
  metadata_base <- "Samplerate: 256000\n"
  # The helper function already handles padding, so we test through it
  bytes <- create_test_guano_bytes(metadata_base)
  
  # The expectation: no warning about "truncating string with embedded nuls"
  expect_warning(
    result <- read_guano_from_bytes(bytes),
    NA  # NA means we expect NO warning
  )
  
  expect_equal(result$Samplerate, 256000L)
})

test_that("read.guano() handles empty metadata gracefully", {
  metadata_str <- ""
  bytes <- create_test_guano_bytes(metadata_str)
  result <- read_guano_from_bytes(bytes)
  
  # Should still have File Name and File Path
  expect_true("File Name" %in% names(result))
  expect_true("File Path" %in% names(result))
})

test_that("read.guano() handles metadata with blank lines", {
  metadata_str <- "Samplerate: 256000\n\n\nHumidity: 65.5\n\n"
  bytes <- create_test_guano_bytes(metadata_str)
  result <- read_guano_from_bytes(bytes)
  
  # Blank lines should be skipped
  expect_equal(result$Samplerate, 256000L)
  expect_equal(result$Humidity, 65.5)
})

test_that("read.guano() ignores unknown metadata fields", {
  metadata_str <- "Samplerate: 256000\nUnknownField: some_value\nHumidity: 65.5\n"
  bytes <- create_test_guano_bytes(metadata_str)
  result <- read_guano_from_bytes(bytes)
  
  # UnknownField should be stored as-is (no type coercion)
  expect_equal(result$Samplerate, 256000L)
  expect_equal(result$Humidity, 65.5)
  expect_equal(result$UnknownField, "some_value")
})

test_that("read.guano() returns NULL for non-RIFF files", {
  # Create a file without RIFF header
  bad_bytes <- charToRaw("XXXX") # Not RIFF
  result <- read_guano_from_bytes(bad_bytes)
  
  expect_null(result)
})

test_that("read.guano() returns NULL for non-WAVE files", {
  # Create RIFF but not WAVE
  riff_id <- charToRaw("RIFF")
  riff_size <- writeBin(100L, raw())
  non_wave_id <- charToRaw("AIFF")  # Not WAVE
  
  file_content <- c(riff_id, riff_size, non_wave_id)
  result <- read_guano_from_bytes(file_content)
  
  expect_null(result)
})

test_that("read.guano() handles missing File Path and File Name assignments", {
  # Verify these are always set
  metadata_str <- "Samplerate: 256000\n"
  bytes <- create_test_guano_bytes(metadata_str)
  result <- read_guano_from_bytes(bytes)
  
  expect_true(is.character(result$`File Name`))
  expect_true(is.character(result$`File Path`))
  expect_true(nchar(result$`File Name`) > 0)
  expect_true(nchar(result$`File Path`) > 0)
})

test_that("read.guano() coerces TE field with default value", {
  # TE (Trigger Event) should default to 1 if empty or missing
  metadata_str <- "TE: \n"  # Empty value
  bytes <- create_test_guano_bytes(metadata_str)
  result <- read_guano_from_bytes(bytes)
  
  expect_equal(result$TE, 1L)
})

test_that("read.guano() coerces TE field from string value", {
  metadata_str <- "TE: 2\n"
  bytes <- create_test_guano_bytes(metadata_str)
  result <- read_guano_from_bytes(bytes)
  
  expect_equal(result$TE, 2L)
})

test_that("read.guano() handles location accuracy and elevation", {
  metadata_str <- "Loc Accuracy: 10\nLoc Elevation: 150.5\n"
  bytes <- create_test_guano_bytes(metadata_str)
  result <- read_guano_from_bytes(bytes)
  
  expect_equal(result$`Loc Accuracy`, 10L)
  expect_equal(result$`Loc Elevation`, 150.5)
})

test_that("read.guano() preserves Note field with escaped newlines", {
  # Note fields may contain escaped newlines (\\n)
  metadata_str <- "Note: This is a note\\nwith a newline\n"
  bytes <- create_test_guano_bytes(metadata_str)
  result <- read_guano_from_bytes(bytes)
  
  # The function converts escaped \\n to actual newlines
  expect_true(grepl("\n", result$Note, fixed = TRUE))
})
