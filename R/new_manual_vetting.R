#' Copy a Random Subset of Files to a New Directory for Manual Vetting
#'
#' \code{manual_vet_extractor} copies a randomly selected subset of files in a
#' project to a new folder to facilitate manual vetting. The function will only
#' move files with an accepted automatic species assignment from SonoBat.
#'
#' @family Manual Vetting Functions
#'
#' @param data_path Character. Path to an existing RData file for the data set
#'  you wish to manually vet.
#' @param WAV_directory Character, the parent directory containing original WAV
#'  files.
#' @param save_directory Character, the destination where selected files will be
#'  copied to
#' @param species_list Character, a single species or list in the form of four
#'  character species codes used by SonoBat, e.g. "Epfu". Defaults to non
#'  Ontario SAR: \code{c("Epfu", "Labo", "Laci", "Lano")}.
#' @param percentage Number. What proportion of files do you wish to extract.
#'  Defaults to 0.05 - 5 percent.
#' @param no_manual Logical vector, defaults to \code{FALSE}. If \code{TRUE} the
#'  function will ignore files that have already been assigned an ID.
#' @param stratified Logical. If \code{TRUE}, samples proportionally from each
#'  location/site (stratified sampling). If \code{FALSE}, samples randomly from
#'  all files regardless of location. Defaults to \code{FALSE}.
#' @param interactive Logical. Whether to prompt user for input when issues arise.
#'  Defaults to \code{interactive()} which detects if R is running interactively.
#'
#' @return Creates folder within the file folder filled with a percentage of
#'  files of the selected copied. When \code{stratified = TRUE}, creates
#'  subfolders for each location within each species folder.
#'
#' @examples
#' \dontrun{
#' manual_vet_extractor("raw_data_project", "C:/Folder/Folder/File_Folder")
#' }
#' @export
manual_vet_extractor <- function(data_path,
                                 WAV_directory,
                                 save_directory,
                                 species_list = c("Epfu", "Labo", "Laci", "Lano"),
                                 percentage = 0.05,
                                 no_manual = FALSE,
                                 stratified = FALSE,
                                 fast_import = TRUE,
                                 interactive = interactive()) {
  message("\n========== Starting Manual Vetting File Selection ==========")

  # Validate inputs
  .validate_percentage(percentage)
  .validate_directories(WAV_directory, save_directory)

  # Prepare data
  dataset <- .prepare_vetting_dataset(data_path, no_manual)
  dataset_with_wavs <- .match_files_to_wavs(dataset, WAV_directory, fast_import)

  # Handle missing files
  dataset_clean <- .handle_missing_wavs(dataset_with_wavs, WAV_directory, interactive)

  # Process sampling
  if (stratified) {
    .process_stratified_sampling(dataset_clean, species_list, save_directory, percentage)
  } else {
    .process_simple_sampling(dataset_clean, species_list, save_directory, percentage)
  }

  message("\n========== Manual Vetting File Selection Complete ==========")
}

# Helper Functions ------------------------------------------------------------

#' Validate percentage parameter
#' @keywords internal
.validate_percentage <- function(percentage) {
  if (!is.numeric(percentage) || length(percentage) != 1) {
    stop(
      "Invalid percentage: must be a single numeric value.\n",
      "Please provide a value between 0 and 1 (e.g., 0.05 for 5%)."
    )
  }
  if (percentage <= 0 || percentage > 1) {
    stop(
      "Invalid percentage: ", percentage, ".\n",
      "Please provide a value between 0 and 1 (e.g., 0.05 for 5%)."
    )
  }
  invisible(TRUE)
}

#' Validate directories exist
#' @keywords internal
.validate_directories <- function(WAV_directory, save_directory) {
  if (!dir.exists(WAV_directory)) {
    stop(
      "WAV directory does not exist: '", WAV_directory, "'.\n",
      "Please check the path and try again."
    )
  }
  if (!dir.exists(save_directory)) {
    message(sprintf("Creating save directory: %s", save_directory))
    dir.create(save_directory, recursive = TRUE)
  }
  invisible(TRUE)
}

#' Prepare vetting dataset by loading and filtering
#' @keywords internal
.prepare_vetting_dataset <- function(data_path, no_manual) {
  .check_data_path(data_path)
  load(data_path)
  
  # Convert to data.table for performance
  dataset <- data.table::as.data.table(observations)
  
  if (no_manual == TRUE) {
    dataset <- dataset[is.na(Species.Manual.ID)]
  }
  dataset <- dataset[!is.na(Species)]
  
  if (nrow(dataset) == 0) {
    stop("No valid observations found after filtering. Check your data.")
  }
  
  return(dataset)
}

#' Match dataset to WAV files
#' @keywords internal
.match_files_to_wavs <- function(dataset, WAV_directory, fast_import) {
  message(sprintf("Matching %d observations to WAV files in: %s", nrow(dataset), WAV_directory))
  WAV_directory_files <- data.table::as.data.table(.get_file_list(WAV_directory, fast_import))
  data.table::setkey(dataset, File.Name)
  data.table::setkey(WAV_directory_files, File.Name)
  dataset_merged <- WAV_directory_files[dataset]
  return(dataset_merged)
}

#' Handle missing WAV files with user interaction
#' @keywords internal
.handle_missing_wavs <- function(dataset, WAV_directory, interactive) {
  missing_idx <- which(is.na(dataset$Full.Path))

  if (length(missing_idx) == 0) {
    return(dataset)
  }

  missing_files <- dataset$File.Name[missing_idx]
  warning(sprintf(
    "%d observations have no matching WAV file in '%s'. These will be excluded.",
    length(missing_idx), WAV_directory
  ))

  if (interactive) {
    show_list <- readline(prompt = "Show missing file names? (y/n): ")
    if (tolower(show_list) %in% c("y", "yes")) {
      print(missing_files)
    }
  }

  # Use data.table syntax for filtering
  dataset_clean <- dataset[!is.na(Full.Path)]

  if (nrow(dataset_clean) == 0) {
    stop("All observations are missing WAV files; nothing to copy for manual vetting.")
  }

  return(dataset_clean)
}

#' Sample files from dataset based on percentage
#' @keywords internal
.sample_files <- function(dataset, percentage) {
  n <- nrow(dataset)

  if (n == 0) {
    return(dataset[integer(0), ])
  }

  if (percentage == 1) {
    return(dataset)
  }

  sample_size <- ceiling(n * percentage)
  if (sample_size > n) sample_size <- n

  sampled_indices <- sample(seq_len(n), sample_size)
  return(dataset[sampled_indices, ])
}

#' Process stratified sampling by location
#' @keywords internal
.process_stratified_sampling <- function(dataset, species_list, save_directory, percentage) {
  message(sprintf("\nUsing stratified sampling: %.1f%% of files per location per species", percentage * 100))
  message(sprintf("Output directory: %s", save_directory))

  for (species in species_list) {
    # Fast data.table subsetting
    species_data <- dataset[Species == species]
    species_count <- nrow(species_data)

    if (species_count == 0) {
      message(sprintf("\n  %s: No files found, skipping.", species))
      next
    }

    locations <- sort(unique(species_data$Location))
    message(sprintf("\n  %s: Found %d files across %d locations", species, species_count, length(locations)))
    message(sprintf("  %s: Sampling from: %s", species, paste(locations, collapse = ", ")))

    total_copied <- 0
    for (location in locations) {
      # Fast data.table filtering
      location_data <- species_data[Location == location]
      location_count <- nrow(location_data)

      if (location_count == 0) next

      location_dir <- file.path(save_directory, species, location)
      if (!dir.exists(location_dir)) {
        dir.create(location_dir, recursive = TRUE)
      }
      sampled_data <- .sample_files(location_data, percentage)
      copied <- file.copy(sampled_data$Full.Path, location_dir)
      total_copied <- total_copied + sum(copied)

      message(sprintf(
        "    - %s: Copied %d of %d files (%.1f%%)",
        location, sum(copied), location_count, (sum(copied) / location_count) * 100
      ))
    }

    message(sprintf("  %s: Total = %d files copied across all locations", species, total_copied))
  }

  invisible(NULL)
}

#' Process simple (non-stratified) sampling
#' @keywords internal
.process_simple_sampling <- function(dataset, species_list, save_directory, percentage) {
  message(sprintf("\nCopying %.1f%% of files per species to: %s", percentage * 100, save_directory))

  for (species in species_list) {
    # Fast data.table subsetting
    species_data <- dataset[Species == species]
    species_count <- nrow(species_data)

    if (species_count == 0) {
      message(sprintf("  %s: No files found, skipping.", species))
      next
    }

    species_dir <- file.path(save_directory, species)
    if (!dir.exists(species_dir)) {
      dir.create(species_dir, recursive = TRUE)
    }

    sampled_data <- .sample_files(species_data, percentage)
    copied <- file.copy(sampled_data$Full.Path, species_dir)

    message(sprintf(
      "  %s: Copied %d of %d files (%.1f%%)",
      species, sum(copied), species_count, percentage * 100
    ))
  }

  invisible(NULL)
}
