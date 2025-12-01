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
#' @param interactive Logical. Whether to prompt user for input when issues arise.
#'  Defaults to \code{interactive()} which detects if R is running interactively.
#'
#' @return Creates folder within the file folder filled with a percentage of
#'  files of the selected copied.
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
                                 fast_import = TRUE,
                                 interactive = interactive()) {
  # Validate percentage parameter
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
  
  .check_data_path(data_path)
  load(data_path)
  dataset <- observations
  # load(data_path)
  # dataset <- .location_subsetter(data_path)
  # dataset <- .species_subsetter(data_path, dataset)
  # dataset <- observations # rename for ease
  if (no_manual == TRUE) {
    dataset <- dataset[is.na(dataset$Species.Manual.ID), ]
  } # drop observations with existing manual IDs if selected
  dataset <- dataset[!is.na(dataset$Species), ] # drop observations without species

  message("\n========== Starting Manual Vetting File Selection ==========")
  message(sprintf("Matching %d observations to WAV files in: %s", nrow(dataset), WAV_directory))

  WAV_directory_files <- .get_file_list(WAV_directory, fast_import) # get file list from the directory of WAV files
  dataset2 <- merge(dataset, WAV_directory_files, by = "File.Name", all.x = T) # merge the list of observations with the list of WAV files provided

  # Handle missing WAV files more gracefully: report, optionally list, then exclude
  missing_idx <- which(is.na(dataset2$Full.Path))
  if (length(missing_idx) > 0) {
    missing_files <- dataset2$File.Name[missing_idx]
    warning(sprintf(
      "%d observations have no matching WAV file in '%s'. These will be excluded.",
      length(missing_idx), WAV_directory
    ))
    # Only prompt if session is interactive
    if (interactive) {
      show_list <- readline(prompt = "Show missing file names? (y/n): ")
      if (tolower(show_list) %in% c("y", "yes")) {
        print(missing_files)
      }
    }
    # Exclude missing
    dataset2 <- dataset2[!is.na(dataset2$Full.Path), ]
    if (nrow(dataset2) == 0) {
      stop("All observations are missing WAV files; nothing to copy for manual vetting.")
    }
  }
  # proceed with sampling per species
  message(sprintf("\nCopying %.1f%% of files per species to: %s", percentage * 100, save_directory))

  for (i in species_list) {
    species_count <- sum(dataset2$Species == i)
    if (species_count == 0) {
      message(sprintf("  %s: No files found, skipping.", i))
      next
    }

    species_dir <- file.path(save_directory, i)
    if (!dir.exists(species_dir)) {
      dir.create(species_dir, recursive = TRUE)
    }

    sample_size <- ceiling(species_count * percentage)
    temp_dataset <- dataset2[sample(which(dataset2$Species == i), sample_size), ]

    copied <- file.copy(temp_dataset$Full.Path, species_dir)
    message(sprintf("  %s: Copied %d of %d files (%.1f%%)", i, sum(copied), species_count, percentage * 100))
  }

  message("\n========== Manual Vetting File Selection Complete ==========")
}
