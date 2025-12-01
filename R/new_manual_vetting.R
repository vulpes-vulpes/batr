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
                                 species_list,
                                 percentage = 0.05,
                                 no_manual = FALSE,
                                 fast_import = TRUE) {
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
  message("Matching observations to WAV files.")
  WAV_directory_files <- .get_file_list(WAV_directory, fast_import) # get file list from the directory of WAV files
  dataset2 <- merge(dataset, WAV_directory_files, by = "File.Name", all.x = T) # merge the list of observations with the list of WAV files provided
  # Handle missing WAV files more gracefully: report, optionally list, then exclude
  missing_idx <- which(is.na(dataset2$Full.Path))
  if (length(missing_idx) > 0) {
    missing_files <- dataset2$File.Name[missing_idx]
    message(sprintf(
      "%d observations have no matching WAV file in '%s'. These will be excluded.",
      length(missing_idx), WAV_directory
    ))
    # Only prompt if session is interactive
    if (interactive()) {
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
  for (i in species_list) {
    message(paste("Copying", i, "vetting files to output directory."))
    species_dir <- file.path(save_directory, i)
    if (!dir.exists(species_dir)) { # Does species folder exist within 5% folder?
      dir.create(species_dir)
    } # If not then create it.
    temp_dataset <- dataset2[sample(which(dataset2$Species == i), (sum(dataset2$Species == i) * percentage)), ] # Create a temporary dataset with a random 5% of the rows for species.
    file.copy(
      temp_dataset$Full.Path,
      species_dir
    ) # Copy the five percent subset to the previously created species folder.
  }
}
