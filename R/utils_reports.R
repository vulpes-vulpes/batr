#' Core Report Rendering Function
#'
#' Internal function that handles all RMarkdown report rendering with validation,
#' error handling, and graceful fallback support.
#'
#' @param template_path Character. Path to the RMarkdown template file.
#' @param output_file Character. Filename for the rendered report (without directory).
#' @param output_dir Character. Directory to save the rendered report.
#' @param params List. Parameters to pass to rmarkdown::render().
#' @param format Character. Output format (default: "pdf_document").
#' @param quiet Logical. Suppress rendering messages (default: FALSE).
#'
#' @return A list with elements:
#'   - success: Logical indicating if rendering succeeded
#'   - output_path: Character path to rendered file (NULL if failed)
#'   - error: Character error message (NULL if successful)
#'
#' @keywords internal
render_report <- function(template_path,
                          output_file,
                          output_dir,
                          params,
                          format = "pdf_document",
                          quiet = FALSE) {
  # Validate template exists
  if (!file.exists(template_path)) {
    return(list(
      success = FALSE,
      output_path = NULL,
      error = paste0("Template not found: ", template_path)
    ))
  }

  # Validate output directory
  if (!dir.exists(output_dir)) {
    tryCatch(
      {
        dir.create(output_dir, recursive = TRUE)
      },
      error = function(e) {
        return(list(
          success = FALSE,
          output_path = NULL,
          error = paste0("Could not create output directory: ", e$message)
        ))
      }
    )
  }

  # Validate parameters
  if (!is.list(params) || length(params) == 0) {
    return(list(
      success = FALSE,
      output_path = NULL,
      error = "Parameters must be a non-empty list"
    ))
  }

  # Attempt rendering
  tryCatch(
    {
      output_path <- rmarkdown::render(
        input = template_path,
        output_file = output_file,
        output_dir = output_dir,
        output_format = format,
        params = params,
        envir = new.env(),
        clean = TRUE,
        quiet = quiet
      )

      list(
        success = TRUE,
        output_path = output_path,
        error = NULL
      )
    },
    error = function(e) {
      list(
        success = FALSE,
        output_path = NULL,
        error = paste0("Render error: ", e$message)
      )
    }
  )
}


#' Generate Single-Site Report
#'
#' Generates a report for a single site using the built-in single-site template.
#'
#' @param data_path Character. Path to an RData file with observations and log data.
#' @param output_dir Character. Directory to save the rendered report.
#' @param output_file Character. Filename for the report (default: "report.pdf").
#' @param site Character. Site name to include (required).
#' @param species Character vector. Species codes to include. If NULL (default),
#'   all species in the data will be included.
#' @param author Character. Author name to display in the report.
#' @param project_name Character. Project name to display in the report.
#' @param monitoring_start Date. Monitoring period start (optional).
#' @param monitoring_end Date. Monitoring period end (optional).
#' @param map Logical. Whether to include location map with basemap (default: FALSE).
#'
#' @return Invisibly, a list with success/failure status and output path.
#'
#' @examples
#' \dontrun{
#' single_site_report(
#'   data_path = "data.RData",
#'   output_dir = "reports/",
#'   output_file = "Site1_Report.pdf",
#'   site = "Site1",
#'   species = c("Epfu", "Mylu"),
#'   author = "Researcher",
#'   project_name = "Project X"
#' )
#' }
#'
#' @export
single_site_report <- function(data_path,
                               output_dir,
                               output_file = "report.pdf",
                               site,
                               species = NULL,
                               author = "",
                               project_name = "",
                               monitoring_start = NULL,
                               monitoring_end = NULL,
                               map = FALSE) {
  # Input validation
  if (!file.exists(data_path)) {
    stop("data_path does not exist: ", data_path)
  }

  # Normalize path to avoid working-directory issues during render
  data_path <- normalizePath(data_path)

  if (length(site) != 1 || is.na(site)) {
    stop("site must be a single site (length 1)")
  }

  # If species not specified, load all available species from the data
  if (is.null(species) || length(species) == 0 || all(is.na(species))) {
    load(data_path, envir = (data_env <- new.env()))
    if (exists("observations", envir = data_env)) {
      obs <- get("observations", envir = data_env)
      species <- unique(obs$Species)
      # Remove NA values and format as character vector
      species <- as.character(species[!is.na(species)])
      if (length(species) == 0) {
        stop("No species found in observations data")
      }
    } else {
      stop("observations not found in data file")
    }
  } else {
    # Remove NA values and format as character vector
    species <- as.character(species[!is.na(species)])
  }

  # Get template path
  template_path <- system.file("rmarkdown", "templates", "batr-single-site",
    "skeleton", "skeleton.Rmd",
    package = "batr"
  )

  if (template_path == "") {
    stop(
      "Built-in single-site template not found in package. ",
      "Please reinstall batr package."
    )
  }

  # Prepare parameters
  params <- list(
    data_path = data_path,
    site = site,
    species = species,
    author = author,
    project_name = project_name,
    monitoring_start = monitoring_start,
    monitoring_end = monitoring_end,
    map = map
  )

  # Render report
  result <- render_report(
    template_path = template_path,
    output_file = output_file,
    output_dir = output_dir,
    params = params,
    format = "pdf_document",
    quiet = FALSE
  )

  if (!result$success) {
    warning(
      "Report rendering failed for site ", site,
      ": ", result$error
    )
  } else {
    message("Report generated: ", result$output_path)
  }

  invisible(result)
}


#' Generate Multi-Site Report
#'
#' Generates a report for multiple sites using the built-in multi-site template.
#' Can handle few to many sites gracefully with progress feedback.
#'
#' @param data_path Character. Path to an RData file with observations and log data.
#' @param output_dir Character. Directory to save the rendered report.
#' @param output_file Character. Filename for the report (default: "report.pdf").
#' @param sites Character vector. Site names to include (required).
#' @param species Character vector. Species codes to include. If NULL (default),
#'   all species in the data will be included.
#' @param author Character. Author name to display in the report.
#' @param project_name Character. Project name to display in the report.
#' @param monitoring_start Date. Monitoring period start (optional).
#' @param monitoring_end Date. Monitoring period end (optional).
#' @param include_map Logical. Whether to include location map (default: TRUE).
#' @param basemap Logical. Whether to use basemap on location map (default: FALSE).
#'
#' @return Invisibly, a list with success/failure status and output path.
#'
#' @details
#' This function renders a single report covering all specified sites.
#' If progressr is installed, progress updates will be displayed during rendering.
#'
#' @examples
#' \dontrun{
#' multi_site_report(
#'   data_path = "data.RData",
#'   output_dir = "reports/",
#'   output_file = "All_Sites_Report.pdf",
#'   sites = c("Site1", "Site2", "Site3"),
#'   species = c("Epfu", "Mylu"),
#'   author = "Researcher",
#'   project_name = "Project X"
#' )
#' }
#'
#' @export
multi_site_report <- function(data_path,
                              output_dir,
                              output_file = "report.pdf",
                              sites,
                              species = NULL,
                              author = "",
                              project_name = "",
                              monitoring_start = NULL,
                              monitoring_end = NULL,
                              include_map = TRUE,
                              basemap = FALSE) {
  # Input validation
  if (!file.exists(data_path)) {
    stop("data_path does not exist: ", data_path)
  }

  # Normalize path to avoid working-directory issues during render
  data_path <- normalizePath(data_path)

  if (length(sites) == 0 || all(is.na(sites))) {
    stop("sites must be non-empty")
  }

  # If species not specified, load all available species from the data
  if (is.null(species) || length(species) == 0 || all(is.na(species))) {
    load(data_path, envir = (data_env <- new.env()))
    if (exists("observations", envir = data_env)) {
      obs <- get("observations", envir = data_env)
      species <- unique(obs$Species)
      # Remove NA values and format as character vector
      species <- as.character(species[!is.na(species)])
      if (length(species) == 0) {
        stop("No species found in observations data")
      }
    } else {
      stop("observations not found in data file")
    }
  } else {
    # Remove NA values and format as character vector
    species <- as.character(species[!is.na(species)])
  }

  # Get template path
  template_path <- system.file("rmarkdown", "templates", "batr-multi-site",
    "skeleton", "skeleton.Rmd",
    package = "batr"
  )

  if (template_path == "") {
    stop(
      "Built-in multi-site template not found in package. ",
      "Please reinstall batr package."
    )
  }

  # Prepare parameters
  params <- list(
    data_path = data_path,
    sites = sites,
    species = species,
    author = author,
    project_name = project_name,
    monitoring_start = monitoring_start,
    monitoring_end = monitoring_end,
    include_map = include_map,
    basemap = basemap
  )

  # Show progress if progressr is available
  progress_handler <- tryCatch(
    {
      requireNamespace("progressr", quietly = TRUE)
      progressr::with_progress({
        p <- progressr::progressor(along = seq_len(1))
        p(message = "Rendering report...")
        NULL
      })
    },
    error = function(e) NULL
  )

  # Render report
  result <- render_report(
    template_path = template_path,
    output_file = output_file,
    output_dir = output_dir,
    params = params,
    format = "pdf_document",
    quiet = FALSE
  )

  if (!result$success) {
    warning(
      "Report rendering failed for site(s) ", paste(sites, collapse = ", "),
      ": ", result$error
    )
  } else {
    message("Report generated: ", result$output_path)
  }

  invisible(result)
}


#' Generate Report from Custom Template
#'
#' Renders a report using a user-supplied RMarkdown template file from any location.
#'
#' @param template_path Character. Path to the user's RMarkdown template file.
#'   Can be absolute or relative to current working directory.
#' @param output_dir Character. Directory to save the rendered report.
#' @param output_file Character. Filename for the report (default: "report.pdf").
#' @param params List. Parameters to pass to the template. Must include all parameters
#'   that the template expects (e.g., data_path, sites, species, etc.).
#' @param format Character. Output format (default: "pdf_document").
#'   Other options: "html_document", "word_document", etc.
#'
#' @return Invisibly, a list with success/failure status and output path.
#'
#' @details
#' Users can create custom templates by copying built-in templates or starting
#' from scratch. Templates should be valid RMarkdown files with YAML front matter
#' declaring parameters using:
#'
#' \preformatted{
#' ---
#' title: "My Custom Report"
#' params:
#'   data_path: !r NA_character_
#'   sites: !r NA_character_
#'   species: !r NA_character_
#'   author: !r NA_character_
#' ---
#' }
#'
#' Templates are typically stored in a `_templates/` subdirectory within a project
#' for organization:
#'
#' \preformatted{
#' my_project/
#' +-- data.RData
#' +-- _templates/
#' |   +-- my_template.Rmd
#' |   +-- advanced_template.Rmd
#' +-- analysis.R
#' }
#'
#' @examples
#' \dontrun{
#' # Using a custom template from a relative path
#' render_custom_report(
#'   template_path = "_templates/my_report.Rmd",
#'   output_dir = "reports/",
#'   output_file = "custom_analysis.pdf",
#'   params = list(
#'     data_path = "data.RData",
#'     sites = c("Site1", "Site2"),
#'     species = c("Epfu", "Mylu"),
#'     author = "Jane Researcher"
#'   )
#' )
#' }
#'
#' @export
render_custom_report <- function(template_path,
                                 output_dir,
                                 output_file = "report.pdf",
                                 params,
                                 format = "pdf_document") {
  # Validate template exists and is readable
  if (!file.exists(template_path)) {
    stop("Template file not found: ", template_path)
  }

  if (!file.access(template_path, mode = 4) == 0) {
    stop("Template file is not readable: ", template_path)
  }

  # Convert relative paths to absolute for clarity
  if (!grepl("^/", template_path) && !grepl("^[A-Za-z]:", template_path)) {
    template_path <- file.path(getwd(), template_path)
  }

  # Validate parameters
  if (!is.list(params)) {
    stop("params must be a list")
  }

  # Render using core function
  result <- render_report(
    template_path = template_path,
    output_file = output_file,
    output_dir = output_dir,
    params = params,
    format = format,
    quiet = FALSE
  )

  if (!result$success) {
    warning("Custom report rendering failed: ", result$error)
  } else {
    message("Custom report generated: ", result$output_path)
  }

  invisible(result)
}


#' List available report templates
#'
#' Returns a character vector of available batr report templates that can be
#' used with \code{\link{use_report_template}}.
#'
#' @return Character vector of template names
#' @export
#' @examples
#' list_report_templates()
list_report_templates <- function() {
  c("single-site", "multi-site")
}


#' Copy a report template to your project
#'
#' Copies a batr report template to your project directory so you can customize it.
#' Templates are ready-to-use RMarkdown files that can be modified for your specific needs.
#'
#' @param template Character string specifying which template to use.
#'   Options: "single-site" or "multi-site". Use \code{\link{list_report_templates}}
#'   to see all available templates.
#' @param path Character string specifying the destination directory.
#'   Defaults to current working directory.
#' @param filename Character string for the output filename (without extension).
#'   If NULL, uses a default name based on the template type.
#'
#' @return Invisibly returns the path to the created file
#' @export
#' @examples
#' \dontrun{
#' # Copy single-site template to current directory
#' use_report_template("single-site")
#'
#' # Copy to a specific location with custom name
#' use_report_template("multi-site",
#'   path = "reports/",
#'   filename = "my_custom_report"
#' )
#' }
use_report_template <- function(template = "single-site",
                                path = ".",
                                filename = NULL) {
  # Validate template choice
  available <- list_report_templates()
  if (!template %in% available) {
    stop(
      "Template '", template, "' not found. Available templates: ",
      paste(available, collapse = ", ")
    )
  }

  # Find template file
  template_file <- system.file(
    "rmarkdown/templates",
    paste0("batr-", template),
    "skeleton/skeleton.Rmd",
    package = "batr"
  )

  if (!file.exists(template_file)) {
    stop("Template file not found. Please reinstall batr.")
  }

  # Set default filename if not provided
  if (is.null(filename)) {
    filename <- paste0(gsub("-", "_", template), "_report")
  }

  # Create destination directory if needed
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  # Create destination path
  dest_file <- file.path(path, paste0(filename, ".Rmd"))

  # Check if file already exists
  if (file.exists(dest_file)) {
    if (interactive()) {
      response <- readline(prompt = sprintf(
        "File '%s' already exists. Overwrite? (y/n): ", dest_file
      ))
      if (!tolower(response) %in% c("y", "yes")) {
        message("Template copy cancelled.")
        return(invisible(NULL))
      }
    } else {
      stop("File '", dest_file, "' already exists. Use a different filename or remove the existing file.")
    }
  }

  # Copy file
  file.copy(template_file, dest_file, overwrite = TRUE)

  message("Report template copied to: ", dest_file)
  message("\nNext steps:")
  message("1. Open ", dest_file)
  message("2. Update the 'params' section with your data paths and settings")
  message("3. Knit the document to generate your report")

  invisible(dest_file)
}
