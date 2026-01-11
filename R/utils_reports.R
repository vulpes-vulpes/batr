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
#' @param include_map Logical. Whether to attempt map rendering (default: TRUE).
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
                          include_map = TRUE,
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

  # Add include_map to parameters if not present
  if (!("include_map" %in% names(params))) {
    params$include_map <- include_map
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
#' @param include_map Logical. Whether to include location map (default: TRUE).
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
                               include_map = TRUE) {
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
    include_map = include_map
  )

  # Render report
  result <- render_report(
    template_path = template_path,
    output_file = output_file,
    output_dir = output_dir,
    params = params,
    format = "pdf_document",
    include_map = include_map,
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
                              include_map = TRUE) {
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
    include_map = include_map
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
    include_map = include_map,
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
#' @param include_map Logical. Whether to attempt map rendering (default: TRUE).
#'   If template uses maps, consider setting to FALSE for offline operation.
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
#' ├── data.RData
#' ├── _templates/
#' │   ├── my_template.Rmd
#' │   └── advanced_template.Rmd
#' └── analysis.R
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
                                 format = "pdf_document",
                                 include_map = TRUE) {
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
    include_map = include_map,
    quiet = FALSE
  )

  if (!result$success) {
    warning("Custom report rendering failed: ", result$error)
  } else {
    message("Custom report generated: ", result$output_path)
  }

  invisible(result)
}
