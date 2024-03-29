% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_Plots.R
\name{old_species_daily_site_plot}
\alias{old_species_daily_site_plot}
\title{Plot Activity for a Species at Each Site Within a Project}
\usage{
old_species_daily_site_plot(
  dataset,
  project_name,
  species,
  monitoring_start,
  monitoring_end,
  gaps = NULL,
  survey_year = NULL,
  save_directory = NULL,
  title = TRUE,
  y_scale = "free_y",
  width = 25,
  height = 20,
  text_size = 8,
  date_label = "\%b"
)
}
\arguments{
\item{dataset}{Object: a \code{species_night_site_projectname} data frame
genereted by the \code{GUANO_loader} function.}

\item{project_name}{Character, a brief and relevant reference phrase that will
be used to name the text file. Ideally matching project name used in other
functions.}

\item{species}{Character: the four letter species code for the species you
wish to plot. E.g. "Epfu".}

\item{monitoring_start}{Character, the date the monitoring began (e.g.
"2019-01-01")}

\item{monitoring_end}{Character, the date the monitoring ceased (e.g.
"2019-01-01")}

\item{gaps}{Object: a \code{gaps_projectname} data frame generated by the
\code{log_file_parser} function. Defaults to \code{null}.}

\item{survey_year}{Character: for projects with data from multiple years, to
select year}

\item{title}{Logical vector. Defaults \code{TRUE}, if \code{FALSE} title is
omitted}

\item{y_scale}{Character. Determines whether scales on the y-axis are matched
or free. Defaults to "free_y" for unmatched axis, set to "fixed" for matched
axis.}

\item{width}{Number. The width in cm of the plot if saved to file. Default is
25 cm.}

\item{height}{Number. The height of the plot if saved to file. Default is 20
cm.}

\item{text_size}{Numeric: adjusts the size of text in the figure.}

\item{save.directory}{Character: if provided a .png image of the plot will be
saved in the folder specified. Defaults to \code{NULL}: no output saved.}

\item{date_breaks}{Date value: adjusts the formatting of the month labels on
the y-axis. See https://www.statmethods.net/input/dates.html for formatting.}
}
\value{
A plot as an object in the current environment, and a saved image if
 selected.
}
\description{
\code{species_daily_site_plot} creates a plot of nightly activity for a chosen
species at all sites within a project.
}
\examples{
\dontrun{species_daily_site_plot(species_night_site_projectname, "Project Name", "Mylu", "2019-01-01", "2019-12-31")}

}
\seealso{
Other Basic Plots: 
\code{\link{monitoring_effort_plot}()},
\code{\link{monthly_activity_plot}()},
\code{\link{multi_species_daily_site_plot}()},
\code{\link{species_daily_site_plot}()},
\code{\link{species_site_aggregated_plot}()}
}
\concept{Basic Plots}
