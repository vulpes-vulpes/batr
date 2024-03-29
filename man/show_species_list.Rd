% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/new_species_setter.R
\name{show_species_list}
\alias{show_species_list}
\title{Show a Species List that has been saved into an RData File}
\usage{
show_species_list(data_path)
}
\arguments{
\item{data_path}{Character. Path to an RData file to add activity data to.
Optional, a save location will be created before data are saved.}
}
\value{
A vector of the saved Species List, if one exists.
}
\description{
Reveals a Species List that has been saved into an existing Rdata file.
}
\examples{
\dontrun{set_species_list("C:/Folder/Folder/Data.RData")}
}
\seealso{
Other species tools: 
\code{\link{list_all_species}()},
\code{\link{set_species_list}()}
}
\concept{species tools}
