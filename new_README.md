
<!-- README.md is generated from README.Rmd. Please edit that file -->

# batr

<!-- badges: start -->
<!-- badges: end -->

The goal of batr is to streamline analyses and reporting of bat acoustic
monitoring data containing [GUANO metadata](https://guano-md.org/). The
package includes code to extract GUANO data directly from attributed WAV
files, and to create summaries, plots, and analyses.

The package has been written as part of and to support conservation
research by the [Toronto Zoo’s Native Bat Conservation
Program](https://www.torontozoo.com/bats), and is shared here in case it
benefits anyone else. It remains a work in progress and feedback and
contributions are welcome.

Check development progress and latest changes [here](ADDME).

## Winter 2023 Update

A major update was released in Winter 2023 which involves a significant
shift in philosophy and rewrite of functionality. This broke all the old
functions, which are now depreciated but still present for the time
being. However, the trade off is that lots of things work better. New
features:

- Integrated GUANO code directly (credit, as always to [David
  Riggs](https://github.com/riggsd/guano-r)) to streamline installation
  and data import
- Streamlined data import and storage approach: you now store the entire
  project in a single RDATA file that is easier to manage
- Added the ability to add extra data to an existing RDATA file rather
  than recreating from scratch as before, and to update the RDATA file
  when GUANO in the original WAV files changes
- Works with **any** species codes in your GUANO, i.e. it is no longer
  hard-coded to only work with the eight bat species of Ontario

If anyone out there wants the old version (0.1.0) you can find it here
[here](https://github.com/vulpes-vulpes/batr/releases/tag/v0.1.0) and
the documentation [here](ADDME). Note that this version is hard-coded
for Ontario bat species only.

## Installation

Install the current version from [GitHub](https://github.com/) using the
following code:

``` r
# install.packages("devtools")
devtools::install_github("vulpes-vulpes/batr")
```

## Prerequisite: GUANO

This package is designed to function with bat acoustic monitoring data,
in the form of a collection of WAV files, that have been processed to
add species identifications and other descriptive information as
metadata. Specifically, the metadata should use the [GUANO
specification](https://github.com/riggsd/guano-spec/blob/master/guano_specification.md)
(thanks to David Riggs for creating this standard), which us used by
most modern bat recording devices and analysis software. At minimum your
files need to have the following columns for batr to use them:

- Species information in the form of species codes (e.g. “Epfu” for big
  brown bat, *Eptesicus fuscus*), either in “Species.Auto.ID” or
  “Species.Manual.ID”, or both, in which case manual ID is taken in
  preference over auto ID to create a “Species” Column on import
- Site names: a data field containing a unique reference name for each
  site where data was collected, this is specified by name as the
  “site_col” on import using the import_GUANO function
- Timestamp
- Latitude, with decimal latitude (can be called “Loc.Position.Lat” or
  just “Latitude”, if you have the former it will be renamed on import)
- Longitude, as above

## Using batr

### Importing Your Data

#### Primary Data (WAV files):

Primary input data, in the form of [GUANO](https://guano-md.org/)
attributed WAV files of bat echolocation calls, need to be arranged and
attributed correctly to work smoothly with batr. Regardless of the
recording and processing approach, *all* WAV files should have the
following GUANO fields at minimum:

- Timestamp (per [GUANO metadata](https://guano-md.org/) specification)
- Species Identification (Species.Auto.ID and/or Species.Maunal.ID)
- Loc Position (formatted as decimal lat / lon, e.g.: “00.000000
  -00.00000”, any number of decimal places can be specified)
- Location / Site Name (unique to each individual recording location,
  note that the [GUANO metadata](https://guano-md.org/)) specification
  does not define a specific field for these data, but batr requires it,
  users may choose their own field for these data and inform batr when
  using the `GUANO_loader` function)

#### Tertiary Data (recorder log / summary files):

Most automated bat acoustic monitoring hardware produce some form of log
file to track device activity, errors etc. These files contain valuable
data to determine survey effort: determining whether the unit was active
at a given time. This is valuable for many analyses, especially those
involving long-term data collection, because it makes it possible to
determine whether a result of zero bats observed in a given time period
occurred because there were zero bats, **or** because the recording
device was inactive for any reason at that time.

batr currently has the ability to import and parse such files from the
following devices:

- Anabat Swift: the daily CSV log files output by the Swift should be
  grouped in one directory per recording location, with te directory
  name **exactly** matching the Location / Site Name specified in the
  GUANO metadata of the WAV files
- Wildlife Acoustics SM3BAT, SM4BAT, SMmini: summary TXT files produced
  by Wildlife Acoustics recorders should be renamed to **exactly** match
  the Location / Site Name specified in the GUANO metadata of the WAV
  files, if there are multiple TXT files for the same monitoring
  location they can be distinguished with a counter following a hyphen
  in the file name (e.g. site-1.txt, site-2.txt)

If you wish to see log files from other devices support please get in
touch and send example files, and I’ll see what I can do.

### Reading GUANO:

The `GUANO_reader` function (see below) examines all WAV files in a
directory (and subdirectories), reads any [GUANO
metadata](https://guano-md.org/) that are present into a table, and
saves the resulting table as a tab delineated TXT file. **Depending on
the number of files included, reading the metadata may take a long
time** (thousands of files will easily take over an hour to read). The
advantage of reading to a TXT file in this manner is that the resulting
file is smaller, more portable and can read more quickly in future. A
limitation is that if the GUANO metadata for any of the WAV files is
modified, the TXT file must be recreated. Future plans include a
mechanism for partial updates, and ultimately database integration.

``` r
GUANO_reader(path_to_directory, project_name)
```

### Loading GUANO:

Once a TXT file has been generated using `GUANO_reader` function, it can
be quickly loaded for further analysis. `GUANO_loader` will read the TXT
file and produce four data frames in the environment:

- clean_data_test: a tidied and sanitised version of the GUANO metadata,
  with key elements useful for further analyses
- raw_data_test: a full reproduction of the GUANO metadata of the WAV
  files
- species_night_site: counts of species observation per species per
  night per site
- species_site_summary: the total number of observations of each species
  at each monitoring location

``` r
GUANO_loader(file = path_to_TXT_file, dataset_name = project_name, town.location = )
```

### Importing Log Files:

These data can be imported using the `log_file_parser` function (see
`?log_file_parser` for usage details). This function produces four data
frames in the environment as follows:

- active_dates: Y/N output of whether each recorder was active on each
  night during the monitoring period
- Gaps_test: monitoring gaps for each recorder (used to visualise survey
  effort in plots)
- monthly_active_nights: the number of nights that each recorder was
  active in each month
- uptimes: percentage uptime for each recorder throughout the monitoring
  period
