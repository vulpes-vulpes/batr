# batr 0.3.0

Major refactor of the package which should improve current usage and make future development, with a number of improvements along the way!

## New Features:

- Significant improvemtns in speed and robustness of import_guano
- Better organised plots, and added shared functions for consistency
- Added new plot functions to create maps (with optional basemaps) and average monthly activity
- Added additional sampling options to manual_vet_extractor
- Completely restructured report generation and added functionality for custom templates
- Added `species_names` parameter to plotting functions for custom species labels:
  - `species_activity_facet_plot()` - Relabel species in facet titles
  - `monthly_activity_plot()` - Relabel species by month
  - `summary_table()` - Relabel species column headers
- Added shared helper functions for species label mapping:
  - `.species_label_map()` - Build species code to label mapping
  - `.species_labeller()` - Create labeller for ggplot2 facets
- Added GitHub issue templates (Bug Report, Feature Request, Documentation)
- Expanded documentation with species_names examples in README and vignettes

## Bug Fixes:

- Fixed R CMD check note for undefined `setNames` function
- Fixed import_guano.R syntax error causing package load failure
- Improved error handling in species labelling functions
- Bug fixes for import_GUANO (update now works!)
- Basic bug fixes for import_logs
- Bug fixes to manual_vet_extractor
- Addressed numerous small issues that were making the package fail checks

## Documentation & Infrastructure:

- Updated README.Rmd with species_names usage examples
- Updated batr vignette with summary_table species_names example
- Added Rd documentation for new parameters
- Restructured package for clearer organisation
- Added comprehensive testing for all functions
- Added new vignettes to document features and workflows

---

# batr 0.2.1

## New Features:

- Added option to increase WAV file import speeds by adding "fast_import = T" to import_GUANO.

---

# batr 0.2.0

## Breaking Changes:

- Major rewrite, changing the way data are stored, everything is new and old things don't work, see the readme for details

## New Features:

- Added a NEWS file for updates :)