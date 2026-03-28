#!/usr/bin/env Rscript
# Demonstration of manual_vet_summary function

library(batr)

# Create sample data simulating a vetting workflow
# 9 total files across 3 species and 2 sites
# 6 files have been manually vetted (Manually.Vetted = "Y")
# Some vetted files show mismatches between Auto.ID and Manual.ID

observations <- data.frame(
    Species = c("Epfu", "Epfu", "Epfu", "Epfu", "Labo", "Labo", "Labo", "Mylu", "Mylu"),
    Location_label = c("Site1", "Site1", "Site2", "Site2", "Site1", "Site2", "Site2", "Site1", "Site1"),
    Species.Auto.ID = c("Epfu", "Epfu", "Epfu", "Epfu", "Labo", "Labo", "Labo", "Mylu", "Mylu"),
    Species.Manual.ID = c("Epfu", "Mylu", "Epfu", NA, "Labo", "Labo", NA, "Mylu", NA),
    Manually.Vetted = c("Y", "Y", "Y", NA, "Y", "Y", NA, "Y", NA),
    stringsAsFactors = FALSE
)

# Save to temporary file
temp_rdata <- tempfile(fileext = ".RData")
save(observations, file = temp_rdata)

cat("\n========================================\n")
cat("Manual Vetting Summary Demonstration\n")
cat("========================================\n")

cat("\nSample Data:\n")
cat("- Total files: 9\n")
cat("- Species: Epfu (4), Labo (3), Mylu (2)\n")
cat("- Manually vetted: 6 files\n")
cat("- Mismatches: 1 file (Epfu auto-ID'd as Mylu manually)\n\n")

# Example 1: Summary by species
cat("========================================\n")
cat("Example 1: Summary by Species\n")
cat("========================================\n\n")
result_species <- manual_vet_summary(temp_rdata, stratify_by = "species")
print(result_species)

# Example 2: Summary by species and site
cat("\n========================================\n")
cat("Example 2: Summary by Species and Site\n")
cat("========================================\n\n")
result_both <- manual_vet_summary(temp_rdata, stratify_by = "species_and_site")
print(result_both)

# Example 3: Filtered to specific species
cat("\n========================================\n")
cat("Example 3: Filtered to Epfu and Labo\n")
cat("========================================\n\n")
result_filtered <- manual_vet_summary(
    temp_rdata,
    stratify_by = "species",
    species_list = c("Epfu", "Labo")
)
print(result_filtered)

# Clean up
unlink(temp_rdata)

cat("\n========================================\n")
cat("Column Descriptions:\n")
cat("========================================\n")
cat("Total_Files:    Total files for the species/site\n")
cat("Vetted_Count:   Files marked with 'Y' in Manually.Vetted\n")
cat("Percent_Vetted: Percentage of files that have been vetted\n")
cat("Match_Count:    Vetted files where Auto.ID == Manual.ID\n")
cat("Percent_Match:  Agreement rate among vetted files\n")
cat("========================================\n\n")
