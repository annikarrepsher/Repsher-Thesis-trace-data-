library(readxl)
zipdatacarib <- read_excel("~/Downloads/zipdatacarib.xlsx")
View(zipdatacarib)
library(readxl)
zipdataLA <- read_excel("~/Documents/zipdataLA.xlsx")
View(zipdataLA)

# Load required libraries
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

# === STEP 1: Load both Excel files ===
carib_raw <- read_excel("zipdatacarib.xlsx", sheet = "Sheet1", skip = 1)
la_raw <- read_excel("zipdataLA.xlsx", sheet = "Sheet1")

# === STEP 2: Clean Caribbean dataset ===

# Remove total row, rename ZIP column
carib_clean <- carib_raw %>%
  filter(`FFL ZIP CODE` != "TOTAL") %>%
  rename(Zipcode = `FFL ZIP CODE`) %>%
  mutate(Zipcode = str_pad(as.character(Zipcode), 5, pad = "0"))

# Replace NA with 0 across all numeric columns (excluding ZIP)
carib_clean[ , -1] <- carib_clean[ , -1] %>%
  mutate(across(everything(), ~replace_na(as.numeric(.), 0)))

# Rename columns properly
colnames(carib_clean) <- c(
  "Zipcode",
  paste0("Bahamas_", 2015:2024),
  paste0("Barbados_", 2015:2024),
  paste0("DominicanRep_", 2015:2024),
  paste0("Haiti_", 2015:2024),
  paste0("Jamaica_", 2015:2024),
  paste0("TrinidadTobago_", 2015:2024)
)

which(is.na(colnames(carib_clean)) | colnames(carib_clean) == "")

# Keep only columns 1 through 61
carib_clean <- carib_clean[, 1:61]

# Rename the columns properly again
colnames(carib_clean) <- c(
  "Zipcode",
  paste0("Bahamas_", 2015:2024),
  paste0("Barbados_", 2015:2024),
  paste0("DominicanRep_", 2015:2024),
  paste0("Haiti_", 2015:2024),
  paste0("Jamaica_", 2015:2024),
  paste0("TrinidadTobago_", 2015:2024)
)

# Now safely calculate totals
carib_clean <- carib_clean %>%
  mutate(Caribbean_Total = rowSums(across(starts_with(c(
    "Bahamas_", "Barbados_", "DominicanRep_", "Haiti_", "Jamaica_", "TrinidadTobago_"
  )))))
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(dplyr)
library(stringr)

# === STEP 1: Clean both datasets (if not already done) ===

# Ensure ZIP is character, 5-digit, and NA-safe in both datasets
la_clean <- la_clean %>%
  mutate(
    Zipcode = str_extract(as.character(Zipcode), "\\d{5}"),
    Zipcode = str_pad(Zipcode, 5, pad = "0")
  )

carib_clean <- carib_clean %>%
  mutate(Zipcode = str_pad(as.character(Zipcode), 5, pad = "0"))

# === STEP 2: Re-merge using full_join to preserve ALL ZIPs ===

zipdf_merged <- full_join(la_clean, carib_clean, by = "Zipcode")

# === STEP 3: Replace NA with 0 ONLY in numeric columns ===

zipdf_merged <- zipdf_merged %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))

zipdf_merged <- zipdf_merged %>%
  mutate(Global_Total = rowSums(across(where(is.numeric) & !contains("Total"))))



#ATTRIBUTE EACH ZIPCODE TO A STATE AND COUNTY 
library(zipcodeR)
# Ensure ZIP is 5-digit character
zipdf_merged$Zipcode <- stringr::str_pad(as.character(zipdf_merged$Zipcode), 5, pad = "0")

# Select ZIP info
zip_info <- zipcodeR::zip_code_db %>%
  select(zipcode, county, state)

# Join with merged dataset
zipdf_enriched <- zipdf_merged %>%
  left_join(zip_info, by = c("Zipcode" = "zipcode"))
zipdf_enriched <- zipdf_enriched %>%
  mutate(Global_Total = rowSums(across(where(is.numeric) & !contains("Total"))))
View(zipdf_enriched)
write.csv(zipdf_enriched,"~/Desktop/zipdf_enr.csv")
          
summary(zipdf_enriched)
min(zipdf_enriched$Global_Total)
max(zipdf_enriched$Global_Total)


# Identify columns to include in the Global_Total
cols_to_include <- colnames(zipdf_enriched)[
  sapply(zipdf_enriched, is.numeric) & 
    !grepl("total", colnames(zipdf_enriched), ignore.case = TRUE)
]

# Then calculate Global_Total safely
zipdf_enriched <- zipdf_enriched %>%
  mutate(Global_Total = rowSums(across(all_of(cols_to_include))))
cols_to_include
zipdf_enriched <- zipdf_enriched %>%
  filter(Zipcode != "TOTAL" & !is.na(state))  # remove TOTAL row and unassigned rows

max(zipdf_enriched$Global_Total)
#TEMPORAL TRENDS PLOTS
# Remove columns containing 2023 or 2024
View(zipdf_enriched)
library(dplyr)
zipdf_enriched <- zipdf_enriched %>%
  select(-matches("2023|2024"))
max(zipdf_enriched$Global_Total) 
View(zipdf_enriched)
unique(zipdf_enriched$state)
unique(zipdf_enriched$Zipcode)
#frequency count by state- states represented in the dataset
library(dplyr)
zipdf_enriched %>%
  count(state, sort = TRUE)
#how many ZIP codes are in each state:
library(dplyr)
zipdf_enriched %>%
  count(state, sort = TRUE)
#total number of firearms from each state
library(dplyr)
zipdf_enriched %>%
  group_by(state) %>%
  summarise(State_Export_Total = sum(Global_Total, na.rm = TRUE)) %>%
  arrange(desc(State_Export_Total))
#number of weapons from each zipcode
zipdf_enriched %>%
  group_by(Zipcode) %>%
  summarise(Zipcode_Export_Total = sum(Global_Total, na.rm = TRUE)) %>%
  arrange(desc(Zipcode_Export_Total))
nrow(zipdf_enriched)

library(dplyr)

# Identify columns that contain 'total' but are NOT 'Global_Total'
cols_to_remove <- names(zipdf_enriched)[
  grepl("(?i)total", names(zipdf_enriched)) & names(zipdf_enriched) != "Global_Total"
]

# Print them so you can check
cat("Columns removed:\n")
print(cols_to_remove)

# Remove those columns
library(dplyr)
zipdf_enriched <- zipdf_enriched %>%
  select(-all_of(cols_to_remove))

# Confirm dimensions after removal
cat("\nNew dataset dimensions:\n")
print(dim(zipdf_enriched))
View(zipdf_enriched)
# Create a copy of the dataset without the Global_Total column
library(dplyr)
zipdf_enr_no_global <- zipdf_enriched %>%
  select(-Global_Total)

# Check that it worked
cat("New dataset created: zipdf_enr_no_global\n")
print(dim(zipdf_enr_no_global))
View(zipdf_enr_no_global)
write.csv(zipdf_enr_no_global,"~/Desktop/zipdf_enr_no_global.csv") 

sum(zipdf_enriched$Global_Total)
table(zipdf_enr_no_global$State)
nrow(zipdf_enr_no_global)
#--------------------------------------------------------------------------------
#ZIP TO TRACT CONVERSION WEIGHTED ALLOCATION 
library(readxl)
ZIP_TRACT_122023 <- read_excel("~/Downloads/ZIP_TRACT_122023.xlsx")
View(ZIP_TRACT_122023)
install.packages("tidyverse")
library(tidyverse)
library(readxl)
library(stringr)
library(dplyr)
# 1 Load HUD ZIP->TRACT crosswalk (Excel)
crosswalk <- read_excel("~/Downloads/ZIP_TRACT_122023.xlsx")
zip_dfnog <- zipdf_enr_no_global
rm(zipdf_enr_no_global)
# 2) Clean/format IDs (ZIP=5 chars; TRACT=11 chars)
library(dplyr)
library(tidyverse)
library(readxl)
library(stringr)
zip_dfnog <- zip_dfnog %>%
  mutate(
    Zipcode = str_pad(as.character(Zipcode), 5, pad = "0")
  )

crosswalk <- crosswalk %>%
  transmute(
    Zipcode = str_pad(as.character(ZIP), 5, pad = "0"),
    tract   = str_pad(as.character(TRACT), 11, pad = "0"),
    tot_ratio = as.numeric(TOT_RATIO)
  ) %>%
  filter(!is.na(tot_ratio), tot_ratio > 0)
# 3 only crosswalk rows for ZIPs that exist in my data
library(dplyr)
crosswalk <- crosswalk %>% semi_join(zip_dfnog %>% distinct(Zipcode), by = "Zipcode")
# 4) Identify which columns are COUNTS (everything except metadata)
non_count_cols <- c("Unnamed: 0", "Zipcode", "City", "State", "county", "state")
count_cols <- setdiff(names(zip_dfnog), non_count_cols)
# 5) Join (many-to-many) + allocate counts by TOT_RATIO
library(dplyr)
library(tidyverse)
library(readxl)
library(stringr)
zip_to_tract_long <- zip_dfnog %>%
  left_join(crosswalk, by = "Zipcode") %>%
  mutate(
    across(all_of(count_cols), ~ .x * tot_ratio)
  )
#Collapse to tract totals (summing allocated pieces)
library(dplyr)
library(tidyverse)
library(readxl)
library(stringr)
finaltract_df <- zip_to_tract_long %>%
  group_by(tract) %>%
  summarise(
    across(all_of(count_cols), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )
# 7) Quick QC checks
# a) How many ZIP rows failed to match any tract?
library(dplyr)
library(tidyverse)
library(readxl)
library(stringr)
unmatched_zips <- zip_dfnog %>%
  anti_join(crosswalk %>% distinct(Zipcode), by = "Zipcode")

cat("Unmatched ZIPs:", nrow(unmatched_zips), "\n")

# b) For matched ZIPs, TOT_RATIO should usually sum ~ 1 within each ZIP
library(dplyr)
ratio_check <- crosswalk %>%
  group_by(Zipcode) %>%
  summarise(ratio_sum = sum(tot_ratio), .groups = "drop") %>%
  summarise(
    min_sum = min(ratio_sum),
    max_sum = max(ratio_sum),
    mean_sum = mean(ratio_sum)
  )

print(ratio_check)
View(finaltract_df)
# actual unmatched ZIP values
library(dplyr)
unmatched_zips %>%
  distinct(Zipcode) %>%
  arrange(Zipcode) %>%
  print(n = 50)

# Check if any are not 5 digits (ZIP+4, NA, weird formatting)
library(dplyr)
library(tidyverse)
library(readxl)
library(stringr)
unmatched_zips %>%
  transmute(Zipcode, nchar_zip = nchar(Zipcode)) %>%
  count(nchar_zip, sort = TRUE)
#they weren't messy or po boxes, some are organization zips so I'm dropping them 
#DROPPING ALL UNMATCHED ZIPS 
library(dplyr)
library(tidyverse)
library(readxl)
library(stringr)
zip_to_tract_long <- zip_dfnog %>%
  inner_join(crosswalk, by = "Zipcode") %>%   # drops unmatched ZIPs intentionally
  mutate(across(all_of(count_cols), ~ .x * tot_ratio))

tract_df <- zip_to_tract_long %>%
  group_by(tract) %>%
  summarise(across(all_of(count_cols), ~ sum(.x, na.rm = TRUE)),
            .groups = "drop")

# total count per ZIP across all count columns
zip_totals <- zip_dfnog %>%
  mutate(
    total_counts_allcols = rowSums(
      across(all_of(count_cols)),
      na.rm = TRUE
    )
  )

# Calculation of how much is lost due to unmatched ZIPs
loss_summary <- zip_totals %>%
  mutate(matched = Zipcode %in% crosswalk$Zipcode) %>%
  summarise(
    matched_total   = sum(total_counts_allcols[matched], na.rm = TRUE),
    unmatched_total = sum(total_counts_allcols[!matched], na.rm = TRUE),
    unmatched_share = unmatched_total / (matched_total + unmatched_total)
  )

loss_summary

#double checking if things are right 
# ZIP-level total (matched ZIPs only)
library(dplyr)
zip_total <- zip_dfnog %>%
  semi_join(crosswalk, by = "Zipcode") %>%
  summarise(total = sum(rowSums(across(all_of(count_cols)), na.rm = TRUE))) %>%
  pull(total)

# Tract-level total
tract_total <- tract_df %>%
  summarise(total = sum(rowSums(across(all_of(count_cols)), na.rm = TRUE))) %>%
  pull(total)

zip_total - tract_total

View(tract_df)
censusconvertdf <- tract_df
rm(tract_df)
View(censusconvertdf)
#checked dataset, some are missing the leading zero; added here
censusconvertdf <- censusconvertdf %>%
  mutate(
    tract = stringr::str_pad(as.character(tract), 11, pad = "0")
  )
write.csv(censusconvertdf, "tractdf_weighted_TOT_RATIO_counts.csv")
#my final converted data set to be used in the models
library(readr)
tractdf_weighted_TOT_RATIO_counts <- read_csv("tractdf_weighted_TOT_RATIO_counts.csv")
View(tractdf_weighted_TOT_RATIO_counts)
#pick a couple of zipcodes with firearms, and double check that the counts add up
#- look into the way the counts have been divided 
View(tract_total)
sum(censusconvertdf$`MEXICO, 2016`)

#linking to github
install.packages("usethis")
usethis::use_git_ignore(".gitignore")
usethis::create_github_token()
# Run this in the CONSOLE
install.packages("gitcreds")
gitcreds::gitcreds_set()
