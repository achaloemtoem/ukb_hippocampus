# --------------------------------------------------------------------------------- #
#                                                                                   #
#  Hippocampal volumes in UK Biobank are associated with APOE only in older adults  #
#                                                                                   #
#                                 Chaloemtoem et al                                 #
#                                                                                   #
#                                                                                   #
#  Script: smoking_measures.R                                                       #
#                                                                                   #
# --------------------------------------------------------------------------------- #


# _____________________________________ INPUT _____________________________________ #

# 1. apoe_table (from the apoe_processing.R script)

# 2. img_eids_filtered (from the relatedness_filter.R script)

# 3. Baseline and imaging visit smoking data-fields (see supplementary table 5)


# _____________________________________ SCRIPT ____________________________________ #

### Prepare the tables

# filter participants
baseline <- filter(baseline, n_eid %in% img_eids_filtered)
imaging <- filter(imaging, n_eid %in% img_eids_filtered)

# strip out the visit indicators on the data-field names
names(baseline) <- names(baseline) %>%
  gsub("_0_0", "", .)

names(imaging) <- names(imaging) %>%
  gsub("_2_0", "", .)


### Create a column to capture history of daily smoking (ever_daily_smoked)
# data-field 1239: current tobacco smoking
# data-field 1249: past tobacco smoking
# 1 = answered “Yes on most or all days” in data-field 1239 or “Smoked on most or all days” in data-field 1249
# 0 = everyone else, -3 = "prefer not to answer"

baseline <- baseline %>%
  mutate(ever_daily_smoked = case_when(
    is.na(n_1239) ~ -3,
    n_1239 == -3 ~ -3,
    n_1239 == 1 ~ 1,
    is.na(n_1249) ~ -3,
    n_1249 == -3 ~ -3,
    n_1249 == 1 ~ 1,
    TRUE ~ 0)) %>%
  mutate(ever_daily_smoked = ifelse(ever_daily_smoked == -3, NA, ever_daily_smoked))

imaging <- imaging %>%
  mutate(ever_daily_smoked = case_when(
    is.na(n_1239) ~ -3,
    n_1239 == -3 ~ -3,
    n_1239 == 1 ~ 1,
    is.na(n_1249) ~ -3,
    n_1249 == -3 ~ -3,
    n_1249 == 1 ~ 1,
    TRUE ~ 0)) %>%
  mutate(ever_daily_smoked = ifelse(ever_daily_smoked == -3, NA, ever_daily_smoked))


### Create a column to capture those that never smoked (never_smoked)
# data-field 2644: Light smoking, at least 100 smokes in lifetime
#                  Only asked if chose “Smoked occasionally” or “Just tried once or twice” in data-field 1249
# 1 = answered “I have never smoked” (4) in data-field 1249 or “No” in data-field 2644
# 0 = everyone else

baseline <- baseline %>%
  mutate(never_smoked = case_when(
    ever_daily_smoked == 1 ~ 0, # those that smoked daily were not asked about past smoking
    is.na(n_1249) ~ -3,
    n_1249 == -3 ~ -3,
    n_1249 == 4 ~ 1,
    is.na(n_2644) ~ -3,
    n_2644 == -1 ~ -3,
    n_2644 == -3 ~ -3,
    n_2644 == 0 ~ 1,
    TRUE ~ 0)) %>%
  mutate(never_smoked = ifelse(never_smoked == -3, NA, never_smoked))

imaging <- imaging %>%
  mutate(never_smoked = case_when(
    ever_daily_smoked == 1 ~ 0, # those that smoked daily were not asked about past smoking
    is.na(n_1249) ~ -3,
    n_1249 == -3 ~ -3,
    n_1249 == 4 ~ 1,
    is.na(n_2644) ~ -3,
    n_2644 == -1 ~ -3,
    n_2644 == -3 ~ -3,
    n_2644 == 0 ~ 1,
    TRUE ~ 0)) %>%
  mutate(never_smoked = ifelse(never_smoked == -3, NA, never_smoked))


### Assign pack years to those reporting light smoking or never smoked
# data-field 20161: pack years is assigned to those that smoked daily as defined by UKB (data-field 20160)
# those that never smoked (never_smokers = 1) -> 0 pack years
# those reporting light smoking (never daily but also more than 100) -> 0.5 pack years

baseline <- baseline %>%
  mutate(n_20161 = case_when(
    never_smoked == 1 ~ 0,
    n_2644 == 1 ~ 0.5,
    TRUE ~ n_20161)) %>%
  dplyr::rename("pack_years" = n_20161)

imaging <- imaging %>%
  mutate(n_20161 = case_when(
    never_smoked == 1 ~ 0,
    n_2644 == 1 ~ 0.5,
    TRUE ~ n_20161)) %>%
  dplyr::rename("pack_years" = n_20161)


### Replace missing imaging visit values with baseline responses

# set up a report to capture missing data before and after backfilling
missing_report <- data.frame("step" = rep("Before"),
                             "data_field" = c("ever_daily_smoked", "pack_years"),
                             "n_missing" = c(sum(is.na(imaging$ever_daily_smoked)),
                                             sum(is.na(imaging$pack_years))))

# rename baseline visit variables
baseline <- baseline %>%
  dplyr::rename("baseline_ever_daily_smoked" = ever_daily_smoked,
                "baseline_pack_years" = pack_years)

# join the tables and replace backfill missing imaging visit values
imaging <- imaging %>%
  left_join(baseline, by = "n_eid") %>%
  mutate(ever_daily_smoked = ifelse(is.na(ever_daily_smoked), baseline_ever_daily_smoked, ever_daily_smoked)) %>%
  mutate(pack_years = ifelse(is.na(pack_years), baseline_pack_years, pack_years))

# capture the number missing after the replacement step
missing_report <- rbind(missing_report, data.frame("step" = rep("After"),
                                                   "data_field" = c("ever_daily_smoked", "pack_years"),
                                                   "n_missing" = c(sum(is.na(imaging$ever_daily_smoked)),
                                                                   sum(is.na(imaging$pack_years)))))

# view the missing report
missing_report %>%
  mutate(percent_missing = n_missing / nrow(imaging) * 100) %>%
  mutate(percent_missing = formatC(percent_missing, digits = 1, format = "f")) %>%
  kable(caption = paste0("Missing values in Imaging Visit Smoking Data Before and After Replacement from Baseline (N=",
                         nrow(imaging), ")")) %>%
  kable_classic(full_width = F, html_font = "Cambria")

# rename and select just the essential smoking columns
imaging_smoking <- imaging %>%
  select(n_eid, ever_daily_smoked, pack_years)

# _________________________________ END OF SCRIPT _________________________________ #
