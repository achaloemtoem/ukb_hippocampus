# --------------------------------------------------------------------------------- #
#                                                                                   #
#  Hippocampal volumes in UK Biobank are associated with APOE only in older adults  #
#                                                                                   #
#                                 Chaloemtoem et al                                 #
#                                                                                   #
#                                                                                   #
#  Script: covariates.R                                                             #
#                                                                                   #
# --------------------------------------------------------------------------------- #


# _____________________________________ INPUT _____________________________________ #

# 1. img_eids_filtered (from the relatedness_filter.R script)

# 2. "Non-imaging" covariates file (age, genetic sex, BMI, education, income)

# 3. "Imaging" covariates file (site, scanning date, head size, rfMRI motion)


# _____________________________________ SCRIPT ____________________________________ #

### Prepare the tables

# filter participants
non_imaging_covariates  <- filter(non_imaging_covariates , n_eid %in% img_eids_filtered)
imaging_covariates <- filter(imaging_covariates , n_eid %in% img_eids_filtered)

# rename columns from data field IDs to something more readable
non_imaging_covariates <- non_imaging_covariates %>%
  dplyr::rename("age" = n_21003_2_0) %>%
  dplyr::rename("sex" = n_22001_0_0) %>%
  dplyr::rename("BMI" = n_21001_2_0) %>%
  dplyr::rename("education" = n_6138_2_0) %>%
  dplyr::rename("income" = n_738_2_0) %>%
  dplyr::rename("baseline_BMI" = n_21001_0_0) %>%
  dplyr::rename("baseline_education" = n_6138_0_0) %>%
  dplyr::rename("baseline_income" = n_738_0_0)

imaging_covariates <- imaging_covariates %>%
  dplyr::rename("site" = "X54.2.0") %>%
  dplyr::rename("date" = "X53.2.0") %>%
  dplyr::rename("head_size" = "X25000.2.0") %>%
  dplyr::rename("rfMRI_motion" = "X25741.2.0")


### Non-imaging covariates
# age, sex and BMI remains as is

# EDUCATION
# data-field 6138 asks about educational qualifications
edu_coding <- data.frame(coding = c(1, 2, 3, 4, 5, 6, -7, -3),
                         meaning = c("College or University degree",
                                     "A levels/AS levels or equivalent",
                                     "O levels/GCSEs or equivalent",
                                     "CSEs or equivalent",
                                     "NVQ or HND or HNC or equivalent",
                                     "Other professional qualifications eg: nursing, teaching",
                                     "None of the above", NA))

# convert this to years of education (see supplementary text)
non_imaging_covariates <- non_imaging_covariates %>%
  
  # imaging values
  mutate(education = plyr::mapvalues(education, edu_coding$coding, edu_coding$meaning)) %>%
  mutate(education_years = case_when(
    education == "None of the above" ~ 7,
    education == "CSEs or equivalent" ~ 10,
    education == "O levels/GCSEs or equivalent" ~ 10,
    education == "A levels/AS levels or equivalent" ~ 13,
    education == "Other professional qualifications eg: nursing, teaching" ~ 15,
    education == "NVQ or HND or HNC or equivalent" ~ 19,
    education == "College or University degree" ~ 20)) %>%
  
  # baseline values
  mutate(baseline_education = plyr::mapvalues(baseline_education, edu_coding$coding, edu_coding$meaning)) %>%
  mutate(baseline_education_years = case_when(
    baseline_education == "None of the above" ~ 7,
    baseline_education == "CSEs or equivalent" ~ 10,
    baseline_education == "O levels/GCSEs or equivalent" ~ 10,
    baseline_education == "A levels/AS levels or equivalent" ~ 13,
    baseline_education == "Other professional qualifications eg: nursing, teaching" ~ 15,
    baseline_education == "NVQ or HND or HNC or equivalent" ~ 19,
    baseline_education == "College or University degree" ~ 20))

# INCOME
# data-field 738 asks participants about their income category
income_coding <- data.frame(coding = c(1, 2, 3, 4, 5, -1, -3),
                            meaning = c("Less than 18,000",
                                        "18,000 to 30,999",
                                        "31,000 to 51,999",
                                        "52,000 to 100,000",
                                        "Greater than 100,000",
                                        NA, NA))

# convert into ordered numeric values by taking the lower bound of each income range
# do not know, prefer not to answer, and missing all converted to NA
non_imaging_covariates <- non_imaging_covariates %>%
  
  # imaging values
  mutate(income = plyr::mapvalues(income, income_coding$coding, income_coding$meaning)) %>%
  mutate(income = case_when(
    income == "Less than 18,000" ~ 0,
    income == "18,000 to 30,999" ~ 18000,
    income == "31,000 to 51,999" ~ 31000,
    income == "52,000 to 100,000" ~ 52000,
    income == "Greater than 100,000" ~ 100000)) %>%
  
  # baseline values
  mutate(baseline_income = plyr::mapvalues(baseline_income, income_coding$coding, income_coding$meaning)) %>%
  mutate(baseline_income = case_when(
    baseline_income == "Less than 18,000" ~ 0,
    baseline_income == "18,000 to 30,999" ~ 18000,
    baseline_income == "31,000 to 51,999" ~ 31000,
    baseline_income == "52,000 to 100,000" ~ 52000,
    baseline_income == "Greater than 100,000" ~ 100000))


### Backfill missing non-imaging covariate imaging values with baseline values

# set up a report to capture missing data before and after backfilling
missing_report <- data.frame("step" = rep("Before"),
                             "data_field" = c("BMI", "education_years", "income"),
                             "n_missing" = c(sum(is.na(non_imaging_covariates$BMI)),
                                             sum(is.na(non_imaging_covariates$education_years)),
                                             sum(is.na(non_imaging_covariates$income))))

# backfill missing imaging visit values
non_imaging_covariates <- non_imaging_covariates %>%
  mutate(BMI = ifelse(is.na(BMI), baseline_BMI, BMI)) %>%
  mutate(education_years = ifelse(is.na(education_years), baseline_education_years, education_years)) %>%
  mutate(income = ifelse(is.na(income), baseline_income, income))

# capture the number missing after the replacement step
missing_report <- rbind(missing_report, data.frame("step" = rep("After"),
                                                   "data_field" = c("BMI", "education_years", "income"),
                                                   "n_missing" = c(sum(is.na(non_imaging_covariates$BMI)),
                                                                   sum(is.na(non_imaging_covariates$education_years)),
                                                                   sum(is.na(non_imaging_covariates$income)))))

# view the missing report
missing_report %>%
  mutate(percent_missing = n_missing / nrow(non_imaging_covariates) * 100) %>%
  mutate(percent_missing = formatC(percent_missing, digits = 1, format = "f")) %>%
  kable(caption = paste0("Missing values in non-imaging covariates Before and After Replacement from Baseline (N=",
                         nrow(non_imaging_covariates), ")")) %>%
  kable_classic(full_width = F, html_font = "Cambria")

# rename and select just the essential non-imaging covariate columns
non_imaging_covariates <- non_imaging_covariates %>%
  select(n_eid, age, sex, BMI, education_years, income)


### Imaging covariates

# convert dates to numeric
# in r this is the number of days since Jan 1 1970 (https://statistics.berkeley.edu/computing/faqs/dates-and-times-r)
imaging_covariates <- imaging_covariates %>%
  mutate(date = as.Date(date)) %>%
  mutate(date = as.numeric(date))

# check the number of missing observations by imaging covariate
# note: we had no missing values except for rfMRI motion
imaging_covariates %>%
  summarise_at(vars(-n_eid), ~ sum(is.na(.x))) %>%
  pivot_longer(cols = everything(), names_to = "Covariate", values_to = "n_missing") %>%
  mutate(percent_missing = n_missing / nrow(imaging_covariates) * 100) %>%
  kable(caption = "Missing Values in Imaging Covariates") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# before normalization, capture the median and mad (median absolute deviation) in order to de-normalize later
# do this for all columns except site
medians <- imaging_covariates %>%
  summarize(across(c("date", "head_size", "rfMRI_motion"), ~median(.x, na.rm = T))) %>%
  pivot_longer(everything(), names_to = "covariate", values_to = "median")

mad <- imaging_covariates %>%
  summarize(across(c("date", "head_size", "rfMRI_motion"), ~mad(.x, na.rm = T))) %>%
  pivot_longer(everything(), names_to = "covariate", values_to = "mad")

pre_scaling <- left_join(medians, mad, by = "covariate")

# scale the confounds using the median and median absolute deviation * 1.48
imaging_covariates <- imaging_covariates %>%
  mutate_at(c("date", "head_size", "rfMRI_motion"),
            ~c(scale(.x, center = median(.x, na.rm = TRUE), scale = (mad(.x, , na.rm = TRUE) * 1.48))))

# remove outliers greater than 8
imaging_covariates <- imaging_covariates %>%
  mutate(date = ifelse(abs(date) > 8, NA, date)) %>%
  mutate(age = ifelse(abs(age) > 8, NA, age)) %>%
  mutate(head_size = ifelse(abs(head_size) > 8, NA, head_size)) %>%
  mutate(rfMRI_motion = ifelse(abs(rfMRI_motion) > 8, NA, rfMRI_motion))

# check the number of missing observations again
imaging_covariates %>%
  summarise_at(vars(-n_eid), ~ sum(is.na(.x))) %>%
  pivot_longer(cols = everything(), names_to = "Covariate", values_to = "n_missing") %>%
  mutate(percent_missing = n_missing / nrow(imaging_covariates) * 100) %>%
  kable(caption = "Missing Values in Imaging Covariates") %>%
  kable_classic(full_width = F, html_font = "Cambria")


### Split on site

# replace the numeric site code with a name to make it clear this is a categorical variable
site1_conf <- imaging_covariates %>%
  filter(site == "11025") %>%
  mutate(site = "site1")

site2_conf <- imaging_covariates %>%
  filter(site == "11026") %>%
  mutate(site = "site2")

site3_conf <- imaging_covariates %>%
  filter(site == "11027") %>%
  mutate(site = "site3")

# replace all NA and missing with the median for the site
site1_conf <- site1_conf %>%
  mutate(date = ifelse(is.na(date), median(site1_conf$date, na.rm = TRUE), date)) %>%
  mutate(head_size = ifelse(is.na(head_size), median(site1_conf$head_size, na.rm = TRUE), head_size)) %>%
  mutate(rfMRI_motion = ifelse(is.na(rfMRI_motion), median(site1_conf$rfMRI_motion, na.rm = TRUE), rfMRI_motion))

site2_conf <- site2_conf %>%
  mutate(date = ifelse(is.na(date), median(site2_conf$date, na.rm = TRUE), date)) %>%
  mutate(head_size = ifelse(is.na(head_size), median(site2_conf$head_size, na.rm = TRUE), head_size)) %>%
  mutate(rfMRI_motion = ifelse(is.na(rfMRI_motion), median(site2_conf$rfMRI_motion, na.rm = TRUE), rfMRI_motion))

site3_conf <- site3_conf %>%
  mutate(date = ifelse(is.na(date), median(site3_conf$date, na.rm = TRUE), date)) %>%
  mutate(head_size = ifelse(is.na(head_size), median(site3_conf$head_size, na.rm = TRUE), head_size)) %>%
  mutate(rfMRI_motion = ifelse(is.na(rfMRI_motion), median(site3_conf$rfMRI_motion, na.rm = TRUE), rfMRI_motion))

# get the means and standard deviations of each covariate by site
means <- site1_conf %>%
  summarize(across(c("date", "head_size", "rfMRI_motion"), ~mean(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "covariate", values_to = "site1_mean") %>%
  left_join(site2_conf %>%
              summarize(across(c("date", "head_size", "rfMRI_motion"), ~mean(.x, na.rm = TRUE))) %>%
              pivot_longer(everything(), names_to = "covariate", values_to = "site2_mean"), by = "covariate") %>%
  left_join(site3_conf %>%
              summarize(across(c("date", "head_size", "rfMRI_motion"), ~mean(.x, na.rm = TRUE))) %>%
              pivot_longer(everything(), names_to = "covariate", values_to = "site3_mean"), by = "covariate")

sds <- site1_conf %>%
  summarize(across(c("date", "head_size", "rfMRI_motion"), ~sd(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "covariate", values_to = "site1_sd") %>%
  left_join(site2_conf %>%
              summarize(across(c("date", "head_size", "rfMRI_motion"), ~sd(.x, na.rm = TRUE))) %>%
              pivot_longer(everything(), names_to = "covariate", values_to = "site2_sd"), by = "covariate") %>%
  left_join(site3_conf %>%
              summarize(across(c("date", "head_size", "rfMRI_motion"), ~sd(.x, na.rm = TRUE))) %>%
              pivot_longer(everything(), names_to = "covariate", values_to = "site3_sd"), by = "covariate")

pre_scaling <- pre_scaling %>%
  left_join(means, by = "covariate") %>%
  left_join(sds, by = "covariate")

# perform z-score transformation by site so that mean is zero and sd is 1
site1_conf <- site1_conf %>%
  mutate_at(c("date", "head_size", "rfMRI_motion"), ~c(scale(.x, center=TRUE, scale=TRUE)))

site2_conf <- site2_conf %>%
  mutate_at(c("date", "head_size", "rfMRI_motion"), ~c(scale(.x, center=TRUE, scale=TRUE)))

site3_conf <- site3_conf %>%
  mutate_at(c("date", "head_size", "rfMRI_motion"), ~c(scale(.x, center=TRUE, scale=TRUE)))

# add columns for date^2
site1_conf <- site1_conf %>%
  mutate(date_2 = date^2)

site2_conf <- site2_conf %>%
  mutate(date_2 = date^2)

site3_conf <- site3_conf %>%
  mutate(date_2 = date^2)


### Stitch the tables split by site together

# rename the columns of each table
site1_conf <- site1_conf %>%
  dplyr::rename("site1_date" = "date") %>%
  dplyr::rename("site1_date_2" = "date_2") %>%
  dplyr::rename("site1_head_size" = "head_size") %>%
  dplyr::rename("site1_rfMRI_motion" = "rfMRI_motion")

site2_conf <- site2_conf %>%
  dplyr::rename("site2_date" = "date") %>%
  dplyr::rename("site2_date_2" = "date_2") %>%
  dplyr::rename("site2_head_size" = "head_size") %>%
  dplyr::rename("site2_rfMRI_motion" = "rfMRI_motion")

site3_conf <- site3_conf %>%
  dplyr::rename("site3_date" = "date") %>%
  dplyr::rename("site3_date_2" = "date_2") %>%
  dplyr::rename("site3_head_size" = "head_size") %>%
  dplyr::rename("site3_rfMRI_motion" = "rfMRI_motion")

# add columns of zeros before merging site-specific tables
site1_conf <- site1_conf %>%
  mutate("site2_date" = 0, "site2_date_2" = 0, "site2_head_size" = 0, "site2_rfMRI_motion" = 0) %>%
  mutate("site3_date" = 0, "site3_date_2" = 0, "site3_head_size" = 0, "site3_rfMRI_motion" = 0)

site2_conf <- site2_conf %>%
  mutate("site1_date" = 0, "site1_date_2" = 0, "site1_head_size" = 0, "site1_rfMRI_motion" = 0) %>%
  mutate("site3_date" = 0, "site3_date_2" = 0, "site3_head_size" = 0, "site3_rfMRI_motion" = 0)

site3_conf <- site3_conf %>%
  mutate("site1_date" = 0, "site1_date_2" = 0, "site1_head_size" = 0, "site1_rfMRI_motion" = 0) %>%
  mutate("site2_date" = 0, "site2_date_2" = 0, "site2_head_size" = 0, "site2_rfMRI_motion" = 0)

# merge the site tables together
processed_imaging_covariates <- rbind(site1_conf, site2_conf, site3_conf)

# _________________________________ END OF SCRIPT _________________________________ #
