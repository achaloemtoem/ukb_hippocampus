# --------------------------------------------------------------------------------- #
#                                                                                   #
#  Hippocampal volumes in UK Biobank are associated with APOE only in older adults  #
#                                                                                   #
#                                 Chaloemtoem et al                                 #
#                                                                                   #
#                                                                                   #
#  Script: alcohol_measures.R                                                       #
#                                                                                   #
# --------------------------------------------------------------------------------- #


# _____________________________________ INPUT _____________________________________ #

# 1. apoe_table (from the apoe_processing.R script)

# 2. img_eids_filtered (from the relatedness_filter.R script)

# 3. Baseline and imaging visit alcohol data-fields (see supplementary table 5)


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


### Calculate the number of drinks per week based on imaging visit responses

imaging <- imaging %>%
  
  # make a column to indicate monthly or weekly frequency
  # data-field 1558: alcohol intake frequency
  mutate(week_month = case_when(
    is.na(n_1558) ~ "missing",
    n_1558 == -3 ~ "missing",
    n_1558 <= 3 ~ "week",
    n_1558 < 6 ~ "month",
    n_1558 == 6 ~ "never",
    TRUE ~ "missing")) %>%
  mutate(n_drinks = NA) %>%
  
  # MONTHLY
  # convert any strings to numeric
  mutate_at(vars(n_4407, n_4418, n_4429, n_4440, n_4451, n_4462), ~as.numeric(.x)) %>%
  
  # change any special (-1, -3) values to NA
  mutate_at(vars(n_4407, n_4418, n_4429, n_4440, n_4451, n_4462), ~ifelse(.x < 0, NA, .x)) %>%
  
  # add all the different drink types
  rowwise() %>%
  mutate(n_drinks = ifelse(week_month == "month",
                           sum(n_4407, n_4418, n_4429, n_4440, n_4451, n_4462, na.rm = TRUE), n_drinks)) %>%
  
  ungroup() %>%
  
  # if all categories were NA, put NA back as the sum (it gets turned to 0 by na.rm = TRUE)
  mutate(n_drinks = ifelse(week_month == "month",
                           ifelse(is.na(n_4407) & is.na(n_4418) & is.na(n_4429) & is.na(n_4440)
                                  & is.na(n_4451) & is.na(n_4462), NA, n_drinks), n_drinks)) %>%
  
  # WEEKLY
  # convert any strings to numeric
  mutate_at(vars(n_1568, n_1578, n_1588, n_1598, n_1608, n_5364), ~as.numeric(.x)) %>%
  
  # change any special (-1, -3) values to NA
  mutate_at(vars(n_1568, n_1578, n_1588, n_1598, n_1608, n_5364), ~ifelse(.x < 0, NA, .x)) %>%
  
  # add all the different drink types
  rowwise() %>%
  mutate(n_drinks = ifelse(week_month == "week",
                           sum(n_1568, n_1578, n_1588, n_1598, n_1608, n_5364, na.rm = TRUE), n_drinks)) %>%
  
  ungroup() %>%
  
  # if all categories were NA, put NA back as the sum (it gets turned to 0 by na.rm = TRUE)
  mutate(n_drinks = ifelse(week_month == "week",
                           ifelse(is.na(n_1568) & is.na(n_1578) & is.na(n_1588) & is.na(n_1598)
                                  & is.na(n_1608) & is.na(n_5364), NA, n_drinks), n_drinks)) %>%
  
  ### SUMMARIZE
  # assign those reporting they never drank 0 drinks
  mutate(n_drinks = ifelse(week_month == "never", 0, n_drinks)) %>%
  
  # convert n_drinks into drinks per week
  mutate(week_drinks = ifelse(week_month == "month", n_drinks / 4.34, n_drinks)) # 4.34 weeks per month
  

### Repeat with baseline visit responses

baseline <- baseline %>%
  mutate(week_month = case_when(
    is.na(n_1558) ~ "missing",
    n_1558 == -3 ~ "missing",
    n_1558 <= 3 ~ "week",
    n_1558 < 6 ~ "month",
    n_1558 == 6 ~ "never",
    TRUE ~ "missing")) %>%
  mutate(n_drinks = NA) %>%
  
  # MONTHLY
  mutate_at(vars(n_4407, n_4418, n_4429, n_4440, n_4451, n_4462), ~as.numeric(.x)) %>%
  mutate_at(vars(n_4407, n_4418, n_4429, n_4440, n_4451, n_4462), ~ifelse(.x < 0, NA, .x)) %>%
  rowwise() %>%
  mutate(n_drinks = ifelse(week_month == "month",
                           sum(n_4407, n_4418, n_4429, n_4440, n_4451, n_4462, na.rm = TRUE), n_drinks)) %>%
  ungroup() %>%
  mutate(n_drinks = ifelse(week_month == "month",
                           ifelse(is.na(n_4407) & is.na(n_4418) & is.na(n_4429) & is.na(n_4440)
                                  & is.na(n_4451) & is.na(n_4462), NA, n_drinks), n_drinks)) %>%
  
  # WEEKLY
  mutate_at(vars(n_1568, n_1578, n_1588, n_1598, n_1608, n_5364), ~as.numeric(.x)) %>%
  mutate_at(vars(n_1568, n_1578, n_1588, n_1598, n_1608, n_5364), ~ifelse(.x < 0, NA, .x)) %>%
  rowwise() %>%
  mutate(n_drinks = ifelse(week_month == "week",
                           sum(n_1568, n_1578, n_1588, n_1598, n_1608, n_5364, na.rm = TRUE), n_drinks)) %>%
  ungroup() %>%
  mutate(n_drinks = ifelse(week_month == "week",
                           ifelse(is.na(n_1568) & is.na(n_1578) & is.na(n_1588) & is.na(n_1598)
                                  & is.na(n_1608) & is.na(n_5364), NA, n_drinks), n_drinks)) %>%
  
  ### SUMMARIZE
  mutate(n_drinks = ifelse(week_month == "never", 0, n_drinks)) %>%
  mutate(week_drinks = ifelse(week_month == "month", n_drinks / 4.34, n_drinks))


### Derive participant drinking status
# data-field 3731 (former drinking) is asked only of those who indicated "Never" in data field 1558

imaging <- imaging %>%
  mutate(alcohol_status = NA) %>%
  mutate(alcohol_status = ifelse(week_month %in% c("week", "month"), "Current", alcohol_status)) %>%
  mutate(alcohol_status = ifelse(week_month == "never",
                                 case_when(
                                   n_3731 == 1 ~ "Former",
                                   n_3731 == 0 ~ "Never"), # missing and no answer remains NA
                                 alcohol_status))

baseline <- baseline %>%
  mutate(alcohol_status = NA) %>%
  mutate(alcohol_status = ifelse(week_month %in% c("week", "month"), "Current", alcohol_status)) %>%
  mutate(alcohol_status = ifelse(week_month == "never",
                                 case_when(
                                   n_3731 == 1 ~ "Former",
                                   n_3731 == 0 ~ "Never"), # missing and no answer remains NA
                                 alcohol_status))


### Replace missing imaging visit values with baseline responses

# set up a report to capture missing data before and after backfilling
missing_report <- data.frame("step" = rep("Before"),
                             "data_field" = c("week_drinks", "alcohol_status"),
                             "n_missing" = c(sum(is.na(imaging$week_drinks)),
                                             sum(is.na(imaging$alcohol_status))))

# rename baseline visit variables
baseline <- baseline %>%
  dplyr::rename("baseline_week_drinks" = week_drinks,
                "baseline_alcohol_status" = alcohol_status)

# join the tables and replace backfill missing imaging visit values
imaging <- imaging %>%
  left_join(baseline, by = "n_eid") %>%
  mutate(week_drinks = ifelse(is.na(week_drinks), baseline_week_drinks, week_drinks)) %>%
  mutate(alcohol_status = ifelse(is.na(alcohol_status), baseline_alcohol_status, alcohol_status))

# capture the number missing after the replacement step
missing_report <- rbind(missing_report, data.frame("step" = rep("After"),
                                                   "data_field" = c("week_drinks", "alcohol_status"),
                                                   "n_missing" = c(sum(is.na(imaging$week_drinks)),
                                                                   sum(is.na(imaging$alcohol_status)))))

# view the missing report
missing_report %>%
  mutate(percent_missing = n_missing / nrow(imaging) * 100) %>%
  mutate(percent_missing = formatC(percent_missing, digits = 1, format = "f")) %>%
  kable(caption = paste0("Missing values in Imaging Visit Alcohol Data Before and After Replacement from Baseline (N=",
                         nrow(imaging), ")")) %>%
  kable_classic(full_width = F, html_font = "Cambria")

# rename and select just the essential alcohol columns
imaging_alcohol <- imaging %>%
  select(n_eid, week_drinks, alcohol_status)
                     
# _________________________________ END OF SCRIPT _________________________________ #
