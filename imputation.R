# --------------------------------------------------------------------------------- #
#                                                                                   #
#  Hippocampal volumes in UK Biobank are associated with APOE only in older adults  #
#                                                                                   #
#                                 Chaloemtoem et al                                 #
#                                                                                   #
#                                                                                   #
#  Script: imputation.R                                                             #
#                                                                                   #
# --------------------------------------------------------------------------------- #


# _____________________________________ INPUT _____________________________________ #

# 1. non_imaging_covariates (from the covariates.R script)

# 2. imaging_alcohol (from the alcohol_measures.R script)

# 3. imaging_smoking (from the smoking_measures.R script)

# 4. processed_imaging_covariates (from the covariates.R script)


# _____________________________________ SCRIPT ____________________________________ #

# merge the tables for imputation
imputation_table <- non_imaging_covariates %>%
  select(n_eid, age, sex, BMI, education_years, income) %>%
  left_join(imaging_smoking, by = "n_eid") %>%
  left_join(imaging_alcohol, by = "n_eid")

# set a list of the non-numeric variables
non_numeric <- c("n_eid", "sex", "ever_daily_smoked", "alcohol_status")

# before normalization, capture the mean and sd of all the columns in order to de-normalize some measures later
means <- imputation_table %>%
  summarize(across(-all_of(non_numeric), ~mean(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "covariate", values_to = "mean")

sd <- imputation_table %>%
  summarize(across(-all_of(non_numeric), ~sd(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "covariate", values_to = "sd")

pre_scaling <- left_join(means, sd, by = "covariate")

# normalize all the continuous / numeric measures
imputation_table <- imputation_table %>%
  mutate(across(-all_of(non_numeric), ~scale(.x, center = TRUE, scale = TRUE)))

# save and reload imputation table
# this makes the columns NOT matrices and fixes the cannot handle matrix error
write.csv(imputation_table, file = paste0(output_path, "pre_imputation.csv"), row.names = F)
imputation_table<- read.csv(paste0(output_path, "pre_imputation.csv"))

# run imputation using mice
post_imputation <- mice(imputation_table, m = 15, method = "cart", seed = 6)

# save the results to a table
imputation_results <- complete(post_imputation, 15)

# make a version without scaling using the means and sd saved in pre_scaling
unscaled_results <- imputation_results %>%
  select(-all_of(non_numeric[! non_numeric == "n_eid"])) %>%
  pivot_longer(cols = -n_eid, names_to = "covariate", values_to = "value") %>%
  left_join(pre_scaling, by = "covariate") %>%
  mutate(value = value * sd + mean) %>%
  select(-c(mean, sd)) %>%
  pivot_wider(id_cols = n_eid, names_from = covariate, values_from = value)

# add the non-numeric variables to unscaled_results
un_scaled %>%
  left_join(select(imputation_results, all_of(non_numeric)), by = "n_eid")

# add the unscaled columns for week_drinks, pack_years, and age to the main imputation results table
imputation_results <- imputation_results %>%
  dplyr::rename("scaled_week_drinks" = week_drinks) %>%
  dplyr::rename("scaled_pack_years" = pack_years) %>%
  dplyr::rename("scaled_age" = age) %>%
  left_join(select(un_scaled, c(n_eid, week_drinks, pack_years, age)), by = "n_eid")

# _________________________________ END OF SCRIPT _________________________________ #
