# --------------------------------------------------------------------------------- #
#                                                                                   #
#  Hippocampal volumes in UK Biobank are associated with APOE only in older adults  #
#                                                                                   #
#                                 Chaloemtoem et al                                 #
#                                                                                   #
#                                                                                   #
#  Script: sliding_window.R                                                         #
#                                                                                   #
# --------------------------------------------------------------------------------- #


# _____________________________________ INPUT _____________________________________ #

# 1. img_eids_filtered (from the relatedness_filter.R script)

# 2. apoe_table (from the apoe_processing.R script)

# 3. imputation_results (from the imputation.R script)

# 4. processed_imaging_covariates (from the covariates.R script)

# 5. UKB genetic principal components (data-field 22009)

# 6. Freesurfer-based neuroimaging data (data-fields 26642, 26663 and 26518)


# _____________________________________ SCRIPT ____________________________________ #

### Get total hippocampal volume residuals

# subset first 10 PCs and filter samples
names(genetic_PCs) <- gsub("n_22009_0", "PC", names(genetic_PCs))

genetic_PCs <- genetic_PCs %>%
  select(n_eid, PC_1, PC_2, PC_3, PC_4, PC_5, PC_6, PC_7, PC_8, PC_9, PC_10) %>%
  filter(n_eid %in% img_eids_filtered)

# filter for APOE genotypes of interest (e2/e3, e3/e3, e3/e4, e4/e4)
# create dummy variables (e3/e3 is the reference)
apoe_table <- apoe_table %>%
  filter(apoe %in% c("e2/e3", "e3/e3", "e3/e4", "e4/e4")) %>%
  mutate(e2e3 = ifelse(apoe == "e2/e3", 1, 0),
         e3e4 = ifelse(apoe == "e3/e4", 1, 0),
         e4e4 = ifelse(apoe == "e4/e4", 1, 0))

# prepare table for running regression and grabbing residuals
regression_table <- geno_apoe %>%
  left_join(imputation_results, by = "n_eid") %>%
  left_join(processed_imaging_covariates, by = "n_eid") %>%
  left_join(genetic_PC, by = "n_eid")

# grab hippocampal volume and total gray matter volume
# data-field 26642: left hippocampal volume (mm3)
# data-field 26663: right hippocampal volume (mm3)
# data-field 26518: total gray matter volume (mm3)
neuroimaging <- neuroimaging %>%
  select(n_eid, X26642.2.0, X26663.2.0, X26518.2.0)

# sum left and right hippocampal volume to get total hippocampal volume
neuroimaging <- neuroimaging %>%
  mutate(total_hippo = X26642.2.0 + X26663.2.0) %>%
  dplyr::rename("total_gray" = X26518.2.0) 

# add total_hippo and total_gray to regression_table
regression_table <- regression_table %>%
  left_join(neuroimaging %>% select(n_eid, total_hippo, total_gray), by = "n_eid")

# run regression
covars = c("sex", "BMI", "income", "education_years", "total_gray", "site", 
           "ever_daily_smoked", "pack_years", "alcohol_status", "week_drinks", 
           "site1_date", "site1_date_2", "site1_head_size", "site1_rfMRI_motion",
           "site2_date", "site2_date_2", "site2_head_size", "site2_rfMRI_motion",
           "site3_date", "site3_date_2", "site3_head_size", "site3_rfMRI_motion",
           "PC_1", "PC_2", "PC_3", "PC_4", "PC_5", "PC_6", "PC_7", "PC_8", "PC_9", "PC_10")

regression <- glm(formula(paste("total_hippo ~",
                                paste(covars, collapse = "+"))),
                  family = "gaussian", data = regression_table)

# grab the residuals
hippo_residuals <- data.frame("n_eid" = regression_table$n_eid,
                              "total_hippo_resid" = regression$residuals)


### Sliding window distributions

# prepare tables
sliding_table <- regression_table %>%
  select(n_eid, apoe, age, total_hippo) %>%
  mutate(age = round(age)) %>%
  left_join(hippo_residuals, by = "n_eid")

# set parameters
win_size = 5
min_age = round(min(sliding_table$age)+win_size)
max_age = round(max(sliding_table$age)-win_size)
  
# RAW VOLUME
sliding_avg_raw <- c("cen_age" = c(), "apoe" = c(), "n" = c(), "mean_vol" = c(), "sd" = c(), "se" = c())

for (i in min_age:max_age){
  mean_res <- sliding_table %>%
    mutate(age = round(age)) %>%
    filter(age %in% (i-win_size):(i+win_size)) %>%
    group_by(apoe) %>%
    summarize(n = n(), mean_vol = mean(total_hippo), sd = sd(total_hippo)) %>%
    mutate(se = sd/sqrt(n), cen_age = i) %>%
    dplyr::select(cen_age, apoe, n, mean_vol, sd, se)
  sliding_avg_raw <- rbind(sliding_avg_raw, mean_res)
}

sliding_avg_raw %>% 
  ggplot(aes(x = cen_age, y = mean_vol, color = apoe)) + 
  geom_point(alpha = 1, shape = 16) + 
  geom_line(alpha = 0.9) + 
  geom_ribbon(aes(ymin = mean_vol-(1.96*se), ymax = mean_vol+(1.96*se), fill = apoe), alpha = 0.2, color = NA) +
  scale_color_brewer(palette="RdBu") + scale_fill_brewer(palette="RdBu") +
  xlab("Age") + ylab("Volume (mm3)") + theme_classic() + 
  scale_y_continuous(n.breaks = 6) +
  theme(aspect.ratio = 0.9, panel.grid = element_blank(), legend.title = element_blank(), legend.background = element_blank())

# RESIDUAL VOLUME
sliding_avg_residual <- c("cen_age" = c(), "apoe" = c(), "n" = c(), "mean_vol" = c(), "sd" = c(), "se" = c())

for (i in min_age:max_age){
  mean_res <- sliding_table %>%
    mutate(age = round(age)) %>%
    filter(age %in% (i-win_size):(i+win_size)) %>%
    group_by(apoe) %>%
    summarize(n = n(), mean_vol = mean(hippo_residuals), sd = sd(hippo_residuals)) %>%
    mutate(se = sd/sqrt(n), cen_age = i) %>%
    dplyr::select(cen_age, apoe, n, mean_vol, sd, se)
  sliding_avg_residual <- rbind(sliding_avg_residual, mean_res)
}

sliding_avg_residual %>% 
  ggplot(aes(x = cen_age, y = mean_vol, color = apoe)) + 
  geom_point(alpha = 1, shape = 16) + 
  geom_line(alpha = 0.9) + 
  geom_ribbon(aes(ymin = mean_vol-(1.96*se), ymax = mean_vol+(1.96*se), fill = apoe), alpha = 0.2, color = NA) +
  scale_color_brewer(palette="RdBu") + scale_fill_brewer(palette="RdBu") +
  xlab("Age") + ylab("Volume (mm3)") + theme_classic() + 
  scale_y_continuous(n.breaks = 6) +
  theme(aspect.ratio = 0.9, panel.grid = element_blank(), legend.title = element_blank(), legend.background = element_blank())

sliding_avg_residual %>% 
  ggplot(aes(x = cen_age, y = mean_vol-offset, color = apoe)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black", alpha = 0.7, size = 0.2) +
  geom_point(alpha = 1, shape = 16) + 
  geom_line(alpha = 0.9) + 
  geom_ribbon(aes(ymin = mean_vol-(1.96*se)-offset, ymax = mean_vol+(1.96*se)-offset, fill = apoe), alpha = 0.2, color = NA) +
  scale_color_brewer(palette="RdBu") + scale_fill_brewer(palette="RdBu") +
  xlab("Age") + ylab("Residual volume (mm3)") + theme_classic() +
  theme(aspect.ratio = 0.9, panel.grid = element_blank(), legend.title = element_blank(), legend.background = element_blank())

# _________________________________ END OF SCRIPT _________________________________ #
