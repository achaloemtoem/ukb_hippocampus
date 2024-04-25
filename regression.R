# --------------------------------------------------------------------------------- #
#                                                                                   #
#  Hippocampal volumes in UK Biobank are associated with APOE only in older adults  #
#                                                                                   #
#                                 Chaloemtoem et al                                 #
#                                                                                   #
#                                                                                   #
#  Script: regression.R                                                             #
#                                                                                   #
# --------------------------------------------------------------------------------- #


# _____________________________________ INPUT _____________________________________ #

# 1. regression_table (from the sliding_window.R script)


# _____________________________________ SCRIPT ____________________________________ #

# categorize age and create dummy variables
regression_table <- regression_table %>%
  mutate(age = round(age)) %>%
  mutate(age_group = ifelse(age >= 70, "70+",
                            ifelse(age >= 60, "60-69",
                                   ifelse(age >= 44, "<60", NA)))) %>%
  mutate(age60_69 = ifelse(age_group == "60-69", 1, 0),
         age70 = ifelse(age_group == "70+", 1, 0))

# prepare list of variables
base_vars = c("e2e3", "e3e4", "e4e4", "age60_69", "age70")

apoe_age = c("e2e3:age60_69", "e2e3:age70",
             "e3e4:age60_69", "e3e4:age70",
             "e4e4:age60_69", "e4e4:age70")

covars = c("sex", "BMI", "income", "education_years", "total_gray", "site", 
           "ever_daily_smoked", "pack_years", "alcohol_status", "week_drinks", 
           "site1_date", "site1_date_2", "site1_head_size", "site1_rfMRI_motion",
           "site2_date", "site2_date_2", "site2_head_size", "site2_rfMRI_motion",
           "site3_date", "site3_date_2", "site3_head_size", "site3_rfMRI_motion",
           "PC_1", "PC_2", "PC_3", "PC_4", "PC_5", "PC_6", "PC_7", "PC_8", "PC_9", "PC_10")

# run regression without interaction between age and APOE
res <- glm(formula(paste("total_hippo ~",
                         paste(c(base_vars, covars), collapse = "+"))),
           family = "gaussian", data = regression_table)

res <- tidy(res)

# run regression with interaction between age and APOE
res_int <- glm(formula(paste("total_hippo ~",
                             paste(c(base_vars, apeo_age, covars), collapse = "+"))),
               family = "gaussian", data = regression_table)

res_int <- tidy(res_int)

# _________________________________ END OF SCRIPT _________________________________ #
