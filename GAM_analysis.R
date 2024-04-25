# --------------------------------------------------------------------------------- #
#                                                                                   #
#  Hippocampal volumes in UK Biobank are associated with APOE only in older adults  #
#                                                                                   #
#                                 Chaloemtoem et al                                 #
#                                                                                   #
#                                                                                   #
#  Script: gam_analysis.R                                                           #
#                                                                                   #
# --------------------------------------------------------------------------------- #


# _____________________________________ INPUT _____________________________________ #

# 1. regression_table (from the regression.R script)


# _____________________________________ SCRIPT ____________________________________ #

library(mgcv)
library(gratia)

# run gam
covars = c("sex", "BMI", "income", "education_years", "total_gray", "site", 
           "ever_daily_smoked", "pack_years", "alcohol_status", "week_drinks", 
           "site1_date", "site1_date_2", "site1_head_size", "site1_rfMRI_motion",
           "site2_date", "site2_date_2", "site2_head_size", "site2_rfMRI_motion",
           "site3_date", "site3_date_2", "site3_head_size", "site3_rfMRI_motion",
           "PC_1", "PC_2", "PC_3", "PC_4", "PC_5", "PC_6", "PC_7", "PC_8", "PC_9", "PC_10")

gam_res <- gam(formula(paste("hippo_total ~ s(age, by = apoe, bs = \"cs\") + apoe + ",
                             paste(covars, collapse = "+"))),
               data = H104_table, method = "REML")

summary(gam_res)

# make partial effect plots to visualize results
plot(gam_res, shade = T, pages = 1)
plot(gam_res, shade = T, pages = 1, shift = coef(gam_res)[1], seWithMean = T)

# get pairwise differences between smooths
gam_diff <- difference_smooths(gam_res, smooth = "s(age)", group_means = T, n = 1000)

# plot the pairwise differences
draw(gam_diff)

# plot e2/e3, e3/e4 and e4/e4 differences from e3/e3
my_colors <- RColorBrewer::brewer.pal(4, "RdBu")[c(1,2,4)]

gam_diff %>%
  mutate(Comparison = paste0(level_2, "-", level_1)) %>%
  filter(Comparison %in% c("e2e3-e3e3", "e3e4-e3e3", "e4e4-e3e3")) %>%
  ggplot(aes(x = age, y = -diff)) +
  geom_ribbon(aes(ymin = -lower, ymax = -upper, fill = Comparison), alpha = 0.1) +
  geom_line(aes(color = Comparison)) +
  geom_hline(yintercept = 0, linetype = "dashed", lwd = 0.2, alpha = 0.7) +
  labs(x = "Age", y = "Difference") +
  scale_color_manual(values = my_colors, labels = c("e2e3-e3e3", "e3e4-e3e3","e4e4-e3e3"))+
  scale_fill_manual(values = my_colors, labels = c("e2e3-e3e3", "e3e4-e3e3","e4e4-e3e3"))+
  theme_classic() +
  theme(aspect.ratio = 0.8)

# _________________________________ END OF SCRIPT _________________________________ #

# Notes:
# 1. gam_res results
#    the “Family” component tells us the model assumes a Gaussian or normal distribution of our errors
#    “Link function = identity” tells us that the model doesn’t transform the predictions
#    for smooths, coefficients are not printed because each smooth has several coefficients - one for each basis function
#    edf (effective degrees of freedom): complexity of the smooth where higher edf means more wiggly curves (e.g., 1 = straight line, 2 = quadratic)

# 2. partial effect plots
#    they show the component effect of each of the terms in the model
#    “s(age,5.63):apoee3e3” reflects partial effect of age smooth in e3e3 individuals
#    "seWithMean = T" plots the SEs of a partial effect term combined with the SEs of the model intercept because CIs at the mean value of a variable can be very tiny, and don’t reflect overall uncertainty in the model 
#    the shift argument can also be used to shift the scale by the value of the intercept so the partial effect plot has a more natural interpretation


