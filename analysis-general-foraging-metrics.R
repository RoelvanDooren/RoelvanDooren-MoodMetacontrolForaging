###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Moodmecodynamics: Roel van Dooren, Roy de Kleijn, Bernhard Hommel, & Zsuzsika Sjoerds
###########################################################################################################################################################################################################################################################################################################################################################################################################################################
### Last adjustment on: 27-01-2020
### r.van.dooren@fsw.leidenuniv.nl

options(scipen = 20)
rm(list = ls()) # Clean up workspace.

###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Load the main workspace -------------------------------------------------
###########################################################################################################################################################################################################################################################################################################################################################################################################################################

load("./workspaces/mood-workspace.RData")

###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Load required datafiles -------------------------------------------------
###########################################################################################################################################################################################################################################################################################################################################################################################################################################

foraging_data <- read.csv2('./data-foraging-both/visual_foraging_processed_array_data.txt', header = T, sep = ' ', dec = '.', stringsAsFactors = TRUE)

# Checking subject numbers based on our experimental logbook tells us the following (for more info, see extract-CRR-ARR-parameters.R):
#  - Subject 9 and 91 did not complete the full experiment: Remove expStartTime 20190308105914 & 20190305100659.
#  - Subject 31, 32 and 40 are double participant numbers: Recode subject numbers with expStartTime 20190326161208 & 20190326160652 & 20190308161404
#  - The baseline phase of subject 47 has been coded as subject 45 with expStartTime 20190312160113: Recode subject 45 with expStartTime 20190312160113
# Note: These subjects will be removed / recoded when processing the data.
expStartTime_to_remove <- c(20190308105914, 20190305100659)
expStartTime_to_recode <- setNames(as.list(c(231, 232, 240, 47)), c(20190326161208, 20190326160652, 20190308161404, 20190312160113))

foraging_data <- foraging_data %>% filter(!expStartTime %in% expStartTime_to_remove)
foraging_data <- foraging_data %>% group_by(expStartTime) %>%
  mutate(subjectID = ifelse(expStartTime %in% as.numeric(names(expStartTime_to_recode)), 
                       expStartTime_to_recode[which(expStartTime == as.numeric(names(expStartTime_to_recode)))][[1]], subjectID) )

foraging_data <- merge(foraging_data, mood_data %>% select(subjectID, condition) %>% distinct())
length(unique(mood_data$subjectID)) == length(unique(foraging_data$subjectID))

###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Confirmatory analyses ---------------------------------------------------
###########################################################################################################################################################################################################################################################################################################################################################################################################################################

# For each of the variables of interest, a linear regression model will be specified with condition (excited vs. sad) and 
# arousal- and valenceratings as predictors:
# - 1) Ratio unique to total amount of patches
# - 2) Median time in patches
# - 3) Variability in leave times
# - 4) Mean travel time between patches
# - 5) Time elapsed before first patch entered
# - 6) Total area explored
# - 7) Total berries collected

variables_of_interest <- c('ratio_patch_repetition', 'median_time_in_patches', 'sd_time_in_patches',
  'interbushinterval', 'time_before_first_patch_entered', 'area_visited', 'berries_found')

# Merge foraging and mood data
lm_dat_format_metrics <- 
  merge(foraging_data, data.table::dcast(setDT(mood_data), subjectID + condition ~ block, value.var = c("pleasurerating", "arousalrating")),
      by = c("subjectID", "condition")) %>%
  group_by(subjectID, block) %>% mutate(
    pleasurerating = ifelse(block == 'baseline', mean(c(pleasurerating_1, pleasurerating_2)),
                            mean(c(pleasurerating_5, pleasurerating_6 ))),
    arousalrating = ifelse(block == 'baseline', mean(c(arousalrating_1, arousalrating_2)),
                           mean(c(arousalrating_5, arousalrating_6 )))
  )

# Run linear regression models for each of the variables of interest
# (Note that the below code has to be executed twice, separately for the "baseline" and "main" blocks)
freq_models <- lapply(variables_of_interest, function(DV) {
  eval(parse(text = paste0('lm(', DV,' ~ arousalrating + pleasurerating + condition, data = lm_dat_format_metrics %>% filter(block == "baseline"))')))
})

# Print model results
for (m in 1:length(freq_models)) {
  write(paste0("\n============\nRegression with DV: ", variables_of_interest[m],"\n============\n"), stdout())
  simple <- freq_models[m][[1]]
  twoway <- update(simple, .~. + arousalrating : pleasurerating + arousalrating : condition + pleasurerating : condition)
  comparison <- anova(simple, twoway)
  
  if (comparison$`Pr(>F)`[2] > 0.05) {
    print('Updated model does not outperform simple effects model')
    print(comparison); print(BIC(simple)); print(BIC(twoway))
    
    print(summary(simple)); 
    print(simple %>% report())
    
    #write("\n============\nCheck of model assumptions\n============\n", stdout())
    #print(durbinWatsonTest(simple))
    #print(vif(simple)) # No predictor's VIF > 10, all fine
    #print(1 / vif(simple)) # Tolerance > 0.2, all fine
    
    # Visually assess the assumptions of normality of residuals, linearity of residuals and homoscedasticity
    #par(mfrow = c(2, 2))
    #plot(simple)
    
  } else {
    print('Updated model outperforms simple effects model')
  }
  readline(prompt="Press [enter] to show next analysis")
}

# Perform Bayesian analyses 
# (Note that the below code has to be executed twice, separately for the "baseline" and "main" blocks)
lm_dat_format_metrics$subjectID <- as.factor(lm_dat_format_metrics$subjectID); lm_dat_format_metrics$condition <- as.factor(lm_dat_format_metrics$condition)
bayes_models <- lapply(variables_of_interest, function(DV) {
  eval(parse(text = paste0('lmBF(', DV,' ~ pleasurerating + arousalrating + condition, data = lm_dat_format_metrics %>% filter(block=="baseline"))')))
})

for (m in 1:length(bayes_models)) {
  write(paste0("\n============\nBayesian Anova with DV: ", variables_of_interest[m],"\n============\n"), stdout())
  print(1/bayes_models[m][[1]]) # BF01
}


###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Save the current workspace ----------------------------------------------
###########################################################################################################################################################################################################################################################################################################################################################################################################################################
save.image("./workspaces/general-metrics-workspace.RData")
