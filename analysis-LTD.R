###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Moodmecodynamics: Roel van Dooren, Roy de Kleijn, Bernhard Hommel, & Zsuzsika Sjoerds
###########################################################################################################################################################################################################################################################################################################################################################################################################################################
### Last adjustment on: 27-01-2020
### r.van.dooren@fsw.leidenuniv.nl

options(scipen = 20)
rm(list = ls()) # Clean up workspace.

###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Import libraries --------------------------------------------------------
###########################################################################################################################################################################################################################################################################################################################################################################################################################################

ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T)
}
packages <- c("plyr", "zeallot", "reshape2", "data.table", "car", "Rmisc", "report", "effsize", "BayesFactor",
              "ggplot2", "ez", "apaTables", "dplyr") # Make sure to load dplyr after plyr and ggplot2
ipak(packages)

###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Load the main workspace -------------------------------------------------
###########################################################################################################################################################################################################################################################################################################################################################################################################################################

load("./workspaces/mood-workspace.RData")

###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Merge data of the foraging runs -----------------------------------------
###########################################################################################################################################################################################################################################################################################################################################################################################################################################

leavetimes_baseline <- read.csv2('./data-processed/baseline-optimal-actual-leavetimes.txt', header = T, sep = ' ', dec = '.', stringsAsFactors = TRUE)
leavetimes_postmanipulation <- read.csv2('./data-processed/post-manipulation-optimal-actual-leavetimes.txt',  
                                         header = T, sep = ' ', dec = '.', stringsAsFactors = TRUE)

df.list <- list(baseline = leavetimes_baseline, post = leavetimes_postmanipulation)
c(leavetimes_baseline, leavetimes_postmanipulation) %<-% 
  lapply(names(df.list), function(df) {  
    dataframe_name = df # Extract the name of the dataframe
    df <- df.list[[dataframe_name]] # Extract the data
    if (dataframe_name == 'baseline') { df$phase = 'baseline' } else { df$phase = 'post-manipulation' }
    
    df <- df %>% mutate(diff = optimal - actual) %>%
      select(-c(actual, optimal)) %>% group_by(subjectID, condition, phase) %>%
      summarise(MU = mean(diff),
                SD = sd(diff, na.rm = T),
                N = sum(!is.na(diff))
      ) %>% ungroup() %>% arrange(MU)
    ; return(df)})

# Bind the two dataframes
merged_leavetimes <- rbind(leavetimes_baseline, leavetimes_postmanipulation) %>% arrange(subjectID)

# Let's remove participants which don't show any variance in the baseline trial
if (length(subj_to_remove) > 0) {
  merged_leavetimes <- merged_leavetimes[-which(merged_leavetimes$subjectID %in% subj_to_remove),]
}

# Check whether there are baseline differences in foraging behavior ~ mood induction condition
model <- ezANOVA(data = leavetimes_baseline[-which(leavetimes_baseline$subjectID %in% subj_to_remove),], 
                 dv = MU, wid = .(subjectID), between = .(condition), type = 3, detailed = TRUE, return_aov = T)
apa.ezANOVA.table(model); calculateEtaSquared(model)$ANOVA$partialetasquared

###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Perform regression analyses ---------------------------------------------
###########################################################################################################################################################################################################################################################################################################################################################################################################################################

# Detect whether there are outliers in the data
ggplot(merged_leavetimes) + geom_boxplot(aes(condition, MU, color = phase)) 
get.outliers <- function(df) { data.frame(MU = boxplot.stats(df$MU, coef = 2.2)$out) }
outliers <- merged_leavetimes %>% group_by(phase, condition) %>% do(get.outliers(.)) 
outliers.participants <- merged_leavetimes %>% inner_join(outliers) %>% pull(subjectID)
length(outliers.participants)

merged_leavetimes <- merged_leavetimes[-which(merged_leavetimes$subjectID %in% outliers.participants),]

# One of the questions we would like to answer, is whether or not valence and/or arousal ratings are correlated with foraging
# behavior. As a first step, we want to check whether the mean valence/arousal ratings during foraging (pre and post
# induction manipulation) predict optimal foraging behavior. Therefore, we can define two regression models:
#   1. Baseline behavior: Optimal foraging score ~ meanValence (= mean(1, 2)) * meanArousal (= mean(1, 2))
#   2. Post-induction behavior: Optimal foraging score ~ meanValence (= mean(5, 6)) * meanArousal (= mean(5, 6))

lm_dat_format <- dcast(setDT(merge(merged_leavetimes, mood_data)), 
    subjectID + condition + phase + MU + SD ~ block, value.var = c("pleasurerating", "arousalrating")) %>%
  group_by(subjectID, phase) %>% mutate(
    pleasurerating = ifelse(phase == 'baseline', mean(c(pleasurerating_1, pleasurerating_2)),
                              mean(c(pleasurerating_5, pleasurerating_6 ))),
    arousalrating = ifelse(phase == 'baseline', mean(c(arousalrating_1, arousalrating_2)),
                              mean(c(arousalrating_5, arousalrating_6 )))
    ) %>% 
  select(subjectID, condition, phase, MU, SD, pleasurerating, arousalrating) %>%
  ungroup() %>% group_by(phase) %>% mutate(
    pleasurerating = scale(pleasurerating, center = T, scale = F),
    arousalrating = scale(arousalrating, center = T, scale = F),
    MU = scale(MU, center = T, scale = F),
    SD = scale(SD, center = T, scale = F))

# First of all, let's check whether the newly created arousal and pleasure values were sign different for the conditions:
lm_dat_format %>% group_by(phase) %>%
  do(wrapper.t.test(., formula = arousalrating ~ condition) ) %>%
  mutate(t.rounded = round(t, 2), p.rounded = round(p, 3), direction = 'greater') %>%
  ungroup() %>% mutate(p.adjusted = round(p.adjust(p.rounded, method = "holm"), 3))

lm_dat_format %>% group_by(phase) %>%
  do(wrapper.t.test(., formula = pleasurerating ~ condition, direction = 'greater') ) %>%
  mutate(t.rounded = round(t, 2), p.rounded = round(p, 3)) %>%
  ungroup() %>% mutate(p.adjusted = round(p.adjust(p.rounded, method = "holm"), 3))

#########################
# 1) Baseline foraging ~ mood
#########################

simple <- lm(MU ~ arousalrating + pleasurerating, data = lm_dat_format %>% filter(phase == 'baseline'))
twoway <- update(simple, .~. + arousalrating : pleasurerating)
anova(simple, twoway); BIC(simple); BIC(twoway)
summary(simple)
simple %>% report() %>% table_long()

### Check model assumptions
# Assess assumption of independent errors
durbinWatsonTest(simple) # Note that p is bootstrapped, and will therefore always return slightly different results

# Assess the assumption of no multicollinearity
vif(simple) # No predictor's VIF > 10, all fine
1 / vif(simple) # Tolerance > 0.2, all fine

# Visually assess the assumptions of normality of residuals, linearity of residuals and homoscedasticity
par(mfrow = c(2, 2))
plot(simple)

# Pre-mood induction: regress MU on pleasure and arousal measurements
lmPleasureArousal <- lmBF(MU ~ pleasurerating + arousalrating, data = lm_dat_format %>% phase=="baseline")
lmPleasure <- lmBF(MU ~ pleasurerating, data = lm_dat_format %>% phase=="baseline")
lmArousal <- lmBF(MU ~  arousalrating, data = lm_dat_format %>% phase=="baseline") 

allBFs <- c(lmPleasureArousal, lmPleasure, lmArousal)
summary(allBFs)

#########################
# 2) Post-manipulation foraging ~ mood
#########################

simple <- lm(MU ~ arousalrating + pleasurerating, data = lm_dat_format %>% filter(phase == 'post-manipulation'))
twoway <- update(simple, .~. + arousalrating : pleasurerating)
anova(simple, twoway); BIC(simple); BIC(twoway)
summary(simple)
simple %>% report() %>% table_short()

### Check model assumptions
# Assess assumption of independent errors
durbinWatsonTest(simple) # Note that p is bootstrapped, and will therefore always return slightly different results

# Assess the assumption of no multicollinearity
vif(simple) # No predictor's VIF > 10, all fine
1 / vif(simple) # Tolerance > 0.2, all fine

# Visually assess the assumptions of normality of residuals, linearity of residuals and homoscedasticity
par(mfrow = c(2, 2))
plot(simple)

# Cache the data on disk
#write.csv2(lm_dat_format, './data-processed/between_subjects_lm.csv', row.names = F)

# Post-mood induction: regress MU on pleasure and arousal measurements
lmPleasureArousal <- lmBF(MU ~ pleasurerating + arousalrating, data = lm_dat_format %>% phase=="post-manipulation")
lmPleasure <- lmBF(MU ~ pleasurerating, data = lm_dat_format %>% phase=="post-manipulation")
lmArousal <- lmBF(MU ~  arousalrating, data = lm_dat_format %>% phase=="post-manipulation") 

allBFs <- c(lmPleasureArousal, lmPleasure, lmArousal)
summary(allBFs)

#########################
# 3) Delta foraging ~ delta mood
#########################

# Next, to have a look at more dynamic components, we look at how changes in valence and/or arousal ratings (delta-scores)
# correlate with changes in optimal foraging behavior (delta-score). In other words, we can define the following regression model:
#   1. Delta optimal foraging score (post - baseline) ~ deltaValence (= mean(5, 6) - mean(1, 2)) * deltaArousal (= mean(5, 6) - mean(1, 2))

lm_dat_format_delta <- lm_dat_format %>% group_by(subjectID) %>% mutate(
    deltaMU = MU[phase == 'post-manipulation'] - MU[phase == 'baseline'],
    deltaSD = SD[phase == 'post-manipulation'] - SD[phase == 'baseline'],
    deltaPleasure = pleasurerating[phase == 'post-manipulation'] - pleasurerating[phase == 'baseline'],
    deltaArousal = arousalrating[phase == 'post-manipulation'] - arousalrating[phase == 'baseline']
    ) %>% select(subjectID, condition, deltaMU, deltaSD, deltaPleasure, deltaArousal) %>% distinct()

simple <- lm(deltaMU ~  deltaArousal + deltaPleasure, data = lm_dat_format_delta )
twoway <- update(simple, .~. + deltaPleasure : deltaArousal)
anova(simple, twoway); BIC(simple); BIC(twoway)
summary(simple)
simple %>% report() %>% table_short()

### Check model assumptions
# Assess assumption of independent errors
durbinWatsonTest(simple) # Note that p is bootstrapped, and will therefore always return slightly different results

# Assess the assumption of no multicollinearity.
vif(simple) # No predictor's VIF > 10, all fine
1 / vif(simple) # Tolerance > 0.2, all fine

# Cache the data on disk
#write.csv2(lm_dat_format_delta, './data-processed/within_subjects_lm.csv', row.names = F)

# Regress deltaMU on deltaPleasure and deltaArousal
lmPleasureArousal <- lmBF(deltaMU ~ deltaPleasure + deltaArousal, data = lm_dat_format_delta)
lmPleasure <- lmBF(deltaMU ~ deltaPleasure, data = lm_dat_format_delta)
lmArousal <- lmBF(deltaMU ~  deltaArousal, data = lm_dat_format_delta) 

allBFs <- c(lmPleasureArousal, lmPleasure, lmArousal)
summary(allBFs)

###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Save the current workspace ----------------------------------------------
###########################################################################################################################################################################################################################################################################################################################################################################################################################################
save.image("./workspaces/LTD-workspace.RData")


###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Create plots ------------------------------------------------------------
###########################################################################################################################################################################################################################################################################################################################################################################################################################################

lm_dat_format_long <- reshape( 
  lm_dat_format_delta %>% select(-deltaSD), varying = c('deltaPleasure', 'deltaArousal'), direction = 'long', idvar = c('subjectID', 'condition'),
  v.name = 'rating', timevar = 'ratingtype', times = c('pleasurerating', 'arousalrating'), new.row.names = NULL
) %>% arrange(subjectID) %>% mutate(rating = as.numeric(rating))
lm_dat_format_long$ratingtype <- factor(lm_dat_format_long$ratingtype, labels = c('Arousal', 'Valence'), levels = c('arousalrating', 'pleasurerating'))

textSize = 20
png('./plots/Figure-4.png', width = 1000)
ggplot(data = lm_dat_format_long, aes(x = rating, y = deltaMU, group = ratingtype)) + 
  geom_point(color = 'grey') + facet_wrap(~ ratingtype) +
  geom_smooth(method = "lm", se = TRUE, color = 'black', linetype = 'dashed', aes(group = ratingtype)) +
  coord_cartesian(ylim = c(-40, 40), xlim = c(-4, 4), clip = 'off') + 
  scale_y_continuous(expand = c(0, 0), breaks = seq(-40, 40, 10)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(-4, 4, 1)) +
  labs(y = "Delta Leave Time Difference (LTD)", x = "Delta rating") +
  theme( plot.margin=unit(c(1, 1, 1, 1), 'cm'),
         legend.position = c(0.5, 1.05),
         legend.direction = 'horizontal',
         axis.line = element_line(linetype = "solid"), 
         axis.title = element_text(size = textSize, colour = "black"), 
         axis.title.x = element_text(margin=margin(15,0,0,0)),
         axis.title.y = element_text(margin=margin(0,15,0,0)),
         legend.title = element_text(size = textSize, colour = "black"), 
         axis.text = element_text(size = textSize, colour = "black"), 
         axis.text.x = element_text(margin=margin(10,0,0,0)), 
         axis.text.y = element_text(margin=margin(0,10,0,0)), 
         legend.text = element_text(size = textSize, colour = "black"), 
         legend.key = element_rect(fill = NA), 
         legend.background = element_rect(fill = NA), 
         axis.ticks = element_line(colour = "black", size = 0.5), 
         panel.grid.major = element_line(colour = NA, linetype = "blank"), 
         panel.grid.minor = element_line(colour = NA, linetype = "blank"), 
         panel.background = element_rect(fill = NA), 
         panel.spacing = unit(5, "lines"),
         strip.text.x = element_text(size = textSize, colour = 'black'),
         strip.background = element_rect(colour = 'white', fill = 'white'),
         plot.background = element_rect(fill = "white", colour = NA))
dev.off()