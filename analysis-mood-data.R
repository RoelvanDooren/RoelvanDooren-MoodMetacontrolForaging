###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Moodmecodynamics: Roel van Dooren, Roy de Kleijn, Bernhard Hommel, & Zsuzsika Sjoerds
###########################################################################################################################################################################################################################################################################################################################################################################################################################################
### Last adjustment on: 24-11-2020
### r.van.dooren@fsw.leidenuniv.nl

options(scipen = 3)
rm(list = ls()) # Clean workspace.

###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Import libraries --------------------------------------------------------
###########################################################################################################################################################################################################################################################################################################################################################################################################################################

ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T)
}
packages <- c("plyr", "zeallot", "Rmisc", "ggplot2", "cowplot", "MBESS", "rstatix", "psych", "ggpubr", "report", "lsmeans", 
              "effsize", "purrr", "gtools", "ez", "apaTables", "dplyr", "data.table", "car", "effsize", "BayesFactor") # Make sure to load dplyr after plyr and ggplot2
ipak(packages)

###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Load the main workspace -------------------------------------------------
###########################################################################################################################################################################################################################################################################################################################################################################################################################################

load("./workspaces/general-workspace.RData")

###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Define functions --------------------------------------------------------
###########################################################################################################################################################################################################################################################################################################################################################################################################################################

####  Parial ETA squared calculation #### 
calculateEtaSquared <- function(results, ...) {
  results$ANOVA$partialetasquared <- results$ANOVA$SSn/(results$ANOVA$SSn+results$ANOVA$SSd)
  loweretasquared <- c()
  upperetasquared <- c()
  for (cR in 1:nrow(results$ANOVA)) {
    Lims <- conf.limits.ncf(F.value = results$ANOVA$F[cR], conf.level = 0.95, df.1 <- results$ANOVA$DFn[cR], df.2 <- results$ANOVA$DFd[cR])
    Lower.lim <- Lims$Lower.Limit/(Lims$Lower.Limit + df.1 + df.2 + 1)
    Upper.lim <- Lims$Upper.Limit/(Lims$Upper.Limit + df.1 + df.2 + 1)
    if (is.na(Lower.lim)) {
      Lower.lim <- 0
    }
    if (is.na(Upper.lim)) {
      Upper.lim <- 1
    }
    loweretasquared <- c(loweretasquared,Lower.lim)
    upperetasquared <- c(upperetasquared,Upper.lim)
  }
  #results$ANOVA$partialetasquared.lower <- loweretasquared
  #results$ANOVA$partialetasquared.upper <- upperetasquared
  results
}

#### Assumption tests wrappers ####
# wrapper.shapiro.test <- function(DV, df) {
#   s <- shapiro.test(DV)
#   qqnorm(DV, main=paste0(unique(df$block), ' ', unique(df$condition)))
#   data.frame(statistic = s$statistic, p.value = s$p.value)
# } 

wrapper.levene.test <- function(DV, IV) {
  l <- leveneTest(DV, group = as.factor(IV))
}

#### Test wrappers ####
wrapper.t.test <- function(formula, df, equalVariances = T, pairedComparison = F, direction = 'two.sided', input = NULL) {
  # Perform frequentist t-test and extract parameters
  t = t.test(formula, df, var.equal = equalVariances, paired = pairedComparison, alternative = direction)
  d <- cohen.d(formula, df)
  
  # Perform Bayesian t-test and extract Bayes factor
  if (pairedComparison == T) {
    if (is.null(input)) { return("Error")
    } else { # BF10 will be extracted
      bayesfactor <- extractBF(ttestBF(x = df[df$block == input[1],]$rating, y = df[df$block == input[2],]$rating, paired=T))$bf }
  } else {
    if (direction == 'smaller') { interval = c(-Inf, 0) # BF-0 will be extracted
    } else if (direction == 'greater') { interval = c(0, Inf) #BF+0 will be extracted
    } else { interval = NULL } # BF10 will be extracted
    bayesfactor <- extractBF(ttestBF(formula=formula, data=df, paired=pairedComparison, nullInterval = interval)[1])$bf
  }
  
  # Store all parameters of interest
  data.frame(t=t$statistic, df=t$parameter, p=t$p.value, d=d$estimate, d.magnitude=d$magnitude, d.method=d$method, 
             lower95CI=d$conf.int[1], upper95CI=d$conf.int[2], bayesfactor=bayesfactor, direction=direction )
}

###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Read and process datafiles ----------------------------------------------
###########################################################################################################################################################################################################################################################################################################################################################################################################################################

#########################
# 1) Process the data generated by Python
#########################

mood_data_python <- data.frame()
subjects_to_remove_expStartTime <- c()
for (f in list.files('./data-mood/datafiles-python/', full.names = T)) {
  data <- read.csv2(f, dec='.', sep=',', header = TRUE, stringsAsFactors = FALSE)
  # Only process datafiles of participants that will not be ommited from the data (see line 83 of analysis.R).
  if (!unique(data$expStartTime) %in% expStartTime_to_remove) { # Removes subjects 9 and 91.
    # If required, replace subjectID based on expStartTime (see line 84).
    if (unique(data$expStartTime) %in% as.numeric(names(expStartTime_to_recode))) {
      data$subjectID = expStartTime_to_recode[which(unique(data$expStartTime) == as.numeric(names(expStartTime_to_recode)))][[1]]
    }
    mood_data_python <- rbind(mood_data_python, data)
  } else {
    subjects_to_remove_expStartTime = cbind(subjects_to_remove_expStartTime, unique(data$subjectID))
  }
}

# Filter out the relevant blocks: after the first foraging trial, and before the post-mood induction trial
mood_data_python <- mood_data_python %>% filter(block != 'example') %>%
  select(subjectID, condition, block, pleasurerating, arousalrating)

#########################
# 2) Process the data generated by E-prime
#########################

mood_data_eprime <- read.csv2('./data-mood/datafiles-eprime/merged-dataframe.txt', sep = '\t')
mood_data_eprime <- mood_data_eprime %>% filter(Procedure.Block. == "Mood" & Task != "Mood3") %>%
  mutate(Task = ifelse(Task == 'Mood2', 'post_moodmanipulation', 'pre_moodmanipulation')) %>%
  select(Subject, Session, SessionDate, SessionTime, Task, Pleasure, Arousal) %>% arrange(as.numeric(Subject))
names(mood_data_eprime) <- c('subjectID', 'Session', 'SessionDate', 'SessionTime', 'block', 'pleasurerating', 'arousalrating')  

# Only process datafiles of participants that will not be ommited from the data
mood_data_eprime <- mood_data_eprime[-which(mood_data_eprime$subjectID %in% subjects_to_remove_expStartTime),]

# If required, replace subjectID based on expStartTime (see line 84).
subj_to_check <- names(which(with(mood_data_eprime, table(subjectID)) != 2))
with(mood_data_eprime[mood_data_eprime$subjectID %in% subj_to_check,], table(subjectID, SessionDate, Session))
mood_data_eprime[mood_data_eprime$subjectID == 31 & mood_data_eprime$SessionDate == '03-26-2019',]$subjectID <- 231
mood_data_eprime[mood_data_eprime$subjectID == 32 & mood_data_eprime$SessionDate == '03-26-2019',]$subjectID <- 232
mood_data_eprime[mood_data_eprime$subjectID == 40 & mood_data_eprime$Session == 2,]$subjectID <- 240

# Add condition (sad vs. excited) as a factor to the dataframe
mood_data_eprime$condition <- ifelse(mood_data_eprime$subjectID %% 2 == 1, 'excited', 'sad')

# Since the ratings were scored from 1 - 9, we have to recalculate them. By subracting 5 from each of these values, we obtain a 
# range from -4 to 4.
mood_data_eprime$pleasurerating <- mood_data_eprime$pleasurerating - 5
mood_data_eprime$arousalrating <- mood_data_eprime$arousalrating - 5

#########################
# 3) Merge the datafiles
#########################

mood_data <- rbind(mood_data_python, mood_data_eprime %>% select(subjectID, condition, block, pleasurerating, arousalrating))
# Let's remove participants which don't show any variance in the baseline trial
if (length(subj_to_remove) > 0) {
  mood_data <- mood_data[-which(mood_data$subjectID %in% subj_to_remove),]
}

# Check whether the data is complete for all subjects
names(which(with(mood_data, table(subjectID)) != 6)) # All fine!

# Make sure that block is an ordered factor
mood_data$block <- factor(mood_data$block, ordered = T,
                          levels = c('baseline_pre_foraging', 'baseline_post_foraging', 'pre_moodmanipulation', 
                                     'post_moodmanipulation', 'main_pre_foraging', 'main_post_foraging'), 
                          labels = c('1', '2', '3', '4', '5', '6'))

###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Confirmatory analyses ---------------------------------------------------
###########################################################################################################################################################################################################################################################################################################################################################################################################################################

# Perform two separate repeated-measures ANOVAs on participants' affect ratings, separately for arousal and valence ratings, with :
# Condition (excited vs. sad) as a between-subject condition and Time point (1, 2, 3, 4, 5 and 6) as a within-subject factor.

# Refactor the data
mood_data <- as.data.frame(lapply(mood_data, function(x) if(is.factor(x)) factor(x) else x))

#########################
# 1) Repeated measures analyses
# Note that, apparently, there are numerical discrepancies between output in JASP and AnovaBF
# (see https://forum.cogsci.nl/discussion/4802/discrepant-results-between-jasp-amp-bayesfactor-repeated-measures-anova).
# See https://richarddmorey.github.io/BayesFactor/ for a good overview of how BayesFactor can be used.
#########################

options(contrasts = c("contr.sum", "contr.poly")) # Contrasts have to be set properly before lsmeans() works
# Perform the RM analysis on the arousal and valence ratings
variables_of_interest <- c('arousalrating', 'pleasurerating')
models <- lapply(variables_of_interest, function(DV) {
  eval(parse(text = paste0('ezANOVA(data = mood_data, wid = .(subjectID),
                      dv =', DV,', within = .(block), between = .(condition), type = 3, detailed = TRUE,
                      return_aov = T)')))
})

# Print model results
for (m in 1:length(models)) {
  write(paste0("\n============\nAnova with DV: ", variables_of_interest[m],"\n============\n"), stdout())
  print(models[m][[1]]$ANOVA)
  print(paste0('Descriptives with DV: ', variables_of_interest[m]))
  print(summarySE(mood_data, measurevar = variables_of_interest[m], groupvar = 'condition', na.rm = T))
}

# Bayesian repeated measures ANOVAs
mood_data$subjectID <- as.factor(mood_data$subjectID); mood_data$condition <- as.factor(mood_data$condition)
anovaBF(arousalrating ~ block*condition + subjectID, data = mood_data, whichRandom = "subjectID") # 1.51e27
anovaBF(pleasurerating ~ block*condition + subjectID, data = mood_data, whichRandom = "subjectID") # 8.89e110

#########################
# 2) Post hoc analyses
#########################

# Check assumption of homogeneity of variance
write("============\nLevene's Test\n============", stdout())
print(mood_data %>% group_by(block) %>% do(wrapper.levene.test(.$arousalrating, .$condition))) # Non-sign
print(mood_data %>% do(wrapper.levene.test(.$arousalrating, .$block))) # Sign

print(mood_data %>% group_by(block) %>% do(wrapper.levene.test(.$pleasurerating, .$condition))) # Mostly sign
print(mood_data %>% do(wrapper.levene.test(.$pleasurerating, .$block))) # Sign

write("============\nT-tests\n============", stdout())
# Perform post-hoc analyses on the effect of Time point
t.test.results_arousal <- mood_data %>% group_by(block) %>%
  do(wrapper.t.test(., formula = arousalrating ~ condition, direction = 'greater') ) %>%
  mutate(t.rounded = round(t, 2), p.rounded = round(p, 3)) %>%
  ungroup() %>% mutate(p.adjusted = round(p.adjust(p.rounded, method = "holm"), 3))
print(t.test.results_arousal)

t.test.results_valence <- mood_data %>% group_by(block) %>%
  do(wrapper.t.test(., formula = pleasurerating ~ condition, equalVariances = F, direction = 'greater') ) %>%
  mutate(t.rounded = round(t, 2), p.rounded = round(p, 3)) %>%
  ungroup() %>% mutate(p.adjusted = round(p.adjust(p.rounded, method = "holm"), 3))
print(t.test.results_valence)

# Perform post-hoc analyses on the main effect of timepoint
mood_data_long <- reshape( 
  mood_data, varying = c('pleasurerating', 'arousalrating'), direction = 'long', idvar = c('subjectID', 'condition', 'block'),
  v.name = 'rating', timevar = 'mood', times = c('pleasurerating', 'arousalrating'), new.row.names = NULL
) %>% arrange(subjectID) %>% mutate(rating = as.numeric(rating))
mood_data_long$mood <- factor(mood_data_long$mood, labels = c('Arousal', 'Valence'), levels = c('arousalrating', 'pleasurerating'))
mood_data_long$condition <- factor(mood_data_long$condition, labels = c('Excited', 'Sad'), levels = c('excited', 'sad'))

# Define the timepoint (i.e., block) combinations
combos <- alply(t(with(mood_data_long, combinations(n = length(unique(block)), r = 2, v = unique(block)) )), 2) %>%
  set_names(map_chr(., ~ paste(., collapse = "-")))

t.test.results_timepoint <-
  mood_data_long %>%
    group_split(mood) %>% 
    setNames(sort(unique(mood_data_long$mood))) %>% 
    map_df(function(x) {
      map_df(combos, function(y) { 
        filter(x, block %in% y) %>% 
          droplevels() %>% 
          #mutate(dir = ifelse(block %in% c(4, 5, 6), 'greater', 'two.sided')) %>%
          do(wrapper.t.test(., formula = rating ~ block, pairedComparison = T, input=y))
      }, .id = "contrast") %>% arrange(contrast)
    }, .id = "mood") %>%
  mutate(t.rounded = round(t, 2), p.rounded = round(p, 3)) %>%
  ungroup() %>% mutate(p.adjusted = round(p.adjust(p.rounded, method = "holm"), 3))
print(t.test.results_timepoint)

# One-tail t-tests to show whether participants in the excited condition significantly increase in arousal/valence, and
# whether participants in the sad conditions significantly decrease in arousal/valence from timepoint 3 to 4.
mood_data_long %>% filter(condition == 'Excited') %>%
  group_split(mood) %>% setNames(sort(unique(mood_data_long$mood))) %>%
  map_df(function(x) {
    map_df(combos, function(y) { 
      filter(x, block %in% y) %>% 
        droplevels() %>% 
        do(wrapper.t.test(., formula = rating ~ block, direction = 'less', pairedComparison = T, input=y))
    }, .id = "contrast") %>% arrange(contrast)
  }, .id = "mood") %>%
  mutate(t.rounded = round(t, 2), p.rounded = round(p, 3)) %>%
  ungroup() %>% mutate(p.adjusted = round(p.adjust(p.rounded, method = "holm"), 3)) %>% filter(contrast == '3-4')

mood_data_long %>% filter(condition == 'Sad') %>%
  group_split(mood) %>% setNames(sort(unique(mood_data_long$mood))) %>%
  map_df(function(x) {
    map_df(combos, function(y) { 
      filter(x, block %in% y) %>% 
        droplevels() %>% 
        do(wrapper.t.test(., formula = rating ~ block, direction = 'greater', pairedComparison = T, input=y))
    }, .id = "contrast") %>% arrange(contrast)
  }, .id = "mood") %>%
  mutate(t.rounded = round(t, 2), p.rounded = round(p, 3)) %>%
  ungroup() %>% mutate(p.adjusted = round(p.adjust(p.rounded, method = "holm"), 3)) %>% filter(contrast == '3-4')


###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Check the validity of the manipulation ----------------------------------
###########################################################################################################################################################################################################################################################################################################################################################################################################################################

# As defined in Jefferies, Smilek, Eich and Enns (2008), we can check whether participants fell into the mood quadrants 
# they had been assigned to: http://profiles.murdoch.edu.au/myprofile/lisa-jefferies/files/2013/01/Jefferies-Smilek-Eich-Enns-20081.pdf
subset_mood_data <- mood_data %>% group_by(condition) %>% filter(block == 5) %>%
  mutate(correct_quadrant = ifelse(condition == 'excited' & pleasurerating > 0 & arousalrating > 0 |
                                     condition == 'sad' & pleasurerating < 0 & arousalrating < 0, 'correct', 'incorrect'))

with(subset_mood_data, table(condition, correct_quadrant)) # 87.72% correct in excited condition, 23.33% correct in sad condition

###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Save the current workspace ----------------------------------------------
###########################################################################################################################################################################################################################################################################################################################################################################################################################################
save.image("./workspaces/mood-workspace.RData")

###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Plot results ------------------------------------------------------------
###########################################################################################################################################################################################################################################################################################################################################################################################################################################

mood_data_long$block <- factor(mood_data_long$block, ordered = T, levels = c('1', '2', '3', '4', '5', '6'), 
                               labels = c('1\nStart', '2\n05:00', '3\n07:00', '4\n17:00', '5\n19:00', '6\n24:00'))

eventColors = c('Sad' = 'blue', 'Excited' = '#E69F00'); eventTypes = c('Sad' = 'solid', 'Excited' = 'dashed')
shapeTypes = c('Sad' = 15, 'Excited' = 19); sizeTypes = c('Sad' = 3, 'Excited' = 3); textSize = 20

ggplot(data = mood_data_long, aes(x = block, y = rating, group = condition)) + 
  geom_rect(aes(x = block, y = rating, xmin = 3, xmax = 4, ymin = -4, ymax = 5), fill = 'grey50', alpha = .3) + # Indicate where the mood-manipulation occured
  geom_rect(aes(x = block, y = rating, xmin = 1, xmax = 2, ymin = -4, ymax = 5), fill = 'grey80', alpha = .3) + # Indicate where the pre-mood foraging task occured
  geom_rect(aes(x = block, y = rating, xmin = 5, xmax = 6, ymin = -4, ymax = 5), fill = 'grey80', alpha = .3) + # Indicate where the post-mood foraging task occured
  stat_summary(fun.y = mean, geom = "point", aes(color = condition, shape = condition, size = condition, size = 3)) + # Plot the mean ratings
  stat_summary(fun.y = mean, geom = "line", aes(color = condition, linetype = condition), size = 1) + # Plot a line that connects the dots
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", aes(color = condition, width = 0.2), show.legend = F) +
  geom_jitter(aes(color = condition), alpha = 0.05, height = 0, width = 0.05, show.legend = F) +
  geom_hline(yintercept = 0, linetype = 'dotted', color = 'black', show.legend = F) +
  scale_colour_manual(name = "", values = eventColors) +
  scale_size_manual(name = "", values = sizeTypes) +
  scale_linetype_manual(name = "", values = c('solid', 'dashed')) +
  scale_shape_manual(name = "", values = shapeTypes) +
  coord_cartesian(ylim = c(-4, 5), xlim = c(1, 6), clip = 'off') + 
  scale_y_continuous(expand = c(0, 0), breaks = seq(-4, 4, 1)) +
  labs(y = "Rating", x = "Time point and average time spent since first mood/arousal grid") +
  stat_compare_means(label = "p.signif", method = "t.test", paired = F, hide.ns = TRUE, label.y = 4, size = 10,
                     symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), symbols = c("***", "**", "*", "ns"))) +
  facet_wrap(~ mood) +
  theme( plot.margin = unit(c(3, 1, 1, 1), 'cm'),

         legend.position = c(0.475, 1.3),
         legend.direction = 'horizontal',         
         legend.title = element_text(size = textSize, colour = "black"), 
         legend.spacing.x = unit(0.70, 'cm'),
         legend.key.size = unit(5, 'line'),
         legend.text = element_text(size = textSize, colour = "black"), 
         legend.key = element_rect(color = "transparent", fill = "transparent"), 
         legend.background = element_rect(color = "transparent", fill = "transparent"), 
         
         axis.line = element_line(linetype = "solid"), 
         axis.title = element_text(size = textSize, colour = "black"), 
         axis.title.x = element_text(margin=margin(15,0,0,0)),
         axis.title.y = element_text(margin=margin(0,15,0,0)),
         axis.text = element_text(size = textSize, colour = "black"), 
         axis.text.x = element_text(margin=margin(10,0,0,0)), 
         axis.text.y = element_text(margin=margin(0,10,0,0)), 
         axis.ticks = element_line(colour = "black", size = 0.5), 
         
         panel.grid.major = element_line(colour = NA, linetype = "blank"), 
         panel.grid.minor = element_line(colour = NA, linetype = "blank"), 
         panel.background = element_rect(fill = "grey98"), 
         panel.spacing = unit(3, "lines"),
         strip.text.x = element_text(size = textSize, colour = 'black', vjust = 2),
         strip.background = element_rect(colour = 'white', fill = 'white'),
         plot.background = element_rect(fill = "white", colour = NA))#
  #geom_segment(aes(x = 1.5, y = -3, xend = 1.5, yend = -2.1), col = "black", arrow = arrow(length = unit(0.3, "cm"))) +
  #annotate("text", x = 1.5, y = -3.25, label = "Foraging task run", angle = 0, size = 5, colour = 'black', face="bold") +
  #geom_segment(aes(x = 5.5, y = -3, xend = 5.5, yend = -2.1), col = "black", arrow = arrow(length = unit(0.3, "cm"))) +
  #annotate("text", x = 5.5, y = -3.25, label = "Foraging task run", angle = 0, size = 5, colour = 'black', face="bold")
ggsave('./plots/Figure-3.png', antialias ='none', width = 13.3, height = 7)

# Create a heatmap: Obtain frequencies, separately for all mood and arousal ratings, for each of the blocks
populate.freq.vec <- function(df, name, count) {
  freq <- structure(rep(0, 9), names = -4:4) # Create vector
  freq[as.character(name)] <- count # Populate vector by frequency
  data.frame(freq)
}

df_heatmap <- dcast(mood_data_long %>% group_by(subjectID, condition, block, mood, rating) %>%
  summarise(n = n()) %>% ungroup() %>% group_by(subjectID, condition, block, mood) %>%
  do(populate.freq.vec(., .$rating, .$n)) %>%
  mutate(rating = row_number()-5) %>% filter(freq > 0), subjectID + condition + block + freq ~ mood) %>%
  ungroup() %>% group_by(block, condition, Arousal, Valence) %>% 
  summarise(Frequency = sum(freq))

heatmaps <- df_heatmap %>% group_by(block) %>%
  do(plots = ggplot(., aes(x=Arousal, y=Valence, fill=Frequency)) +
  geom_tile() + coord_cartesian(ylim = c(-4, 4), xlim = c(-4, 4), clip = 'off') +
  scale_fill_gradient(low="white", high="blue") + ggtitle(paste0('Time point ', unique(.$block))) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(-4, 4, 1)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(-4, 4, 1)) +
  facet_wrap(~condition) +
  theme( plot.margin = unit(c(3, 1, 1, 1), 'cm'),
         legend.position = c(0.775, 1.35),
         #legend.position = c(0.475, 1.3),
         legend.direction = 'horizontal',         
         legend.title = element_text(size = textSize, colour = "black"), 
         legend.spacing.x = unit(0.70, 'cm'),
         legend.key.size = unit(3, 'line'),
         
         axis.line = element_line(linetype = "solid"), 
         axis.title = element_text(size = textSize, colour = "black"), 
         axis.title.x = element_text(margin=margin(15,0,0,0)),
         axis.title.y = element_text(margin=margin(0,15,0,0)),
         #legend.title = element_text(size = textSize, colour = "black"), 
         #legend.spacing.x = unit(0.70, 'cm'),
         #legend.key.size = unit(3.25, 'line'),
         axis.text = element_text(size = textSize, colour = "black"), 
         axis.text.x = element_text(margin=margin(10,0,0,0)), 
         axis.text.y = element_text(margin=margin(0,10,0,0)), 
         legend.text = element_text(size = textSize, colour = "black"), 
         legend.key = element_rect(fill = NA), 
         legend.background = element_rect(fill = NA), 
         axis.ticks = element_line(colour = "black", size = 0.5), 
         panel.grid.major = element_line(colour = NA, linetype = "blank"), 
         panel.grid.minor = element_line(colour = NA, linetype = "blank"), 
         panel.background = element_rect(fill = "white"), 
         panel.spacing = unit(5, "lines"),
         strip.text.x = element_text(size = textSize, colour = 'black'),
         strip.background = element_rect(colour = 'white', fill = 'white'),
         plot.title = element_text(size = textSize, colour = 'black', vjust = 5),
         plot.background = element_rect(fill = "white", colour = NA))
  ) 

# Save heatmaps
for (plot in seq(1, length(heatmaps$plots)) ) {
  lapply(heatmaps$plots[plot], print)
  ggsave(paste0('./plots/Heatmap-', plot,'.png'), antialias ='none', width = 13.3, height = 7)
}