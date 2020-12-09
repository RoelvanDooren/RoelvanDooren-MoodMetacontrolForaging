###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Moodmecodynamics: Roel van Dooren, Roy de Kleijn, Bernhard Hommel, & Zsuzsika Sjoerds
###########################################################################################################################################################################################################################################################################################################################################################################################################################################
### Last adjustment on: 27-01-2020
### r.van.dooren@fsw.leidenuniv.nl

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

load("./workspaces/general-workspace.RData")

###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Plot supplementary figure -----------------------------------------------
###########################################################################################################################################################################################################################################################################################################################################################################################################################################

optimal_actual_leavetimes <- read.csv2('./data-processed/optimal-actual-leavetimes-0-to-20-in-100ms.txt',  
                                       header = T, sep = ' ', dec = '.')

opt_act_data <- optimal_actual_leavetimes %>% mutate(diff = optimal - actual) %>%
  select(-c(actual, optimal)) %>% group_by(subjectID, window) %>% 
  summarise(MU = mean(diff),
            SD = sd(diff, na.rm = T),
            N = sum(!is.na(diff)),
            upper_limit = MU + (SD/sqrt(N)),
            lower_limit = MU - (SD/sqrt(N))
  ) %>% ungroup() %>% arrange(MU)
rm(optimal_actual_leavetimes)

# Let's remove participants which don't show any variance in the baseline trial
subj_to_remove <- opt_act_data %>% select(subjectID, MU) %>% group_by(subjectID) %>%
  summarise(SD = sd(MU, na.rm = T)) %>% filter(SD == 0) %>% pull(subjectID)
opt_act_data <- opt_act_data[-which(opt_act_data$subjectID %in% subj_to_remove),]

textSize = 20
ggplot(data = opt_act_data %>% filter(subjectID == 7 & MU < 50), aes(x = window, y = MU)) + 
  geom_point(color = 'grey') +
  geom_hline(yintercept = 0, color = 'black', linetype = 'dotted') +
  coord_cartesian(ylim = c(-40, 40), xlim = c(0, 20), clip = 'on') + 
  scale_y_continuous(expand = c(0, 0), breaks = seq(-40, 40, 20)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 20, 2)) +
  geom_smooth(method = "lm", se = TRUE, color = 'black', linetype = 'dashed') +
  labs(y = "Leave Time Difference (LTD)", x = "Time window in seconds") +
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
         strip.text.x = element_text(size = textSize, colour = 'black'),
         strip.background = element_rect(colour = 'white', fill = 'white'),
         plot.background = element_rect(fill = "white", colour = NA))
ggsave('./plots/Figure-supplement.png', antialias ='none', width = 13.3)
