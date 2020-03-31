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
packages <- c("plyr", "zeallot", "Rmisc", "ggplot2", "ggpubr", "ez", "apaTables", "dplyr") # Make sure to load dplyr after plyr and ggplot2
ipak(packages)

###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Load the main workspace -------------------------------------------------
###########################################################################################################################################################################################################################################################################################################################################################################################################################################

load("./workspaces/general-workspace.RData")

###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Main text Figure 2 ------------------------------------------------------
###########################################################################################################################################################################################################################################################################################################################################################################################################################################

# Read datafile of one particular participant which can be used as an example
data <- read.csv2('./data-foraging-both/foraging-path-transposed/0068_20190312121547_visual_foraging_path_array_baseline.txt', dec = '.', sep = ',') # Load selected datafile.

# Remove pre-trial data
data <- data[-which(data$timespent < 0), ]

#########################
# 1) Define parameters 
#########################
data$rewardReceived <- 0; data$rewardReceived[data$berry_found == "True"] <- 1
interval = 9 # defined in seconds
data$avg_rewards <- NA; data$cur_rewards <- NA; 

# Calculate the ARR and CRR by looping through all rows in the data
for (r in 1:nrow(data)) {
  avg_rate <- mean(data$rewardReceived[1:r])
  data$avg_rewards[r] <- avg_rate
  
  if (data$timespent[r] > interval){ # Take the average reward over the last INTERVAL seconds (i.e., reward history)
    timestamps <- (data$timespent[r] - interval):data$timespent[r]
    cur_rate <- mean(data$rewardReceived[which(data$timespent >= min(timestamps) & data$timespent <= max(timestamps))])
    data$cur_rewards[r] <- cur_rate
  }
}

#########################
# 2) Extract patch enter and leave times
#########################

# Obtain the positions where participants entered a patch
enter_times <- data %>% select(view, timespent) %>% 
  filter(ifelse(view == lag(view), 'constant', 'switch') == 'switch') %>%
  filter(view == 'focused') %>% pull(timespent)

# Obtain the positions where participants left a patch
leave_times <- data %>% select(view, timespent) %>% 
  filter(ifelse(view == lag(view), 'constant', 'switch') == 'switch') %>% filter(view == 'aerial')
if (length(leave_times$timespent) < length(enter_times)) {
  leave_times <- leave_times %>% rbind(., c('aerial', max(data$timespent))) %>%
    mutate_at(grep("timespent", colnames(.)), list(as.numeric)) %>% pull(timespent)
} else { leave_times <- leave_times %>% pull(timespent) }

#########################
# 3) Extract intersection points
#########################
# For each patch visit, detect the peak amplitude of the CRR signal and extract the 
# first observed intersection point (i.e., ARR >= CRR) after this peak amplitude

peak_timestamps = c()
intersect.points.x = c()
intersect.points.y = c()
for (each_visit in 1:length(enter_times)) {
  # Extract the data for the current patch visit
  dat <- data[which(data$timespent >= enter_times[each_visit] & data$timespent <= leave_times[each_visit]),]
  
  # For the current time window, detect where the CRR peaks are located, 
  # but only for those positions where CRR > ARR.
  dat_peaks <- dat[find_peaks(dat$cur_rewards, m = 1),] %>% filter(cur_rewards > avg_rewards)
  
  # Find the timestamp with the maximum CRR (= highest amplitude / peak).
  peak_timestamp <- tail(dat_peaks$timespent[which(dat_peaks$cur_rewards == max(dat$cur_rewards, na.rm = T))], 1)  
  peak_timestamps <- c(peak_timestamps, peak_timestamp)
  
  # Next, let's use timestamps larger than the peak location timestamp to inform us on which 
  # intersection point (= CRR <= ARR) to extract.
  intersect.point_timestamp <- head(dat[dat$timespent > peak_timestamp,]$timespent[
    with(dat[dat$timespent > peak_timestamp,], which( diff (avg_rewards >= cur_rewards) > 0))], 1)
  
  # It's possible that no peak or intersection point is detected.
  # Note: If no peak is detected, no intersection point can be extracted.
  if ( is.na(intersect.point_timestamp) || length(intersect.point_timestamp) == 0 ) { 
    dat <- data[which(data$timespent >= enter_times[each_visit]),]
    if (length(peak_timestamp) > 0) { # Extract the first intersection point after the current observed peak.
      intersect.point_timestamp <- head(dat[dat$timespent > peak_timestamp,]$timespent[
        with(dat[dat$timespent > peak_timestamp,], which( diff (avg_rewards >= cur_rewards) > 0))], 1) # Extract the first intersection point
    } else { # If no peak has been detected within the current time window, simply extract the first intersection point after patch entry
      intersect.point_timestamp <- dat$timespent[head(with(dat, which( diff (avg_rewards >= cur_rewards) > 0)), 1)]
    }
    
    # Extracting the intersection point is not possible if no intersection point is available within the remainder of the data (= until trial end).
    if (length(intersect.point_timestamp) == 0) { # If that is the case, set the optimal leave time to maximum timestamp of dat.
      intersect.point_timestamp <- max(dat$timespent)
    }
  }
  
  #########################
  # 4) Get intersection points
  #########################  
  intersect.point <- which(data$timespent == intersect.point_timestamp) 
  
  # Find the slopes for each line segment.
  avg_rewards.slopes <- with(data, avg_rewards[intersect.point + 1] - avg_rewards[intersect.point])
  cur_rewards.slopes <- with(data, cur_rewards[intersect.point + 1] - cur_rewards[intersect.point])
  
  # Find the intersection for each segment.
  x.points <- intersect.point + ( (data$cur_rewards[intersect.point] - data$avg_rewards[intersect.point]) / 
                                    (avg_rewards.slopes - cur_rewards.slopes) )
  y.points <- data$avg_rewards[intersect.point] + (avg_rewards.slopes * ( x.points - intersect.point))
  
  intersect.points.x <- cbind(intersect.points.x, intersect.point_timestamp)
  intersect.points.y <- cbind(intersect.points.y, y.points)
}

#########################
# 5) Create a plot
#########################  

eventColors = c('Average reward rate (ARR)' = 'black', 'Current reward rate (CRR)' = '#E69F00', 
                'Patch entered' = 'darkgreen', 'Patch left' = 'darkred')
eventTypes = c('Average reward rate (ARR)' = 'dashed', 'Current reward rate (CRR)' = 'solid', 
               'Patch entered' = 'dotted', 'Patch left' = 'dotdash')
shapeTypes = c('Peak value current reward rate (CRR)' = 1, 'Optimal leave times' = 5); textSize = 20

plot <- ggplot(data = data, aes(x = timespent, y = avg_rewards)) + 
  geom_line(aes(y = avg_rewards, color = 'Average reward rate (ARR)', linetype = 'Average reward rate (ARR)'))  + # Plot the ARR
  geom_line(aes(y = cur_rewards, color = 'Current reward rate (CRR)', linetype = 'Current reward rate (CRR)')) + # Plot the CRR
  coord_cartesian(ylim = c(0, 0.03), xlim = c(0, 301), clip = 'off') + 
  labs(y = "Reward rate", x = "Time spent in seconds") +
  sapply(enter_times, function(xint) geom_vline(aes(xintercept = xint, color = 'Patch entered', linetype = 'Patch entered'), show.legend = F)) + # Plot patch enter timepoint
  sapply(leave_times, function(xint) geom_vline(aes(xintercept = xint, color = 'Patch left', linetype = 'Patch left'), show.legend = F)) + # Plot patch leave timepoint
  sapply(peak_timestamps, 
         function(xint) geom_point(aes(x = xint, y = data[which(data$timespent == xint),]$cur_rewards,
          shape = 'Peak value current reward rate (CRR)'), size = 3)) + # Plot maximum CRR values (per patch visit)
  sapply(1:length(intersect.points.x), 
         function(int) geom_point(aes(x = intersect.points.x[int], y = intersect.points.y[int],
          shape = 'Optimal leave times'), size = 3)) + # Plot intersection points (= optimal leave times) as black diamonds
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 0.03, 0.005)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 301, 50)) +
  scale_colour_manual(name = "", values = eventColors) +
  scale_linetype_manual(name = "", values = eventTypes) +
  scale_shape_manual(name = "", values = shapeTypes) +
  theme( plot.margin = unit(c(5, 1, 1, 1), 'cm'),
         #legend.position = "top right",
         #legend.position = c(0.16, 0.88),
         #legend.position = c(1.3, 0.88),
         legend.direction = "horizontal",
         legend.position = "top",
         #legend.spacing = unit(-1.33, "cm"),
         legend.spacing.x = unit(0.70, 'cm'),
         legend.key.size = unit(2.5, 'line'),
         #legend.key.size = unit(5, "cm")
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
         plot.background = element_rect(fill = "white", colour = NA))
ggarrange(plot, common.legend = F, legend = c(0.45, 1.3)) %>% ggexport(filename = "./plots/Figure-2.png", width = 1000)

# dev.off() # Close the pdf file