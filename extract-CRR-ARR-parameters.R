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
packages <- c("plyr", "zeallot", "Rmisc", "ggplot2", "cowplot", "ez", 
              "apaTables", "dplyr") # Make sure to load dplyr after plyr and ggplot2
ipak(packages)

###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Define functions --------------------------------------------------------
###########################################################################################################################################################################################################################################################################################################################################################################################################################################

find_peaks <- function (x, m = 3){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}

###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Load datafiles ----------------------------------------------------------
###########################################################################################################################################################################################################################################################################################################################################################################################################################################

# If the data has already been processed, we can skip quite a large part of the current script!
DataProcessed = TRUE
  
if (!DataProcessed) {
  # Create a new directory in which we store data of both the baseline- and main phase.
  if (!file.exists('data-foraging-both')) {
    dir.create(file.path('./data-foraging-both'))
    dir.create(file.path('./data-foraging-both/patch-coordinates'))
    dir.create(file.path('./data-foraging-both/foraging-path-transposed'))
  }
  # Make sure that R can read in the coordinate files: Replace comma with semicolon
  if (length(list.files('./data-foraging-both/patch-coordinates')) == 0) {
    system('cd preprocessing-scripts && python replace-comma-with-semicolon-coordinates.py')
  }
  
  # Read in all datafiles
  general_data <- data.frame()
  for (f in list.files('./data-foraging-both/patch-coordinates/', full.names = T)) {
    general_data <- rbind(general_data, tryCatch({ read.csv2(f, dec = '.', sep = ',', header = TRUE, stringsAsFactors = TRUE) },  error=function(e) NULL))
  }
  
  # Let's start by identifying those participants that have a deviant number of rows in the data: 2 phases * 9 rows of data per phase
  deviant_pp <- names(which(with(general_data, table(subjectID)) != 18))
  with(general_data[which(general_data$subjectID %in% deviant_pp),], table(subjectID, expStartTime))
  
  # Checking these subject numbers based on our experimental logbook tells us the following:
  #  - Subject 9 and 91 did not complete the full experiment: Remove expStartTime 20190308105914 & 20190305100659.
  #  - Subject 31, 32 and 40 are double participant numbers: Recode subject numbers with expStartTime 20190326161208 & 20190326160652 & 20190308161404
  #  - The baseline phase of subject 47 has been coded as subject 45 with expStartTime 20190312160113: Recode subject 45 with expStartTime 20190312160113
  # Note: These subjects will be removed / recoded when processing the data.
  expStartTime_to_remove <- c(20190308105914, 20190305100659)
  expStartTime_to_recode <- setNames(as.list(c(231, 232, 240, 47)), c(20190326161208, 20190326160652, 20190308161404, 20190312160113))
  
  ###########################################################################################################################################################################################################################################################################################################################################################################################################################################
  # Process data ------------------------------------------------------------
  ###########################################################################################################################################################################################################################################################################################################################################################################################################################################
  
  # We want to compare optimal- and  actual patch leaving behavior (optimal stopping). To do so, we calculate the following two parameters:
  # average reward rate (= ARR / overall reward history) and current reward rate (= CRR / recent reward history).
  
  # We decided to take a data-driven approach to define the window over which the CRR is calculated. Specifically, on the baseline data
  # (pre-mood induction) we calculated optimal leave times for a multitude of intervals (1 - 20 second(s), steps of 1 second).
  # As participants should not necessarily show any exploratory/exploitative preference within the baseline trial, we want to see
  # which time window results in the smallest difference between optimal- and actual leave times (approximating 0).
  
  # Make sure that R can read in the path files: Transpose from wide to long format
  if (length(list.files('./data-foraging-both/foraging-path-transposed')) == 0) {
    system('cd preprocessing-scripts && python transpose-foraging-path-files.py')
  }
  
  # Define multiple intervals over which we would like to calculate the CRR
  window_intervals = seq(0, 20, .1) # sequence between 0 and 20 seconds, in steps of 100 ms.
  
  # Create a folder to which the plots will be saved
  if (!file.exists('plots')) {
    dir.create(file.path('./plots'))
    dir.create(file.path('./plots/ARR-CRR-individual-plots-0-20-100ms'))
  }
  
  # Create a folder to which the df will be saved
  if (!file.exists('data-processed')) {
    dir.create(file.path('./data-processed'))
    dir.create(file.path('./data-processed/per-participant-per-window'))
  }
  
  # Loop through all datafiles
  all.datafiles <- list.files('./data-foraging-both/foraging-path-transposed', pattern = 'baseline', full.names = T)
  pb1 <- progress_estimated(length(window_intervals) * length(all.datafiles)) # Initiate a progress bar
  optimal_actual_leavetimes <- data.frame() # Create an empty dataframe in which we can store the optimal- and actual leave times.
  
  for (f in all.datafiles) { # For each datafile.
    data <- read.csv2(f, dec = '.', sep = ',') # Load selected datafile.

    # Only process datafiles of participants that will not be ommited from the data (see line 83).
    if (!unique(data$expStartTime) %in% expStartTime_to_remove) {
      # If required, replace subjectID based on expStartTime (see line 84).
      if(unique(data$expStartTime) %in% as.numeric(names(expStartTime_to_recode))) {
        data$subjectID = expStartTime_to_recode[which(unique(data$expStartTime) == as.numeric(names(expStartTime_to_recode)))][[1]]
      }
      
      # Remove pre-trial data
      data <- data[-which(data$timespent < 0),]
      
      # Prepare a participant specific pdf-file to which plots can be saved
      pdf(paste0('./plots/ARR-CRR-individual-plots-0-20-100ms/baseline_participant_', unique(data$subjectID), '_0-20-100ms-steps.pdf'), paper = "USr", width = 9.3, height = 9.3)
      par(mfrow = c(3, 1)) # Save 3 plots per page
      
      # Loop through all intervals
      for (interval in window_intervals) { 
        print(paste0("Processing subjectID: ", unique(data$subjectID), ', window: ', interval, ' second(s).'))
        optimal_actual_leavetimes_subj <- setNames(data.frame(matrix(0, ncol = 5)), c('subjectID', 'window', 'condition', 'actual', 'optimal')) 
        
        #########################
        # 1) Define model parameters 
        #########################
        
        data$rewardReceived <- 0; data$rewardReceived[data$berry_found == "True"] <- 1
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
        # 2) Create intersection plot
        #########################
        plot(data$avg_rewards, type = "l", col = "blue", ylim = c(0, 0.02), # Plot the ARR
             main = paste0('SubjectID: ', unique(data$subjectID), '\nWindow: ', interval, ' second(s)')) 
        lines(data$cur_rewards, type = "l", col = "red") # Plot the CRR
        
        # Obtain and plot the positions where participants entered a patch
        enter_times <- data %>% select(view, timespent) %>% 
          filter(ifelse(view == lag(view), 'constant', 'switch') == 'switch') %>%
          filter(view == 'focused') %>% pull(timespent)
        abline(v = c(which(data$timespent %in% enter_times)), col = 'green', lwd = 3, lty = 2)
        
        # Obtain and plot the positions where participants left a patch
        leave_times <- data %>% select(view, timespent) %>% 
          filter(ifelse(view == lag(view), 'constant', 'switch') == 'switch') %>% filter(view == 'aerial')
        if (length(leave_times$timespent) < length(enter_times)) {
          leave_times <- leave_times %>% rbind(., c('aerial', max(data$timespent))) %>%
            mutate_at(grep("timespent", colnames(.)), list(as.numeric)) %>% pull(timespent)
        } else { leave_times <- leave_times %>% pull(timespent) }
        abline(v = c(which(data$timespent %in% leave_times)), col = 'purple', lwd = 3, lty = 2)
        
        #########################
        # 3) Extract intersection points
        #########################
        
        # For each patch visit, detect the peak amplitude of the CRR signal and extract the 
        # first observed intersection point (i.e., ARR >= CRR) after the this peak amplitude
        for (each_visit in 1:length(enter_times)) {
          # Extract the data for the current patch visit
          dat <- data[which(data$timespent >= enter_times[each_visit] & data$timespent <= leave_times[each_visit]),]
          
          # For the current time window, detect where the CRR peaks are located, 
          # but only for those positions where CRR > ARR.
          dat_peaks <- dat[find_peaks(dat$cur_rewards, m = 1),] %>% filter(cur_rewards > avg_rewards)
          
          # Find the timestamp with the maximum CRR (= highest amplitude / peak).
          peak_timestamp <- tail(dat_peaks$timespent[which(dat_peaks$cur_rewards == max(dat$cur_rewards, na.rm = T))], 1)  
          
          # Plot the peak.
          abline(v = c(which(data$timespent == peak_timestamp)), col = 'orange', lwd = 3, lty = 2)
          
          # Next, let's use timestamps larger than the peak location timestamp to inform us on which 
          # intersection point (= CRR == ARR) to extract.
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
              #print('But: first intersection point after someone left a patch? Or after someone entered a patch?')
              intersect.point_timestamp <- dat$timespent[head(with(dat, which( diff (avg_rewards >= cur_rewards) > 0)), 1)]
            }
            
            # Extracting the intersection point is not possible if no intersection point is available within the remainder of the data (= until trial end).
            if (length(intersect.point_timestamp) == 0) { # If that is the case, set the optimal leave time to maximum timestamp of dat.
              intersect.point_timestamp <- max(dat$timespent)
            }
          }
          
          # Save optimal- and actual leave times to the optimal_actual_leavetimes_subj df
          optimal_actual_leavetimes_subj[each_visit,]$actual <- leave_times[each_visit]
          optimal_actual_leavetimes_subj[each_visit,]$optimal <- intersect.point_timestamp
          
          #########################
          # 4) Add intersection points to plot
          #########################  
          
          intersect.point <- which(data$timespent == intersect.point_timestamp) 
          
          # Find the slopes for each line segment.
          avg_rewards.slopes <- with(data, avg_rewards[intersect.point+1] - avg_rewards[intersect.point])
          cur_rewards.slopes <- with(data, cur_rewards[intersect.point+1] - cur_rewards[intersect.point])
          
          # Find the intersection for each segment.
          x.points <- intersect.point + ( (data$cur_rewards[intersect.point] - data$avg_rewards[intersect.point]) / 
                                            (avg_rewards.slopes - cur_rewards.slopes) )
          y.points <- data$avg_rewards[intersect.point] + (avg_rewards.slopes * ( x.points - intersect.point))
          points(x.points, y.points, col = 'black', pch = 19) # Plot intersection points as black dots
        }
        
        # Add subjectID and window to optimal_actual_leavetimes df.
        optimal_actual_leavetimes_subj$subjectID <- unique(data$subjectID)
        optimal_actual_leavetimes_subj$window <- as.character(interval) 
        optimal_actual_leavetimes_subj$condition <- unique(data$condition)
        
        # Bind optimal_actual_leavetimes_subj to df including all participants.
        optimal_actual_leavetimes <- rbind(optimal_actual_leavetimes, optimal_actual_leavetimes_subj)
        
        # Cache the optimal_actual_leavetimes_subj df to the disk.
        write.table(optimal_actual_leavetimes_subj, file = 
                      paste0('./data-processed/per-participant-per-window/optimal-actual-leavetimes-0-to-20-in-100ms_', unique(data$subjectID), '_window_', as.character(interval)), row.names = FALSE)
        
        pb1$tick()$print() # Update counter
      }
      
      dev.off() # Close participant-specific pdf file
    }
  }
  pb1$stop() # Kill the progress bar
  
  # Cache the optimal_actual_leavetimes df to the disk.
  write.table(optimal_actual_leavetimes, file = './data-processed/optimal-actual-leavetimes-0-to-20-in-100ms.txt', row.names = FALSE)
}

###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Extract optimal time window ---------------------------------------------
###########################################################################################################################################################################################################################################################################################################################################################################################################################################

# Note: when analysing ./data-processed/optimal-actual-leavetimes-0-to-20-in-100ms.txt file, we obtain 9.00 seconds as the optimal time interval

# If needed, load the optimal_actual_leavetimes df.
optimal_actual_leavetimes <- read.csv2('./data-processed/optimal-actual-leavetimes-8.75-to-9.25-in-10ms.txt',  
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

# Compute the mean MU for each window, and extract the window with the mean value closest to 0
min_value <- opt_act_data %>% group_by(window) %>% mutate(mean_MU = mean(MU, na.rm = T)) %>%
  select(window, mean_MU) %>% arrange(window) %>% distinct() %>% ungroup() %>%
  filter(abs(mean_MU) == min(abs(mean_MU))) %>% pull(window)
print(min_value)

# Save the workspace
save.image("./workspaces/general-workspace.RData")

###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Process foraging data post-mood induction -------------------------------
###########################################################################################################################################################################################################################################################################################################################################################################################################################################

# If the post-mood induction foraging data has already been processed, we can skip another part of the script!
TestDataProcessed = TRUE

if (!TestDataProcessed) {
  
  # Define interval (i.e., min_value) over which we would like to calculate the CRR for the post-manipulation data 
  interval = min_value
  
  # Loop through all datafiles
  all.datafiles <- list.files('./data-foraging-both/foraging-path-transposed', pattern = 'main', full.names = T)
  pb1 <- progress_estimated(length(interval) * length(all.datafiles)) # Initiate a progress bar
  termination_data <- data.frame() # Create an empty dataframe in which we can store the termination data.
  optimal_actual_leavetimes <- data.frame() # Create an empty dataframe in which we can store the optimal- and actual leave times.
  ARR_CRR_data <- data.frame() # Create an empty dataframe in which we can store the mean ARR and CRR.
  
  # Create a folder to which the processed data will be saved
  if (!file.exists('data-processed/per-participant/')) {
    dir.create(file.path('./data-processed/per-participant/'))
  }
  
  for (i in 1:length(all.datafiles)) { # For each datafile.
    data <- read.csv2(all.datafiles[i], dec = '.', sep = ',') # Load selected datafile.
    
    # Only process datafiles of participants that will not be ommited from the data (see line 83).
    if (!unique(data$expStartTime) %in% expStartTime_to_remove) {
      # If required, replace subjectID based on expStartTime (see line 84).
      if(unique(data$expStartTime) %in% as.numeric(names(expStartTime_to_recode))) {
        data$subjectID = expStartTime_to_recode[which(unique(data$expStartTime) == as.numeric(names(expStartTime_to_recode)))][[1]]
      }
      
      # Remove pre-trial data
      data <- data[-which(data$timespent < 0),]
      
      # Prepare a participant specific pdf-file to which plots can be saved
      pdf(paste0('./plots/ARR-CRR-individual-plots/baseline_9swindow_participant_', unique(data$subjectID), '.pdf'), paper = "USr", width = 9.3, height = 9.3)
      par(mfrow = c(3, 1)) # Save 3 plots per page
      
      # Process the data for the 9-second interval
      print(paste0("Processing subjectID: ", unique(data$subjectID), ', window: ', interval, ' second(s).'))
      optimal_actual_leavetimes_subj <- setNames(data.frame(matrix(0, ncol = 6)), c('subjectID', 'window', 'condition', 'entertime', 'actual', 'optimal')) 
        
      #########################
      # 1) Define model parameters 
      #########################
      
      data$rewardReceived <- 0; data$rewardReceived[data$berry_found == "True"] <- 1
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
      # 2) Create intersection plot
      #########################
      
      plot(data$avg_rewards, type = "l", col = "blue", ylim = c(0, 0.02), # Plot the ARR
           main = paste0('SubjectID: ', unique(data$subjectID), '\nWindow: ', interval, ' seconds')) 
      lines(data$cur_rewards, type = "l", col = "red") # Plot the CRR
      
      # Obtain and plot the positions where participants entered a patch
      enter_times <- data %>% select(view, timespent) %>% 
        filter(ifelse(view == lag(view), 'constant', 'switch') == 'switch') %>%
        filter(view == 'focused') %>% pull(timespent)
      abline(v = c(which(data$timespent %in% enter_times)), col = 'green', lwd = 3, lty = 2)
      
      # Obtain and plot the positions where participants left a patch
      leave_times <- data %>% select(view, timespent) %>% 
        filter(ifelse(view == lag(view), 'constant', 'switch') == 'switch') %>% filter(view == 'aerial')
      if (length(leave_times$timespent) < length(enter_times)) {
        leave_times <- leave_times %>% rbind(., c('aerial', max(data$timespent))) %>%
          mutate_at(grep("timespent", colnames(.)), list(as.numeric)) %>% pull(timespent)
      } else { leave_times <- leave_times %>% pull(timespent) }
      abline(v = c(which(data$timespent %in% leave_times)), col = 'purple', lwd = 3, lty = 2)
      
      #########################
      # 3) Extract intersection points
      #########################
      
      # For each patch visit, detect the peak amplitude of the CRR signal and extract the 
      # first observed intersection point (i.e., ARR >= CRR) after the this peak amplitude
      for (each_visit in 1:length(enter_times)) {
        # Extract the data for the current patch visit
        dat <- data[which(data$timespent >= enter_times[each_visit] & data$timespent <= leave_times[each_visit]),]
        
        # Extract data to create the termination plot
        termination_dat <- dat %>% select(condition, timespent, rewardReceived, cur_rewards) %>% filter(rewardReceived == 1) %>%
          arrange(-row_number()) %>% slice(1:10) %>% # Reverse order the berries found, and extract the first 10 observations
          mutate(berry = -row_number())
        termination_data <- rbind(termination_data, termination_dat)
        
        # For the current time window, detect where the CRR peaks are located, 
        # but only for those positions where CRR > ARR.
        dat_peaks <- dat[find_peaks(dat$cur_rewards, m = 1),] %>% filter(cur_rewards > avg_rewards)
        
        # Find the timestamp with the maximum CRR (= highest amplitude / peak).
        peak_timestamp <- tail(dat_peaks$timespent[which(dat_peaks$cur_rewards == max(dat$cur_rewards, na.rm = T))], 1)  
        
        # Plot the peak.
        abline(v = c(which(data$timespent == peak_timestamp)), col = 'orange', lwd = 3, lty = 2)
        
        # Next, let's use timestamps larger than the peak location timestamp to inform us on which 
        # intersection point (= CRR == ARR) to extract.
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
            #print('But: first intersection point after someone left a patch? Or after someone entered a patch?')
            intersect.point_timestamp <- dat$timespent[head(with(dat, which( diff (avg_rewards >= cur_rewards) > 0)), 1)]
          }
          
          # Extracting the intersection point is not possible if no intersection point is available within the remainder of the data (= until trial end).
          if (length(intersect.point_timestamp) == 0) { # If that is the case, set the optimal leave time to maximum timestamp of dat.
            intersect.point_timestamp <- max(dat$timespent)
          }
        }
        
        # Save optimal- and actual leave times to the optimal_actual_leavetimes_subj df
        optimal_actual_leavetimes_subj[each_visit,]$entertime <- enter_times[each_visit]
        optimal_actual_leavetimes_subj[each_visit,]$actual <- leave_times[each_visit]
        optimal_actual_leavetimes_subj[each_visit,]$optimal <- intersect.point_timestamp
        
        #########################
        # 4) Add intersection points to plot
        #########################  
        
        intersect.point <- which(data$timespent == intersect.point_timestamp) 
        
        # Find the slopes for each line segment.
        avg_rewards.slopes <- with(data, avg_rewards[intersect.point+1] - avg_rewards[intersect.point])
        cur_rewards.slopes <- with(data, cur_rewards[intersect.point+1] - cur_rewards[intersect.point])
        
        # Find the intersection for each segment.
        x.points <- intersect.point + ( (data$cur_rewards[intersect.point] - data$avg_rewards[intersect.point]) / 
                                          (avg_rewards.slopes - cur_rewards.slopes) )
        y.points <- data$avg_rewards[intersect.point] + (avg_rewards.slopes * ( x.points - intersect.point))
        points(x.points, y.points, col = 'black', pch = 19) # Plot intersection points as black dots
      }
      
      # Add subjectID and window to optimal_actual_leavetimes df.
      optimal_actual_leavetimes_subj$subjectID <- unique(data$subjectID)
      optimal_actual_leavetimes_subj$window <- as.character(interval) 
      optimal_actual_leavetimes_subj$condition <- unique(data$condition)
      
      # Bind optimal_actual_leavetimes_subj to df including all participants.
      optimal_actual_leavetimes <- rbind(optimal_actual_leavetimes, optimal_actual_leavetimes_subj)
    
      # Save processed datafile
      write.table(data, file = paste0('./data-processed/per-participant/processed-foraging-path-participant', 
                                      unique(data$subjectID)), row.names = FALSE)
      
      ARR_CRR <- data %>% select(subjectID, condition, block, avg_rewards, cur_rewards) %>% 
        group_by(subjectID, condition, block) %>%
        summarise(mean_ARR = mean(avg_rewards),
                  sd_ARR = sd(avg_rewards, na.rm = T),
                  mean_CRR = mean(cur_rewards, na.rm = T),
                  sd_CRR = sd(cur_rewards, na.rm = T)
        )
      ARR_CRR_data <- rbind(ARR_CRR_data, data.frame(ARR_CRR))
      
      pb1$tick()$print() # Update counter
      dev.off() # Close participant-specific pdf file
    }
  }
  pb1$stop() # Kill the progress bar
  
  # Cache the created dataframes to the disk.
  #write.table(termination_data, file = './data-processed/post-manipulation-termination-data.txt', row.names = FALSE)
  write.table(optimal_actual_leavetimes, file = './data-processed/post-manipulation-optimal-actual-leavetimes.txt', row.names = FALSE)
  #write.table(ARR_CRR_data, file = './data-processed/ARR-CRR-data.csv', row.names = FALSE)
}
