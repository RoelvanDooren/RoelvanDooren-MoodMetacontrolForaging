###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Moodmecodynamics: Roel van Dooren, Roy de Kleijn, Bernhard Hommel, & Zsuzsika Sjoerds
###########################################################################################################################################################################################################################################################################################################################################################################################################################################
### Last adjustment on: 27-01-2020
### r.van.dooren@fsw.leidenuniv.nl

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
packages <- c("plyr", "zeallot", "Rmisc", "ggplot2", "cowplot", "MBESS", "rstatix", "psych", "ggpubr", "report", "stringr",
              "ez", "apaTables", "dplyr", "data.table", "car", "effsize") # Make sure to load dplyr after plyr and ggplot2
ipak(packages)


###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Create output files -----------------------------------------------------
###########################################################################################################################################################################################################################################################################################################################################################################################################################################

# Create a new output folder
dir.create(file.path('./data-foraging-both/processed-participant-data'))

# Define output files to which the extracted parameters can be added.
if (!("dat_foraging_patches_visited.txt" %in% list.files("./data-foraging-both/"))) {
  patches_visited <- data.frame()
} else {
  patches_visited <- read.csv2('./data-foraging-both/dat_foraging_patches_visited.txt', sep = ';', header = T) 
}

if (!("visual_foraging_processed_array_data.txt" %in% list.files("./data-foraging-both/"))) {
  processedfile_paths <- data.frame(matrix(nrow = 0, ncol = 52))
  names(processedfile_paths) <- c('expStartTime', 'subjectID', 'condition', 'block', 'timespent', 'berries_found',
                                  'area_visited', 'time_before_first_patch_entered', 'amount_of_patches_entered', 'amount_of_unique_patches_entered', 
                                  'ratio_patch_repetition', 'mean_time_in_patches', 'sd_time_in_patches', 
                                  'median_time_in_patches', 'total_time_in_patches',
                                  'min_time_in_patch', 'max_time_in_patch', 'interbushinterval', "eucdist_after_food_200", 
                                  "total_angle_after_food_200", "mean_angle_after_food_200", "eucdist_after_food_300", "total_angle_after_food_300", 
                                  "mean_angle_after_food_300", "mean_speed_polygon", "mean_speed_magnifier",  
                                  "mean_speed_pxps_polygon", "mean_speed_pxps_magnifier", "mean_speed_after_food_200", 
                                  "mean_speed_after_food_200_pxps", "mean_speed_after_food_300", "mean_speed_after_food_300_pxps",
                                  "mean_angle_after_food_200_t1", "mean_angle_after_food_300_t1",
                                  "mean_angle_after_food_200_t2", "mean_angle_after_food_300_t2",
                                  "mean_angle_after_food_200_t3", "mean_angle_after_food_300_t3",
                                  "mean_angle_after_food_200_t4", "mean_angle_after_food_300_t4",
                                  "mean_angle_after_food_200_t5", "mean_angle_after_food_300_t5",
                                  
                                  "mean_speed_after_food_200_t1", "mean_speed_after_food_300_t1",
                                  "mean_speed_after_food_200_t2", "mean_speed_after_food_300_t2",
                                  "mean_speed_after_food_200_t3", "mean_speed_after_food_300_t3",
                                  "mean_speed_after_food_200_t4", "mean_speed_after_food_300_t4",
                                  "mean_speed_after_food_200_t5", "mean_speed_after_food_300_t5"
                                  )
  write.table(processedfile_paths, file = './data-foraging-both/visual_foraging_processed_array_data.txt',
              append = TRUE, row.names = FALSE, col.names = names(processedfile_paths))
} else {
  processedfile_paths <- read.csv2('./data-foraging-both/visual_foraging_processed_array_data.txt', sep = ' ', header = T) 
}

# Calculate absolute difference between timepoints: indicating acceleration vs. deceleration.
if (!("visual_foraging_processed_array_data_speed.txt" %in% list.files("./data-foraging-both/"))) {
  processedfile_speed <- data.frame(matrix(nrow = 0, ncol = 25))
  names(processedfile_speed) <- c("expStartTime", "subjectID", "condition", "block", "timespent", 
                                  "berries_found", 'delta_after_food_200', 'delta_after_food_300',
                                  "delta_speed_after_food_200", "delta_speed_after_food_300",
                                  "delta_speed_after_food_200_t1", "delta_speed_after_food_300_t1",
                                  "delta_speed_after_food_200_t2", "delta_speed_after_food_300_t2",
                                  "delta_speed_after_food_200_t3", "delta_speed_after_food_300_t3",
                                  "delta_speed_after_food_200_t4", "delta_speed_after_food_300_t4",
                                  "delta_speed_after_food_200_t5", "delta_speed_after_food_300_t5",
                                  "mean_speed_pxps_t1", "mean_speed_pxps_t2", "mean_speed_pxps_t3", "mean_speed_pxps_t4", "mean_speed_pxps_t5")
  write.table(processedfile_speed, file = './data-foraging-both/visual_foraging_processed_array_data_speed.txt',
              append = TRUE, row.names = FALSE, col.names = names(processedfile_speed))
} else {
  processedfile_speed <- read.csv2('./data-foraging-both/visual_foraging_processed_array_data_speed.txt', sep = ' ', header = T)
}

rm(list=(ls()[!ls() %in% c("select.files", "patches_visited", "processedfile_paths")])) # Clean up workspace.

###########################################################################################################################################################################################################################################################################################################################################################################################################################################
# Extract dynamic parameters ----------------------------------------------
###########################################################################################################################################################################################################################################################################################################################################################################################################################################

select.files <- list.files('./data-foraging-both/foraging-path-transposed/', pattern = 'visual_foraging_path_array', full.names = T)

#########################
# 1) Extract general parameters
#########################

pb1 <- progress_estimated(length(select.files))
for (f in select.files) {
  # Update counter.
  pb1$tick()$print()
  
  # Read data.
  dat <- read.csv2(f, dec = '.', sep = ',', stringsAsFactors = FALSE)
  write(paste0("\nAdding data of participant ", dat$subjectID[1], " block ", dat$block[1], " to datafile\n"), stdout())
  
  # Transform DVs into numeric variables
  dat$timespent <- as.numeric(gsub(",", ".", dat$timespent))
  dat$current_angle_polygon <- as.numeric(gsub(",", ".", dat$current_angle_polygon))
  dat$current_angle_magnifier <- as.numeric(gsub(",", ".", dat$current_angle_magnifier))
  dat$speed_polygon <- as.numeric(gsub(",", ".", dat$speed_polygon))
  dat$speed_magnifier <- as.numeric(gsub(",", ".", dat$speed_magnifier))
  
  # Remove pre-trial data
  dat <- dat[-which(dat$timespent < 0),]
  
  # Create tuple of coordinates
  for (eachrow in 1:nrow(dat)) {
    # Calculate minimum and maximum difference between previous and current frame
    # Next, we take the minimum turning angle, as it's value that is most probable in a 1/60Hz frame interval
    if (dat$view[eachrow] == 'aerial') {
      dat$tuple_coord[eachrow] <- list(c(round(as.numeric(dat$xcoordMagnifier[eachrow])), round(as.numeric(dat$ycoordMagnifier[eachrow]))))
      dat$diff[eachrow] <- pmin(abs(dat$current_angle_magnifier[eachrow] - dat$current_angle_magnifier[ifelse(eachrow == 1, eachrow, eachrow-1)]), 
                                abs((((dat$current_angle_magnifier[eachrow] - dat$current_angle_magnifier[ifelse(eachrow == 1, eachrow, eachrow-1)]) + 180) %% 360) - 180))
    } else { # dat$view == 'focused
      dat$tuple_coord[eachrow] <- list(c(round(as.numeric(dat$xcoordPolygon[eachrow])), 
                                         round(as.numeric(dat$ycoordPolygon[eachrow]))))
      dat$diff[eachrow] <- pmin(abs(dat$current_angle_polygon[eachrow] - dat$current_angle_polygon[ifelse(eachrow == 1, eachrow, eachrow-1)]), 
                                abs((((dat$current_angle_polygon[eachrow] - dat$current_angle_polygon[ifelse(eachrow == 1, eachrow, eachrow-1)]) + 180) %% 360) - 180))
    }
  }
  
  # Do we want to correct the total amount of berries found?
  dat[nrow(dat), 'points_scored'] <- length(which(dat$berry_found == 'True'))
  
  # Identify resource encounters
  resourceStart <- which(dat$berry_found == "True")
  # Create column with NAs. These are subsequently replaced by angle_after_food and speed_after_food values.
  dat[,c("angle_after_food_200", "angle_after_food_300", "speed_after_food_200", "speed_after_food_300",
         "euc.dist_after_food_200", "euc.dist_after_food_300", "euc.dist")] <- NA
  
  # Check turning, speed and euclidean parameters for resource encounters
  if (length(resourceStart) > 0) {
    for (each_encounter in 1:(length(resourceStart))) {
      resourceIntervalStart <- dat$timespent[resourceStart[each_encounter]] # Detect time at encountering resources
      selectedIntervals_200 <- which(dat$timespent - resourceIntervalStart <= 0.2 & dat$timespent - resourceIntervalStart > 0)
      selectedIntervals_300 <- which(dat$timespent - resourceIntervalStart <= 0.3 & dat$timespent - resourceIntervalStart > 0)
      
      if (resourceStart[each_encounter] != nrow(dat)) {
        # If a resource is encountered on the last iteration: Do not include, since time window concerns frames after food encounter.
        # Calculate euclidean distances 
        for (x in 1:length(dat[selectedIntervals_300,]$tuple_coord)) {
          if (dat$view[selectedIntervals_300[x]] == "focused") {
            # (Calculations are based on 300 ms time windows, as 200 ms time windows are also part of this window)
            # Euclidean distances should take into account that the borders of the 200x200 px array can be crossed, which influences the distance calculations
            mat = rbind(unlist(dat[resourceStart[each_encounter],]$tuple_coord), unlist(dat$tuple_coord[selectedIntervals_300[x]])) # Create a matrix
            # Note that the below adjustments are probably not necessary, as we cannot cross the border while in a bush.
            dat[selectedIntervals_300[x],]$euc.dist <- ifelse(dist(mat) > 100, 199 - dist(mat), dist(mat))
          }
        }
        
        # Calculate mean euclidean distance for each resource encounter
        dat$euc.dist_after_food_200[resourceStart[each_encounter]] <- mean(dat[selectedIntervals_200,]$euc.dist)
        dat$euc.dist_after_food_300[resourceStart[each_encounter]] <- mean(dat[selectedIntervals_300,]$euc.dist)
        dat$angle_after_food_200[resourceStart[each_encounter]] <- sum(dat[selectedIntervals_200,]$diff)
        dat$speed_after_food_200[resourceStart[each_encounter]] <- mean(dat[selectedIntervals_200,]$speed_polygon)
        dat$angle_after_food_300[resourceStart[each_encounter]] <- sum(dat[selectedIntervals_300,]$diff)
        dat$speed_after_food_300[resourceStart[each_encounter]] <- mean(dat[selectedIntervals_300,]$speed_polygon)
      }
    }
  }
  
  # Recalculate the total amount of (unique) patches visited. Start by extracting the patch coordinates
  patch_coordinates <- read.csv2(sprintf(paste0('./data-foraging-both/patch-coordinates/', 
                          str_sub(f, 47, 50), '_', dat$expStartTime[1],
                          '_processed_block_', ifelse(str_detect(f, 'main'), 'main', 'baseline'), 
                          '.txt')), dec = '.', sep = ',') %>% pull(patch_center_coordinates)
  
  # To do so, we extract the points at which participants switched to a focused view
  switch_to_focused_timestamps <- dat %>% filter(ifelse(view == lag(view), 'constant', 'switch') == 'switch' & 
                                                   view == 'focused') %>% pull(timespent)
  
  # However, note that, in order to identify which patch was visited, we actually need the last timepoint BEFORE one switched to the focused view
  dat_switch_to_focused <- dat[which(dat$timespent %in% switch_to_focused_timestamps)-1,]
  
  euc.distances <- data.frame()
  timespent_in_patches <- data.frame()
  if (max(dat$total_patches_entered) > 0) {
    
    patch_visit_info <- dat %>% select(subjectID, expStartTime, condition, block, view, timespent, points_scored) %>% 
      filter(ifelse(view == lag(view), 'constant', 'switch') == 'switch') %>% # extract frames on which participants switched (relative to frame+1)
      mutate(patch_berries_found = ifelse(is.na(lead(points_scored)), max(points_scored), lead(points_scored)) - points_scored) %>%
      mutate(exit_timestamp = ifelse(is.na(lead(timespent)) & view == 'focused', max(dat$timespent),
                                     ifelse(view == 'focused', lead(timespent), NA))) %>%
      filter(!is.na(exit_timestamp)) %>% 
      mutate(switch_num = 1:n(), 
             time_diff_between_switches = ifelse(is.na(lead(timespent)), max(dat$timespent), lead(timespent)) - timespent,  
             patch_visit_duration = exit_timestamp - timespent) %>%
      select(-c(points_scored))
    
    for (eachrow in 1:nrow(dat_switch_to_focused)) {
      for (patch in patch_coordinates) {
        patch_list = as.numeric(str_replace_all(unlist(strsplit(
          as.character(patch), ";")), "[^[0-9].]", ""))
        # Calculate the euclidean distance between the position of the magnifier and a patch.
        matrix = rbind(unlist(dat_switch_to_focused$tuple_coord[eachrow]), patch_list)
        euc.distances <- rbind(euc.distances, data.frame(
          subjectID = dat$subjectID[1],
          expStartTime = dat$expStartTime[1],
          condition = dat$condition[1],
          block=dat$block[1],
          switch_num = eachrow,
          patch.coords = patch, # Save the patch coordinates
          euc.distance = dist(matrix)[1])) # Extract the distance value
      }
    }
    
    # Let's concatenate consecutive patches if patch_visit_durations are low and time between patch visits is low as well
    patches <- merge(euc.distances %>% group_by(switch_num) %>% filter(euc.distance == min(euc.distance)), patch_visit_info)
    patches <- patches[order(patches$switch_num),]
    
    # Detect which patches were visited and how many times.
    patch_info_to_save <- patches %>% ungroup() %>% 
      count(subjectID, expStartTime, block, patch.coords) %>% # Detect frequency of each value in the list
      group_by(subjectID, expStartTime, block) %>% #mutate(unique_patches = n, total_visited = sum(n))
      mutate(total_visited = sum(n)) %>% group_by(subjectID, expStartTime, block, total_visited) %>% 
      summarise(unique_patches = n())
    patches_visited <- rbind(patches_visited, as.data.frame(patch_info_to_save))
    
    timespent_in_patches <- patches %>% ungroup() %>%
      summarize(patch_visit_duration_mean = mean(patch_visit_duration), 
                patch_visit_duration_sd = sd(patch_visit_duration),
                patch_visit_duration_median = median(patch_visit_duration),
                patch_visit_duration_total = sum(patch_visit_duration),
                patch_visit_duration_max = max(patch_visit_duration),
                patch_visit_duration_min = min(patch_visit_duration),
                interbushinterval = mean(lead(timespent)-exit_timestamp, na.rm = T)
      )
  }
  
  # Total area explored per participant
  area_visited <- (100/(600*600)) * length(unique(dat$tuple_coord))
  
  # Save processed data for future reference
  write.table(apply(dat, 2, as.character), 
      file = paste0('./data-foraging-both/processed-participant-data/processed_array_subject_',
      dat$subjectID[1], '_expStartTime_', dat$expStartTime[1], '_block_', dat$block[1], '.txt'), 
      row.names = FALSE, col.names = TRUE)
  
  timespent_total = max(dat$timespent) - min(dat$timespent)
  
  # Prepare dataframe to be exported 
  frame_to_export <- data.frame(
    expStartTime = dat$expStartTime[1],
    subjectID = dat$subjectID[1],
    condition = dat$condition[1],
    block = dat$block[1],
    timespent = max(dat$timespent) - min(dat$timespent),
    berries_found = dat[nrow(dat), 'points_scored'],
    area_visited = area_visited,
    time_before_first_patch_entered = patches$timespent[1] - min(dat$timespent),
    amount_of_patches_entered = ifelse(nrow(patch_info_to_save) > 0, patch_info_to_save$total_visited[1], NA),
    amount_of_unique_patches_entered = ifelse(nrow(patch_info_to_save) > 0, patch_info_to_save$unique_patches[1], NA),
    ratio_patch_repetition = ifelse(nrow(patch_info_to_save) > 0, patch_info_to_save$unique_patches[1], NA) / ifelse(nrow(patch_info_to_save) > 0, patch_info_to_save$total_visited[1], NA),
    mean_time_in_patches = ifelse(nrow(timespent_in_patches) > 0, timespent_in_patches$patch_visit_duration_mean, NA),
    sd_time_in_patches = ifelse(nrow(timespent_in_patches) > 0, timespent_in_patches$patch_visit_duration_sd, NA),
    median_time_in_patches = ifelse(nrow(timespent_in_patches) > 0, timespent_in_patches$patch_visit_duration_median, NA),
    total_time_in_patches = ifelse(nrow(timespent_in_patches) > 0, timespent_in_patches$patch_visit_duration_total, NA),
    min_time_in_patch = ifelse(nrow(timespent_in_patches) > 0, timespent_in_patches$patch_visit_duration_min, NA),
    max_time_in_patch = ifelse(nrow(timespent_in_patches) > 0, timespent_in_patches$patch_visit_duration_max, NA),
    interbushinterval = ifelse(nrow(timespent_in_patches) > 0, timespent_in_patches$interbushinterval, NA),
    eucdist_after_food_200 = mean(subset(dat, berry_found == "True")$euc.dist_after_food_200, na.rm = T),
    total_angle_after_food_200 = sum(subset(dat, berry_found == "True")$angle_after_food_200, na.rm = T),
    mean_angle_after_food_200 = mean(subset(dat, berry_found == "True")$angle_after_food_200, na.rm = T),
    eucdist_after_food_300 = mean(subset(dat, berry_found == "True")$euc.dist_after_food_300, na.rm = T),
    total_angle_after_food_300 = sum(subset(dat, berry_found == "True")$angle_after_food_300, na.rm = T),
    mean_angle_after_food_300 = mean(subset(dat, berry_found == "True")$angle_after_food_300, na.rm = T),
    mean_speed_polygon = ifelse(nrow(timespent_in_patches) > 0, mean(dat$speed_polygon), NA),
    mean_speed_magnifier = ifelse(nrow(timespent_in_patches) > 0, mean(dat$speed_magnifier), NA),
    mean_speed_pxps_polygon = ifelse(nrow(timespent_in_patches) > 0, mean(dat$speed_polygon)*60, NA),
    mean_speed_pxps_magnifier = ifelse(nrow(timespent_in_patches) > 0, mean(dat$speed_magnifier)*90, NA),
    mean_speed_after_food_200 = mean(subset(dat, berry_found == "True")$speed_after_food_200, na.rm = T),
    mean_speed_after_food_200_pxps = mean(subset(dat, berry_found == "True")$speed_after_food_200, na.rm = T)*90,
    mean_speed_after_food_300 = mean(subset(dat, berry_found == "True")$speed_after_food_300, na.rm = T), 
    mean_speed_after_food_300_pxps = mean(subset(dat, berry_found == "True")$speed_after_food_300, na.rm = T)*90,
    
    mean_angle_after_food_200_t1 = mean(subset(dat, berry_found == "True" & timespent <= round(timespent_total) / 5)$angle_after_food_200, na.rm = T),
    mean_angle_after_food_300_t1 = mean(subset(dat, berry_found == "True" & timespent <= round(timespent_total) / 5)$angle_after_food_300, na.rm = T),
    mean_angle_after_food_200_t2 = mean(subset(dat, berry_found == "True" & timespent > round(timespent_total) / 5 & timespent <= (round(timespent_total) / 5)*2)$angle_after_food_200, na.rm = T),
    mean_angle_after_food_300_t2 = mean(subset(dat, berry_found == "True" & timespent > round(timespent_total) / 5 & timespent <= (round(timespent_total) / 5)*2)$angle_after_food_300, na.rm = T),
    mean_angle_after_food_200_t3 = mean(subset(dat, berry_found == "True" & timespent > (round(timespent_total) / 5)*2 & timespent <= (round(timespent_total) / 5)*3)$angle_after_food_200, na.rm = T),
    mean_angle_after_food_300_t3 = mean(subset(dat, berry_found == "True" & timespent > (round(timespent_total) / 5)*2 & timespent <= (round(timespent_total) / 5)*3)$angle_after_food_300, na.rm = T),
    mean_angle_after_food_200_t4 = mean(subset(dat, berry_found == "True" & timespent > (round(timespent_total) / 5)*3 & timespent <= (round(timespent_total) / 5)*4)$angle_after_food_200, na.rm = T),
    mean_angle_after_food_300_t4 = mean(subset(dat, berry_found == "True" & timespent > (round(timespent_total) / 5)*3 & timespent <= (round(timespent_total) / 5)*4)$angle_after_food_300, na.rm = T),
    mean_angle_after_food_200_t5 = mean(subset(dat, berry_found == "True" & timespent > (round(timespent_total) / 5)*4)$angle_after_food_200, na.rm = T),
    mean_angle_after_food_300_t5 = mean(subset(dat, berry_found == "True" & timespent > (round(timespent_total) / 5)*4)$angle_after_food_300, na.rm = T),
    
    mean_speed_after_food_200_t1 = mean(subset(dat, berry_found == "True" & timespent <= round(timespent_total) / 5)$speed_after_food_200, na.rm = T),
    mean_speed_after_food_300_t1 = mean(subset(dat, berry_found == "True" & timespent <= round(timespent_total) / 5)$speed_after_food_300, na.rm = T),
    mean_speed_after_food_200_t2 = mean(subset(dat, berry_found == "True" & timespent > round(timespent_total) / 5 & timespent <= (round(timespent_total) / 5)*2)$speed_after_food_200, na.rm = T),
    mean_speed_after_food_300_t2 = mean(subset(dat, berry_found == "True" & timespent > round(timespent_total) / 5 & timespent <= (round(timespent_total) / 5)*2)$speed_after_food_300, na.rm = T),
    mean_speed_after_food_200_t3 = mean(subset(dat, berry_found == "True" & timespent > (round(timespent_total) / 5)*2 & timespent <= (round(timespent_total) / 5)*3)$speed_after_food_200, na.rm = T),
    mean_speed_after_food_300_t3 = mean(subset(dat, berry_found == "True" & timespent > (round(timespent_total) / 5)*2 & timespent <= (round(timespent_total) / 5)*3)$speed_after_food_300, na.rm = T),
    mean_speed_after_food_200_t4 = mean(subset(dat, berry_found == "True" & timespent > (round(timespent_total) / 5)*3 & timespent <= (round(timespent_total) / 5)*4)$speed_after_food_200, na.rm = T),
    mean_speed_after_food_300_t4 = mean(subset(dat, berry_found == "True" & timespent > (round(timespent_total) / 5)*3 & timespent <= (round(timespent_total) / 5)*4)$speed_after_food_300, na.rm = T),
    mean_speed_after_food_200_t5 = mean(subset(dat, berry_found == "True" & timespent > (round(timespent_total) / 5)*4)$speed_after_food_200, na.rm = T),
    mean_speed_after_food_300_t5 = mean(subset(dat, berry_found == "True" & timespent > (round(timespent_total) / 5)*4)$speed_after_food_300, na.rm = T)
    
  )
  
  # Append data to the pre-defined dataframe.
  write.table(frame_to_export, file = './data-foraging-both/visual_foraging_processed_array_data.txt', append = TRUE, row.names = FALSE, col.names = FALSE)
}
pb1$stop()

# Save the patches_visited df as .csv.
write.csv2(patches_visited, './data-foraging-both/dat_foraging_patches_visited.txt', row.names = F)

#########################
# 2) Extract speed parameters
#########################

select.files <- list.files('./data-foraging-both/foraging-path-transposed/', pattern = 'visual_foraging_path_array', full.names = T)

# Perform calculations to obtain delta values of speed
pb1 <- progress_estimated(length(select.files))
for (f in select.files) {
    
  dat <- read.csv2(f, dec = '.', sep = ',', stringsAsFactors = FALSE)
  write(paste0("\nAdding data of participant ", dat$subjectID[1], " block ", dat$block[1], " to datafile\n"), stdout())  
  
  # Transform DVs into numeric variables
  dat$timespent <- as.numeric(gsub(",", ".", dat$timespent))
  dat$current_angle_polygon <- as.numeric(gsub(",", ".", dat$current_angle_polygon))
  dat$current_angle_magnifier <- as.numeric(gsub(",", ".", dat$current_angle_magnifier))
  dat$speed_polygon <- as.numeric(gsub(",", ".", dat$speed_polygon))
  dat$speed_magnifier <- as.numeric(gsub(",", ".", dat$speed_magnifier))
  
  # Remove pre-trial data
  dat <- dat[-which(dat$timespent < 0),]
  
  # Identify resource encounters
  resourceStart <- which(dat$berry_found == "True")
  
  dat[,c('delta_speed_after_food_200', 'delta_speed_after_food_300', 'delta_after_food_200', 'delta_after_food_300')] <- NA
  
  # Perform calculations for resource encounters
  if (length(resourceStart) > 0) {
    for (each_encounter in 1:(length(resourceStart))) {
      if (resourceStart[each_encounter] != nrow(dat)) {
        
        resourceIntervalStart <- dat$timespent[resourceStart[each_encounter]] # Detect time at encountering resources
        selectedIntervals_200 <- which(dat$timespent - resourceIntervalStart <= 0.2 &
                                         dat$timespent - resourceIntervalStart > 0)
        selectedIntervals_300 <- which(dat$timespent - resourceIntervalStart <= 0.3 & 
                                         dat$timespent - resourceIntervalStart > 0)
        
        # Calculate delta delta of joystick coordinates (x and y)
        xdiff_200 <- mutate(dat[resourceStart[each_encounter]:tail(selectedIntervals_200, 1,1),],
                            xdiff = abs(lag(xcoordjoystick) - xcoordjoystick))$xdiff
        ydiff_200 <- mutate(dat[resourceStart[each_encounter]:tail(selectedIntervals_200, 1,1),],
                            ydiff = abs(lag(ycoordjoystick) - ycoordjoystick))$ydiff
        xxdiff_200 <- mutate(data.frame(xdiff_200), xxdiff_200 = abs(lag(xdiff_200) - xdiff_200))$xxdiff_200
        yydiff_200 <- mutate(data.frame(ydiff_200), yydiff_200 = abs(lag(ydiff_200) - ydiff_200))$yydiff_200
        
        xdiff_300 <- mutate(dat[resourceStart[each_encounter]:tail(selectedIntervals_300, 1,1),],
                            xdiff = abs(lag(xcoordjoystick) - xcoordjoystick))$xdiff
        ydiff_300 <- mutate(dat[resourceStart[each_encounter]:tail(selectedIntervals_300, 1,1),],
                            ydiff = abs(lag(ycoordjoystick) - ycoordjoystick))$ydiff
        xxdiff_300 <- mutate(data.frame(xdiff_300), xxdiff_300 = abs(lag(xdiff_300) - xdiff_300))$xxdiff_300
        yydiff_300 <- mutate(data.frame(ydiff_300), yydiff_300 = abs(lag(ydiff_300) - ydiff_300))$yydiff_300
        
        # Save values in variable
        dat$delta_after_food_200[resourceStart[each_encounter]] <- mean(abs(xxdiff_200 + yydiff_200), na.rm = T)
        dat$delta_after_food_300[resourceStart[each_encounter]] <- mean(abs(xxdiff_300 + yydiff_300), na.rm =T)
        
        # Calculate delta of speed (of the polygon).
        dat$delta_speed_after_food_200[resourceStart[each_encounter]] <-
          mean(mutate(dat[resourceStart[each_encounter]:tail(selectedIntervals_200, 1,1),],
                      speed_diff = abs(lag(speed_polygon) - speed_polygon))$speed_diff, na.rm = T)
        dat$delta_speed_after_food_300[resourceStart[each_encounter]] <-
          mean(mutate(dat[resourceStart[each_encounter]:tail(selectedIntervals_300, 1,1),],
                      speed_diff = abs(lag(speed_polygon) - speed_polygon))$speed_diff, na.rm = T)
      }
    }
  }
  
  # Save processed data for future reference
  write.table(apply(dat, 2, as.character), 
              file = paste0('./data-foraging-both/processed-participant-data/processed_array_speed_subject_',
              dat$subjectID[1], '_expStartTime_', dat$expStartTime[1], '_block_', dat$block[1], '.txt'),
              row.names = FALSE, col.names = TRUE)
  
  # Total trial duration
  timespent_total = max(dat$timespent) - min(dat$timespent)
  
  # Prepare dataframe to be exported
  frame_to_export <- data.frame(
    expStartTime = dat$expStartTime[1],
    subjectID = dat$subjectID[1], 
    condition = dat$condition[1],
    block = dat$block[1],
    timespent = timespent_total, 
    berries_found = dat[nrow(dat), 'points_scored'],
    delta_after_food_200 = mean(subset(dat, berry_found == "True")$delta_after_food_200, na.rm = T),
    delta_after_food_300 = mean(subset(dat, berry_found == "True")$delta_after_food_300, na.rm = T),
    delta_speed_after_food_200 = mean(subset(dat, berry_found == "True")$delta_speed_after_food_200, na.rm = T),
    delta_speed_after_food_300 = mean(subset(dat, berry_found == "True")$delta_speed_after_food_300, na.rm = T),
    delta_speed_after_food_200_t1 = mean(subset(dat, berry_found == "True" & timespent <= round(timespent_total) / 5)$delta_speed_after_food_200, na.rm = T),
    delta_speed_after_food_300_t1 = mean(subset(dat, berry_found == "True" & timespent <= round(timespent_total) / 5)$delta_speed_after_food_300, na.rm = T),
    delta_speed_after_food_200_t2 = mean(subset(dat, berry_found == "True" & timespent > round(timespent_total) / 5 & timespent <= (round(timespent_total) / 5)*2)$delta_speed_after_food_200, na.rm = T),
    delta_speed_after_food_300_t2 = mean(subset(dat, berry_found == "True" & timespent > round(timespent_total) / 5 & timespent <= (round(timespent_total) / 5)*2)$delta_speed_after_food_300, na.rm = T),
    delta_speed_after_food_200_t3 = mean(subset(dat, berry_found == "True" & timespent > (round(timespent_total) / 5)*2 & timespent <= (round(timespent_total) / 5)*3)$delta_speed_after_food_200, na.rm = T),
    delta_speed_after_food_300_t3 = mean(subset(dat, berry_found == "True" & timespent > (round(timespent_total) / 5)*2 & timespent <= (round(timespent_total) / 5)*3)$delta_speed_after_food_300, na.rm = T),
    delta_speed_after_food_200_t4 = mean(subset(dat, berry_found == "True" & timespent > (round(timespent_total) / 5)*3 & timespent <= (round(timespent_total) / 5)*4)$delta_speed_after_food_200, na.rm = T),
    delta_speed_after_food_300_t4 = mean(subset(dat, berry_found == "True" & timespent > (round(timespent_total) / 5)*3 & timespent <= (round(timespent_total) / 5)*4)$delta_speed_after_food_300, na.rm = T),
    delta_speed_after_food_200_t5 = mean(subset(dat, berry_found == "True" & timespent > (round(timespent_total) / 5)*4)$delta_speed_after_food_200, na.rm = T),
    delta_speed_after_food_300_t5 = mean(subset(dat, berry_found == "True" & timespent > (round(timespent_total) / 5)*4)$delta_speed_after_food_300, na.rm = T),
    mean_speed_pxps_t1 = mean(subset(dat, timespent <= round(timespent_total) / 5)$speed_polygon, na.rm = T)*60,
    mean_speed_pxps_t2 = mean(subset(dat, timespent > round(timespent_total) / 5 & timespent <= (round(timespent_total) / 5)*2)$speed_polygon, na.rm = T)*60,
    mean_speed_pxps_t3 = mean(subset(dat, timespent > (round(timespent_total) / 5)*2 & timespent <= (round(timespent_total) / 5)*3)$speed_polygon, na.rm = T)*60,
    mean_speed_pxps_t4 = mean(subset(dat, timespent > (round(timespent_total) / 5)*3 & timespent <= (round(timespent_total) / 5)*4)$speed_polygon, na.rm = T)*60,
    mean_speed_pxps_t5 = mean(subset(dat, timespent > (round(timespent_total) / 5)*4)$speed_polygon, na.rm = T)*60)
  
  # If file already exists, append new data to dataframe.
  write.table(frame_to_export, file = './data-foraging-both/visual_foraging_processed_array_data_speed.txt',
              append = TRUE, row.names = FALSE, col.names = FALSE)
  
  # Update counter
  pb1$tick()$print()
}
pb1$stop()