#! /usr/bin/env python
import glob, os, ast

def init_datafile_path(expStartTime, subjectID, block, eachfile, filename=None):
	if filename is None:
		filename = os.path.join("../data-foraging-both/foraging-path-transposed/") + str(subjectID) + "_" + str(expStartTime) + "_visual_foraging_path_array_" + str(block) + ".txt"
	f = open(filename, 'w')
	output = 'expStartTime, subjectID, condition, block, timespent, xcoordPolygon, ycoordPolygon, xcoordMagnifier, ycoordMagnifier, xcoordjoystick, ycoordjoystick, current_angle_polygon, current_angle_magnifier, previous_angle_polygon, previous_angle_magnifier, speed_polygon, speed_magnifier, berry_found, points_scored, timesteps_left, view, patch_encounter_magnifier, switch_button_pressed, total_patches_entered, unique_patches_entered, patch_enter_timstamps, patch_leave_timestamps\n'
	f.write(output)
	f.close()

def write_data_path(output, expStartTime, subjectID, block, eachfile, filename=None):
	if filename is None:
		filename = os.path.join("../data-foraging-both/foraging-path-transposed/") + str(subjectID) + "_" + str(expStartTime) + "_visual_foraging_path_array_" + str(block) + ".txt"
	f = open(filename, 'a')
	f.write("%s\n" % ''.join([l for l in str(output) if l not in ("'"," ")]))
	f.close()

# Read datafiles
all_datafiles = glob.glob('../*/foraging-path/*.txt')
select_datafiles = [file for file in all_datafiles if "_visual_foraging_path_block_" in file] + [file for file in all_datafiles if "visual_foraging_path_practice_trial" in file]

# Melt dataframes to long format
for eachfile in select_datafiles:
	subjectID = os.path.splitext(os.path.basename(eachfile))[0][10:14]
	expStartTime = os.path.splitext(os.path.basename(eachfile))[0][15:29]	
	block = ("baseline" if "baseline" in eachfile else "main")
	
	init_datafile_path(expStartTime, subjectID, block, eachfile)
	with open(eachfile, 'r') as file:
		for line in file:		
			allrows = ast.literal_eval(line)
			for eachrow in allrows:
				write_data_path(str(eachrow).strip('[]'), expStartTime, subjectID, block, eachfile)
