#! /usr/bin/env python

##=============================================================================
## Moodmecodynamics: Roel van Dooren, Roy de Kleijn, Bernhard Hommel, & Zsuzsika Sjoerds
## Constants file foraging game.
##
## Created by:
## Roel van Dooren
## r.van.dooren@fsw.leidenuniv.nl
##
## Last modified: 2019/03/01 07:18
## Copyright (c) 2019 Roel van Dooren. All rights reserved.
##=============================================================================

import pygame

# PARAMETERS
DISPSIZE_FORAGING = (600, 600) 
DISPSIZE_PRACTICE = (600, 600)
DISPSIZE = (600, 600)	
#DISPSIZE = (1920, 1080)	
BGC = (0, 0, 0)							# Background color
FONTCOLOR = (250, 250, 250)				# Font color

# EYETRACKER PARAMETERS
TRACKER_ON = False 
SAMPLERATE = 120. # Sampling rate of the eyetracker (i.e., gaze data = 120 Hz; pupil data = 40 Hz).
DELAY_START_END_MARKER_TOBII = int(round(1000 / SAMPLERATE, 1)) / 2
TRACKERTYPE = 'dummy' #tobii
DISPTYPE = 'pygame'

# EXPERIMENT PARAMETERS VISUAL FORAGING
NUMTRIALS = 1
PRETRIALTIME = 10.0			 			# Defined in seconds, note that this should be set as a float
TRIALTIME = 300							# Defined in seconds
UPDATECOUNTER = 18000
TOTALCOUNTER = 18000
PXPERSECOND_AERIAL = 60		  			# Speed of magnifier/character per second
PXPERSECOND_FOCUSED = 90	 			# Speed of polygon per second
PXPERSECOND_PRACTICE = 40				# Speed of polygon per second in the practice block

RADIUS_MAGNIFIER = 15 					# Radius of magnifier (in aerial view)
RADIUS_POLYGON = 2						# Radius of the polygon (in patch view; rescaled by factor DISPSIZE_FORAGING[0] / ( (RADIUS_PATCH+1)*2)  
RADIUS_PATCH = 29			 			# Radius of each patch (should always be an uneven number!) (due to environmental rescaling from 200x200 to 600x600, this value is multiplied with factor 3)
PATCH_FROM_BORDER_PX = 5				# Make sure that the patches do not get too close to the borders (due to environmental rescaling from 200x200 to 600x600, this value is multiplied with factor 3).
MIN_DISTANCE_BETWEEN_PATCHES_PX = 3 	# Minimum distance between different patches in pixels (due to environmental rescaling from 200x200 to 600x600, this value is multiplied with factor 3).
MIN_DISTANCE_BETWEEN_BERRIES_PX = 2 	# Minimum distance between berries in pixels (due to environmental rescaling from 40x40 to 600x600, this value is multiplied with factor 15).

UNIQUEPATCHES = 3 						# Total number of unique patches
CELLS_PER_PATCH = 3 					# Total number of cells per patch (i.e., duplicates)
PATCHCOLORS = [(255, 0, 0), (255, 0, 0), (255, 0, 0)] #[(255, 0, 0), (0, 0, 255), (255, 0, 255)] # Amount of unique colors (should equal UNIQUEPATCHES)
VALUE_PER_BERRY = [1, 1, 1]#[3, 5, 7] 			# The points scored per obtained berry. The middle index is used for the first patch that is encountered.
UNIQUE_COORDS_PER_PATCH = 40 			# N of unique coordinates that should be present in each bush

# SCRIPT PARAMETERS
FRAMERATE = 60
DATA_DIRECTORY = 'output'
STUDYCODE = 'BP53_2019_'
DEBUG = False
AFFECT_GRID = True