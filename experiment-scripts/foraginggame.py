#! /usr/bin/env python

##=============================================================================
## Moodmecodynamics: Roel van Dooren, Roy de Kleijn, Bernhard Hommel, & Zsuzsika Sjoerds
## 
## Created by:
## Roel van Dooren
## r.van.dooren@fsw.leidenuniv.nl
##
## Last modified: 2019/03/01 07:18
## Copyright (c) 2019 Roel van Dooren. All rights reserved.
##=============================================================================

import pygame
from constants import *
import random
import sys
import numpy as np
from math import sin, cos, radians
from pygame.locals import *
from timeit import default_timer as timer
from scipy.spatial import distance
import os
import math
import pythonlibRD
from pygame import gfxdraw
import copy
from pygaze.display import Display
from pygaze.screen import Screen
from pygaze import libscreen
from pygaze import eyetracker
import pygaze
import affect_grid

##=============================================================================

class TobiiSession(object):
	"""docstring for TobiiSession"""

	def __init__(self, expStartTime, subjectID, block, disp, *args, **kwargs):
		super(TobiiSession, self).__init__()
		
		self.expStartTime = expStartTime
		self.subjectID = subjectID
		self.block = block
		self.disp = disp
		
		if TRACKER_ON: self.create_tracker(tracker_on = TRACKER_ON, sample_rate = SAMPLERATE)
			
	def create_tracker(self, tracker_on = True, sample_rate = 1000, data_directory = './tobii_data'):
		""" Tracker sets up the connection and inputs the parameters.
		only start tracker after the screen is taken, its parameters are set,
		and output file names are created. """
		if not os.path.isdir(data_directory):
			os.mkdir(data_directory)  # Create output folder if it doesn't yet exist
			
		self.output_file = os.path.join(data_directory, STUDYCODE) + str(self.subjectID) + '_' + str(self.expStartTime) + '_block_' + str(self.block)
		
		if tracker_on: # Create the actual eyetracker object.
			try:
				self.tracker = eyetracker.EyeTracker(self.disp, trackertype = TRACKERTYPE, data_file = self.output_file, bgc = BGC)
				self.tracker.start_recording() # Start eyetracking
			except: print('\nCould not connect to tracker with type {}'.format(TRACKERTYPE))
		else: self.tracker = None # Do not even create a dummy tracker.
						
	def tracker_setup(self):
		# Calibrate the eyetracker.
		self.log_marker("245") # Send CALIBRATION_START marker
		self.tracker.calibrate() # Calibrate the eyetracker
		self.log_marker("246") # Send CALIBRATION_END marker

	def log_marker(self, marker):
		if TRACKER_ON:
			self.tracker.log(marker) # Send marker
			pygame.time.delay(DELAY_START_END_MARKER_TOBII)
			self.tracker.log("0") # Reset marker

	def closeTobii(self):
		if self.tracker:
			if self.tracker.connected():
				self.tracker.stop_recording()
			self.tracker.close()

##=============================================================================

class Instructions(object):
	"""Displays instructions on screen and waits for user input"""
	def __init__(self, surface, disp):
		self.surface = surface
		self.disp = disp
		self.font = pygame.font.Font(None, 25)

	def instructionpage(self, image = False, responseRequired = True):
		self.surface.fill(BGC)
		if image:
			self.surface.blit(image, image.get_rect(center=(self.surface.get_width() / 2, self.surface.get_height() / 2)))
		self.disp.show(); pygame.event.clear()
		if not responseRequired :
			#self.surface.fill(BGC); self.disp.show()
			return
		else:
			while True:
				event = pygame.event.poll()
				if event.type == pygame.KEYDOWN:
					if event.key == pygame.K_SPACE:
						self.surface.fill(BGC); self.disp.show(); pygame.time.delay(500)
						return

	def totalScorePage(self, text, textpos, image):
		self.surface.fill(BGC)
		self.surface.blit(image, image.get_rect(center=(self.surface.get_width() / 2, self.surface.get_height() / 2)))
		self.surface.blit(text, textpos) # blit participants' score to the display
		self.disp.show()
		while True:
			event = pygame.event.poll()
			if event.type == pygame.KEYDOWN:
				if event.key == pygame.K_SPACE:
					self.surface.fill(BGC); self.disp.show(); pygame.time.delay(500)
					return

##=============================================================================

class Hand(object):
	"""Creates a hand that moves within the focused environment"""
	def __init__(self):
		self.handposture = 'hand_open' # Hand is displayed as an open hand
		self.grabTime = pygame.time.get_ticks()

	def setHandImage(self, berry_found = False) :
		"""Module responsible for setting the hand image based on whether a berry is encountered or not"""
		# Define the hand that is presented (open vs. grab vs. closed).
		if berry_found : # Set the hand to a grabbing response whenever a berry is found
			self.handposture = 'hand_grab'
			self.grabTime = pygame.time.get_ticks()
			
		if self.handposture	== 'hand_grab' : 
			if pygame.time.get_ticks() - self.grabTime > 150: # Only change hand posture to a closed posture after 150 ms
				self.handposture = 'hand_close'
				self.closeTime = pygame.time.get_ticks()
		elif self.handposture == 'hand_close' :
			if pygame.time.get_ticks() - self.closeTime > 150: # Only change hand posture to an open posture after 150 ms
				self.handposture = 'hand_open'
		else : self.handposture = 'hand_open'
				
		self.handImage = pygame.image.load(os.path.join("images", "character/") + self.handposture + '.png')

class Character(object):
	"""Creates a character that can move within the aerial environment"""
	def __init__(self):
		self.frontfoot = 'rightfoot' # Character starts moving with the right foot
		self.walkTime = pygame.time.get_ticks()

	def setCharacterImage(self, direction, speed) :
		"""Module responsible for setting the character image based on the current direction the character is facing"""
		
		# Define the gaze direction of the character
		if direction >= 45 and direction <= 135 : self.gaze = 'front'
		elif direction > 135 and direction < 225 : self.gaze = 'left'	
		elif direction >= 225 and direction <= 315 : self.gaze = 'back'
		else: self.gaze = 'right'	
		
		if speed < 0.2 : # If the character is not moving
			self.characterImage = self.gaze + "_static.png"
		else: # If the character is moving
			self.characterImage = "_".join([self.gaze, self.frontfoot]) + ".png"
			
			if pygame.time.get_ticks() - self.walkTime > 250: # Only change foot every 250 ms
				self.frontfoot = ("rightfoot" if self.frontfoot == "leftfoot" else "leftfoot") # Update frontfoot
				self.walkTime = pygame.time.get_ticks()
				
		self.characterImage = pygame.image.load(os.path.join("images", "character/") + self.characterImage)

##=============================================================================

class Magnifier(object):
	"""Creates a magnifier for searching in the aerial view"""
	def __init__(self):
		self.position = (DISPSIZE_FORAGING[0]/2, DISPSIZE_FORAGING[1]/2)
		self.direction = random.randint(0, 359); self.olddirection = self.direction; self.speed = 0
		self.patch_encounter = 'false'
		self.joystick()

	def joystick(self):
		"""Checks whether a joystick is connected and, if so, sets it accordingly"""
		if [pygame.joystick.Joystick(x) for x in range(pygame.joystick.get_count())] != []:
			self.joystick = pygame.joystick.Joystick(0); self.joystick.init()

	def move(self):
		"""Move the magnifier to a given location by using the current position, speed and direction"""
		self.position = (self.position[0] + self.speed * cos(radians(self.direction)),
						 self.position[1] + self.speed * sin(radians(self.direction)))
		self.position = np.clip(self.position, 0, DISPSIZE_FORAGING[0]-1)

		if self.position[0] == 0.0 or self.position[0] == float(DISPSIZE_FORAGING[0]-1):
			self.position[0] = (float(DISPSIZE_FORAGING[0]-1) if self.position[0] == 0.0 else 0.0)
		if self.position[1] == 0.0 or self.position[1] == float(DISPSIZE_FORAGING[0]-1):
			self.position[1] = (float(DISPSIZE_FORAGING[0]-1) if self.position[1] == 0.0 else 0.0)

	def setMagnifier(self):
		"""Set the magnifier to a location on the 'aerial view' surface."""
		self.loc = (int(round(self.position[0])), int(round(self.position[1])))
	
			
class Agent(object):
	"""Creates a visual foraging agent for searching in patches"""
	def __init__(self):
		self.position = (DISPSIZE_FORAGING[0]/2, DISPSIZE_FORAGING[1]/2)
		self.direction = random.randint(0, 359); self.olddirection = self.direction
		self.speed = 0; self.points_scored = 0
		self.food_encounter = False
		self.joystick()

	def joystick(self):
		"""Checks whether a joystick is connected and, if so, sets it accordingly"""
		if [pygame.joystick.Joystick(x) for x in range(pygame.joystick.get_count())] == []:
			print "No joystick detected."
		else:
			self.joystick = pygame.joystick.Joystick(0); self.joystick.init()

	def move(self):
		"""Move the polygon to a given location by using the current position, speed and direction"""
		self.position = (self.position[0] + self.speed * cos(radians(self.direction)),
						 self.position[1] + self.speed * sin(radians(self.direction)))
		self.position = np.clip(self.position, 0, float(DISPSIZE_FORAGING[0]-1))

		if self.position[0] == 0.0 or self.position[0] == float(DISPSIZE_FORAGING[0]-1):
			self.position[0] = (float(DISPSIZE_FORAGING[0]-1) if self.position[0] == 0.0 else 0.0)
		if self.position[1] == 0.0 or self.position[1] == float(DISPSIZE_FORAGING[0]-1):
			self.position[1] = (float(DISPSIZE_FORAGING[0]-1) if self.position[1] == 0.0 else 0.0)

	def rotatePolygon(self, polygon, theta):
		"""Rotates the given polygon which consists of corners represented as (x,y),
		around the ORIGIN, clock-wise, theta degrees"""
		theta = radians(theta)
		rotatedPolygon = []
		for corner in polygon:
			rotatedPolygon.append((corner[0]*cos(theta)-corner[1]*sin(theta), corner[0]*sin(theta)+corner[1]*cos(theta)))
		return rotatedPolygon

	def movePolygon(self, polygon, x, y):
		"""Moves the given polygon which consists of corners represented as (x, y)"""
		movedPolygon = []
		for corner in polygon:
			movedPolygon.append((corner[0]+x, corner[1]+y))
		return movedPolygon

	def setPolygon(self):
		"""Set the polygon to a location on the 'focused view' surface."""
		self.loc = (int(round(self.position[0])), int(round(self.position[1])))
		self.polygon_points = self.rotatePolygon([[0, 10], [-5, -10], [5, -10]], (self.direction + 270) % 360)
		self.polygon_points = self.movePolygon(self.polygon_points, self.loc[0], self.loc[1])

##=============================================================================

class Environment(object):
	"""Creates foraging environments"""
	def __init__(self, expStartTime, subject, condition, agent, block):
		self.expStartTime = expStartTime
		self.subjectID = subject
		self.condition = condition
		self.block = block
		self.output = Output(self.subjectID, self.expStartTime, self.condition, agent, self.block)
		self.patch_central_coords = []; self.encountered_all_coords = []
		self.color_value_mapping = []; self.patches_entered = 0
	
	def collectPixelsInRadius(self, R, x, y):
		"""Collects all coordinates that are present in radius R of central coordinates x and y"""
		neighborhood = []
		X = int(R) # R is the radius
		for i in range(-X,X+1):
			Y = int((R*R-i*i)**0.5) # bound for y given x
			for j in range(-Y,Y+1):
				neighborhood.append((x + i, y + j))
		return neighborhood

	def eucdistances(self, xypos_magnifier):
		"""Calculates the euclidean distances between xypos of magnifier and xypos of patches
		Returns the central_patch_coords for all patches and for the minimimal euclidean distance between xypos of magnifier and a patch"""
		dist = []
		for xypos in self.xypos:
			distxy = {'patch_coord' : xypos, 'eucdist' : distance.euclidean(xypos_magnifier, xypos)}
			dist.append(distxy)
		# Detect which patch is closest by means of the euclidean distances that we just calculated
		min_eucl = min([x['eucdist'] for x in dist])
		# Return coordinates of patch with lowest euclidean distance to xypos_magnifier
		patch_central_coords = [x['patch_coord'] for x in dist if x['eucdist'] == min_eucl][0]
		all_central_coords = [x['patch_coord'] for x in dist]
		return patch_central_coords, all_central_coords

	def gen_seen(self, surface):
		"""Method responsible for recreating the surface of a previously entered patch.
		The method checks the position of the encountered patch, and extracts the left x and upper y borders from each of the coordinates in the patch"""
		zoomed_seenSurface = pygame.Surface(( (RADIUS_PATCH+1)*2, (RADIUS_PATCH+1)*2 ), flags=0)
		self.seenSurface_all_coords = self.collectPixelsInRadius(RADIUS_PATCH, self.patch_central_coords[0], self.patch_central_coords[1])
		self.seenSurface_all_coords = [x for x in self.encountered_all_coords if x in self.seenSurface_all_coords]
		if self.seenSurface_all_coords != []: # As long as at least one resource has previously been encountered
			self.seenSurface_all_coords = [tuple(np.subtract(x, (self.patch_central_coords[0]-RADIUS_PATCH, \
									  self.patch_central_coords[1]-RADIUS_PATCH))) for x in self.seenSurface_all_coords]
		zoomed_seenSurface = pygame.transform.scale(zoomed_seenSurface, DISPSIZE_FORAGING)
		return zoomed_seenSurface

	def gen_zoomed(self, xypos_magnifier, gaus_env):
		"""Method responsible for creating the zoomed mapSurface used in the 'focused view' environment.
		The program will know which patch is currently encountered by relying on euclidean distance measures
		Next, the position of each coordinate in the encountered patch is checked, and extracted from the left x and upper y borders"""
		zoomed_mapSurface = pygame.Surface(( (RADIUS_PATCH+1)*2, (RADIUS_PATCH+1)*2 ), flags=0)
		# Return coordinates of patch with lowest euclidean distance to xypos_magnifier
		self.patch_central_coords, all_central_coords = self.eucdistances(xypos_magnifier)
		self.current_patch_info = [patch for patch in self.overall_patch_info if patch['patch_center_coords'] == self.patch_central_coords][0]
		
		# Subtract maximum left x and upper y coordinates of the radius from each of the coordinates in the coordlist
		# By subtracting these values, we are sure that each zoomed_mapSurface equals (RADIUS_PATCH+1)*2 x (RADIUS_PATCH+1)*2 pxs
		patch_all_coords = [tuple(np.subtract(x, (self.patch_central_coords[0]-RADIUS_PATCH, \
												  self.patch_central_coords[1]-RADIUS_PATCH))) for x in self.current_patch_info['patch_all_coords'][0]]
		# Draw resources at the zoomed mapSurface coordinates (results in resources distributed within a circle)
		for coord in patch_all_coords:
			gfxdraw.pixel(zoomed_mapSurface, coord[0], coord[1], self.current_patch_info['color'])
		
		# Detect which index in the overall patch list resembles the dictionary containing the current patch information	
		self.index = [index for index, patch in enumerate(self.overall_patch_info) if patch['patch_center_coords'] == self.patch_central_coords][0]
		self.overall_patch_info[self.index]['times_entered'] += 1
		
		# Set the color to value mapping once the very first patch has been entered
		if self.patches_entered == 1 :
			value_per_berry_first_patch = VALUE_PER_BERRY[int(len(VALUE_PER_BERRY)/ 2)]
			
			# Remove the picked color from the color list, as well as the picked value per berry
			PATCHCOLORS.remove(self.current_patch_info['color'])
			VALUE_PER_BERRY.remove(value_per_berry_first_patch)
						
			# Once the value of the first color is set, pick a value for the remaining colors
			for it, update_patch in enumerate([patch for patch in self.overall_patch_info]) :
				if update_patch['color'] == self.current_patch_info['color'] :
					# Identify those cells that have the same color mapping as the index
					self.overall_patch_info[it]['value_per_berry'] = value_per_berry_first_patch
				else:
					for it_color, each_color in enumerate(PATCHCOLORS) :
						if update_patch['color'] == each_color :
							self.overall_patch_info[it]['value_per_berry'] = VALUE_PER_BERRY[it_color % UNIQUEPATCHES]
				
				# Save the color-value mapping 		
				self.color_value_mapping.append( {'color' : self.overall_patch_info[it]['color'], 'value' : self.overall_patch_info[it]['value_per_berry']} )
							
			# Extract unique color-value mappings
			self.color_value_mapping = np.unique(self.color_value_mapping)
		
		# Extract the value per berry 
		self.current_patch_info['value'] = [mapping['value'] for mapping in self.color_value_mapping if mapping['color'] == self.current_patch_info['color']][0]

		# From the magnifier's current position, we calculate where the polygon should be presented in the 'focused view' environment		
		self.rescaling_factor = DISPSIZE_FORAGING[0] / ( (RADIUS_PATCH+1)*2)
		self.xymag_adj = [x*self.rescaling_factor for x in [tuple(np.subtract(xypos_magnifier, \
						 (self.patch_central_coords[0]-RADIUS_PATCH, self.patch_central_coords[1]-RADIUS_PATCH)))][0]]
		
		zoomed_mapSurface = pygame.transform.scale(zoomed_mapSurface, DISPSIZE_FORAGING)
		return zoomed_mapSurface

	def gen_focused(self, num_patches = UNIQUEPATCHES*CELLS_PER_PATCH):
		"""Method responsible for creating randomly distributed resource patches that are used in the 'focused view' environment.
		The function will pick UNIQUE_COORDS_PER_PATCH coordinates that are present in RADIUS_PATCH (see constants.py)"""
		focused_mapSurface = pygame.Surface(DISPSIZE_FORAGING, flags = 0)
		self.output.init_datafile_mapSurface(self.block)
				
		# Patches will get different colors, as that allows us to set, in a later stage, the value per berry 
		if UNIQUEPATCHES == 3 : # So far, I only integrated three differently colored berries
			self.patchcolors = [{'color' : patchcolor} for patchcolor in PATCHCOLORS] * CELLS_PER_PATCH
		else:
			pick_colors = True
			while pick_colors :
				self.patchcolors = [{'color' : (random.randint(0, 255), random.randint(0, 255), random.randint(0, 255))} for i in range(UNIQUEPATCHES)] * CELLS_PER_PATCH
				if len(set([c['color'] for c in self.patchcolors])) == UNIQUEPATCHES : pick_colors = False
		
		self.overall_patch_info = []
		for patch in range(num_patches): # For each patch we would like to create
			pixelsinradius = self.collectPixelsInRadius(RADIUS_PATCH-PATCH_FROM_BORDER_PX, self.xypos[patch][0], self.xypos[patch][1])
			
			# Randomly sample one coordinate from the pixels in the radius of the berry bush
			self.overall_list_with_coordinates = []
			for unique_coord in range(UNIQUE_COORDS_PER_PATCH):
				random_coord = random.sample(pixelsinradius, 1)[0]
				self.overall_list_with_coordinates.append(random_coord)
				
				# Remove all coordinates in a radius of MIN_DISTANCE_BETWEEN_BERRIES_PX pixels of the picked coordinate
				pixelsinradius_berry = self.collectPixelsInRadius(MIN_DISTANCE_BETWEEN_BERRIES_PX, random_coord[0], random_coord[1])
				pixelsinradius = [coord for coord in pixelsinradius if coord not in pixelsinradius_berry]
			
			patch_info = {'patch_center_coords' : self.xypos[patch], 'patch_all_coords' : self.overall_list_with_coordinates,
						  'color' : self.patchcolors[patch]['color'], 'times_entered' : 0, 'value_per_berry' : 0, 'points_scored_patch' : 0}
			self.overall_patch_info.append(patch_info)
			
			for coord in self.overall_list_with_coordinates:
				gfxdraw.pixel(focused_mapSurface, coord[0], coord[1], self.patchcolors[patch]['color'])		
				
		self.output.write_mapSurface(self.overall_list_with_coordinates, self.block) # Save mapsurface for purposes of recreation
		return focused_mapSurface

	def gen_aerial(self, num_patches = UNIQUEPATCHES*CELLS_PER_PATCH):
		"""Method responsible for creating the aerial_mapSurface used in the 'aerial view' environment"""
		aerial_mapSurface = pygame.Surface(DISPSIZE_FORAGING, flags=0)
		
		overlaps = True
		while overlaps:
			aerial_mapSurface.fill(BGC)
			self.overall_list_with_coordinates = []; self.xypos = []
			for eachpatch in range(num_patches):
				# Draw random coordinates for each patch, with the pre-specified exceptions that patches 
				# should not be located in the center of the surface, nor around the edges.
				locations = range(0+(RADIUS_PATCH+PATCH_FROM_BORDER_PX), (DISPSIZE_FORAGING[0]/2 - RADIUS_PATCH)) + \
						    range((DISPSIZE_FORAGING[0]/2 + RADIUS_PATCH+1), DISPSIZE_FORAGING[0]-(RADIUS_PATCH+PATCH_FROM_BORDER_PX))				
				xpos = random.choice(locations); ypos = random.choice(locations)
				
				# Make sure that the picked locations do not overlap. Note: minimum distance between patches can be specified
				list_with_coordinates = self.collectPixelsInRadius((RADIUS_PATCH + MIN_DISTANCE_BETWEEN_PATCHES_PX), xpos, ypos)
				mapcoordlist = [x for eachlist in self.overall_list_with_coordinates for x in eachlist]
				
				while any([x for x in list_with_coordinates if x in mapcoordlist]): # If drawn coordinates are already taken
					list_with_coordinates, xpos, ypos = self.drawnewcoords(self.overall_list_with_coordinates) # Draw new coordinates
				self.overall_list_with_coordinates.append(list_with_coordinates); self.xypos.append((xpos, ypos))
				# Draw resources at the zoomed mapSurface coordinates (results in a circle)
				pygame.draw.polygon(aerial_mapSurface, (255, 255, 255), list_with_coordinates)

			pxarray = pygame.PixelArray(aerial_mapSurface) # Convert the mapsurface to a pixelarray object
			overlaps = False

		return aerial_mapSurface

	def drawnewcoords(self, coordall):
		"""Method responsible for drawing new coordinates whenever picked coordinates are already present in the mapcoordlist"""
		overlaps = True
		while overlaps:
			locations = range(0+(RADIUS_PATCH+PATCH_FROM_BORDER_PX), (DISPSIZE_FORAGING[0]/2 - RADIUS_PATCH)) + \
						range((DISPSIZE_FORAGING[0]/2 + RADIUS_PATCH+1), DISPSIZE_FORAGING[0]-(RADIUS_PATCH+PATCH_FROM_BORDER_PX))				
			xpos = random.choice(locations); ypos = random.choice(locations)
			
			list_with_coordinates = self.collectPixelsInRadius((RADIUS_PATCH + MIN_DISTANCE_BETWEEN_PATCHES_PX), xpos, ypos)
			mapcoordlist = [x for eachlist in coordall for x in eachlist]
			if [x for x in list_with_coordinates if x in mapcoordlist] == []:
				overlaps = False
		return list_with_coordinates, xpos, ypos

##=============================================================================

class Output(TobiiSession):
	"""Initializes output files and takes care of saving data"""
	def __init__(self, subjectID, expStartTime, condition, agent, block):
		self.subjectID = subjectID
		self.expStartTime = expStartTime
		self.condition = condition
		self.agent = agent
		self.init_datafile_foraging(block) # Initialise foraging output file

	def init_datafile_foraging(self, block):
		f = open(os.path.join(DATA_DIRECTORY, STUDYCODE) + str(self.subjectID) + "_" + str(self.expStartTime) + "_visual_foraging_block_" + str(block) + ".txt", 'w')
		output = 'expStartTime,subjectID,condition,block,timespent,patch_number,patch_center_coordinates,patch_times_entered,patch_color,patch_value_per_berry,patch_points_scored,total_points_scored,total_patches_entered,unique_patches_entered,all_encountered_berry_coords\n'
		f.write(output)
		f.close()

	def init_datafile_foragingpath(self, block):
		filename = os.path.join(DATA_DIRECTORY, STUDYCODE) + str(self.subjectID) + "_" + str(self.expStartTime) + "_visual_foraging_path_block_" + str(block) + ".txt"

	def init_datafile_mapSurface(self, block):
		filename = os.path.join(DATA_DIRECTORY, STUDYCODE) + str(self.subjectID) + "_" + str(self.expStartTime) + "_visual_foraging_mapSurface_block_" + str(block) + ".txt"

	def init_datafile_questions(self):
		f = open(os.path.join(DATA_DIRECTORY, STUDYCODE) + str(self.subjectID) + "_" + str(self.expStartTime) + "_questions.txt", 'w')
		output = 'expStartTime,subjectID,condition,question_num,response,responseTime\n'
		f.write(output)
		f.close()

	def write_data(self, block, trialStartTime, pretrialtime, parameters, all_encountered_berry_coords):
		f = open(os.path.join(DATA_DIRECTORY, STUDYCODE) + str(self.subjectID) + "_" + str(self.expStartTime) + "_visual_foraging_block_" + str(block) + ".txt", 'a')
		
		total_patches_entered = sum( [ patch['times_entered'] for patch in parameters ] )
		unique_patches_entered = len( [ patch['patch_center_coords'] for patch in parameters if patch['times_entered'] > 0 ] )
		
		for it, patch_parameters in enumerate(parameters):
			output = str(self.expStartTime) + "," + str(self.subjectID) + "," + str(self.condition) + "," + \
					 str(block) + "," + str(timer() - trialStartTime - pretrialtime) + "," + str(it+1) + "," + \
					 str(patch_parameters['patch_center_coords']) + "," + str(patch_parameters['times_entered']) + "," + \
					 str(patch_parameters['color']) + "," + str(patch_parameters['value_per_berry']) + "," + \
					 str(patch_parameters['points_scored_patch']) + "," + str(self.agent.points_scored) + "," + \
					 str(total_patches_entered) + "," + str(unique_patches_entered) + "," + \
					 ("NA" if len(all_encountered_berry_coords) == 0 else "--".join([str(encountered_berry_coords) for encountered_berry_coords in all_encountered_berry_coords])) + "\n"
			f.write(output)
		f.close()

	def write_data_path(self, output, block):
		f = open(os.path.join(DATA_DIRECTORY, STUDYCODE) + str(self.subjectID) + "_" + str(self.expStartTime) + "_visual_foraging_path_block_" + str(block) + ".txt", 'a')
		f.write("%s" % output)
		f.close()

	def write_mapSurface(self, mapSurface, block):
		f = open(os.path.join(DATA_DIRECTORY, STUDYCODE) + str(self.subjectID) + "_" + str(self.expStartTime) + "_visual_foraging_mapSurface_block_" + str(block) + ".txt", 'a')
		f.write("%s" % mapSurface)
		f.close()

	def write_data_questions(self, trialnum = None, response = None):
		f = open(os.path.join(DATA_DIRECTORY, STUDYCODE) + str(self.subjectID) + "_" + str(self.expStartTime) + "_questions.txt", 'a')
		output = str(self.expStartTime) + "," + str(self.subjectID) + "," + \
				 str(self.condition) + "," + str(trialnum) + "," + str(response[0]) + "," + str(response[1]) + "\n" 
		f.write(output)
		f.close()

##=============================================================================
## Main experiment
##=============================================================================

class App(TobiiSession):
	"""Takes care of running the main experiment"""
	def __init__(self, expStartTime, subject, block, condition, *args, **kwargs):
		self.expStartTime = expStartTime
		self.subjectID = self.zerofill(subject, 4)
		self.block = block
		self.condition = condition
		pygame.init()
		
		setattr(pygaze.settings, 'FULLSCREEN', (False if DEBUG else True))
		self.disp = Display(screennr = 0)
		self._display_surf = Screen()
		if DEBUG: self._display_surf.screen = pygame.display.set_mode(DISPSIZE, pygame.HWSURFACE | pygame.DOUBLEBUF)
		else: self._display_surf.screen = pygame.display.set_mode(DISPSIZE,pygame.HWSURFACE | pygame.DOUBLEBUF | pygame.FULLSCREEN)				
		self._display_surf.screen.fill(BGC)
		
		# Initialise higher-order objects
		self.magnifier = Magnifier() # Initialize magnifier: used in aerial view
		self.character = Character() # Initialize character
		self.agent = Agent() # Initialize polygon: used in focused view
		self.hand = Hand() # Initialize hand: used in focused view
		self.instructions = Instructions(self._display_surf.screen, self.disp)
		self.output = Output(self.subjectID, self.expStartTime, self.condition, self.agent, self.block)
		self.env = Environment(self.expStartTime, self.subjectID, self.condition, self.agent, self.block)
		
		if TRACKER_ON: self.tobii = TobiiSession(self.expStartTime, self.subjectID, self.block, self.disp)		
		super(App, self).__init__(self.expStartTime, self.subjectID, self.block, self.disp, *args, **kwargs)
		
		self.total_counter = TOTALCOUNTER # Remains the same during trial time countdown computations
		self.update_counter = UPDATECOUNTER # Decreases as a function of trial time countdown computations
		self.pretrialtime = PRETRIALTIME # Note that this should be specified as a float!
		self.trial_time = TRIALTIME+self.pretrialtime # Take the x seconds pre-trial time into account
		self.clock = pygame.time.Clock()
		self.font = pygame.font.Font(None, 25)
		self.view = 'aerial'; self.axis0 = 'NA'; self.axis1 = 'NA'
		self.switch_button_pressed = False
		self.patch_enter_timestamps = []; self.patch_leave_timestamps = []
		
	def zerofill(self, number, width):
		width -= len(str(number))
		if (width > 0) :
			i = 0
			while i < width:
				number =   str(number).join("0")+ str(number)
				i += 1
		return number

	def clockHand(self, size, theta, xloc, yloc):
		dx = int(cos(radians(theta)) * size)
		dy = int(sin(radians(theta)) * size)
		return xloc + dx, yloc + dy

	def setAlpha(self, surface, perpixelalpha, alpha = 255):
		"""Replaces all per pixel alpha values of surface with perpixelalpha (0 - 255 : transparent - opaque)
		Sets the alpha of the entire surface to alpha. Note that changing alpha does not affect per pixel alpha values"""
		self.array = pygame.surfarray.pixels_alpha(surface)
		self.alpha_array = np.ones(self.array.shape, dtype=self.array.dtype) * perpixelalpha
		np.copyto(self.array, self.alpha_array)
		surface.set_alpha(alpha)
		return self.array

	def on_execute(self):
		pygame.mouse.set_visible(False)
		
		if not DEBUG: # Present instructions
			if self.block == 'baseline': # If we are in the baseline block			
				self.instructions.instructionpage(pygame.image.load(os.path.join("images", "foraging") + "/game_start.png"))
				self.instructions.instructionpage(pygame.image.load(os.path.join("images", "foraging") + "/bush_found.png"))
				self.instructions.instructionpage(pygame.image.load(os.path.join("images", "foraging") + "/bush_entered.png"))
				self.instructions.instructionpage(pygame.image.load(os.path.join("images", "foraging") + "/bush_entered_points.png"))
				self.instructions.instructionpage(pygame.image.load(os.path.join("images", "foraging") + "/leave_and_revisit.png"))
				self.instructions.instructionpage(pygame.image.load(os.path.join("images", "foraging") + "/ask_questions.png"))
			else: # If we are in the main experimental block
				self.instructions.instructionpage(pygame.image.load(os.path.join("images", "foraging") + "/replay_game.png"))

		print("""# ---------------RUN EXPERIMENT--------------- #""")
		if TRACKER_ON: self.tobii.log_marker("255") # Send EXPERIMENT_START marker
			
		if AFFECT_GRID:
			print("""# ---------------PRESENT AFFECT GRID--------------- #""")
			if TRACKER_ON: self.tobii.log_marker("225") # Send AFFECT_GRID_START marker
			AffectGrid = affect_grid.App(self.expStartTime, self.subjectID, externalscreen = self._display_surf.screen, blocknr = self.block + '_pre_foraging', condition = self.condition)
			AffectGrid.on_execute()
			if TRACKER_ON: self.tobii.log_marker("226") # Send AFFECT_GRID_START marker
			self._display_surf.screen.fill(BGC); self.disp.show() # Clear the surface
			pygame.mouse.set_visible(False)
			
		# Set the monitor size to DISPSIZE		
		if DEBUG: self._display_surf.screen = pygame.display.set_mode(DISPSIZE, pygame.HWSURFACE | pygame.DOUBLEBUF)
		else: self._display_surf.screen = pygame.display.set_mode(DISPSIZE, pygame.HWSURFACE | pygame.DOUBLEBUF | pygame.FULLSCREEN)		
		self.instructions.instructionpage(pygame.image.load(os.path.join("images", "general") + "/loading.png"), responseRequired = False)
					
		for trialNum in range(NUMTRIALS):
			self.update_counter = UPDATECOUNTER			
			self.run_trial_foraging() # Run a visual foraging trial
			self._display_surf.screen.fill(BGC); self.disp.show() # Clear the surface
			
			if AFFECT_GRID:
				print("""# ---------------PRESENT AFFECT GRID--------------- #""")
				if TRACKER_ON: self.tobii.log_marker("225") # Send AFFECT_GRID_START marker
				AffectGrid = affect_grid.App(self.expStartTime, self.subjectID, externalscreen = self._display_surf.screen, blocknr = self.block + '_post_foraging', condition = self.condition)
				AffectGrid.on_execute()
				if TRACKER_ON: self.tobii.log_marker("226") # Send AFFECT_GRID_END marker
				self._display_surf.screen.fill(BGC); self.disp.show() # Clear the surface
				pygame.mouse.set_visible(False)

		if TRACKER_ON: 
			self.tobii.log_marker("256") # Send EXPERIMENT_END marker.
			self.tobii.closeTobii() # Close the eyetracker
		
		print("""# ---------------PRESENT QUESTIONS--------------- #""")
		
		# Set the monitor size to DISPSIZE		
		if DEBUG: self._display_surf.screen = pygame.display.set_mode(DISPSIZE, pygame.HWSURFACE | pygame.DOUBLEBUF)
		else: self._display_surf.screen = pygame.display.set_mode(DISPSIZE, pygame.HWSURFACE | pygame.DOUBLEBUF | pygame.FULLSCREEN)		
		
		if self.block == 'main' :
			self.output.init_datafile_questions()
			questions = ["How did you approach the first search task? Did you use a specific strategy in order to collect as many berries as possible?", \
						 "How did you approach the second search task? Did you use a specific strategy in order to collect as many berries as possible?"]
			for num, question in enumerate(questions) :
				question = pythonlibRD.OpenQuestionPygame(surface = self._display_surf.screen, title = question, subtitle = 'Use the keyboard to type in your answer.')
				response = question.do()
				self.output.write_data_questions(num+1, response)
				self._display_surf.screen.fill(BGC); self.disp.show() # Clear the surface
		
		# Upon experiment completion
		self.on_cleanup()

	def run_trial_foraging(self):
		self._running_foragingtrial = True
		self.output.init_datafile_foragingpath(self.block)
		self.path_array = []
		
		# Create environments
		self.grass = pygame.transform.scale(pygame.image.load(os.path.join("images", "layout") + "/grass.png").convert_alpha(), (600, 600))
		self.bush = pygame.image.load(os.path.join("images", "layout") + "/bush.png").convert_alpha()
		self.grass_focused = pygame.transform.scale(pygame.image.load(os.path.join("images", "layout") + "/grass.png").convert_alpha(), (600, 600))
		self.bush_focused = pygame.transform.scale(pygame.image.load(os.path.join("images", "layout") + "/bush.png"), (600, 600))

		# Set per pixel alpha values 
		self.alpha_grass = self.setAlpha(self.grass, 255)

		# Create environments and regenate the surfaces that keep track of what participants encountered
		self.aerial_mapSurface = self.env.gen_aerial() # Initialise aerial mapSurface
		self.focused_mapSurface = self.env.gen_focused() # Initialise focused mapSurface
		self.aerial_seenSurface = pygame.Surface(DISPSIZE_FORAGING, flags=0)
		self.focused_seenSurface = pygame.Surface(DISPSIZE_FORAGING, flags=0)
		self.aerial_seenSurface.fill(BGC)

		'''
		print("""# ---------------CALIBRATE TOBII EYETRACKER--------------- #""")
		if TRACKER_ON:
			try: # Calibrate Tobii Eyetracker.
				self.wait.present_image(pygame.image.load(os.path.join("images", "practice") + "/startcalibration.png"))
				self.tobii.tracker_setup()
				self.wait.present_image(pygame.image.load(os.path.join("images", "practice") + "/endcalibration.png"))
				self.tobii.tracker.start_recording() # Start eyetracking
				print("""# ---------------SETUP COMPLETE--------------- #""")
			except AttributeError: print("Can't connect to eyetracker type {}".format(TRACKERTYPE))
		'''
				
		# Set the monitor size to DISPSIZE_FORAGING		
		#self.instructions.instructionpage(responseRequired = False)
		if DEBUG: self._display_surf.screen = pygame.display.set_mode(DISPSIZE_FORAGING, pygame.HWSURFACE | pygame.DOUBLEBUF)
		else: self._display_surf.screen = pygame.display.set_mode(DISPSIZE_FORAGING, pygame.HWSURFACE | pygame.DOUBLEBUF | pygame.FULLSCREEN)

		# Run the foraging trial
		if TRACKER_ON: self.tobii.log_marker("235") # Send TRIAL_START marker
		
		self.trialStartTime = timer() # Note that trialStartTime also takes the x seconds of 'Ready?' into account
		self.clock.tick(FRAMERATE) # Update the clock tick timer
		while self._running_foragingtrial:
			framerate = self.clock.tick(FRAMERATE) # Will be used to set self.speed to X px per second
			self.switch_button_pressed = False # Reset the parameter that keeps track of whether the switch button is pressed
			for event in pygame.event.get(): self.on_event(event)
			if self.view == 'aerial': self.on_loop_aerial(framerate)
			else: self.on_loop_focused(framerate) # self.view == 'focused'
			self.on_render()
			
			total_patches_entered = sum( [ patch['times_entered'] for patch in self.env.overall_patch_info ] )
			unique_patches_entered = len( [ patch['patch_center_coords'] for patch in self.env.overall_patch_info if patch['times_entered'] > 0 ] )
			
			self.path_array.append(
   					 [self.expStartTime, self.subjectID, self.condition, self.block, timer() - self.trialStartTime - self.pretrialtime,
					  self.agent.position[0], self.agent.position[1], self.magnifier.position[0], self.magnifier.position[1],
					  self.axis0, self.axis1, self.agent.direction,  self.magnifier.direction, self.agent.olddirection, 
					  self.magnifier.olddirection, self.agent.speed, self.magnifier.speed, self.agent.food_encounter,
					  self.agent.points_scored, self.update_counter, self.view,
					  self.magnifier.patch_encounter, self.switch_button_pressed, total_patches_entered, unique_patches_entered,
					  "--".join([str(enter_timestamp) for enter_timestamp in self.patch_enter_timestamps]), 
					  "--".join([str(leave_timestamp) for leave_timestamp in self.patch_leave_timestamps])])
			if timer()-self.trialStartTime >= self.trial_time:
				self._running_foragingtrial = False

		if TRACKER_ON: self.tobii.log_marker("236") # Send TRIAL_END marker
		
		# Write data to datafile
		self.output.write_data(self.block, self.trialStartTime, self.pretrialtime, self.env.overall_patch_info, self.env.encountered_all_coords)
		self.output.write_data_path(self.path_array, self.block)

		# Set the monitor size to DISPSIZE		
		if DEBUG: self._display_surf.screen = pygame.display.set_mode(DISPSIZE, pygame.HWSURFACE | pygame.DOUBLEBUF)
		else: self._display_surf.screen = pygame.display.set_mode(DISPSIZE, pygame.HWSURFACE | pygame.DOUBLEBUF | pygame.FULLSCREEN)	
		
		# Present feedback to participants' on their performance
		self._display_surf.screen.fill(BGC); self.disp.show()
		textScore = self.font.render(str(self.agent.points_scored), 1, (FONTCOLOR))
		textpos = textScore.get_rect(center=(self._display_surf.screen.get_width() / 2, self._display_surf.screen.get_height() / 1.75))
		image = pygame.image.load(os.path.join("images", "foraging") + "/score.png")
		self.instructions.totalScorePage(textScore, textpos, image)

	def on_event(self, event): # Processing button presses/joystick interaction in the visual foraging task
		if timer() - self.trialStartTime >= self.pretrialtime:
			# Make sure that polygon can only be moved after pretrialtime has passed
			if DEBUG and event.type == pygame.QUIT or DEBUG and event.type == KEYDOWN and event.key == K_ESCAPE:
				self._running_foragingtrial = False
			# Only enable buttonpresses if we're making use of a keyboard
			if [pygame.joystick.Joystick(x) for x in range(pygame.joystick.get_count())] == []: # Indicates that no joystick is detected
				if self.view == 'aerial':
					if event.type == KEYDOWN and event.key == K_j or event.type == KEYDOWN and event.key == K_l:
						self.magnifier.olddirection = self.magnifier.direction
						self.magnifier.direction = (self.magnifier.direction - (35 if event.key == K_j else -35)) % 360
					elif event.type == KEYDOWN and event.key == pygame.K_SPACE and self.magnifier.patch_encounter == 'true':
						if TRACKER_ON: self.tobii.log_marker("201") # Send ZOOM_IN marker
						
						# Generate the zoomed view focused_mapSurface and focused_seenSurface
						self.env.patches_entered += 1
						self.zoomed_mapSurface = self.env.gen_zoomed(self.magnifier.position, self.focused_mapSurface)
						self.agent.position = self.env.xymag_adj # Make sure that the polygon is at the exact same position as the magnifier if one zooms in
						self.agent.direction = self.magnifier.direction # Make sure that the polygon is in the exact same direction as the magnifier if one zooms in
						self.agent.olddirection = self.magnifier.olddirection # Make sure that the polygon's olddirection is equal to the olddirection of the magnifier
						self.view = 'focused'
						self.switch_button_pressed = True
						self.patch_enter_timestamps.append(timer() - self.trialStartTime - self.pretrialtime)
					elif event.type == KEYDOWN and event.key == pygame.K_SPACE:
						self.switch_button_pressed = True
				else:
					if event.type == KEYDOWN and event.key == K_j or event.type == KEYDOWN and event.key == K_l:
						self.agent.olddirection = self.agent.direction
						self.agent.direction = (self.agent.direction - (35 if event.key == K_j else -35)) % 360
					elif event.type == KEYDOWN and event.key == pygame.K_SPACE:
						if TRACKER_ON: self.tobii.log_marker("202") # Send ZOOM_OUT marker
						
						self.magnifier.position = self.new_xypos_rescaled # Make sure that the magnifier is at the exact same position as the polygon if one zooms out
						self.magnifier.direction = self.agent.direction # Make sure that the magnifier is in the exact same direction as the polygon if one zooms out
						self.view = 'aerial'
						self.switch_button_pressed = True
						self.patch_leave_timestamps.append(timer() - self.trialStartTime - self.pretrialtime)
			else: # Disable buttonpresses if we're making use of a joystick
				if self.view == 'aerial':
					if event.type == JOYAXISMOTION:
						self.magnifier.olddirection = self.magnifier.direction
						self.axis0 = self.magnifier.joystick.get_axis(0)
						self.axis1 = self.magnifier.joystick.get_axis(1)
						self.magnifier.direction = math.degrees(math.atan2(self.axis1, self.axis0)) % 360
					elif event.type == JOYBUTTONDOWN and event.button == 0 and self.magnifier.patch_encounter == 'true':
						if TRACKER_ON: self.tobii.log_marker("201") # Send ZOOM_IN marker
						
						# Generate the zoomed view focused_mapSurface and focused_seenSurface
						self.env.patches_entered += 1
						self.zoomed_mapSurface = self.env.gen_zoomed(self.magnifier.position, self.focused_mapSurface)
						self.agent.position = self.env.xymag_adj # Make sure that the polygon is at the exact same position as the magnifier if one zooms in
						self.agent.direction = self.magnifier.direction # Make sure that the polygon is in the exact same direction as the magnifier if one zooms in
						self.agent.olddirection = self.magnifier.olddirection # Make sure that the polygon's olddirection is equal to the olddirection of the magnifier
						self.view = 'focused'
						self.switch_button_pressed = True
						self.patch_enter_timestamps.append(timer() - self.trialStartTime - self.pretrialtime)
					elif event.type == JOYBUTTONDOWN and event.button == 0:
						self.switch_button_pressed = True
				else:
					if event.type == JOYAXISMOTION:
						self.agent.olddirection = self.agent.direction
						self.axis0 = self.agent.joystick.get_axis(0)
						self.axis1 = self.agent.joystick.get_axis(1)
						self.agent.direction = math.degrees(math.atan2(self.axis1, self.axis0)) % 360
					elif event.type == JOYBUTTONDOWN and event.button == 0:
						if TRACKER_ON: self.tobii.log_marker("202") # Send ZOOM_OUT marker
						
						self.agent.food_encounter = False
						self.magnifier.position = self.new_xypos_rescaled # Make sure that the magnifier is at the exact same position as the polygon if one zooms out
						self.magnifier.direction = self.agent.direction # Make sure that the magnifier is in the exact same direction as the polygon if one zooms out
						self.view = 'aerial'
						self.switch_button_pressed = True
						self.patch_leave_timestamps.append(timer() - self.trialStartTime - self.pretrialtime)

	def on_loop_aerial(self, framerate): # While we're in a trial in aerial view				
		if timer() - self.trialStartTime >= self.pretrialtime: # Note that a trial starts after x seconds
			if [pygame.joystick.Joystick(x) for x in range(pygame.joystick.get_count())] == []: # If we're making use of a keyboard
				self.magnifier.speed = PXPERSECOND_AERIAL / (1 / (framerate / 1000.0)) # Note that speed is adjusted to framerate
			else: # If we're making use of a joystick, adjust speed to amount of force exerted by participants
				self.magnifier.speed = ((PXPERSECOND_AERIAL / (1 / (framerate / 1000.0)) # Note that speed is adjusted to framerate
									/ 100) *  max(abs(self.magnifier.joystick.get_axis(0)), abs(self.magnifier.joystick.get_axis(1))) * 100)
		
		# Move the magnifier and the on-screen character
		self.magnifier.move(); self.character.setCharacterImage(self.magnifier.direction, self.magnifier.speed) 

		# Update the counter based on remaining time
		if timer() - self.trialStartTime >= self.pretrialtime: # Note that a trial starts after x seconds
			self.update_counter = int((self.total_counter/(self.trial_time-self.pretrialtime)) * (self.trial_time - (timer()- self.trialStartTime)))
			if self.update_counter < 0:
				self.update_counter = 0

		mappxarray = pygame.PixelArray(self.aerial_mapSurface)
		self.magnifier.patch_encounter = 'false'
		seenpxarray = pygame.PixelArray(self.aerial_seenSurface)

		# Check whether a patch is present within the current radius of the magnifier
		pixelsinradius = self.env.collectPixelsInRadius(RADIUS_MAGNIFIER-0.1, self.magnifier.position[0], self.magnifier.position[1])
		# Should be a bit less though, to remove pixels overlapping the circle's borders
		for eachpixel in pixelsinradius:
			eachpixel = np.clip(eachpixel, 0, DISPSIZE_FORAGING[0]-1)
			if mappxarray[int(round(eachpixel[0])), int(round(eachpixel[1]))] > 0: # If there is a resource in the radius of the magnifier
				seenpxarray[int(round(eachpixel[0])), int(round(eachpixel[1]))] = pygame.Color(255, 0, 0)
				# Adjust the transparency of self.alpha_grass_array (which reflects the self.grass surface) to 0 (transparant)
				# for each of the pixels present in the radius of the magnifier
				self.alpha_grass[int(round(eachpixel[0])), int(round(eachpixel[1]))] = 0
		# If the centre of the magnifier is in the patch, set the patch_encounter parameter to true
		if mappxarray[int(round(self.magnifier.position[0])), int(round(self.magnifier.position[1]))] > 0:
			if TRACKER_ON: self.tobii.log_marker("111") # Send PATCH_ENCOUNTERED marker
			self.magnifier.patch_encounter = 'true'

	def on_loop_focused(self, framerate): # While we're in a trial in focused view				
		if timer() - self.trialStartTime >= self.pretrialtime: # Note that a trial starts after x seconds
			# If we're making use of a keyboard
			if [pygame.joystick.Joystick(x) for x in range(pygame.joystick.get_count())] == []:
				self.agent.speed = PXPERSECOND_FOCUSED / (1 / (framerate / 1000.0)) # Note that speed is adjusted to framerate
			else: # If we're making use of a joystick, adjust speed to amount of force exerted by participants
				self.agent.speed = ((PXPERSECOND_FOCUSED / (1 / (framerate / 1000.0)) # Note that speed is adjusted to framerate
									/ 100) * max(abs(self.agent.joystick.get_axis(0)), abs(self.agent.joystick.get_axis(1))) * 100)
		
		# Move the polygon and present the hand image
		self.agent.move(); self.hand.setHandImage(self.agent.food_encounter) 

		# Get the pixels in the radius of the polygon
		pixelsinradius = self.env.collectPixelsInRadius(RADIUS_MAGNIFIER-0.1, self.agent.position[0], self.agent.position[1])
		xypos_rescaled = [x / float(self.env.rescaling_factor) for x in self.agent.position]
		xypos_rescaled = [np.add(xypos_rescaled, (self.env.patch_central_coords[0]-RADIUS_PATCH, \
					  self.env.patch_central_coords[1]-RADIUS_PATCH))][0]
		self.new_xypos_rescaled = xypos_rescaled

		# Update the counter based on remaining time
		if timer() - self.trialStartTime >= self.pretrialtime: # Note that a trial starts after x seconds
			self.update_counter = int((self.total_counter/(self.trial_time-self.pretrialtime)) * (self.trial_time - (timer()- self.trialStartTime)))
			if self.update_counter < 0: self.update_counter = 0

		self.zoomed_seenSurface = self.env.gen_seen(self._display_surf.screen)
		mappxarray = pygame.PixelArray(self.focused_mapSurface)
		seenpxarray_focused = pygame.PixelArray(self.focused_seenSurface)

		# Check whether a berry is present within the current radius of the polygon
		pixelsinradius = self.env.collectPixelsInRadius(RADIUS_POLYGON-0.1, xypos_rescaled[0], xypos_rescaled[1])
		point_counter = self.agent.points_scored

		# Should be a bit less though, to remove pixels overlapping the circle's borders
		for eachpixel in pixelsinradius:
			eachpixel = np.clip(eachpixel, 0, DISPSIZE_FORAGING[0]-1)
			# Check whether a resource is present within the current radius of the polygon 
			if mappxarray[int(round(eachpixel[0])), int(round(eachpixel[1]))] > 0: # If there is a resource present within the radius
				if seenpxarray_focused[int(round(eachpixel[0])), int(round(eachpixel[1]))] == 0: # If a participant did not encounter it before					
					self.agent.points_scored += self.env.current_patch_info['value']
					self.env.overall_patch_info[self.env.index]['points_scored_patch'] += self.env.current_patch_info['value']
					seenpxarray_focused[int(round(eachpixel[0])), int(round(eachpixel[1]))] = self.env.current_patch_info['color']

					# Add position of resource encounter to a list: Will be used to recreate the zoomed_seenSurface when participants re-enter the patch
					self.env.encountered_all_coords.append((int(round(eachpixel[0])), int(round(eachpixel[1]))))
		
		# If a resource is present in the radius of the polygon, set the self.agent.food_encounter to true
		if self.agent.points_scored > point_counter :
			if TRACKER_ON: self.tobii.log_marker("101") # Send BERRY_ENCOUNTERED marker
			self.agent.food_encounter = True
		else: self.agent.food_encounter = False

	def on_render(self):
		self._display_surf.screen.fill(BGC)
		if self.view == 'aerial':
			# Check which patch is most close to the magnifier's current location
			patch_central_coords, all_central_coords = self.env.eucdistances(self.magnifier.position)
			# Present the self.bush centered at all_central_coords
			for each_patch in all_central_coords:
				surfacePos = pygame.transform.scale(self.bush, ((RADIUS_PATCH+1)*3, (RADIUS_PATCH+1)*3)).get_rect(center = (each_patch[0], each_patch[1]))
				self._display_surf.screen.blit(pygame.transform.scale(self.bush, ((RADIUS_PATCH+1)*3, (RADIUS_PATCH+1)*3)), surfacePos) # Present bushes	
			self._display_surf.screen.blit(pygame.transform.scale(self.grass, (600, 600)), (0,0)) # Present grass background			
		else: # self.view == 'focused':
			self._display_surf.screen.blit(self.grass_focused, (0, 0)); self._display_surf.screen.blit(self.bush_focused, (0, 0)) # Present grass and bush
			self.berry = pygame.image.load(os.path.join("images", "layout") + 
						'/berry_' + str(self.env.current_patch_info['color']) + '_' + str(self.env.current_patch_info['value']) + '.png')
			for x in self.env.seenSurface_all_coords : # Present found berries
				surfacePos = self.berry.get_rect(center = (x[0]*self.env.rescaling_factor , x[1]*self.env.rescaling_factor))
				self._display_surf.screen.blit(self.berry, surfacePos) 
		self.draw_info_overlay(); self.disp.show()

	# Draws character, hand and trial information (e.g., clock, score)
	def draw_info_overlay(self):
		if self.view == 'aerial':
			# Set and draw magnifier
			self.magnifier.setMagnifier() # Required in order to present the character at the right location
			self._display_surf.screen.blit(self.character.characterImage, self.character.characterImage.get_rect(center = self.magnifier.loc))
		else: # self.view == 'focused'
			self.agent.setPolygon() # Set the polygon to it's new location, based on previous position, speed and direction
			self._display_surf.screen.blit(self.hand.handImage, self.hand.handImage.get_rect(center = self.agent.loc))
			
		# When pretrial time has not ran out, present 'Get ready!' to participants
		if timer() - self.trialStartTime < self.pretrialtime:
			text = self.font.render("Ready?", 1, FONTCOLOR)
			textpos = text.get_rect(center=(DISPSIZE_FORAGING[0]/2, (DISPSIZE_FORAGING[0]/2)-50))
			self._display_surf.screen.blit(text, textpos)

		# Draw trial information		
		text = self.font.render("Score: " + str(self.agent.points_scored), 1, FONTCOLOR)
		counter = self.font.render(str(self.update_counter), 1, FONTCOLOR)
		textpos = text.get_rect(topleft=(10, 10)); counterpos = counter.get_rect(topright=(535, 10))
		self._display_surf.screen.blit(text, textpos); self._display_surf.screen.blit(counter, counterpos)	
		pygame.draw.circle(self._display_surf.screen, FONTCOLOR, (self._display_surf.screen.get_width() - 30, 30), 15)

		# If pretrial time has passed
		if timer() - self.trialStartTime >= self.pretrialtime:
			xpos, ypos = self.clockHand(15, (timer()*180) % 360, self._display_surf.screen.get_width() - 30, 30)
			pygame.draw.line(self._display_surf.screen, BGC,(self._display_surf.screen.get_width() - 30, 30),(xpos, ypos), 2)

	def on_cleanup(self):
		print("""# ---------------EXPERIMENT COMPLETED--------------- #""")
		if self.block == 'baseline' :
			self.instructions.instructionpage(pygame.image.load(os.path.join("images", "general") + "/continue.png"))
		else: 
			self.instructions.instructionpage(pygame.image.load(os.path.join("images", "general") + "/experiment_end.png"))
		pygame.quit()

if __name__ == "__main__":
	expStartTime = sys.argv[1]
	subject_ID = sys.argv[2]
	block = sys.argv[3]
	condition = sys.argv[4]
	
	# Seed the random number generator. Allows for a full replication of the study design
	if block == 'baseline' : 
		random.seed(int(subject_ID))
	else: random.seed(int(subject_ID) + 1)

	theApp = App(expStartTime, subject_ID, block, condition)
	theApp.on_execute()