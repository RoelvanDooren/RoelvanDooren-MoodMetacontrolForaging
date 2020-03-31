#! /usr/bin/env python

##=============================================================================
## Moodmecodynamics: Roel van Dooren, Roy de Kleijn, Bernhard Hommel, & Zsuzsika Sjoerds
## 
## Created by:
## Roel van Dooren
## r.van.dooren@fsw.leidenuniv.nl
##
## Last modified: 2019/02/27 08:19
## Copyright (c) 2017 Roel van Dooren. All rights reserved.
##=============================================================================

import pygame, random, sys, os
from math import sin, cos, radians
import numpy as np
from pygame.locals import *
from timeit import default_timer as timer
import time
import os
import math
from constants import *
import affect_grid

def rotatePolygon(polygon,theta):
    """Rotates the given polygon which consists of corners represented as (x,y),
    around the ORIGIN, clock-wise, theta degrees"""
    theta = radians(theta)
    rotatedPolygon = []
    for corner in polygon:
        rotatedPolygon.append(( corner[0]*cos(theta)-corner[1]*sin(theta) , corner[0]*sin(theta)+corner[1]*cos(theta)) )
    return rotatedPolygon

def movePolygon(polygon,x,y):
    """Moves the given polygon which consists of corners represented as (x,y)"""
    movedPolygon = []
    for corner in polygon:
        movedPolygon.append(( corner[0]+x , corner[1]+y))
    return movedPolygon

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

class Agent(object):
    def __init__(self):
        self.position = (7.5, 50.0)
        self.speed = 0; self.direction = 0
        self.total_turned = 0; self.total_collisions = 0
        self.last_angle = self.direction
        self.collision_encounter = "False"

    def joystick(self):
		"""Checks whether a joystick is connected and, if so, sets it accordingly"""		
		if [pygame.joystick.Joystick(x) for x in range(pygame.joystick.get_count())] == []:
			print "No joystick detected."
		else:
			self.joystick = pygame.joystick.Joystick(0)			
			self.joystick.init()
	
    def move(self):
        self.position = (self.position[0] + self.speed * cos(radians(self.direction)),
                         self.position[1] + self.speed * sin(radians(self.direction)))
        self.position = np.clip(self.position, 0, 199)

class Wait(object):
    def __init__(self, surface):
        self.surface = surface
        self.x = surface.get_width() / 2; self.y = surface.get_height() / 3
        self.font = pygame.font.Font(None, 25)

    def intro(self, image):
        self.surface.fill(BGC)
        self.surface.blit(image, image.get_rect(center=(self.surface.get_width() / 2, self.surface.get_height() / 2)))
        pygame.display.flip()
        while True:
            event = pygame.event.poll()
            if event.type == pygame.KEYDOWN:
                if event.key == pygame.K_SPACE:
					self.surface.fill(BGC); pygame.display.flip(); pygame.time.delay(500)					
					return

class Environment(object):
    def __init__(self):
        self.map = np.zeros((200, 200))
	
    def gen_practice(self):
		maze = pygame.image.load(os.path.join("images", "practice") + "/maze_foraging_practice.bmp")
		mapSurface = pygame.transform.scale(maze, (200, 200))
		return mapSurface

class ForagingPractice(object):
    def __init__(self, expStartTime, subject, ghost):
        pygame.init()
        self._running = True
        self.ghost = ghost
        self.expStartTime = expStartTime
        self.subjectID = subject
        self.pretrialtime = PRETRIALTIME # Note that this should be specified as a float!
        self.trialStartTime = 0
        self.trialNum = "practice"
        self.path_array = []
        self.turning = 'straight'
        self.axis0 = 'NA'; self.axis1 = 'NA'

    def draw_info_overlay(self):
        loc = (int(round(self.agent.position[0]*3)), int(round(self.agent.position[1]*3)))
        self._display_surf.blit(self.character.characterImage, self.character.characterImage.get_rect(center = loc))
	
        if timer() - self.trialStartTime < self.pretrialtime:
			font = pygame.font.Font(None, 40)
			text = font.render("Ready?", 1, FONTCOLOR)
			textpos = text.get_rect(center=(self._display_surf.get_width() / 2, self._display_surf.get_height() / 2.5))
			self._display_surf.blit(text, textpos)
		
    def zerofill(self, number, width):	
		width -= len(str(number))
		if (width > 0) :
			i = 0
			while i < width:
				number =   str(number).join("0")+ str(number)
				i += 1
		return number
			
    def init_datafile(self):
        f = open(os.path.join(DATA_DIRECTORY, STUDYCODE) + str(self.subjectID) + "_" + str(self.expStartTime) + "_visual_foraging_practice.txt", 'w')
        output = 'expStartTime,subjectID,trial_num,totaltrialtime,total_collisions,total_turned\n'
        f.write(output)
        f.close()

    def init_datafile_path(self):
		filename = os.path.join(DATA_DIRECTORY, STUDYCODE) + str(self.subjectID) + "_" + str(self.expStartTime) + "_visual_foraging_path_practice_trial.txt"

    def write_data(self):
        f = open(os.path.join(DATA_DIRECTORY, STUDYCODE) + str(self.subjectID) + "_" + str(self.expStartTime) + "_visual_foraging_practice.txt", 'a')
        output = str(self.expStartTime) + "," + str(self.subjectID) + "," + \
				 str(self.trialNum) + "," + str(timer() - self.trialStartTime-PRETRIALTIME) + "," + \
                 str(self.agent.total_collisions) + "," + str(self.agent.total_turned) + "\n"
        f.write(output)
        f.close()

    def write_data_path(self, output):
        f = open(os.path.join(DATA_DIRECTORY, STUDYCODE) + str(self.subjectID) + "_" + str(self.expStartTime) + "_visual_foraging_path_practice_trial.txt", 'a')
        f.write("%s" % output)
        f.close()

    def on_init(self):
        pygame.init()
        self.subjectID = self.zerofill(self.subjectID, 4)
        self.init_datafile()
        self.clock = pygame.time.Clock()
        if DEBUG: self._display_surf = pygame.display.set_mode(DISPSIZE, pygame.HWSURFACE | pygame.DOUBLEBUF)
        else: self._display_surf = pygame.display.set_mode(DISPSIZE, pygame.HWSURFACE | pygame.DOUBLEBUF | pygame.FULLSCREEN)
        
        # Initialise higher-order objects
        self.wait = Wait(self._display_surf)
        self.character = Character() # Initialize character
        self.env = Environment()
        
    def on_event(self, event): # Processing button presses in the visual foraging task
        if timer() - self.trialStartTime >= self.pretrialtime:
			# Make sure that polygon can only be moved after pretrialtime has passed
			# Only enable buttonpresses if we're making use of a keyboard
			if [pygame.joystick.Joystick(x) for x in range(pygame.joystick.get_count())] == []: 
				if event.type == KEYDOWN and event.key == K_j: 
					self.agent.direction = (self.agent.direction - 35) % 360
					self.agent.total_turned += 35
					self.turning = 'counterclockwise'
				elif event.type == KEYDOWN and event.key == K_l:
					self.agent.direction = (self.agent.direction + 35) % 360
					self.agent.total_turned += 35
					self.turning = 'clockwise'
				else:
					self.turning = 'straight'
			# Disable buttonpresses if we're making use of a joystick
			else:
				if event.type == JOYAXISMOTION:
					self.agent.olddirection = self.agent.direction
					self.axis0 = self.agent.joystick.get_axis(0)
					self.axis1 = self.agent.joystick.get_axis(1)					
					self.agent.direction = math.degrees(math.atan2(self.axis1, self.axis0)) % 360
					# Check whether participants' turn is clockwise or counterclockwise
					if self.agent.direction - self.agent.olddirection > 0.0:
						self.turning = 'clockwise'
					elif self.agent.direction - self.agent.olddirection < 0.0:
						self.turning = 'counterclockwise'
					else:
						self.turning = 'straight'					
					self.agent.total_turned += abs(self.agent.direction - self.agent.olddirection)
				else:
					self.turning = 'straight'
					
    def on_loop(self, framerate):
        pygame.mouse.set_visible(False)
        if timer() - self.trialStartTime >= self.pretrialtime: # Note that a trial starts after x seconds
            # If we're making use of a keyboard
            if [pygame.joystick.Joystick(x) for x in range(pygame.joystick.get_count())] == []: 
				self.agent.speed = PXPERSECOND_PRACTICE / (1 / (framerate / 1000.0)) # Note that speed is adjusted to framerate
            else: # If we're making use of a joystick
				self.agent.speed = ((PXPERSECOND_PRACTICE / (1 / (framerate / 1000.0)) # Note that speed is adjusted to framerate
									/ 100) * max(abs(self.agent.joystick.get_axis(0)), abs(self.agent.joystick.get_axis(1))) * 100)
		
		# Move the polygon and the on-screen character
        self.agent.move(); self.character.setCharacterImage(self.agent.direction, self.agent.speed) 
               
        position = (int(round(self.agent.position[0])),int(round(self.agent.position[1])))
        mappxarray = pygame.PixelArray(self.mapSurface)
        seenpxarray = pygame.PixelArray(self.seenSurface)
       
        if mappxarray[position[0],position[1]] !=0 and mappxarray[position[0],position[1]] == 16777215: # Detects the white borders (== 16777215) of the maze
            if seenpxarray[position[0],position[1]] == 0: # If a collision occurs
				self.agent.total_collisions += 1
				seenpxarray = pygame.PixelArray(self.seenSurface) # Reset pixel array
				if self.ghost == "false":
					self.agent.position = (7.5, 50.0) # Reset position of polygon
					self.agent.collision_encounter = "True"
        else:
			self.agent.collision_encounter = "False"	
        # Color schemes could be different between platforms.        
        if mappxarray[position[0],position[1]] == 2273612: # Detects the maze's exit ( == 2273612)
			self._running = False

    def on_render(self):
		self._display_surf.fill(BGC)
		self._display_surf.blit(pygame.transform.scale(self.seenSurface, (600, 600)), (0,0))
		self._display_surf.blit(pygame.transform.scale(self.mapSurface, (600, 600)), (0,0)) # Present maze
		self.draw_info_overlay()
		pygame.display.flip()

    def on_cleanup(self):
        self.wait.intro(pygame.image.load(os.path.join("images", "practice") + "/end_practice.png"))
        pygame.quit()

    def run_practice(self):
		self._running = True
		self.init_datafile_path() 
		self.mapSurface = self.env.gen_practice()
		self.seenSurface = pygame.Surface((200, 200), flags=0)
		self.seenSurface.fill(BGC)
		self.agent = Agent()
		self.agent.joystick() # Set joystick as input if available
		
		# Set the monitor size to DISPSIZE_FORAGING		
		if DEBUG: self._display_surf = pygame.display.set_mode(DISPSIZE_PRACTICE, pygame.HWSURFACE | pygame.DOUBLEBUF)
		else: self._display_surf = pygame.display.set_mode(DISPSIZE_PRACTICE, pygame.HWSURFACE | pygame.DOUBLEBUF | pygame.FULLSCREEN)

		self.trialStartTime = timer()
		self.clock.tick(FRAMERATE) # Update the clock tick timer
		while (self._running):
			framerate = self.clock.tick(FRAMERATE) # Will be used to set self.speed to X px per second
			for event in pygame.event.get():
				self.on_event(event)
			self.on_loop(framerate)
			self.on_render()
			
			if timer()-self.trialStartTime >= PRETRIALTIME:
				self.path_array.append([self.expStartTime, self.subjectID, self.trialNum, timer() - self.trialStartTime, 
								  self.agent.position[0], self.agent.position[1],  self.axis0, self.axis1, self.turning,
								  self.agent.collision_encounter, self.agent.total_collisions])
				
		self.write_data()
		self.write_data_path(self.path_array)		

    def on_execute(self):
        if self.on_init() == False:
            self._running = False
        pygame.mouse.set_visible(False)
                
        # Present welcome screen and pre-instructions
        self.wait.intro(pygame.image.load(os.path.join("images", "general") + "/welcome.png"))
        self.wait.intro(pygame.image.load(os.path.join("images", "practice") + "/practice_instructions.png"))
        self.wait.intro(pygame.image.load(os.path.join("images", "practice") + "/practice_instructions_continued.png"))
    
        self.run_practice()
        
		# Set the monitor size to DISPSIZE_FORAGING		
        if DEBUG: self._display_surf = pygame.display.set_mode(DISPSIZE, pygame.HWSURFACE | pygame.DOUBLEBUF)
        else: self._display_surf = pygame.display.set_mode(DISPSIZE, pygame.HWSURFACE | pygame.DOUBLEBUF | pygame.FULLSCREEN)        

        self.on_cleanup()

if __name__ == "__main__":    
    expStartTime = sys.argv[1]
    subject_ID = sys.argv[2]
    
    if DEBUG: ghost = "true"
    else: ghost = "false"
    
    practice_trial = ForagingPractice(expStartTime, subject_ID, ghost)
    practice_trial.on_execute()