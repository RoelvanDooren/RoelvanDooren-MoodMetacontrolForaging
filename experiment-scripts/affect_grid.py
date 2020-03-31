#! /usr/bin/env python

##=============================================================================
## Affect Grid
## 
## Created by:
## Roel van Dooren
## r.van.dooren@fsw.leidenuniv.nl
##
## Last modified: 2017/09/27
## Copyright (c) 2017 Roel van Dooren. All rights reserved.
##=============================================================================
## User instructions.
##
## The affect grid can be integrated in your own experiments by executing:
## AffectGrid = affect_grid.App(expStartTime, subject_ID, experimentName, self.surface, blocknum)
## AffectGrid.on_execute()
##
## Note: If self.surface is not specified, python will initiate a new (parallel) surface.
##=============================================================================


import pygame, random, sys, os
from math import sin, cos, radians
import numpy as np
from pygame.locals import *
from timeit import default_timer as timer
import time, os
from constants import *

class Stimulus(object):
    def __init__(self, surface):
        self.surface = surface
        self.x = surface.get_width() / 2
        self.y = surface.get_height() / 2
        
    def gen_practice(self):
		mapSurface = pygame.image.load(os.path.join("images", "affect_grid") + "/AffectGrid_EN.bmp").convert()
		position_surface = mapSurface.get_rect(center = (self.x, self.y))
		return mapSurface, position_surface

    def draw_continue(self):
		continue_button = pygame.image.load(os.path.join("images", "affect_grid") + "/button.png").convert_alpha()
		self.position_button = Rect(self.x / 6 + self.x, self.y / 10, 100, 48)
		self.surface.blit(continue_button, self.position_button)
		
class App(object):
    def __init__(self, expStartTime, subject, experiment = None, externalscreen = None, blocknr = None, condition = None):
        self._running = True
        self.experiment = (experiment if experiment else '')
        self.nestedexperiment = (True if externalscreen else 'NA')
        self.blocknr = (blocknr if blocknr else '')
        self.condition = (condition if condition else '')
        self.allcells = 'NA'
        self.size = self.width, self.height = 600, 600
        self.expStartTime = expStartTime
        self.subjectID = subject
        self.trialStartTime = 0

    def zerofill(self, number, width):	
		width -= len(str(number))
		if (width > 0) :
			i = 0
			while i < width:
				number =   str(number).join("0")+ str(number)
				i += 1
		return number
			
    def init_datafile(self, filename=None):
        if filename is None:
            filename = os.path.join(DATA_DIRECTORY, STUDYCODE) + str(self.subjectID) + "_" + str(self.expStartTime) + "_affect_grid.txt"
        
        all_datafiles = [file for file in os.listdir("./output") if file.endswith(".txt")]
        select_datafiles = [file for file in all_datafiles if "_affect_grid.txt" in file]
        if STUDYCODE + str(self.subjectID) + "_" + str(self.expStartTime) + "_affect_grid.txt" not in select_datafiles:
			f = open(filename, 'w')
			output = 'expStartTime,subjectID,condition,block,starttime,endtime,responsetime,pleasurerating,arousalrating\n'
			f.write(output)
			f.close()

    def write_data(self, filename=None):
        if filename is None:
            filename = os.path.join(DATA_DIRECTORY, STUDYCODE) + str(self.subjectID) + "_" + str(self.expStartTime) + "_affect_grid.txt"
        f = open(filename, 'a')
        output = str(self.expStartTime) + "," + str(self.subjectID) + "," + str(self.condition) + "," + \
				 str(self.blocknr) + "," + str(self.gridStartTime) + "," + str(self.gridEndTime) + "," + \
				 str(self.gridEndTime - self.gridStartTime) + "," + str(self.pleasure) + "," + str(self.arousal) + "\n"
        f.write(output)
        f.close()

    def on_init(self):
        pygame.init()
        self.subjectID = self.zerofill(self.subjectID, 4)
        self.init_datafile()
        self.clock = pygame.time.Clock()
        self._display_surf = pygame.display.set_mode((600, 600), pygame.HWSURFACE | pygame.DOUBLEBUF | pygame.FULLSCREEN)
        self.env = Stimulus(self._display_surf)
        self.mapSurface = self.env.gen_practice()[0]
        self.position_surface = self.env.gen_practice()[1]
        self.font = pygame.font.Font(None, 75)
        self.x = self._display_surf.get_width() / 2
        self.y = self._display_surf.get_height() / 2
        self.grid_coords()

    def grid_coords(self):
		self.topleft_grid = (self.position_surface.topleft[0]+83, self.position_surface.topleft[1]+33)
		self.bottomleft_grid = (self.position_surface.topleft[0]+83, self.position_surface.topleft[1]+377)
		self.topright_grid = (self.position_surface.topleft[0]+473, self.position_surface.topleft[1]+33)
		self.bottomright_grid = (self.position_surface.topleft[0]+473, self.position_surface.topleft[1]+377)       
		self.eachcell = ((self.topright_grid[0] - self.topleft_grid[0]) / 9.0,  (self.bottomright_grid[1] - self.topright_grid[1]) / 9.0)
        
		self.allcells = []
		self.values_i = [v for v in range(-4, 5, 1)]
		self.values_x = [w for w in range(4, -5, -1)]
		for i in range(9):
			for x in range(9):
				cell = { 'pleasure' : self.values_i[i],  
						 'arousal' : self.values_x[x],
						 'cell_coords' : pygame.Rect(int(self.bottomleft_grid[0] + (self.eachcell[0] * i)),     				# left
													 int(self.bottomleft_grid[0] + ((self.eachcell[1] * x)+self.eachcell[1])),  # top
													 int(self.eachcell[0]), int(self.eachcell[1]))} 							# width and heigth
				self.allcells.append(cell)

    def on_loop(self):
		pygame.mouse.set_visible(True)
		self._running = True
		self.pleasure = 'NA'; self.arousal = 'NA'
		self.gridStartTime = timer()
		while self._running:        
			for event in pygame.event.get():      
				if event.type == pygame.MOUSEBUTTONDOWN:
					mousepos = pygame.mouse.get_pos()
					for eachcell in self.allcells:
						button = eachcell['cell_coords']
						if button.collidepoint(mousepos):
							self._display_surf.blit(self.mapSurface, self.position_surface)
							self.env.draw_continue()
							string = self.font.render('x', 1, (0,0,0))
							
							self._display_surf.blit(string, pygame.Rect(button[0] + (button[2]/2) - 13, button[1] + (button[3]/2) - 31, 0, 0))
							self.pleasure = eachcell['pleasure']; self.arousal = eachcell['arousal']
							pygame.display.flip()
					
				pygame.event.clear()
				if event.type == pygame.MOUSEBUTTONDOWN and self.pleasure != 'NA' and self.arousal != 'NA':
					mousepos = pygame.mouse.get_pos()
					if self.env.position_button.collidepoint(mousepos):
						self.gridEndTime = timer()
						self.write_data()
						self._running = False 
						
    def on_render(self):
		self._display_surf.fill((255, 255, 255))
		self._display_surf.blit(self.mapSurface, self.position_surface)
		self.env.draw_continue()
		pygame.display.flip()

    def on_cleanup(self):					
        pygame.quit()

    def run_practice(self):		
		self.trialStartTime = timer()
		self.clock.tick(60)
		self.on_render()
		self.on_loop()	

    def on_execute(self):
        if self.on_init() == False:
            self._running = False
        self.subjectID = self.zerofill(self.subjectID, 4)
        self.run_practice()  
        			
        self._display_surf.fill(BGC)
        pygame.display.flip()
        pygame.time.delay(500)      
			
        if self.nestedexperiment == False: 
			self.on_cleanup()

if __name__ == "__main__":
    expStartTime = sys.argv[1]
    subject_ID = sys.argv[2]
    blocknr = sys.argv[3]
    condition = sys.argv[4]
    
    theApp = App(expStartTime, subject_ID, experiment = None, externalscreen = None, blocknr = blocknr, condition = condition)
    theApp.on_execute()
