#! /usr/bin/env python
import glob, os

def init_datafile(expStartTime, subjectID, block, filename=None):
	if filename is None:
		filename = os.path.join("../data-foraging-both/patch-coordinates/") + str(subjectID) + "_" + str(expStartTime) + "_processed_block_" + str(block) + ".txt"

def write_data(data, filename=None):
	if filename is None:
		filename = os.path.join("../data-foraging-both/patch-coordinates/") + str(subjectID) + "_" + str(expStartTime) + "_processed_block_" + str(block) + ".txt"
	f = open(filename, 'a')
	output = str(data)
	f.write(output)
	f.close()

# Read datafiles.
all_datafiles = glob.glob('../*/patch-coordinates/*_visual_foraging_block_*.txt')

# Loop through all files.
for eachfile in all_datafiles:
	subjectID = os.path.splitext(os.path.basename(eachfile))[0][10:14]
	expStartTime = os.path.splitext(os.path.basename(eachfile))[0][15:29]
	block = ("baseline" if "baseline" in eachfile else "main")

	# Initialise datafile
	init_datafile(expStartTime, subjectID, block) 
	
	with open(eachfile, 'r') as file:
		for line in file:
			output = ''
			stack = []
			for char in line:
				if char == '(':
					stack.append(char)
				elif char == ')':
					stack.pop()    
				# Check if we are inside a curly bracket
				if len(stack)>0 and char==',':
					output += ';'
				else:
					output += char
			write_data(output)