# -*- coding: utf-8 -*-
"""
Created on Sun Jun 11 23:00:56 2017

@author: Martin Vasilev

Eye-tracking experiment following Eyetrack's convention

"""


# import settings and libraries:
from constants import * # all experiment settings
#import pylink
#from psychopy import prefs
#prefs.general['audioLib']= ['pygame']
from psychopy.visual import *
#import random
from psychopy import core
from psychopy.core import *
from psychopy.event import waitKeys, Mouse, getKeys
#from psychopy.sound import Sound
from pygaze.eyetracker import EyeTracker
from pygaze.display import Display
from pygaze.screen import Screen
#import pygaze.libtime as timer

#------------------------
# Experiment set-up part:
#------------------------

# Ask for session info: 
#expSetup = {'Participant': '',
#           'Condition (Randomize)': ''}
#dlg = gui.DlgFromDict(dictionary=expSetup, title= 'Run experiment: '+ expName)

#if int(expSetup[u'Participant'])<10:
#	LOGFILENAME = expName+ str(0)+ expSetup['Participant']+'.EDF'
#else:
#LOGFILENAME= expName+ expSetup['Participant']#+'.EDF'
#print(LOGFILENAME)
# open main screen:
win= Window(size=DISPSIZE, color= BGC)
fr= win.getActualFrameRate()# get frame rate for edf file
win.close()
#win=  Window(size=DISPSIZE, units= 'pix', fullscr= useFullscreen, color= BGC)
disp= Display()

scr= Screen(dispsize=DISPSIZE)

myMouse = Mouse(disp) # create a mouse object
#myMouse.setPos((2000,2000))
myMouse.setVisible(0) # make the mouse invisible

globalClock= core.Clock()
#--------------
# Instructions:
#--------------

#FileID= open(expDir+'\instructions.txt', 'r')
#FileID= open('instructions.txt', 'r')
#Instring= FileID.read()
#Instring= Instring + '\n\nPress any key to continue'

#scr.draw_text(text= Instring, colour= FGC, font= Font, center=True, fontsize=TextSize)

#disp.fill(scr)
#disp.show()

# wait for a response:
#resplist = waitKeys(maxWait=float('inf'))
#scr.clear()
#disp.fill(scr)
#disp.show()

# Initialize Eye-tracker:
tracker= EyeTracker(disp, trackertype= 'eyelink',resolution= DISPSIZE, fgc= FGC, bgc= BGC,
		         saccvelthresh= saccvelthresh, saccaccthresh= saccaccthresh, eventdetection= "native",  logfilename= LOGFILENAME)

# Write some info into edf file:
tracker.log('DISPLAY COORDS %d %d %d %d' % (0, 0, DISPSIZE[0]-1, DISPSIZE[1]-1))
tracker.log('FRAMERATE %d' % (fr))
tracker.send_command("calibration_type= %s" % caltype) 											
tracker.calibrate()
	
#-------------------------
#     Story 1       :
#-------------------------

if int(expSetup["Condition (Randomize)"])==1:
	cond= 1
	Story1=''
	Story2='B'
	Story1cond=1
	Story2cond=2

else:
	cond= 2
	Story1='B'
	Story2=''
	Story1cond=2
	Story2cond=1

item= 1
hasText= [2,3,5,7,9,10,11,13,15,16,17,18,20,21,22,23,25]



#for i in range(0,5):
#	Quest(disp, scr, tracker, DorothyQ[i], i+1, DorothyAns[i])


Story1End=False
while not Story1End: # for each of the trials
	trialEnd= False			
	stimuliOn= False
	
	while not stimuliOn: # repeats loop until gazebox is triggered within x seconds
		
		if item in hasText:
			#print trial ID in EDF file:
			tracker.log('TRIALID E%dI%dD0' % (Story1cond, item))
			# print trial ID on tracker screen:
			tracker.status_msg('TRIAL E%dI%dD0' % (Story1cond, item)) 
		else:
			#print trial ID in EDF file:
			tracker.log('TRIALID P%dI%dD0' % (Story1cond, item))
			# print trial ID on tracker screen:
			tracker.status_msg('TRIAL P%dI%dD0' % (Story1cond, item)) 			

		# print text stimuli to edf:
		if item in hasText:
			stim2edf(tracker, 'DorothyText/Dorothy' + str(item)+'.txt', offsetX, Pix_per_Letter, yStart)
		
		# drift check:
		tracker.drift_correction()	
		
		### Prepare trial stimuli:
			
		#scr.draw_text(text= sentenceString, colour= FGC, font= Font, 
		#              pos= sentPos, fontsize=TextSize, center=False)
		scr.draw_image('Dorothy'+ Story1+ '/Dorothy'+Story1+ str(item)+ '.bmp')
  
		# prepare gaze screen:
		Gazescr= Screen(dispsize=DISPSIZE)
		Gazescr.draw_rect(colour=FGC, x=offsetX, y=GazeBoxY, w=GazeBoxSize,
					h=GazeBoxSize, pw=1, fill=True)
		gazeBnds_x= (offsetX, offsetX+GazeBoxSize)
		gazeBnds_y= (DISPSIZE[1]/2-GazeBoxSize/2, DISPSIZE[1]/2-GazeBoxSize/2+GazeBoxSize)
		# display gaze box:
		disp.fill(Gazescr)
		
		# draw gaze box on tracker monitor:
		tracker.send_command("draw_fiiled_box %d %d %d %d %d" % (offsetX,
					DISPSIZE[1]/2-GazeBoxSize/2, offsetX+GazeBoxSize, DISPSIZE[1]/2-GazeBoxSize/2+GazeBoxSize, 15))
		
		tracker.start_recording()
		
		gazeBoxTriggered=False
		onTarget= False
		gazeTimeOut= False
		gazeStart= globalClock.getTime()
		disp.show()
		tracker.log('GAZE TARGET ON')
		
		
		# loop that triggers the gaze-box
		while not gazeBoxTriggered and not onTarget:
			sample= tracker.sample() # get current eye position
			elapsedTime= globalClock.getTime()-gazeStart # time since gaze box appeared
			onTarget= sample[0]>= gazeBnds_x[0] and sample[0]<= gazeBnds_x[1] and sample[1]>= gazeBnds_y[0] and sample[1]<= gazeBnds_y[1]
			if onTarget: # the eye is on the gaze box
				wait(gazeBoxDur/1000)
				onTarget= sample[0]>= gazeBnds_x[0] and sample[0]<= gazeBnds_x[1] and sample[1]>= gazeBnds_y[0] and sample[1]<= gazeBnds_y[1]
				if onTarget: # eye still on gaze box after x ms
					gazeBoxTriggered= True
					stimuliOn= True
					#tracker.send_command("clear_screen %d" % (0))
				else:
					onTarget=False
			
			if elapsedTime> gazeBoxDisplayTime: # gaze box timeout
				tracker.log('TRIAL ABORTED')
				tracker.stop_recording()
				tracker.calibrate()
				onTarget=True
				gazeBoxTriggered=True
		
	tracker.log('GAZE TARGET OFF')
	tracker.log('DISPLAY ON')
	tracker.log('SYNCTIME')
														
	disp.fill(scr)
	disp.show()
	trialStart= globalClock.getTime()
	

	while not trialEnd:
		trialTime= globalClock.getTime()- trialStart
		pressed= getKeys()
		#pressed, time= pylink.getEYELINK().getLastButtonPress()
		if 'right' in pressed: # terminate trial when mouse is clicked (temporary)
		#if '2' == str(pressed):
			goNext= True
			trialEnd= True
		if 'left' in pressed and item>1: # terminate trial when mouse is clicked (temporary)
		#if '4' == str(pressed) and item>1:
			goNext= False
			trialEnd= True
		

		if trialTime> TrialTimeout: # end trial automatically if no response by participant
			trialEnd= True
			scr.clear(colour=BGC)
		
	scr.clear(colour=BGC) # clear subject screen
	disp.show()
	tracker.send_command("clear_screen %d" % (0)) # clear tracker screen	
	
	# end of trial messages:
	tracker.log('ENDBUTTON 5')
	tracker.log('DISPLAY OFF')
	tracker.log('TRIAL_RESULT 5')
	tracker.log('TRIAL OK')
	tracker.stop_recording()
	
	if goNext:
		item= item+1
	if not goNext and item>1:
		item= item-1

	Story1End= item==26
	#DorothyEnd= item==3


################
#  Questions:  #
################

for i in range(0,5):
	Quest(disp, scr, tracker, DorothyQ[i], i+1, DorothyAns[i])


########################
#     Second story     #
########################

item= 26
hasText= [2,4,5,6,8,10,11,12,13,15,16,17,18,20,22,23,24,26]

Story2End=False
while not Story2End: # for each of the trials
	trialEnd= False			
	stimuliOn= False
	
	while not stimuliOn: # repeats loop until gazebox is triggered within x seconds
		
		if item-25 in hasText:
			#print trial ID in EDF file:
			tracker.log('TRIALID E%dI%dD0' % (Story2cond, item))
			# print trial ID on tracker screen:
			tracker.status_msg('TRIAL E%dI%dD0' % (Story2cond, item)) 
		else:
			#print trial ID in EDF file:
			tracker.log('TRIALID P%dI%dD0' % (Story2cond, item))
			# print trial ID on tracker screen:
			tracker.status_msg('TRIAL P%dI%dD0' % (Story2cond, item)) 			

		# print text stimuli to edf:
		if item-25 in hasText:
			stim2edf(tracker, 'TiktokText/Tiktok' + str(item-25)+'.txt', offsetX, Pix_per_Letter, yStart)
		#stim2edf(sentenceString, sentPos, tracker)
		
		# drift check:
		tracker.drift_correction()	
		
		### Prepare trial stimuli:
			
		#scr.draw_text(text= sentenceString, colour= FGC, font= Font, 
		#              pos= sentPos, fontsize=TextSize, center=False)
		scr.draw_image('Tiktok'+ Story2+ '/Tiktok'+Story2+ str(item-25)+ '.bmp')
  
		# prepare gaze screen:
		Gazescr= Screen(dispsize=DISPSIZE)
		Gazescr.draw_rect(colour=FGC, x=offsetX, y=GazeBoxY, w=GazeBoxSize,
					h=GazeBoxSize, pw=1, fill=True)
		gazeBnds_x= (offsetX, offsetX+GazeBoxSize)
		gazeBnds_y= (DISPSIZE[1]/2-GazeBoxSize/2, DISPSIZE[1]/2-GazeBoxSize/2+GazeBoxSize)
		# display gaze box:
		disp.fill(Gazescr)
		
		# draw gaze box on tracker monitor:
		tracker.send_command("draw_box %d %d %d %d %d" % (offsetX,
					DISPSIZE[1]/2-GazeBoxSize/2, offsetX+GazeBoxSize, DISPSIZE[1]/2-GazeBoxSize/2+GazeBoxSize, 3))
		
		tracker.start_recording()
		
		gazeBoxTriggered=False
		onTarget= False
		gazeTimeOut= False
		gazeStart= globalClock.getTime()
		disp.show()
		tracker.log('GAZE TARGET ON')
		
		
		# loop that triggers the gaze-box
		while not gazeBoxTriggered and not onTarget:
			sample= tracker.sample() # get current eye position
			elapsedTime= globalClock.getTime()-gazeStart # time since gaze box appeared
			onTarget= sample[0]>= gazeBnds_x[0] and sample[0]<= gazeBnds_x[1] and sample[1]>= gazeBnds_y[0] and sample[1]<= gazeBnds_y[1]
			if onTarget: # the eye is on the gaze box
				wait(gazeBoxDur/1000)
				onTarget= sample[0]>= gazeBnds_x[0] and sample[0]<= gazeBnds_x[1] and sample[1]>= gazeBnds_y[0] and sample[1]<= gazeBnds_y[1]
				if onTarget: # eye still on gaze box after x ms
					gazeBoxTriggered= True
					stimuliOn= True
					#tracker.send_command("clear_screen %d" % (0))
				else:
					onTarget=False
			
			if elapsedTime> gazeBoxDisplayTime: # gaze box timeout
				tracker.log('TRIAL ABORTED')
				tracker.stop_recording()
				tracker.calibrate()
				onTarget=True
				gazeBoxTriggered=True
		
	tracker.log('GAZE TARGET OFF')
	tracker.log('DISPLAY ON')
	tracker.log('SYNCTIME')
														
	disp.fill(scr)
	disp.show()
	trialStart= globalClock.getTime()
	

	while not trialEnd:
		trialTime= globalClock.getTime()- trialStart
		pressed= getKeys()
		#pressed, time= pylink.getEYELINK().getLastButtonPress()
		if 'right' in pressed: # terminate trial when mouse is clicked (temporary)
		#if 2 in pressed:
			goNext= True
			trialEnd= True
		if 'left' in pressed and item>26: # terminate trial when mouse is clicked (temporary)
		#if 4 in pressed and item>26:
			goNext= False
			trialEnd= True
		

		if trialTime> TrialTimeout: # end trial automatically if no response by participant
			trialEnd= True
			scr.clear(colour=BGC)
		
	scr.clear(colour=BGC) # clear subject screen
	disp.show()
	tracker.send_command("clear_screen %d" % (0)) # clear tracker screen	
	
	# end of trial messages:
	tracker.log('ENDBUTTON 5')
	tracker.log('DISPLAY OFF')
	tracker.log('TRIAL_RESULT 5')
	tracker.log('TRIAL OK')
	tracker.stop_recording()
	
	if goNext:
		item= item+1
	if not goNext and item>26:
		item= item-1

	Story2End= item==25+26+1
	#DorothyEnd= item==3

################
#  Questions:  #
################

for i in range(5,10):
	Quest(disp, scr, tracker, TiktokQ[i-5], i+1, TiktokAns[i-5])
	

tracker.close()
disp.close()
core.quit()
