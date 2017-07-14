# -*- coding: utf-8 -*-
"""
Created on Sun Jun 11 23:00:56 2017

@author: Martin Vasilev
"""


#---------------------#
#  General settings:  #
#---------------------#
lab= True
checkPPL= False # draws a rectangle around sentence to make sure letter width is correct
expName = "OZ" # used for saving data (keep short)
expDir= 'C:\Users\Martin Vasilev\Dropbox\pyTrack'
corpusFile= "C:\\Users\\Experimenter\\Desktop\\Martin Vasilev\\WinPython-PyGaze-0.5.1\sentences.txt"
#eyedatafile= 'test.edf'

#---------------------#
#  Display settings:  #
#---------------------#
# screen resolution:
if lab:
	DISPSIZE = (1920, 1080) # P103j
else:
	DISPSIZE = (1366, 768) # laptop
 
	
 # laptop 
#DISPSIZE = (1024, 768) # lab
offsetX= 481 # sentence offset on x-axis
yStart=193 # same, y axis
DISPTYPE = 'psychopy'
sentPos= (offsetX, DISPSIZE[1]/2)
Pix_per_Letter= 12

#FGC = (-1, -1, -1) # text colour
#BGC = (1, 1, 1) # background colour 
FGC= (0,0,0)
BGC= (255, 255, 255)

#------------------------#
#  Experiment settings:  #
#------------------------#
Font= 'Courier New'
TextSize= 22
InstrTextSize= 32
GazeBoxSize= 40 # in pixels
GazeBoxY= yStart-11
GazeBoxColor= (-1, -1, -1)
gazeBoxDur= 50 # how many ms the eye needs to stay on the gaze box before triggering it
gazeBoxDisplayTime= 7 # how many seconds to wait to trigger the gaze box
TrialTimeout= 5*60
ncond=4

expSetup = {'Participant': '',
           'Condition (Randomize)': ''}
from psychopy import gui
dlg = gui.DlgFromDict(dictionary=expSetup, title= 'Run experiment: '+ expName)
LOGFILENAME= expName+ expSetup['Participant']


#---------------------#
#  Tracker settings:  #
#---------------------#
# Other constants:
#useFullscreen=True
caltype= "HV9" # 3-point horizontal (use "HV9" for 9-point grid)
trackertype = 'eyelink'
saccvelthresh = 35 # degrees per second, saccade velocity threshold
saccaccthresh = 9500 # degrees per second, saccade acceleration threshold'	

#------------------------#
#  Additional functions: #
#------------------------#

# prints stimuli to edf file
#def stim2edf(filename, sentPos, tracker):
#	from psychopy.core import wait
#	chars= list(sent) #get characters in sentence
#	x_start= []
#	x_end= []
#	
#	for i in range(0, len(sent)): # loop to calulate x position of letters
#		if i==0:
#			x_start.append(sentPos[0]);
#			x_end.append(sentPos[0]+ Pix_per_Letter)
#		else:
#			x_start.append(x_end[i-1])
#			x_end.append(x_end[i-1]+Pix_per_Letter)
#			
#	y_start= 520
#	y_end= 560
#	
#	tracker.log('DISPLAY TEXT 1')
#	
#	for i in range(0, len(sent)-1): # print coords of letters on the screen
#		tracker.log('REGION CHAR %d 1 %s %d %d %d %d' % (i, chars[i], x_start[i], y_start, x_end[i], y_end))
#		wait(0.001) # wait time for consitency purposes with Eyetrack
#		tracker.log('DELAY 1 MS')
#		wait(0.001)

def stim2edf(tracker, filename, offsetX, Pix_per_Letter, yStart, height= 22, line= 50):
	from psychopy.core import wait    
    
	text= []
	with open(filename) as f:
		for l in f:
			text.append(l.strip().split('\n'))
	x_start= []
	x_end= []
	y_start= []
	y_end= []
	char= []
	letter= []
	Nchar=0
       
	tracker.log('DISPLAY TEXT 1')
	for i in range(0, len(text)):
		if i==0:
			#y1= yStart
			#y2= yStart+height
			y1= yStart-line/2
			y2= yStart+height+line/2
		else:
			y1= y2+ 1
			y2= y1+height+line
            
		for j in range(0, len(text[i][0])): # loop to calulate x position of letters
			if j==0:
				x_start= offsetX
				x_end= offsetX+ Pix_per_Letter
				y_start= y1
				y_end= y2
			else:
				x_start= x_end
				x_end= x_end+Pix_per_Letter
				y_start= y1
				y_end= y2
            
			char= Nchar
			Nchar= Nchar+1
			letter= text[i][0][j]
									
			tracker.log('REGION CHAR %d 1 %s %d %d %d %d' % (char, letter, x_start, y_start, x_end, y_end))
			if char== ' ':
				tracker.send_command("draw_filled_box %d %d %d %d %d" % (x_start, y_start, x_end, y_end, 5))
			else:
				tracker.send_command("draw_box %d %d %d %d %d" % (x_start, y_start, x_end, y_end, 5))
			#wait(0.001) # wait time for consitency purposes with Eyetrack
			tracker.log('DELAY 1 MS')
			#wait(0.001)


#def stim2edf(tracker, filename, offsetX, Pix_per_Letter, yStart=119, height= 18, line= 62):
#    from psychopy.core import wait    
#    
#    text= []
#    with open(filename) as f:
#        for l in f:
#            text.append(l.strip().split('\n'))
#    x_start= []
#    x_end= []
#    y_start= []
#    y_end= []
#    char= []
#    letter= []
#    Nchar=0
#       
#    
#    for i in range(0, len(text)):
#        if i==0:
#            y1= yStart
#            y2= yStart+height
#        else:
#            y1= y2+ line
#            y2= y1+height
#            
#        for j in range(0, len(text[i][0])): # loop to calulate x position of letters
#            if j==0:
#                x_start.append(offsetX);
#                x_end.append(offsetX+ Pix_per_Letter)
#                y_start.append(y1)
#                y_end.append(y2)
#            else:
#                x_start.append(x_end[j-1])
#                x_end.append(x_end[j-1]+Pix_per_Letter)
#                y_start.append(y1)
#                y_end.append(y2)
#               
#            char.append(Nchar)
#            Nchar= Nchar+1
#            letter.append(text[i][0][j])
            #print(i,j, text[i][0][j])
												
#    tracker.log('DISPLAY TEXT 1') 
#    for i in range(0, len(char)):
#        tracker.log('REGION CHAR %d 1 %s %d %d %d %d' % (char[i], letter[i], x_start[i], y_start[i], x_end[i], y_end[i]))
#	if char[i]== ' ':
#		tracker.send_command("draw_filled_box %d %d %d %d %d" % (x_start[i], y_start[i], x_end[i], y_end[i], 5))
#	else:
#		tracker.send_command("draw_box %d %d %d %d %d" % (x_start[i], y_start[i], x_end[i], y_end[i], 5))
#        wait(0.001) # wait time for consitency purposes with Eyetrack
#        tracker.log('DELAY 1 MS')
#        wait(0.001)
#        
        
            


		
	
	
