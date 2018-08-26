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
caltype= "Hv9" # 3-point horizontal (use "HV9" for 9-point grid)
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
        

DorothyQ= ['QUESTION:\n\n\n\n\n\n\n\nWhat did the sign tacked to the tree warn Dorothy to look out for?\n\n\n\n' +
		'1)Poison Ivy\n\n2)Quick sand\n\n3)Crinklink\n\n4)Cassowary the demon bird',
		'QUESTION:\n\n\n\n\n\n\n\nWho did Dorothy stumble upon next to the lake of black water?\n\n\n\n' + 
		'1)The ferryman\n\n2)The scarecrow\n\n3)The fisherman\n\n4)The Tinman',
		'QUESTION:\n\n\n\n\n\n\n\nTo take Dorothy across the lake of black water, what did the ferryman transform into?\n\n\n\n' +
		'1)A giant\n\n2)A boat\n\n3)A bird\n\n4)A fish',
		'QUESTION:\n\n\n\n\n\n\n\nWhat did Crinklink make Dorothy do?\n\n\n\n' +
		'1)Mow the lawn\n\n2)Cut his long scraggly beard\n\n3)Make him lashings of eye ball soup\n\n4)Wash his dirty dishes',
		'QUESTION:\n\n\n\n\n\n\n\nWho was Crinklink, really?\n\n\n\n' +
		'1)The Tin Woodman\n\n2)The Wizard of Oz\n\n3)The Nome King\n\n4)The Witch of the East'
		] 

DorothyAns= [3,1,1,4,2]


TiktokQ= ['QUESTION:\n\n\n\n\n\n\n\nAt the beginning of the story who did Tiktok travel to visit?\n\n\n\n' +
	    '1)The Nome king\n\n2)The Wizard of Oz\n\n3)The Blacksmith\n\n4)The Sandman',
	    'QUESTION:\n\n\n\n\n\n\n\nWhat did the Nome king do to Tiktok when they first meet?\n\n\n\n' +
	    '1)Strike Tiktok with a mace breaking him into hundreds of pieces'+
	    '\n\n2)Make tea, so they could sit down a chat about the weather'+
	    '\n\n3)Laugh in his face, as he had never seen such a contraption until now'+
	    '\n\n4)Send in his guards to arrest Tiktok',
	    'QUESTION:\n\n\n\n\n\n\n\nAfter breaking Tiktok, who was the Nome King afraid would attack him?\n\n\n\n' +
	    '1)Headmaster Baum\n\n2)Princess Ozma\n\n3)The Wizard of Oz\n\n4)The Brave Lion',
	    'QUESTION:\n\n\n\n\n\n\n\nWho repaired Tiktok?\n\n\n\n' + 
	    '1)The Nome King\n\n2)The Tin Woodsman\n\n3)The Wizard of Oz\n\n4)Kaliko',
	    'QUESTION:\n\n\n\n\n\n\n\nWhat did the Nome King give to Tiktok to pass onto Princess Ozma of Oz?\n\n\n\n' +
	    '1)Jewels\n\n2)New metal springs\n\n3)The Royal Sceptre\n\n4)Ruby Slippers'

]

TiktokAns= [1,1,2,4,1] 
           
def Quest(disp, scr, tracker, Question, item, corr_ans, FGC= (0,0,0), TextSize= 28, Font= 'Courier New'):
	from psychopy import event
	from psychopy.core import wait
	#import pylink	
	
	allowedResp= ['1', '2', '3', '4']
	#allowedResp= [1, 2, 3, 4]
	answered= False
	
	# Initial question stamps:
	tracker.log('TRIALID F1I%dD1' % (item))
	tracker.log('QUESTION_ANSWER %d' % (corr_ans))
	tracker.log('DELAY 500 MS')
	tracker.start_recording()
	wait(0.05)
	tracker.status_msg('Question F1I%dD1' % (item)) 
	wait(0.5)
	tracker.log('DISPLAY ON')
	tracker.log('SYNCTIME')
	
	##################
	scr.draw_text(text= Question, colour= FGC, font= Font, center=True, fontsize=TextSize)

	disp.fill(scr)
	disp.show()	
	
	#d = pylink.getEYELINK().getLastButtonPress()
	#print(d)
	
	###	
	while not answered:
		pressed= event.getKeys()
		if any(i in pressed for i in allowedResp):
			answered= True	
			ans= int(pressed[0])
			tracker.stop_recording()
		
#		pressed, time= pylink.getEYELINK().getLastButtonPress()
#		if any(i in str(pressed) for i in allowedResp):
#			answered= True	
#			ans= int(pressed)
#			print(ans)
#			tracker.stop_recording()
			
	# clear screen:
	scr.clear()
	disp.fill(scr)
	disp.show()
	
	# Print end of question stamps to edf:
	tracker.log('ENDBUTTON %d' % (ans))
	tracker.log('DISPLAY OFF')
	tracker.log('TRIAL_RESULT %d' % (ans))
	tracker.log('TRIAL OK')

		
	
	
