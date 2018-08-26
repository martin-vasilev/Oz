use Fcntl;

print "\t\t\tLINK6DROP program (6/23/05)\n\nThis program converts Eyelink ASC output files from eyetrack0.7.10c3 spacing\n
program into a format that can be read by the eyedoctor program\n";
print "NOTE This program uses the UMASS style of condition and item numbers E##I##D##\n";


 #print "Please provide output file extension (e.g., da1): ";
 #$ext = <STDIN>; 
#chomp ($ext);   


 print "Please provide the data list file: ";
 $lstname = <STDIN>; 
chomp ($lstname);   

#print "Please provide the item filler list file: ";
 #$fillst = <STDIN>; 
#chomp ($fillst);   

open (LISTFILE, $lstname) or die "Can't open listfile: $!\n";
#gets the list file in from the user which contains the names of the files that will be run

print "Please provide the output filename: ";
$output = <STDIN>;
chomp ($output);

sysopen(FH, $output, O_WRONLY|O_CREAT) or die "Can't open output file: $!\n";
$sub = 0;
print FH "subject\titem\tsentcond\tquestcond\tdependnum\tstarttime\tendtime\tsubresp\tcorrectresp\taccuracy\n";
while ($filename = <LISTFILE>){



	# print "Please provide the data file: ";
 #	$name = <STDIN>; 
	chomp ($filename);   

	open (INDATA, $filename) or die "Can't open datafile: $!\n";
	#gets the data file in ASCII format from the user
	$sub++;

	

	# sysopen(FH, $name, O_WRONLY|O_EXCL|O_CREAT) or die "Can't open output file: $!\n";
	# this one will not run if the file exists : O_EXCL means fail if exists
	print "Output file name:\t$filename\n";
	######################################################
	#####        End of File Setup                   #####
	######################################################



	######################################################
	#####        Begin file reading                  #####
	######################################################


	while ($line = <INDATA>) {
	chomp ($line);
	@dataline = split(" ", $line); # this splits the line up using space as the delimiter

	$numel = @dataline; # gets the total number of elements on the line -- I don't know if this is needed or not

	if ($dataline[2] eq "TRIALID") # this finds the beginning of a trial
	   {
		$conditem = $dataline[3]; # this is the condition/item identifier
		@conditionitem = split(/I/,$conditem);
		@itemcatch = split(/D/,$conditionitem[1]);
		$item = $itemcatch[0];
		$cond = $conditionitem[0];
		$depend = $itemcatch[1];
		substr($cond, 0, 1) = " ";       # delete first character
		$trialokflag = 0;
		print "$item\t$cond\t$depend\n";
	   }
	if($depend == 0)
	  {
	    $lastcond = $cond;
	  }
	if($depend > 0)
	  {
	    if ($dataline[2] eq "QUESTION_ANSWER") # this finds correct answer for the question
	      {
		$correct = $dataline[3];
	      }
	    if(($dataline[2] eq "DISPLAY") && ($dataline[3] eq "ON")) 
	      {
		print FH "$sub\t$item\t$lastcond\t$cond\t$depend";
		$stime = $dataline[1];
		print FH "\t$stime";
	      }
	    if ($dataline[2] eq "ENDBUTTON")	
	      {
		$endtime = $dataline[1];
		$button = $dataline[3];
		if ($button == $correct)
		  {
		    $accuracy = 1;
		  }
		else
		  {
		    $accuracy = 0;
		  }
		print FH "\t$endtime\t$button\t$correct\t$accuracy\n";
	      }
	    
	    if (($dataline[2] eq "TRIAL") && ($dataline[3] eq "OK"))
	      {
		$trialokflag = 1;
		#print FH "\t$trialokflag\n";
	      }
	  }
      }
	
	print ">>>>>finished file: $filename<<<<<\n";
	close INFILE;
      }# this is the end of the while loop that runs the list file stuff
close FH;
print ">>>>>finished all files<<<<<\n";
<>
