use Fcntl;

print "\t\t\tfixdist_para program (21/08/16)\n\nThis program reads DA1 output files \nfrom multiline studies and exports\na file of fixations suitable for use in R.**\n";
print "Note that the program requires\na special tab delimited count file with\nthe location of every word in a sentence\nand each line in the paragraph.\n";

if(0>0){
	print "yes\n";
}else{
	print "no\n";
}
print "Please provide the data list file: ";
$lstname = <STDIN>; 
chomp ($lstname);   
print "Please provide a count file with the locations of all the words: ";
$count = <STDIN>;
chomp ($count);
print "Please provide a file with the words: ";
$wordfile = <STDIN>;
chomp ($wordfile);
print "What is the maximum number of lines in an item: ";
$maxline = <STDIN>;
chomp ($maxline);
open (WORDS, $wordfile) or die "Can't open count file: $!\n";
	while ($textline = <WORDS>)
	  {
  		@paraline = split(/\s/, $textline); # this splits the line up using whitespace (tab or space) as the delimiter
  		#$numel = $#paraline; # gets the total number of elements on the line -- I don't know if this is needed or not
  		$conditem = $paraline[0]; # this is the condition/item identifier
  		@conditionitem = split(/I/,$conditem);
  		@itemcatch = split(/L/,$conditionitem[1]);
  		$line3 = $itemcatch[1];
  		$item3 = $itemcatch[0];
  		$cond3 = $conditionitem[0];
  		$itemtype = substr($cond3, 0, 1);
  		substr($cond3, 0, 1) = " ";       # delete first character
	    for($w = 1; $w < $#paraline + 1; $w++)
	      {
			  @puncword = split(/\W/,$paraline[$w]);#trying to split off any punctuation from the word leaving just the word
			  @justpuncs = split(/\w*/, $paraline[$w]);#trying to split the word off leaving just the punctuation
			  $fullwords[$item3][$cond3][$line3][$w] = $puncword[0];
			  print "word is: $fullwords[$item3][$cond3][$line3][$w]\t";
			  #print(output "$trial\t");
  			  $fullpuncs[$item3][$cond3][$line3][$w] = $justpuncs[1];
  			  print "punctuation is: $fullpuncs[$item3][$cond3][$line3][$w]\n";
	      }
	  }

open (CNT, $count) or die "Can't open count file: $!\n";
	while ($region = <CNT>)
	  {
	    chomp ($region);
	    @wordpos = split(/\s\s*/, $region);
	    $item2 = $wordpos[1];
		$cond2 = $wordpos[2];
		$line2 = $wordpos[3];
		print "$wordpos[4]\t";
	    for($reg = 4; $reg < $#wordpos + 1; $reg++)
	      {
		$fulldata[$item2][$cond2][$line2][($reg - 4)] = $wordpos[$reg];
		print "data is: $fulldata[$item2][$cond2][$line2][($reg - 4)]\n";
		#print(output "$trial\t");
	      }
	  }
print "Please enter an item number so we can double check things: ";
$itemcheck = <STDIN>;
chomp ($itemcheck);
print "Please enter a condition number so we can double check things: ";
$condcheck = <STDIN>;
chomp ($condcheck);
print "Please enter a line number so we can double check things: ";
$linecheck = <STDIN>;
chomp ($linecheck);
print "item = $itemcheck\t line = $linecheck\t number of words = $fulldata[$itemcheck][$condcheck][$linecheck][0]\n";
print "Please provide an output filename: ";
$output = <STDIN>;
chomp ($output);
open (LISTFILE, $lstname) or die "Can't open listfile: $!\n";
sysopen(FH, $output, O_WRONLY|O_CREAT) or die "Can't open output file: $!\n";
#$sub = 0;
print FH "subject\torder\titem\tcondition\tline\twordnum\tword\tPunctuation\tStart\tEnd\tUndersweep\tLine1stFix\tLaunch\tAltLaunch\tAlt2Launch\tAlt3Launch\tLand\tAltLand\tAlt2Land\tAlt3Land\tSacLen\tAltSacLen\tAlt2SacLen\tAlt3SacLen\tSacDur\tAltSacDur\tAlt2SacDur\tAlt3SacDur\tFF\tAltFF\tAlt2FF\tAlt3FF\tGaze\tGazeOutlier\tAltGaze\tAltGazeOutlier\tAlt2Gaze\tAlt2GazeOutlier\tAlt3Gaze\tAlt3GazeOutlier\tSP\tAltSP\tAlt2SP\tAlt3SP\tRO\tAltRO\tAlt2RO\tAlt3RO\tRI\tAltRI\tAlt2RI\tAlt3RI\tTotalTime\n";#print header
while ($filename = <LISTFILE>)
  {
    chomp ($filename);
    open (INDATA, $filename) or die "Can't open datafile: $!\n";
    $sub = substr($filename,0,-5);
    $trialnum = 0;
    print "Data file name:\t$filename\n";
    while ($line = <INDATA>) 
      {
	chomp($line);
	$trialnum++;
	@dataline = split(/\s\s*/, $line); # this splits the line up using any whitespace as the delimiter
	$numel = $#dataline; # gets the total number of elements on the line
	$item = $dataline[2];
	$cond = $dataline[1];
	$numfix = $dataline[7];
	for($l = 0; $l < $maxline + 1; $l++)#cycle through the lines of text for an item one at a time
	{
		$numword = $fulldata[$item][$cond][$l][0];#once we know the line we are on we get the number of words on that line
		for($p = 1; $p < $numword + 1; $p++)#then we cycle through the words on the line one at a time to calculate the eye measures for each word
		{
			$FF = "na";
			$AltFF = "na";
			$Alt2FF = "na";
			$Alt3FF = "na";
			$Gaze = "na";
			$AltGaze = "na";
			$Alt2Gaze = "na";
			$Alt3Gaze = "na";
			$SP = 0;
			$AltSP = 0;
			$Alt2SP = 0;
			$Alt3SP = 0;
			$TT = "na";
			$RI = 0;
			$RO = 0;
			$AltRI = 0;
			$AltRO = 0;
			$Alt2RI = 0;
			$Alt2RO = 0;
			$Alt3RI = 0;
			$Alt3RO = 0;
			$SacDur = "na";
			$AltSacDur = "na";
			$Alt2SacDur = "na";
			$Alt3SacDur = "na";
			$FFflag = 0;
			$FPflag = 0;
			$FF3flag = 0;
			$FP3flag = 0;
			$AltFFflag = 0;
			$AltFPflag = 0;
			$Alt2FFflag = 0;
			$Alt2FPflag = 0;
			$maxY = 0;
			$maxX = 0;
			$AltmaxX = 0;
			$word = $fullwords[$item][$cond][$l][$p];
			$punctuation = $fullpuncs[$item][$cond][$l][$p];
			$startx = $fulldata[$item][$cond][$l][$p];
			$endx = $fulldata[$item][$cond][$l][$p+1];
			$launch = "na";
			$Altlaunch = "na";
			$Alt2launch = "na";
			$Alt3launch = "na";
			$land = "na";
			$Altland = "na";
			$Alt2land = "na";
			$Alt3land = "na";
			$saccadelength = "na";
			$AltSaccadelength = "na";
			$Alt2Saccadelength = "na";
			$Alt3Saccadelength = "na";
			$sweepflag = 0;
			$gazeout = 0;
			$altgazeout = 0;
			$alt2gazeout = 0;
			$alt3gazeout = 0;
			print FH "$sub\t$trialnum\t$item\t$cond\t$l\t$p\t$word\t$punctuation\t$startx\t$endx";
			for($x = 8; $x < $numel; $x = $x + 4)#this cycles through the fixations on the trial looking for ones on the target word we got in the loop above
			{
				if((($dataline[$x+3]-$dataline[$x+2])>80) and ($dataline[$x] > -1))#excludes short and off passage fixations from entering the calculations 
				{
				$firstlinefix = 0;
				$undersweep = 0;#this resets with each fixation for calculating the alt measures
				if($dataline[$x+1] > $maxY)
				{
					$maxX = 0;
					$AltmaxX = 0;
					$firstlinefix = 1; #codes wether a fixation is the first to occur on a new line
					if(($dataline[$x+4]<$startx) and ($x+4 < $numel-1) and ($dataline[$x+4] > -1))
					{
					$undersweep = 1;#this resets with each fixation for calculating the alt measures
					}
					$maxY = $dataline[$x+1];
				}elsif($dataline[$x+1] != $dataline[$x-3])
				{
					$firstlinefix = 1; #codes for re-return sweeps or crazy regressions basically any first fixation to a line
					if(($dataline[$x+4]<$startx) and ($x+4 < $numel-1) and ($dataline[$x+4] > -1))
					{
					$undersweep = 1;#this resets with each fixation for calculating the alt measures
					}
				}
				if(($dataline[$x] > $maxX)and($dataline[$x+1]==$l))
				{
					$maxX = $dataline[$x];
					#print "$maxX\t"
				}
				if(($dataline[$x] > $AltmaxX)and($dataline[$x+1]==$l)and($firstlinefix==0))#calculate an alternative max x position that ignores the first fixation on the line
				{
					$AltmaxX = $dataline[$x];
					#print "$maxX\t"
				}
				if(($dataline[$x] > $endx)and($dataline[$x+1]==$l)and($launch eq "na"))
				{
					if($x==8){
						$launch = "na2";
						$SacDur = "na";
					}else{
						$SacDur = $dataline[$x+2] - $dataline[$x-1];
						if($dataline[$x]<$dataline[$x-4])
						{
							$launch = $endx - $dataline[$x-4];
						}else{
							$launch = $startx - $dataline[$x-4];
						}
						$land = $dataline[$x] - ($startx - 1);#space counts as first position!
						$saccadelength = $launch + $land;
					}
				}
				if(($dataline[$x] > $endx)and($dataline[$x+1]==$l)and($Altlaunch eq "na")and($undersweep == 0))
				{
					if($x==8){
						$Altlaunch = "na2";
						$AltSacDur = "na";
					}else{
						$AltSacDur = $dataline[$x+2] - $dataline[$x-1];
						if($dataline[$x]<$dataline[$x-4])
						{
							$Altlaunch = $endx - $dataline[$x-4];
						}else{
							$Altlaunch = $startx - $dataline[$x-4];
						}
						$Altland = $dataline[$x] - ($startx - 1);
						$AltSaccadelength = $Altlaunch + $Altland;
						}
				}
				if(($dataline[$x] > $endx)and($dataline[$x+1]==$l)and($Alt2launch eq "na"))
				{
					if($x==8){
						$Alt2launch = "na2";
						$Alt2SacDur = "na";
					}else{
						$Alt2SacDur = $dataline[$x+2] - $dataline[$x-1];
						if($dataline[$x]<$dataline[$x-4])
						{
							$Alt2launch = $endx - $dataline[$x-4];
						}else{
							$Alt2launch = $startx - $dataline[$x-4];
						}
						$Alt2land = $dataline[$x] - ($startx - 1);
						$Alt2Saccadelength = $Alt2launch + $Alt2land;
						}
				}
				if(($dataline[$x] > $endx)and($dataline[$x+1]==$l)and($Alt3launch eq "na"))
				{
					if($x==8){
						$Alt3launch = "na2";
						$Alt3SacDur = "na";
					}else{
						$Alt3SacDur = $dataline[$x+2] - $dataline[$x-1];
						if($dataline[$x]<$dataline[$x-4])
						{
							$Alt3launch = $endx - $dataline[$x-4];
						}else{
							$Alt3launch = $startx - $dataline[$x-4];
						}
						$Alt3land = $dataline[$x] - $startx;
						$Alt3Saccadelength = $Alt3launch + $Alt3land;
						}
				}
				if($maxY > $l)
				{
					$FPflag = 1;
					$FFflag = 1;
					$FF3flag = 1;
					$FP3flag = 1;
					$AltFPflag = 1;
					$AltFFflag = 1;
					$Alt2FFflag = 1;
					$Alt2FPflag = 1;
				}
				if(($FFflag==1)and($dataline[$x] < $startx))
				{
					$FPflag = 1;
				}
				if(($AltFFflag==2)and($dataline[$x] < $startx))
				{
					$AltFPflag = 1;
				}
				if(($Alt2FFflag==2)and($dataline[$x] < $startx))
				{
					$Alt2FPflag = 1;
				}
				if(($FF3flag==1)and($dataline[$x] < $startx))
				{
					$FP3flag = 1;
				}
				if($dataline[$x+1]==$l)#only consider fixations on the line
				{
					if(($dataline[$x] >= $startx) and ($dataline[$x] < $endx))#only the fixations on the target now
					{
						if(($firstlinefix == 1) and ($sweepflag == 0))
						{
							$sweepflag = 1;
							if(($dataline[$x+4]<$startx) and ($x+4 < $numel-1))
							{
								print FH "\tY\t1";
							}else{
								print FH "\tN\t1";
							}
						}elsif($sweepflag == 0){
							$sweepflag = 1;
							print FH "\tN2\t0";
						}
						if($maxX > $endx)
						{
						$FPflag = 1;
						$FFflag = 1;
						}
						if($AltmaxX > $endx)
						{
						$AltFPflag = 1;
						$AltFFflag = 1;
						$FF3flag = 1;
						}
							if($FFflag == 0)
							{
								$FF = $dataline[$x+3] - $dataline[$x+2];
								$FFflag = 1;
								if($x==8){
									$SacDur = "na";
									$launch = "na3";
								}else{
									$SacDur = $dataline[$x+2] - $dataline[$x-1];
									if($dataline[$x]<$dataline[$x-4])
									{
										$launch = $endx - $dataline[$x-4];
									}else{
										$launch = $startx - $dataline[$x-4];
									}
									$land = $dataline[$x] - ($startx-1);
									$saccadelength = $launch + $land;
								}
							}
							if($FPflag == 0)
							{
								$Gaze = $Gaze + ($dataline[$x+3] - $dataline[$x+2]);
								if(($dataline[$x+3] - $dataline[$x+2])>799)
								{
									$gazeout = 1;
								}
							}
							if($FPflag == 1)
							{
								$SP = $SP + ($dataline[$x+3] - $dataline[$x+2]);
							}
							if(($AltFFflag == 0) and ($undersweep == 0))
							{
							$AltFF = $dataline[$x+3] - $dataline[$x+2];
							$AltFFflag = 2;
							if($x==8){
								$AltSacDur = "na";
								$Altlaunch = "na3";
							}else{
								$AltSacDur = $dataline[$x+2] - $dataline[$x-1];
								if($dataline[$x]<$dataline[$x-4])
								{
									$Altlaunch = $endx - $dataline[$x-4];
								}else{
									$Altlaunch = $startx - $dataline[$x-4];
								}
								$Altland = $dataline[$x] - ($startx - 1);
								$AltSaccadelength = $Altlaunch + $Altland;
								}
							}
							if($Alt2FFflag == 0) #need to fix this so that only undersweeps are included beyond the target
							{
							$Alt2FF = $dataline[$x+3] - $dataline[$x+2];
								if($undersweep == 1){
									$Alt2FFflag = 1;
								}else{
									$Alt2FFflag = 2;
								}
								if($x==8){
									$Alt2SacDur = "na";
									$Alt2launch = "na3";
								}else{
									$Alt2SacDur = $dataline[$x+2] - $dataline[$x-1];
									if($dataline[$x]<$dataline[$x-4])
									{
										$Alt2launch = $endx - $dataline[$x-4];
									}else{
										$Alt2launch = $startx - $dataline[$x-4];
									}
									$Alt2land = $dataline[$x] - ($startx-1);
									$Alt2Saccadelength = $Altlaunch + $Altland;
								}
							}
							if(($FF3flag == 0) and ($firstlinefix == 0))
							{
								$Alt3FF = $dataline[$x+3] - $dataline[$x+2];#Alt3 measures exclude all first line fixations
								$FF3flag = 1;
								if($x==8){
									$Alt3SacDur = "na";
									$Alt3launch = "na3";
								}else{
									$Alt3SacDur = $dataline[$x+2] - $dataline[$x-1];
									if($dataline[$x]<$dataline[$x-4])
									{
										$Alt3launch = $endx - $dataline[$x-4];
									}else{
										$Alt3launch = $startx - $dataline[$x-4];
									}
									$Alt3land = $dataline[$x] - ($startx-1);
									$Alt3Saccadelength = $Alt3launch + $Alt3land;
								}
							}
							if(($AltFPflag == 0) and ($undersweep == 0))
							{
								$AltGaze = $AltGaze + ($dataline[$x+3] - $dataline[$x+2]);#Alt measures exclude inter-word undersweep fixations
								if(($dataline[$x+3] - $dataline[$x+2])>799)
								{
									$altgazeout = 1;
								}
							}
							if($Alt2FPflag == 0)
							{
								$Alt2Gaze = $Alt2Gaze + ($dataline[$x+3] - $dataline[$x+2]);#Alt2 measures include undersweep fixations but don't allow them to end gaze.
								if(($dataline[$x+3] - $dataline[$x+2])>799)
								{
									$alt2gazeout = 1;
								}
							}
							if(($FP3flag == 0) and ($firstlinefix == 0))
							{
								$Alt3Gaze = $Alt3Gaze + ($dataline[$x+3] - $dataline[$x+2]);#Alt3 measures exclude all line initial fixations
								if(($dataline[$x+3] - $dataline[$x+2])>799)
								{
									$alt3gazeout = 1;
								}
							}
							if($AltFPflag == 1)
							{
								$AltSP = $AltSP + ($dataline[$x+3] - $dataline[$x+2]);
							}
							if($Alt2FPflag == 1)
							{
								$Alt2SP = $Alt2SP + ($dataline[$x+3] - $dataline[$x+2]);
							}
							if($FP3flag == 1)
							{
								$Alt3SP = $Alt3SP + ($dataline[$x+3] - $dataline[$x+2]);
							}
							$TT = $TT + ($dataline[$x+3] - $dataline[$x+2]);
							if(($FPflag == 0) and ($dataline[$x+4] < $startx))
							{
								$RO = 1;
							}
							if(($FPflag == 1) and ($dataline[$x-4] > $endx))
							{
								$RI = 1;
							}
							if(($AltFPflag == 0) and ($dataline[$x+4] < $startx))
							{
								$AltRO = 1;
							}
							if(($AltFPflag == 1) and ($dataline[$x-4] > $endx))
							{
								$AltRI = 1;
							}
							if(($Alt2FPflag == 0) and ($dataline[$x+4] < $startx))
							{
								$Alt2RO = 1;
							}
							if(($Alt2FPflag == 1) and ($dataline[$x-4] > $endx))
							{
								$Alt2RI = 1;
							}
							if(($FP3flag == 0) and ($dataline[$x+4] < $startx))
							{
								$Alt3RO = 1;
							}
							if(($FP3flag == 1) and ($dataline[$x-4] > $endx))
							{
								$Alt3RI = 1;
							}
						}
					}
				}
			}#end of x loop
			if($sweepflag == 0){
					print FH "\tN3\t0"; #prints out the undersweep info in cases where the target isn't fixated.
			}
			print FH "\t$launch\t$Altlaunch\t$Alt2launch\t$Alt3launch\t$land\t$Altland\t$Alt2land\t$Alt3land\t$saccadelength\t$AltSaccadelength\t$Alt2Saccadelength\t$Alt3Saccadelength\t$SacDur\t$AltSacDur\t$Alt2SacDur\t$Alt3SacDur\t$FF\t$AltFF\t$Alt2FF\t$Alt3FF\t$Gaze\t$gazeout\t$AltGaze\t$altgazeout\t$Alt2Gaze\t$alt2gazeout\t$Alt3Gaze\t$alt3gazeout\t$SP\t$AltSP\t$Alt2SP\t$Alt3SP\t$RI\t$AltRI\t$Alt2RI\t$Alt3RI\t$RO\t$AltRO\t$Alt2RO\t$Alt3RO\t$TT\n";
		}#end of p loop
	}#end of l loop
  }
close INDATA;
}
close LISTFILE;
close FH;
print "Finished!";
<>
