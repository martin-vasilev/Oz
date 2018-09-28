use Fcntl;

print "\t\t\tfixdist_para program (21/08/16)\n\nThis program reads DA1 output files \nfrom multiline studies and exports\na file of fixations suitable for use in R.**\n";
print "Note that the program requires\na special tab delimited count file with\nthe location of every word in a sentence\nand each line in the paragraph.\n";

 
print "Please provide the data list file: ";
$lstname = <STDIN>; 
chomp ($lstname);   
print "Please provide a count file with the locations of all the words: ";
$count = <STDIN>;
chomp ($count);
print "getting regions...";

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
print FH "subject\torder\titem\tcondition\tline\tlinefinal\tlineinitial\tfixnum\tmaxfix\tpriorX\tcurrentX\tnextX\tpreprereg\tprereg\tstartreg\tendreg\tpostreg\tcurrentword\tmaxword\tregress\tfixduration\n";
while ($filename = <LISTFILE>)
  {
    chomp ($filename);
    open (INDATA, $filename) or die "Can't open datafile: $!\n";
    $sub = $filename;
    $trialnum = 0;
    print "Data file name:\t$filename\n";
    while ($line = <INDATA>) 
      {
	chomp($line);
	$trialnum++;
	$regress = 0;
	$maxp = 0;
	$maxline = 0;
	
	@dataline = split(/\s\s*/, $line); # this splits the line up using the tab as the delimiter
	$numel = $#dataline; # gets the total number of elements on the line
	$item = $dataline[2];
	print "item = $item\t";
	$cond = $dataline[1];
	print "cond = $cond\t";
	$numfix = $dataline[7];
	print "numfix = $numfix\t";
	#$numword = $fulldata[$item][$cond][0];
	for($x = 8; $x < $numel; $x = $x + 4)#cycle through the fixations for the trial 
	  {
  		if((($dataline[$x+3]-$dataline[$x+2])>40) and ($dataline[$x] > -1))#excludes short and off passage fixations from entering the calculations 
  		{
	    $fixnum = ($x - 4)/4;
		print "fixnum = $fixnum\t";
		$linenum = $dataline[$x+1];#this gets the line the fixation is on this is crucial for limiting our region search to the correct line in the loops below.
		print "line = $linenum\t";
		if($linenum > $maxline)
		{
			$maxline = $linenum;
			$maxp = 0;
			$regress = 0;
			$lineinitial = 1;
		}else{
			$lineinitial = 0;
		}
		if(($dataline[$x+1])<($dataline[$x+5]))
		{
			$linefinal = 1;
		}else{
			$linefinal = 0;
		}
		if($linenum < $maxline)
		{
			$regress = 1;
		}
	    $charx = $dataline[$x];
		print "charx = $charx\t";
	    if($x==8)
	      {
		$precharx = -99;
		print "$precharx\t";
	      }
	    elsif($dataline[$x-4] > -1)
 	      {
		$precharx = $dataline[$x-4];
		print "$precharx\t";
	      }
		else{
			$precharx = "na";
			print "$precharx\t";
		  }
	    if($x > ($numel - 6))
	      {
		$postcharx = -99;
	      }
	    elsif($dataline[$x+4] > -1)
	      {
		$postcharx = $dataline[$x+4];
	      }
		  else{
			  $postcharx = "na";
		  }
	    $fixdur = $dataline[$x+3] - $dataline[$x+2];
	    #$blink = $dataline[$x+4];#this isn't needed as the blinks are removed during preprocessing in eyedoctor
		$numword = $fulldata[$item][$cond][$linenum][0];
		print "numword = $numword\n";
	    for($p = 1; $p < $numword + 1; $p++)
	      {
		if(($charx >= $fulldata[$item][$cond][$linenum][$p]) and ($charx < $fulldata[$item][$cond][$linenum][$p+1])) #charx if loop
		 {
		   $word = $p;
		   if($p == 1)
		     {
		       $prereg2 = -1;
		       $prereg = -1;
		       $postreg = $fulldata[$item][$cond][$linenum][$p+2];
		     }elsif($p == 2)
		       {
			 $prereg2 = -1;
			 $prereg = $fulldata[$item][$cond][$linenum][$p-1];
			 $postreg = $fulldata[$item][$cond][$linenum][$p+2];
		       }elsif($p == $numword)
			 {
			   $prereg2 = $fulldata[$item][$cond][$linenum][$p-2]; 
			   $prereg = $fulldata[$item][$cond][$linenum][$p-1];
			   $postreg = -1;
			 }else
			   {
			     $prereg2 = $fulldata[$item][$cond][$linenum][$p-2]; 
			     $prereg = $fulldata[$item][$cond][$linenum][$p-1];
			     $postreg = $fulldata[$item][$cond][$linenum][$p+2];
			   }
		   if($p > $maxp)
		     {
		       $maxbnd = $fulldata[$item][$cond][$linenum][$p];
		       $maxp = $p;
		       $regress = 0;
		     }
		   $p = $numword + 2;
		   #$prebound = $fulldata[$item][$cond][$p];
		   #$postbound = $fulldata[$item][$cond][$p+1];
		 }#ends charx if loop
		elsif($charx == -1)
		  {
		    $word = -1;
		    $p = $numword + 2;
		  }
		elsif($charx == 999)
		  {
		    $word = 999;
		    if($maxp > 0)
		      {
			$maxp=$numword;
		      }
		    $p = $numword + 2;
		  }
	      }#ends p loop
	    if($charx < $maxbnd)
	      {
		$regress = 1;
	      }
	    print FH "$sub\t$trialnum\t$item\t$cond\t$linenum\t$linefinal\t$lineinitial\t$fixnum\t$numfix\t$precharx\t$charx\t$postcharx\t$prereg2\t$prereg\t$fulldata[$item][$cond][$linenum][$word]\t$fulldata[$item][$cond][$linenum][$word+1]\t$postreg\t$word\t$maxp\t$regress\t$fixdur\n";
	}#ends exclusions loop
}#ends x loop
}#ends indata loop
close INDATA;
}#ends while loop
close LISTFILE;
close FH;
<>
