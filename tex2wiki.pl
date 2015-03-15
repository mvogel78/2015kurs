#!/usr/bin/perl

$filename = $ARGV[0];

use Image::Imgur;

my $key = "981e9afbf919858b1504214ad744e97c"; ## dev key imgur

my $imgur = new Image::Imgur(key => $key);


open IN, $filename;
#open IN, "second/beforetesting.tex";
#open IN, "start/introduction2.tex";

open OUT, ">urls.txt";

$itlevel=0;
$tablevel=0;
$verblevel=0;

while(<IN>) {
    s/^ +//;
    s/\\\\[\w]*$//;
    s/\$\\mathtt{([A-z0-9()\., ]+)}\$/{{{#!latex\n $1\n}}}/g;
    s/\\emph{([A-z0-9()\., ]+)}/''$1''/g;
    s/\\texttt{([A-z0-9()\., =]+[^\w]*)}/$1/g;
    s/\\textbf{([A-z0-9()\., =]+[^\w]*)}/'''$1'''/g;
    if (/^\\begin{verbatim}/) { 
	$verblevel++;
	print "{{{#!highlight r\n";
    }
    if (/^\\end{verbatim}/) {
	$verblevel--; 
	print "}}}\n" 
    }
    if ($verblevel==0) { 
	s/\|/&brvbar;/g;
	s/</&lt;/;
	s/>/&gt;/;
    }
    if ($verblevel==0 && $tablevel==0) { 
	s/ \$+/ {{{#!latex /g;
	s/([^\\])\$([ \.\,\n\r\t}])/$1}}}$2/g;
    }
     if (/^"/) { print $_ }
     if (/\\begin{itemize}/) { 
	$itlevel++;
	next;
     }
    if (/\\end{itemize}/) { 
	$itlevel--;
	next;
    }
    if (/\\begin{tabular}/) { 
	$tablevel++;
	next;
    }
    if (/\\end{tabular}/) { 
	$tablevel--;
	next;
    }
    if (/\\item[\[ ](.*)/) { 
	print "   " x $itlevel;
	print "* " . $1 . "\n";
	next;
    }
    if($tablevel){
	s/\&[^l^g^b]/\|/g;
	s/\$//g;
	s/ +/\ /g;
	if(/^[^\\]/){
	    chomp;
	    print "||" . $_ . "||\n" ;
	}
    }
    if (/^\\title{(.*)}/) { $title = "= " . $1 . " ="}
    if (/^\\titlepage/){ print "$title\n"}
    if (/^\\begin{frame}(.*)*\\frametitle{(.*)}/) { 
	$tmp = $2;
	$tmp =~ s/\\texttt{//;
	$tmp =~ s/}//;
	print "== " . $2 . " ==\n";
    }
    if (/includegraphics.*{(.*)}/){
	my $bild = "sesssion2/" . $1;
	my $url1 = $imgur->upload($bild) or die "upload failed";
	print "<img alt='" . $bild . "' src='". $url1 ."' />\n";
	print OUT $url1 . "\n";
    }
    if ((/^[A-z0-9\+>^\\\[]+ /) && !($tablevel) ) { 
	print $_; 
    }
}
