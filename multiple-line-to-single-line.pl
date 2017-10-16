#!usr/bin/perl/
use strict;
use warnings;

my $in1=shift;
my $out1=shift;


open IN,"$in1";
$/=">"; <IN>; $/="\n>";
open OUT, ">$out1";
while(my $line=<IN>){
		chomp $line;
		#
		my @line=split/\n/,$line;
		my $name=shift @line;
		my $seq=join("",@line);
		print OUT ">$name\n$seq\n";
}
close IN;
close OUT ;