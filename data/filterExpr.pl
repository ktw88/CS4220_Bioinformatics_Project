#!/usr/bin/perl

use strict;
use warnings;

my $inputFile = $ARGV[0];
my $aveExprAtLeast = $ARGV[1];
my $maxExprAtLeast = $ARGV[2];

open(my $EXPR, $inputFile) || die $!;

my $header = <$EXPR>;
while(my $line = <$EXPR>){
	chomp($line);
	my @lineArr = split(/\t/, $line);
	
	my $minExpr = 1000000000000;
	my $maxExpr = -1;
	my $aveExpr = -1;
	my $totalExpr = 0;
	for(my $i = 2; $i < scalar(@lineArr); $i++)	{
		if($lineArr[$i] < $minExpr){
			$minExpr = $lineArr[$i];
		}
		if($lineArr[$i] > $maxExpr){
			$maxExpr = $lineArr[$i];
		}
		$totalExpr += $lineArr[$i];
	}
	$aveExpr = $totalExpr / (scalar(@lineArr) - 2);
	
	if($maxExpr > $maxExprAtLeast && $aveExpr > $aveExprAtLeast){
		print join("\t", @lineArr) . "\n";
	}
}

close($EXPR);
