#!/usr/bin/env perl

$sum = 0;

while(<>){
   if(/^\s*$/){
      print $sum, "\n";
      $sum = 0;
   }
   else{
      $sum += $_;
   }
}
print $sum, "\n";
