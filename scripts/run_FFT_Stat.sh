#!/bin/bash

#This script assumes it is being run from inside the project root directory
DIR=/home/willian/git_repos/SU2SimSuit
DIR=`pwd`

Ns=$1
Nt=$2
filePath=$DIR/output/Statistical

#--------------------------------------------
cd $DIR/output
i=1
j=1
while [ $i -le 3 ]; do
   while [  $j -le 3 ]; do #For each configuration
      filename=`echo $filePath`Average$i$j.dat
      $DIR/bin/FFT_Tmunu.run $Ns $Ns $Ns $Nt 0 $filename
      filename=`echo $filePath`Error$i$j.dat
      $DIR/bin/FFT_Tmunu.run $Ns $Ns $Ns $Nt 0 $filename
      if [ $? -eq "1" ]; then
         echo  "Something went terribly wrong!!!"
         break
      fi
      let j=j+1
   done
   j=1
   let i=i+1
done
#Once again, the output folder coincides with the input folder.
#We once again fix this
#mkdir -p $DIR/output/FFT_Stat
#find $DIR/output -name Stat*.fft | xargs mv -t $DIR/output/FFT_Stat/
cd $DIR
