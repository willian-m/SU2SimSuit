#!/bin/bash

#This script assumes it is being run from inside the project root directory
DIR=/home/willian/git_repos/SU2SimSuit

Ns=8
Nt=8
BETA=2.8
filePath=$DIR/output/links008008008008beta2.80Sweep
COUNTER=1000
ENDCOUNTER=10000
STEP=10

#--------------------------------------------
cd $DIR/output
while [  $COUNTER -le $ENDCOUNTER ]; do #For each configuration
   echo "Processing lattice no. "$COUNTER
   COUNTERf=`printf '%09d' $COUNTER`
   filename=$(ls $filePath$COUNTERf.datsource*)
   sourcePos=$(echo $filename | awk  -F'[^0-9]+' '{OFS="."; print $7}') #IMPORTANT: change in file path may imply in change of print $N   
   
   cd $DIR/output
   $DIR/bin/FFT_Tmunu.run $Ns $Ns $Ns $Nt $sourcePos $filename
   if [ $? -eq "1" ]; then
      echo  "Something went terribly wrong!!!"
      break
   fi
   let COUNTER=COUNTER+STEP
done
cd $DIR