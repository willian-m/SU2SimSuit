#!/bin/bash

#This script assumes it is being run from inside the project root directory
DIR=/home/willian/git_repos/SU2SimSuit
DIR=`pwd`

Ns=8
Nt=8
BETA=2.8
filePath=$DIR/output/T0iT0j/links008008008008beta2.80Sweep
COUNTER=1000
ENDCOUNTER=10000
STEP=10

#--------------------------------------------
cd $DIR/output
i=1
j=1
while [ $i -le 3 ]; do
while [ $j -le 3 ]; do
   while [  $COUNTER -le $ENDCOUNTER ]; do #For each configuration
      echo "Processing lattice no. "$COUNTER
      COUNTERf=`printf '%09d' $COUNTER`
      filename=$(ls $filePath$COUNTERf.datsource*ij$i$j.dat)
      sourcePos=$(echo $filename | awk  -F'[^0-9]+' '{OFS="."; print $9}') #IMPORTANT: change in file path may imply in change of print $N   
   
      $DIR/bin/FFT_Tmunu.run $Ns $Ns $Ns $Nt $sourcePos $filename
      if [ $? -eq "1" ]; then
         echo  "Something went terribly wrong!!!"
         break
      fi
      let COUNTER=COUNTER+STEP
   done
done
done
#Once again, the output folder coincides with the input folder.
#We once again fix this
mkdir -p $DIR/output/FFT_T0iT0j
mv $DIR/output/T0iT0j/*.fft $DIR/output/FFT_T0iT0j/
cd $DIR
