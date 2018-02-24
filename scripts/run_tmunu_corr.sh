#!/bin/bash

#This script assumes it is being run from inside the project root directory
DIR=/home/willian/git_repos/SU2SimSuit
DIR=`pwd`
#--------------------------------------------
Ns=8
Nt=8
BETA=2.8
filePath=$DIR/output/lat_conf/links008008008008beta2.80Sweep
COUNTER=1000
ENDCOUNTER=10000
STEP=10

#--------------------------------------------
cd $DIR/output
while [  $COUNTER -le $ENDCOUNTER ]; do #For each configuration
   echo "Processing lattice no. "$COUNTER
   COUNTERf=`printf '%09d' $COUNTER`
   
   $DIR/bin/tmunu_corr.run $Ns $Ns $Ns $Nt $BETA $filePath$COUNTERf.dat

   if [ $? -eq "1" ]; then
      echo  "Something went terribly wrong!!!"
      break
   fi
   let COUNTER=COUNTER+STEP
done
#Program output files on the same folder as input files.
#We fix this here
mkdir -p $DIR/output/T0iT0j
mv $DIR/output/lat_conf/*source*.dat $DIR/output/T0iT0j/
cd $DIR
