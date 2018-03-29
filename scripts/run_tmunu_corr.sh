#!/bin/bash

#This script assumes it is being run from inside the project root directory
DIR=/home/willian/git_repos/SU2SimSuit
DIR=`pwd`
#--------------------------------------------
Ns=$1
Nt=$2
BETA=$3
#filePath=$DIR/output/lat_conf/links008008008008beta2.80Sweep
COUNTER=$4
ENDCOUNTER=$5
STEP=$6


Nsf=`printf '%03d' $Ns`
Ntf=`printf '%03d' $Nt`
BETAf=`printf '%.2f' $BETA`
filePath=$DIR/output/lat_conf/links"$Nsf""$Nsf""$Nsf""$Ntf"beta"$BETAf"Sweep

#--------------------------------------------
cd $DIR/output
while [  $COUNTER -le $ENDCOUNTER ]; do #For each configuration
   echo "Processing lattice no. "$COUNTER
   COUNTERf=`printf '%09d' $COUNTER`
   echo $filePath$COUNTERf.dat
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
find $DIR/output/lat_conf/ -name \*source\*.dat | xargs mv -t $DIR/output/T0iT0j/
#mv $DIR/output/lat_conf/*source*.dat $DIR/output/T0iT0j/
cd $DIR
