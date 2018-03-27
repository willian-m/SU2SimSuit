#!/bin/bash

#This script assumes it is being run from inside the project root directory
DIR=/home/willian/git_repos/SU2SimSuit
DIR=`pwd`

#Size is N/2 + 1, since this is an average over a Fourier Transform
Ns=$1
Nt=$2
BETA=$3
Ps=$4
Pt=$5
#filePath=$DIR/output/FFT_T0iT0j/links008008008008beta2.80Sweep
BEGINCOUNTER=$6
ENDCOUNTER=$7
STEP=$8
NBINS=$9


Nsf=`printf '%03d' $Ns`
Ntf=`printf '%03d' $Nt`
BETAf=`printf '%.2f' $BETA`
filepath=$DIR/output/FFT_T0iT0j/links"$Nsf""$Nsf""$Nsf""$Ntf"beta"$BETAf"Sweep
#--------------------------------------------
cd $DIR/output
i=1
j=1
COUNTER=$BEGINCOUNTER
while [ $i -le 3 ]; do
   while [  $j -le 3 ]; do #For each configuration
      #Link file in the appropiate place
      while [ $COUNTER -le $ENDCOUNTER ]; do
         COUNTERf=`printf '%09d' $COUNTER`
         filename=$(ls $filepath$COUNTERf.datsource*ij$i$j.datinverted.fft)
<<<<<<< HEAD
#         echo $filename
=======
         echo $filename
>>>>>>> 11032376fa265263fe636415c45034294f274dce
         ln -s $filename fort.$COUNTER
         let COUNTER=COUNTER+STEP
      done
      #Executes the program
      $DIR/bin/statisticalAverager.run $Ps $Ps $Ps $Pt $BEGINCOUNTER $ENDCOUNTER $STEP $NBINS
#      sleep 5s
      if [ $? -eq "1" ]; then
         echo  "Something went terribly wrong!!!"
         break
      fi
      #Clearing the links for next iteration
      rm fort.*
      #Rename output file, so it will not be overwritten in next iteration
      mv StatisticalAverage.dat StatisticalAverage$i$j.dat
      mv StatisticalError.dat StatisticalError$i$j.dat
      COUNTER=$BEGINCOUNTER
      let j=j+1
  done
  j=1
  let i=i+1
done

cd $DIR
