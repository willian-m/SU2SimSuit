#!/bin/bash

#This script assumes it is being run from inside the project root directory
DIR=/home/willian/git_repos/SU2SimSuit
DIR=`pwd`

Ns=$1
Nt=$2
filePath=$DIR/output/T0iT0j/links008008008008beta2.80Sweep
BETA=$3
COUNTER=$4
ENDCOUNTER=$5
STEP=$6

Nsf=`printf '%03d' $Ns`
Ntf=`printf '%03d' $Nt`
BETAf=`printf '%.2f' $BETA`
filePath=$DIR/output/T0iT0j/links"$Nsf""$Nsf""$Nsf""$Ntf"beta"$BETAf"Sweep
#--------------------------------------------
cd $DIR/output
i=1
j=1
while [ $COUNTER -le $ENDCOUNTER ]; do
   while [ $i -le 3 ]; do
      while [  $j -le 3 ]; do #For each configuration
         echo "Processing lattice no. "$COUNTER
         COUNTERf=`printf '%09d' $COUNTER`
         filename=$(ls $filePath$COUNTERf.datsource*ij$i$j.dat)
         sourcePos=$(echo $filename | awk  -F'[^0-9]+' '{OFS="."; print $9}') #IMPORTANT: change in file path may imply in change of print $N   
         $DIR/bin/FFT_Tmunu.run $Ns $Ns $Ns $Nt $sourcePos $filename
         if [ $? -eq "1" ]; then
            echo  "Something went terribly wrong!!!"
            break
         fi
         let j=j+1
      done
      j=1
      let i=i+1
   done
   i=1
   j=1
   let COUNTER=COUNTER+STEP
done
#Once again, the output folder coincides with the input folder.
#We once again fix this
echo 'Making Dir'$DIR/output/FFT_T0iT0j
mkdir -p $DIR/output/FFT_T0iT0j
find $DIR/output/T0iT0j/ -name *.fft | xargs mv -t $DIR/output/FFT_T0iT0j/
#mv $DIR/output/T0iT0j/*.fft $DIR/output/FFT_T0iT0j/
cd $DIR
