#!/bin/bash

#This script assumes it is being run from inside the project root directory
DIR=/home/willian/git_repos/SU2SimSuit
DIR=`pwd`

#Size is N/2 + 1, since this is an average over a Fourier Transform
Ns=$1
Nt=$2

#--------------------------------------------
cd $DIR/output
i=1
j=1
COUNTER=$BEGINCOUNTER
while [ $i -le 3 ]; do
   while [  $j -le 3 ]; do #For each configuration
      #Link file in the appropiate place
      ln -s StatisticalAverage$i$j.dat StatisticalAverage.dat
      ln -s StatisticalError$i$j.dat StatisticalError.dat
      $DIR/bin/orbitAvrg.run $Ns $Ns $Ns $Nt
      if [ $? -eq "1" ]; then
         echo  "Something went terribly wrong!!!"
         break
      fi
      #Clearing the links for next iteration
      rm StatisticalAverage.dat StatisticalError.dat
      #Rename output file, so it will not be overwritten in next iteration
      mv orbitAveraged.out orbitAveraged$i$j.out
      let j=j+1
  done
  j=1
  let i=i+1
done

cd $DIR
