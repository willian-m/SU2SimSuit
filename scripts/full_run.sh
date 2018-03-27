#!/bin/bash

#This script will call the others to perform a full analysis, from lattice generation to result ploting
#We assume to be running from a folder with a "bin" and a "script" folder, which conatins the necessary 
#binaries and scripts

Ns=8
Nt=8
BETA=2.8
Ps=5 #Ns/2 + 1 => Number of momentum
Pt=5
<<<<<<< HEAD
NMC=10000
=======
NMC=1500
>>>>>>> 11032376fa265263fe636415c45034294f274dce
THERM=1000
STEP=10
NBINS=20

./scripts/run_gen_lat_conf.sh $Ns $Nt $BETA $NMC $STEP
./scripts/run_tmunu_corr.sh $Ns $Nt $BETA $THERM $NMC $STEP 
<<<<<<< HEAD
./scripts/run_FFT_tmunu.sh $Ns $Nt $BETA $THERM $NMC $STEP
=======
./scripts/run_FFT_tmunu.sh $Ns $Nt $THERM $NMC $STEP
>>>>>>> 11032376fa265263fe636415c45034294f274dce
./scripts/run_statisticalAverager.sh $Ns $Nt $BETA $Ps $Pt $THERM $NMC $STEP $NBINS
./scripts/run_orbitAvrg.sh $Ps $Pt
gnuplot plots/make_plots.plt
