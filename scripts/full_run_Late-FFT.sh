#!/bin/bash

#This script will call the others to perform a full analysis, from lattice generation to result ploting
#We assume to be running from a folder with a "bin" and a "script" folder, which conatins the necessary 
#binaries and scripts
#IMPORTANT: Your locale must be set to en_US for our scripts to work. Possibly others may work as well, but are not tested. The curcial thing is that your shell prints the decimal point as a dot. For instance, pt_BR uses a comma and these scripts fail on it.
##===> TIP: You can bypass the system locale by editing or adding the file ~/.configs/locale.conf. The changes will take effect at next login. for immediate effect, unset LANG variable and then source /etc/profile.d/locale.sh

Ns=8
Nt=8
BETA=2.8
Ps=5 #Ns/2 + 1 => Number of momentum
Pt=5
NMC=3000
THERM=1000
STEP=10
NBINS=20

#./scripts/run_gen_lat_conf.sh $Ns $Nt $BETA $NMC $STEP
#./scripts/run_tmunu_corr.sh $Ns $Nt $BETA $THERM $NMC $STEP 
#./scripts/run_stat_avrg_dble.sh $Ns $Nt $BETA $Ns $Nt $THERM $NMC $STEP $NBINS
./scripts/run_FFT_Stat.sh $Ns $Nt 
./scripts/run_orb_avrg_stat.sh $Ps $Pt
gnuplot plots/make_plots.plt
