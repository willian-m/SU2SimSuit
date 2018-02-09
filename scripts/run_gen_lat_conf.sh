#!/bin/bash

#This script assumes it is being run from inside the project root directory
DIR=/home/willian/git_repos/SU2SimSuit

Ns=8
Nt=8
BETA=2.8
filePath=$DIR/output/links008008008008beta2.80Sweep
NMC=10000
STEP=10

#--------------------------------------------
cd $DIR/output
$DIR/bin/gen_lat_conf.run $Ns $Ns $Ns $Nt $BETA H $NMC $STEP 
cd $DIR
