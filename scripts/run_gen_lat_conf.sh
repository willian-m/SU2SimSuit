#!/bin/bash

#This script uses the following folder as root directory of the project
DIR=/home/willian/git_repos/SU2SimSuit
DIR=`pwd`
Ns=8
Nt=8
BETA=2.8
#filePath=$DIR/output/links008008008008beta2.80Sweep
NMC=10000
STEP=10

#--------------------------------------------
mkdir -p $DIR/output
mkdir -p $DIR/output/lat_conf
cd $DIR/output/lat_conf
$DIR/bin/gen_lat_conf.run $Ns $Ns $Ns $Nt $BETA H $NMC $STEP 
cd $DIR
