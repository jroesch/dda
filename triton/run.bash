#!/bin/bash

qsub -l nodes=$1:ppn=$2 -l walltime=0:05:00 -vPARAMS="${@:3}" -o output -e err -V mpirun.bash
