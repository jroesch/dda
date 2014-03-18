#!/bin/bash

cd $PBS_O_WORKDIR
export NODES=`cat $PBS_NODEFILE`
mpirun bootstrap.bash $PARAMS
