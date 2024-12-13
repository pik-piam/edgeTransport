#!/bin/bash

#SBATCH --qos=short
#SBATCH --job-name=compScen_newDef
#SBATCH --output=%x-%j.out
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=32000
#SBATCH --time=02:00:00

srun --ntasks=1 --nodes=1 Rscript cluster_newDefault_CompScen.R $1
