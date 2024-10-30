#!/bin/bash

#SBATCH --qos=short
#SBATCH --job-name=compScen_edgetPR
#SBATCH --output=%x-%j.out
#SBATCH --ntasks=4
#SBATCH --nodes=4
#SBATCH --ntasks-per-node=1
#SBATCH --mem=32000
#SBATCH --time=02:00:00

srun --ntasks=1 --nodes=1 Rscript compScenPR.R $1 "1" &
srun --ntasks=1 --nodes=1 Rscript compScenPR.R $1 "2" &
srun --ntasks=1 --nodes=1 Rscript compScenPR.R $1 "3" &
srun --ntasks=1 --nodes=1 Rscript compScenPR.R $1 "4" &

wait
