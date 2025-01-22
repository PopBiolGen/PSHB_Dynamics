#!/bin/bash

#SBATCH --account=pawsey1103

#SBATCH --array=1-8

#SBATCH --time=23:00:00

#SBATCH --nodes=1

#SBATCH --ntasks=1

#SBATCH --cpus-per-task=4

#SBATCH --mem=200G

#SBATCH -o ./output%j.%a.out

#SBATCH --mail-user=andrew.coates@curtin.edu.au
#SBATCH --mail-type=ALL,TIME_LIMIT_90

module load r/4.3.0

module load gdal/3.7.3-ztwnpws
module load geos/3.12.0-wntgupl
module load .proj/9.2.1-svjrii5
module load .udunits/2.2.28-lffxtmr
module load jags/4.3.0-esckzev

cd $MYSCRATCH/PSHB

Rscript src/spartan_call.R ${SLURM_ARRAY_TASK_ID}