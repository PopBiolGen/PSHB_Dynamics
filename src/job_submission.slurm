#!/bin/bash

#SBATCH --array=1-10

#SBATCH -p cascade

#SBATCH --time=32:00:00

#SBATCH --nodes=1

#SBATCH --ntasks=1

#SBATCH --cpus-per-task=18

#SBATCH --mem=100000

#SBATCH --mail-user=acoates@unimelb.edu.au
#SBATCH --mail-type=ALL,TIME_LIMIT_90

module purge
module load foss/2022a R/4.2.2

cd /data/cephfs/punim1173/salmonlice

Rscript --vanilla src/2loci/parameters_2loci.R ${SLURM_ARRAY_TASK_ID}