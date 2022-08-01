#!/bin/bash
# Submission script for NIC5
#SBATCH --job-name=test
#SBATCH --output=res.txt
#SBATCH --time=00:10:00 # hh:mm:ss
#
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --mem-per-cpu=1024 # 1GB
#SBATCH --partition=Low
#
#SBATCH --mail-user=bichngoc.nguyen@uliege.be
#SBATCH --mail-type=ALL


module load R


Rscript pop_synth_cluster.R "Data/pop_synth" "BH" 1 2
