#!/bin/bash
#SBATCH --nodes=1
#SBATCH --cpus-per-task=24
#SBATCH --time=00:10:00
#SBATCH --partition=regular
export OMP_NUM_THREADS=24
R CMD INSTALL --preclean comrad_1.4.0.tar.gz
Rscript test.R
