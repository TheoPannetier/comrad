#!/bin/bash
#SBATCH --nodes=1
#SBATCH --cpus-per-task=32
#SBATCH --time=00:10:00
#SBATCH --partition=gelifes
export OMP_NUM_THREADS=32
R CMD INSTALL --preclean comrad_1.4.0.tar.gz
Rscript test.R
