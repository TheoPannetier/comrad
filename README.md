# comrad SIMD
we build the .cpp code with the `-march=native` compiler flag. This tells the compiler to select the highest SIMD-version supported by the CPU on which the compiler runs. On peregrine, gelifes nodes are slightly different from regular nodes and the login node (AMD vs Intel CPUs). To get the best out of these CPUs, we should compile our package on the node we run the simulations, not the login node. Thus, we shift the the build step into the submit-scripts:
```
submit-regular.sh

#!/bin/bash
#SBATCH --nodes=1
#SBATCH --cpus-per-task=24
#SBATCH --time=00:10:00
#SBATCH --partition=regular
export OMP_NUM_THREADS=24
R CMD INSTALL --preclean comrad_1.4.0.tar.gz
Rscript test.R
```
```
submit-gelifes.sh

#!/bin/bash
#SBATCH --nodes=1
#SBATCH --cpus-per-task=32
#SBATCH --time=00:10:00
#SBATCH --partition=gelifes
export OMP_NUM_THREADS=32
R CMD INSTALL --preclean comrad_1.4.0.tar.gz
Rscript test.R
```
Note that `--cpus-per-task` and `export OMP_NUM_THREADS` must match.
`comrad_1.4.0.tar.gz` is the source-package that could be build with `R CMD build` on any machine.


comrad
======

R package to simulate a COMpetitive RADiation, based on the eco-evolutionary
model of Pontarp et al. (2012)


Branch|[![Travis CI logo](pics/TravisCI.png)](https://travis-ci.org)|[![Codecov logo](pics/Codecov.png)](https://www.codecov.io)
---|---|---
`master`|[![Build Status](https://travis-ci.org/TheoPannetier/comrad.svg?branch=master)](https://travis-ci.org/TheoPannetier/comrad)|[![Codecov test coverage](https://codecov.io/gh/TheoPannetier/comrad/branch/master/graph/badge.svg)](https://codecov.io/gh/TheoPannetier/comrad?branch=master)
`develop`|[![Build Status](https://travis-ci.org/TheoPannetier/comrad.svg?branch=develop)](https://travis-ci.org/TheoPannetier/comrad)|[![Codecov test coverage](https://codecov.io/gh/TheoPannetier/comrad/branch/develop/graph/badge.svg)](https://codecov.io/gh/TheoPannetier/comrad?branch=develop)
  
