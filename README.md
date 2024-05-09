# geochem

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License: GPLv3](https://img.shields.io/badge/license-GPLv3-blue.svg)](LICENSE.md)
[![giovannellilab](https://img.shields.io/badge/BY-Giovannelli_Lab-blue)](https://www.donatogiovannelli.com/)

Geochemistry pipeline for processing data from IRMS, IC and ICP-MS.


## Installation

#### Creation of a conda environment

Creating a conda environment simplifies having different R and packages versions.
It is strongly recommended to create an environment and install all required packages in there.

``` bash
conda create -n geochem -y

# NOTE: for Macs with M1/M2 chips
CONDA_SUBDIR=osx-64 conda create -n geochem -y
```

#### Installation of dependencies

``` bash
conda activate geochem
conda install r-base r-essentials r-ggtern r-rio -y
```

#### Installation of `geochem`

Installation of the package can be done by running the following code in `R`:

``` r
devtools::install_github("giovannellilab/geochem", dependencies=TRUE)
```
