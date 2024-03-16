## Creation of the conda environment

```{bash}
mamba create -n geochem r-essentials r-base -y

mamba activate geochem
```


## Installation of dependencies

```{bash}
# Install remotes packages for installing smwrGraphs
Rscript -e "install.packages('remotes')"

# Install smwrGraphs dependencies
Rscript -e "install.packages('KernSmooth')"

# Install smwrGraphs (https://code.usgs.gov/water/analysis-tools/smwrGraphs#package-installation)
Rscript -e "remotes::install_gitlab('water/analysis-tools/smwrData', host='code.usgs.gov')"
Rscript -e "remotes::install_gitlab('water/analysis-tools/smwrBase', host='code.usgs.gov')"
Rscript -e "remotes::install_gitlab('water/analysis-tools/smwrGraphs', host='code.usgs.gov')"

# Install kernel for Jupyter
Rscript -e "install.packages('IRkernel')"
```
