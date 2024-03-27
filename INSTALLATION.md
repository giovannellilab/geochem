## Creation of the conda environment

```{bash}
mamba create -n geochem r-essentials r-base -y
```

* Installation on Mac (chip M1/2)

```{bash}
# Must be OSX64 for installing ggtern's dependencies
CONDA_SUBDIR=osx-64 conda create -n geochem r-essentials r-base -y
```


## Installation of dependencies

```{bash}
mamba activate geochem

# Install rio for importing data
Rscript -e "install.packages('rio')"

# Install remotes packages for installing smwrGraphs
Rscript -e "install.packages('remotes')"

# Install dependencies for smwrGraphs
mamba install r-akima -y

# Install smwrGraphs (https://code.usgs.gov/water/analysis-tools/smwrGraphs#package-installation)
Rscript -e "remotes::install_gitlab('water/analysis-tools/smwrData', host='code.usgs.gov')"
Rscript -e "remotes::install_gitlab('water/analysis-tools/smwrBase', host='code.usgs.gov')"
Rscript -e "remotes::install_gitlab('water/analysis-tools/smwrGraphs', host='code.usgs.gov')"

# Install ggtern for the ternary plots
mamba install r-ggtern -y

# Install devtools for downgrading ggplot2 (see https://stackoverflow.com/a/78098253)
mamba install r-devtools -y
Rscript -e "devtools::install_version('ggplot2', version='3.4.4')"

# Install data analysis packages
mamba install r-corrplot r-factoextra -y

# Install kernel for Jupyter
Rscript -e "install.packages('IRkernel')"
```
