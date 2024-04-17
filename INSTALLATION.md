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

# Install devtools for downgrading ggplot2 (see https://stackoverflow.com/a/78098253)
mamba install r-devtools -y
Rscript -e "devtools::install_version('ggplot2', version='3.3.5')"

# Install a specific version of ggtern (see https://stackoverflow.com/a/75723535)
Rscript -e "devtools::install_version('ggtern', version='3.3.5', dependencies=FALSE)"

# Install data analysis packages
mamba install r-corrplot r-factoextra -y

# Install kernel for Jupyter
Rscript -e "install.packages('IRkernel')"
```


## Creation of environment.yml for further installations
```{bash}
conda env export --no-builds | grep -v "^prefix: " > environment.yml
```
