## Creation of the conda environment

IMPORTANT NOTE: you can either use `conda` or `mamba`

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

# Install R dependencies
mamba install r-rio r-devtools r-corrplot r-factoextra r-irkernel -y

# Install a specific version of ggplot2 (see https://stackoverflow.com/a/78098253)
Rscript -e "devtools::install_version('ggplot2', version='3.3.5')"

# Install a specific version of ggtern (see https://stackoverflow.com/a/75723535)
Rscript -e "devtools::install_version('ggtern', version='3.3.5', dependencies=FALSE)"
```


## Creation of environment.yml for further installations
```{bash}
conda env export --no-builds | grep -v "^prefix: " > environment.yml
```
