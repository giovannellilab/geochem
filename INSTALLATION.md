## Creation of the conda environment

IMPORTANT NOTE: you can either use `conda` or `mamba`

```{bash}
mamba create -n geochem -y
```

* Installation on Mac (chip M1/2)

```{bash}
# Must be OSX64 for installing ggtern's dependencies
CONDA_SUBDIR=osx-64 conda create -n geochem -y
```


## Installation of dependencies

```{bash}
mamba activate geochem

# NOTE: Install a specific version of ggtern (see https://stackoverflow.com/a/75723535)
mamba install r-essentials r-base r-ggplot2=3.3.5 r-ggtern=3.3.5 -y

# Install other dependencies
mamba install r-rio r-corrplot r-factoextra r-irkernel -y
```


## Creation of environment.yml for further installations
```{bash}
conda env export --no-builds | grep -v "^prefix: " > environment.yml
```
