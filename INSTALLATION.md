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

mamba install r-essentials r-base r-ggtern r-rio r-corrplot r-factoextra r-irkernel -y
```


## Creation of environment.yml for further installations
```{bash}
conda env export --no-builds | grep -v "^prefix: " > environment.yml
```
