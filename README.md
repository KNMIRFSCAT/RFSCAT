# RFSCAT

## RFSCAT scatterometer simulation

This file describes how to install and use the rfscat software,
the KNMI software package for simulation of scatterometer instruments.
This software depends on parts of the genscat library which is bundled
together with the rfscat software in this repository.

## Installation instructions for linux:

download the software from github as zipfile or clone it
from the git repository https://github.com/KNMIRFSCAT/RFSCAT

1. set up compilers (bash example):
```bash
export GENSCAT_LINK=gfortran
export GENSCAT_CLINK=gcc
export GENSCAT_SHLINK=gfortran
export GENSCAT_F90=gfortran
export GENSCAT_F77=gfortran
export GENSCAT_CC=gcc
```
or set up compilers (csh example):
```csh
setenv  GENSCAT_LINK   gfortran
setenv  GENSCAT_CLINK  gcc
setenv  GENSCAT_SHLINK gfortran
setenv  GENSCAT_F90    gfortran
setenv  GENSCAT_F77    gfortran
setenv  GENSCAT_CC     gcc
```
if you wish you can choose different fortran and c compilers that
you may have installed.

2. build the software
```
make 
```
If all runs well, the executable rfscat_simulation should be
present now inside the rfscat folder.

Usage:
define the simulation settings, these are defined in an ascii input
   file. Just take a copy of rfscat_settings.dat and modify it
   to fit your needs
   
run the program: ./rfscat_simulation <your rfscat settings file>

names of the output files are defined in the settings file

Please also refer to our scatterometer page for more information http://projects.knmi.nl/scatterometer/rfscat/ 

Testing data files: please contact scat@knmi.nl for

PL1B data and LUT (look up tables)

For big-endian (typically unix systems) you need to point the nrcs_luts
symbolic link to the directory  with LUT_bigendian

This software has been developed and tested in a linux environment.
It may run on other unix like systems (BSD, MacOS) but in its current
shape it will not compile on windows.
