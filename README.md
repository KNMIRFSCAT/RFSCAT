# RFSCAT
RFSCAT scatterometer simulation

This file describes how to install and use the rfscat software,
the KNMI software package for simulation of scatterometer instruments.
Note that this software depends on the genscat library which must be
installed first.

Installation instructions for rfscat and genscat:

download both rfscat and genscat folders
Linux system:


1: cd genscat

set up compilers (exsample):

GENSCAT_LINK=gfortran
export GENSCAT_LINK

GENSCAT_CLINK=gcc
export GENSCAT_CLINK

GENSCAT_SHLINK=gfortran
export GENSCAT_SHLINK

GENSCAT_F90=gfortran
export GENSCAT_F90

GENSCAT_F77=gfortran
export GENSCAT_F77

GENSCAT_CC=gcc
export GENSCAT_CC

2: ./Set_Makeoptions

   make 

3: cd ../rfscat

   make

Usage:
define the simulation settings, these are defined in an ascii input
   file. Just take a copy of rfscat_settings.dat and modify it
   to fit your needs
   
run the program: ./rfscat_simulation <your rfscat settings file>

names of the output files are defined in the settings file


Testing data files: please contact scat@knmi.nl for

PL1B data and LUT (look up tables)
                
For big-endian (typically unix systems) you need to point the nrcs_luts
symbolic link to the directory  with LUT_bigendian


