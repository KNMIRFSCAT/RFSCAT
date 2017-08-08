# RFSCAT
RFSCAT scatterometer simulation

This file describes how to install and use the rfscat software,
the KNMI software package for simulation of scatterometer instruments.
Note that this software depends on the genscat library which must be
installed first.
------------------------
Installation instructions for rfscat and genscat:

download both rfscat and genscat folders
Linux system:

> cd genscat
> ./Set_Makeoptions
> make 

> cd ../rfscat
> make

Usage:
==>define the simulation settings, these are defined in an ascii input
   file. Just take a copy of rfscat_settings.dat and modify it
   to fit your needs
==>run the program: ./rfscat_simulation <your rfscat settings file>
==>names of the output files are defined in the settings file


Testing data files (provided seperately): 
PL1B data
LUT (look up tables)
                
For big-endian (typically unix systems) you need to point the nrcs_luts
symbolic link to the directory  with LUT_bigendian


