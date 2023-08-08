#!/bin/bash

# Sindiso Nyathi, Desiree LaBeaud, Jason Andrews, Shannon Bennet, Izabella Rezende, Panpim Thongsripon, Katie Walter, Renu Verma
# Dengue Evolution Project Analytic Pipeline.
# Run treetime on a bunch of files. 

# Quit if an error occurs. 
set -e

# Set the working directory. Working Directory is the main directory for LaBeaud Lab. 
cd /Users/sindiso/Library/CloudStorage/Box-Box/Sindiso Nyathis Files/Dengue Evolution/Phylodynamics/

# open input files
exec {fdA}<DENV_global/country_treetime_filenames_align.txt
exec {fdB}<DENV_global/country_treetime_filenames_dates.txt

while read -r -u "$fdA" lineA && read -r -u "$fdB" lineB
do
    treetime --aln "$lineA"  --dates "$lineB" --outdir DENV_global/Countrydatasets/
done

exec {fdA}>&- {fdB}>&- # close input files