# Overview

This repository contains the simulated data and code necessary to run the analyses and produce the figures for the manuscript, "Impact of unequal testing on vaccine effectiveness estimates across two study designs: a simulation study".

## Getting Started
The project includes five R scripts, all of which are located in the "code" folder.

The following three R scripts contain the code to run the agent-based model (ABM) and create the simulated data:

1. ***ABM_Main.R***: contains the main code to run the ABM and generate simulated datasets. This script calls on both ***ABM_mainFunctions.R*** and  ***Aggdf_mainFunctions.R***. 

2. ***ABM_mainFunctions.R***: contains code for the main ABM functions to generate the transmission network, assign healthcare engagement status, conduct transmission dynamics for SARS-CoV-2, conduct symptomatic SARS-CoV-2 testing, etc. 

3. ***Aggdf_mainFunctions.R***: contains code to aggregate individual-level information to  population-level data measurements.   

The following two R scripts are used to conduct the analyses and generate figures.

4. ***MainAnalyses_and_Figures.R***: contains the code to conduct the main analyses pertaining to objectives 1-3 and to generate figures 3-6.

5. ***AppendixAnalyses_and_Figures.R***: contains the code to conduct the supporting analyses found in the supplementary information and to generate all supplementary figures.

All simulated data necessary to recreate the analyses and the figures is located in the "data" folder; copies of all figures can be found in the "figures" folder.

### Dependencies

* Simulations and analyses were performed using R (R Core Team 2023) (version 4.3.1) with computations performed on the Niagara supercomputer at the SciNet HPC Consortium (Loken et al. 2010; Ponce et al. 2019).
* R packages necessary to recreate the analyses and figures include igraph (Csardi and Nepusz 2006) (version 1.5.1), graph4lg (Savary et al. 2021)(version 1.8.0) and data.table (Matt Dowle and Arun Srinivasan 2023) (version 1.14.8).



