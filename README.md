# Pilot data and results for the comprep-sport project

Pilot data, results, and precision estimation for the project "Computational 
Reproducibility in Sports Science".

# Repository Structure

## Data

- [pilot_inclusion.csv](data/pilot_inclusion.csv) holds the inclusion/exclusion 
data for the piloted articles.
- [pilot_pc.csv](data/pilot_pc.csv) holds the primary claims identified from the 
included articles during the pilot.
- [sim_results.Rds](data/sim_results.Rda) has the results of the simulation used
 for the precision estimation saved as a R data frame.

## Scripts

- [pilot_analysis.R](scripts/pilot_analysis.R): Analysis code for the pilot.
- [precision_estimation.R](scripts/precision_estimation.R): Analysis code for 
the simulation for precision estimation.

## Figures

- [pc_count.png](plots/pc_count.png): Histogram of primary counts per article in
the pilot.
- [pc_count_overlay.png](plots/pc_count_overlay.png): Histogram of primary 
counts per article in the pilot overlayed by a fitted negative binomial 
distribution.
- [sim_results.png](plots/sim_results.png): Histogram of the simulation results.

# License

The data in this repository is released under a 
[CC-BY](https://creativecommons.org/licenses/by/4.0/) license. The code is 
released under a [MIT](LICENSE.md) license.