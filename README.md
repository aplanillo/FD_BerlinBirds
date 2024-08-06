<h1> Data and Scripts to run the functinal diversity analyses of the breeding bird community in Berlin </h1>

These analyses focuses on three aspect of functional diversity: Functional richness, functional evenness and functional divergence.
We analysed the response of the whole community, as well as for three functional groups: urban exploiters, urban adapters and urban avoiders, to the urbanization gradient at three spatial scales and accounting for indirect effects.

The scripts are prepared to run from the R project <code> FD_BerlinBirds </code> and are found in the folder <code> rscripts </code>. For the code to work properly, it is recomended to run first the ordination scripts and then, the SEMs scripts. 

Folder <code> data </code>:
Contains the data necessary for the analyses:
- _grid_birds_abundance_corrected_: Abundance of bird species from the breeding bird monitoring program in Berlin.
- _birds_all_traits_: functional traits for the species.
- _species_groups_responses_: classification of the species into the functional groups.
- _grids_environmental_variables_: environmental data extracted at monitoring grid scale (1x1km).
- _territories_environmental_variables_: environmental data extracted at the centroid of the bird territory scale (100x100m).
- _locations_environmental_variables_20m_: environmental data extracted at the exact location of the birds (10x10m).

Folder <code> rscripts </code>:

- source_packages.R: contains all the R packages to run the scripts.
- source_functions.R: self-made functions to use in the scripts.
- **FD_ordination_allbirds.Rmd**: Ordination analysis of the whole bird community, functional traits and environmental effects.
- **FD_ordination_groups.Rmd**: Ordination analysis of the three functional groups, functional traits and environmental effects.
- **SEMs_allbirds.Rmd**: Structural Equation Models for the whole bird community, analysing direct and indirect effects of human disturbance in the urbanization gradient, mediated by vegetation structure.
- **SEMs_groups.Rmd**: Structural Equation Models for the three functional groups, analysing direct and indirect effects of human disturbance in the urbanization gradient, mediated by vegetation structure.
