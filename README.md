# MoveTraitsDatabase

Developmental version of MoveTraits database

# Description

The MoveTrait database uses movement data and a standardized workflow to extract a suite of simple, standardized and comparable movement metrics across species. 
The database is introduced in the manuscript "Movement traits: Biologging unlocks integration of animal behaviour into trait-based ecology" (preprint link).

In its first Version (V0.1) the database used open access movebank studies (license types CC_0, CC_BY, CC_BY_NC) and an open-access dataframe published alongside Tucker et al. 2023 (https://www.science.org/doi/abs/10.1126/science.abo6499) and publicly available for download under: https://zenodo.org/records/7704108, file *Tucker_Road_Spatial.rds* as input. We intend to integrate traits from closed-access datasets in the future.

The traits currently extracted are displacement distances at different time scales, maximum displacement distances over set periods, range sizes, intensity of use, and diurnality. Please refer to Table S1 for a full overview. All traits are summarized at the species and individual level as mean, median, CV, 05th and 95th percentile. We intend to implement a wider range of trait extractions in the future.

# Input Data

*Tucker_Road_Spatial.rds*

Movebank studies to download are idnetified in *01a_movebank_studies_to_download.R* and are downloaded in *01b_Download_movebank_data.R* 

# Output Data

Traits database at three hierarchichal levels: species summaries, individual summaries, underlying within-individual traits as nested dataframe

*MoveTrait.v0.1_species.sum_03032025.rds*
*MoveTrait.v0.1_individual.sum_03032025.rds*
*MoveTrait.v0.1_withinindividual_03032025.rds*


# Instructions for contributions

As we are many, and to not lose oversight, I would like to ask all to provide contributions as follows:

1. create a Fork from this repository (button "Fork" at the top right)
2. make changes in your Fork (that will be on your github account). You can clone your github repository on RStudio (read how to do it e.g. [here](https://happygitwithr.com/rstudio-git-github.html)).
3. make a Pull Request to the original repository (button "Contribute" top left).
4. use the button "Sync fork" to keep up to date with the original repository.
   
