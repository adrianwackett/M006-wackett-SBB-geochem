# M006-wackett-SBB-geochem

This repository houses all of the data, R code, and research materials associated with the manuscript **New geochemical and geochronological insights on forearc magmatism across the Sanak-Baranof belt, Southern Alaska: A tale of two belts** authored by Adrian A. Wackett, Diane R. Smith, Cameron Davidson, and John I. Garver and published in [*Geosphere*](https://doi.org/10.1130/GES02642.1). 

This repo follows the standardized repo template available at [jelinski-lab-pedology/MXXX-project-template](https://github.com/jelinski-lab-pedology/MXXX-project-template). This standardized repo structure was created by Nicolas A. Jelinski at the University of Minnesota-Twin Cities and draws inspiration from many sources [^1][^2][^3][^4][^5][^6]. 

The repo structure modified for this project [M006-wackett-SBB-geochem] is as follows:

-   ./M006-wackett-SBB-geochem
    -   /00-data
        -  /a-raw
        -  /b-prepared 
    -   /01-code
        -   /a-Rscripts
    -   /02-output
        -   /a-figures
        -   /b-tables
    -   /03-support-files
        -   /supplement
    -   /04-manuscript
        -   /a-drafts
        -   /b-submitted
        -   /c-final-and-proofs
    -   \[LICENSE]
    -   \[M006-wackett-SBB-geochem.Rproj\]
    -   \[README.md\]


## Detailed table of contents with annotated descriptions

### /00-data

This folder houses all of the input data used in this project, both formatted and unformatted. 

#### /00-data/a-raw

Contains data tables with all of the new data reported as part of this study. Files are retrievable in either .xlsx or .xlsb formats that are unsuitable for direct processing in R. Files included are:

-   Suppl. Table S1, Major and trace element data, Final.xlsx
-   Suppl. Table S2, U Pb data, Final.xlsx
-   Suppl. Table S3, Hf Data, Final.xlsb
-   TABLE 1, SR AND ND DATA, FINAL.xlsx
-   TABLE 2, U PB AND HF DATA, FINAL.xlsx

#### /00-data/b-prepared

This folder contains tabular data in .csv format for direct use in R workflow. These files include:

-   Crawfish-eHf-age.csv
-   Crawfish-eNd-87Sr-age.csv
-   Crawfish-Ycn-age.csv
-   eastern-sbb-age-distance-zircons-final.csv
-   sbb-age-distance-all-samples.csv
-   sbb-age-distance-zircons-final.csv
-   sbb-SrY-distance-compilation-54SiO2-cutoff.csv
-   western-sbb-age-distance-zircons-final.csv

### /01-code

Contains all of the source code and R scripts for basic data processing and analysis, as well as generation of associated manuscript figures. *All analyses conducted using Version R 4.2.2 (2022-10-31).*

#### /01code/a-Rscripts

R scripts for reproducing all statistical analyses referenced in the final accepted version of this manuscript. *Note that the sequential numbering indicates the intended order of the scripts, beginning with 00-.* Files include:

-   00-prepare-workspace.R
-   01-global-declarations.R
-   02-qaqc-outlier-analysis.R
-   03-regression-analyses.R

### /02-output
Contains final, polished figures and tables that are referenced in the manuscript. Associated figure captions can be found at the end of the .pdf documents available in the /04-manuscript subfolders.

#### /02-output/a-figures
Contains final manuscript figures generated from workflows in IgPet [^7], ArcGIS, and R. All files are in .pdf format.

#### /02-output/b-tables
Data tables generated from workflow in Microsoft Excel and R. All files in .pdf format.

### /03-support-files
Contains supplementary materials associated with the final accepted version of the manuscript. The supplemental materials are also freely available through [*Zenodo*](https://doi.org/10.5281/zenodo.8241978)

#### /03-support-files/supplement
Contains a single zipped file which houses all of the supplemental materials associated with the manuscript. The supplemental materials are also freely available [here](https://doi.org/10.5281/zenodo.8241978) through the *Zenodo* data portal.

### /04-manuscript

#### /04-manuscript/a-drafts
Includes the original manuscript draft prior to initial submission to *Geosphere* on 25JAN2023. 

#### /04-manuscript/b-submitted
Includes the revised manuscript draft resubmitted to *Geosphere* on 13AUG2023.

#### /04-manuscript/c-final-and-proofs
Includes the conditionally accepted version of manuscript with initial proofs as of 08JAN2024.

### LICENSE
Standard open source MIT license.

### M006-wackett-SBB-geochem.Rproj
This .Rproj file is pre-configured to start with a *completely clean workspace EVERY TIME* by selecting in *Project Options > General* "Restore .RData into workspace at startup" = NO and "Save workspace to .RData on exit" = NO, "Disable .Pprofile execution on session start/resume" = CHECKED, "Quit child processes on exit" = CHECKED. This should help ensure other potential users of this repo do not experience unexpected errors, conflicts, or masks due to workspace-specific backgrounds[^1].

### README.md


[^1]: [Alex Douglas::Setting up a reproducible project in R](https://alexd106.github.io/intro2R/project_setup.html)
[^2]: [Ties de Kok::How to keep your research projects organized: folder structure](https://towardsdatascience.com/how-to-keep-your-research-projects-organized-part-1-folder-structure-10bd56034d3a)
[^3]: [Kenyon White::ProjectTemplate](https://github.com/KentonWhite/ProjectTemplate)
[^4]: [Project Template](http://projecttemplate.net/index.html)
[^5]: [Anna Krystalli::Projects in R Studio](http://projecttemplate.net/index.html)
[^6]: [Matt Dray & Anna De Palma - rostrum.blog::A GitHub repo template for R analysis](https://www.rostrum.blog/posts/2019-06-11-r-repo-template/)
[^7]: [Michael J. Carr & Esteban Gazel - Igpet software for modeling igneous processes](https://www.researchgate.net/publication/308486485_Igpet_software_for_modeling_igneous_processes_examples_of_application_using_the_open_educational_version)