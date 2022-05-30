# Stranding of marine megafauna in SE Brazil, 2015--2020

Here you will find all data and code needed to reproduce the results from Prado et al. (2022).

Based on systematic daily beach surveys carried out during five consecutive years along >800 km of coastline, we aimed to describe the TSP of sea turtle, seabird, and cetacean strandings in southeast Brazil. We adopted an exploratory modelling approach using temporal and spatial explanatory variables to discuss possible factors influencing stranding patterns. We expect that our results will serve as a baseline to monitor deviations in stranding numbers between areas and over time, helping to track environmental and biological changes, as well as anthropogenic threats.

***
## Project structure

Workspace is set as follows, and we used an .RProj file to wrap it.

```shell
Strandings_PMP-BS_2015_2020
├── README.md
├── data
│   ├── shape_beach-survey-transects_PMP
│   │   └── …
│   ├── shape_brazil
│   │   └── ...
│   ├── effort_ … .csv
│   ├── strandingDatasetPMP_BS_2015_2020_ … .xlsx
│   ├── acuracySpeciesIdentificationDataset.xlsx
├── data_out
│   ├── acuracySpeciesIdentificationDataset_IUCN_Names.csv
│   ├── dfFinal.csv
│   ├── dfSumFinal.csv
│   ├── originalStretches.gpkg
│   ├── sectorsPolygon.gpkg
│   ├── sectorsPolygonLines.gpkg
│   └── thisWorkStretches.gpkg
├── results
│   └── …
├── script
│   ├── 01_data_wrangling.R
│   ├── 02_study_area_fig1.R
│   ├── 03_results_tableS1.R
│   ├── 04_results_SIMBA_accuracy.R
│   ├── 05_results_tableS2.R
│   ├── 06_results_tableS3.R
│   ├── 06_results_tableS3.R
│   ├── 07_results_summary_efffort_stranding.R
│   ├── 08_results_fig2.R
│   ├── 09_results_fig3.R
│   ├── 10_results_figs_S1_S2.R
│   ├── 11_results_figS3.R
│   ├── 12_results_figs_S4_S5.R
│   └── 13_GAMLSS_figs_4_5_S6_S7.R
├── StrandingsTetrapodsPMP20152020_ICES.Rproj
└── sessionInfo.txt
```

`data` has raw files, and `data_out` is the data after the wrangling processes (see `script/01_data_wrangling.R`). You should be able to reproduce all results using scrips in `script` and files from `data_out`.

`sessionInfo.txt` lists all packages, versions, and dependencies needed.

---
## Contributors

This code was created by [Jonatas F. H. Prado](https://github.com/JonatasPrado) and [Nicholas W. Daudt](https://github.com/nwdaudt).

***
## Citation
Please refer to the original article if using any piece of this repository.

Prado, J.H.F.; Daudt, N.W.; Perez, M.S.; Castilho, P.V.; Monteiro, D.S. (2022) Intensive and wide-ranging beach surveys uncover temporal and spatial stranding patterns of marine megafauna. *ICES J. Mar. Sci.*
