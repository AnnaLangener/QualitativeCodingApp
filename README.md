## Shiny App to Code Qualitative ESM data


[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21043127.svg)](https://doi.org/10.5281/zenodo.21043127)


Here we provide a shiny app that can be used by other researchers to
code qualitative ESM data based on a given coding scheme. It contains
several columns showing the raw observation (see screenshots below
“activity_open”) and the given code (“Code”). To accommodate information from different coding dimensions (e.g., company mentioned alongside activities), our app includes two additional columns for capturing such information. The first column, “Additional Information,” is intended for details about participants' feelings and the social context during activities. The second column, “Other/Comments,” can be used for any other relevant information or observations. Our codes have a
hierarchical structure and are therefore displayed in different colors.

This app was build as part of the project “Assessing Daily Life
Activities: Comparing Predefined Categories with the Qualitative
Analysis of Open-Ended Responses in Experience Sampling Methodology
(ESM)” Marie Stadel, Anna M. Langener, Katie Hoemann, Laura Bringmann.

![](Images/Example_app1.jpg)

To facilitate the coding process, a list of codes appears when clicking on the “Code” field, and matching codes are suggested interactively while typing.

<img src="Images/Example_app2.png" width="326" />

To use this app, download the repository and open `app.R` in RStudio
(<https://posit.co/download/rstudio-desktop/>). The consolidated app
loads the data and mode-specific codebooks from a project folder that
you select when starting a coding session.

The app runs locally, so there are no privacy concerns when using it.

## Start a coding session

All three coding activities are available from one start screen. Open
`app.R` in RStudio, click **Run App**, and make the following
selections:

1. **Coding activity.** Choose **Deductive coding**, **Inductive
   coding: Phase 1**, or **Inductive coding: Phase 2**. This determines
   which codebook and coding fields are shown and where the results are
   saved.
2. **Coder (user).** Enter a name or coder ID. A new coder name creates
   a new results folder. Reusing the same name opens that coder's
   existing results, if present.
3. **Participant ID.** Enter an ID exactly as it occurs in the
   `Participant_ID` column of the data. Only that participant's
   observations are loaded into the coding table.
4. Click **Choose project folder and start**, then select the project
   folder that contains the data and codebooks described below. The app
   reads the required files from this folder; the data file itself is
   not selected separately.

Both the coder and participant ID are required. Leading and trailing
spaces are ignored. Because these values are used in folder and file
names, they cannot contain `< > : " / \ | ? *`.

Use **Change coding activity** above the coding table to return to the
start screen. This reloads the app, so the activity, coder, participant,
and project folder can be selected again.

## Prepare the project folder

Every activity reads
`20250819_Swinging_Moods_ESM_data_anonymized.xlsx` from the root of the
selected project folder. The data must contain a `Participant_ID`
column. The app orders the selected data by `Participant_ID` and `Obs`
and reports an error if the entered participant ID is not present.

All activities require the columns `Participant_ID`, `Day`, `Obs`,
`Time1`, `Thought`, `Activity`, `Location`, and `Company`. Deductive
coding additionally requires `Thought_ENG`, `Activity_ENG`,
`Location_ENG`, and `Company_ENG`; both inductive phases require
`Event`.

The selected activity determines which additional files are required:

- **Deductive coding** reads these files from `Codebooks/`:
  `Thoughts_Delespaul1995.csv`, `Activities_ATUS2024.csv`,
  `Locations_Stadel2024.csv`, and
  `SocialContext_Delespaul1995.csv`.
- **Inductive coding: Phase 1** does not load a codebook.
- **Inductive coding: Phase 2** reads
  `Inductive coding/Codebook_ShinyApp_02042026.xlsx`.

Codebooks must contain a column named `Code`. The deductive thought and
activity codebooks must also contain a `Level` column; those levels are
displayed in different colors.

For example, the selected project folder should have this structure:

``` text
project folder/
├── 20250819_Swinging_Moods_ESM_data_anonymized.xlsx
├── Codebooks/
│   ├── Thoughts_Delespaul1995.csv
│   ├── Activities_ATUS2024.csv
│   ├── Locations_Stadel2024.csv
│   └── SocialContext_Delespaul1995.csv
└── Inductive coding/
    └── Codebook_ShinyApp_02042026.xlsx
```

Only the files needed for the selected coding activity have to be
present.

## Coder and participant result files

The app creates one CSV result file per coder and participant. If that
file already exists, it is loaded so coding can continue where it
stopped. Otherwise, the app creates it with empty (`NA`) coding values.

The result path depends on the selected activity:

``` text
Deductive coding:          <project>/<coder>/Act_<participant>.csv
Inductive coding: Phase 1: <project>/Inductive coding/Phase 1/<coder>/Act_<participant>.csv
Inductive coding: Phase 2: <project>/Inductive coding/Phase 2/<coder>/Act_<participant>.csv
```

## Analyze the data

This repository also contains some simple R code to start analyzing the
data:
[Results_Analysis.Rmd](https://github.com/AnnaLangener/QualitativeCodingApp/blob/Anna_Tests/Results_Analysis.Rmd "Results_Analysis.Rmd").
For example, we provide some code to read the results from each
participant and calculate the frequency of how often a particular code
was used.

## Questions/Problems?

Write an email to anna.m.langener\[at\]dartmouth.edu, langener95\[at\]gmail.com,
or m.stadel\[at\].tilburguniversity.edu
