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
(<https://posit.co/download/rstudio-desktop/>). On its start screen, the
consolidated app lets you select the data and mode-specific codebook
files for each coding session.

The app runs locally, so there are no privacy concerns when using it.

## Start a coding session

All three coding activities are available from one start screen. Open
`app.R` in RStudio, click **Run App**, and make the following
selections:

1. **Coding activity.** Choose **Deductive coding**, **Inductive
   coding: Phase 1**, or **Inductive coding: Phase 2**. This determines
   which codebook and coding fields are shown.
2. **Coder (user).** Enter a name or coder ID. This becomes part of the
   coded-data filename so different coders get separate files.
3. **Data file.** Select the CSV or Excel file containing the ESM data.
4. **Participant.** Choose the column containing participant IDs, then
   choose the participant to code from the IDs found in that column.
   Only that participant's observations are loaded into the coding
   table.
5. **Display columns.** After selecting the data file, choose which of
   its columns should appear in the coding table. The columns normally
   used by the selected coding activity are selected by default.
6. **Coding variables and codebooks.** For deductive coding and inductive
   Phase 2, enter each variable name manually and click **Add variable**;
   repeat for as many
   variables as needed.
   See [Create a codebook for a variable](#create-a-codebook-for-a-variable)
   for the required format and examples.
   Add at least one variable and supply one codebook file for each.
   Variable names must be unique; spaces and punctuation are converted
   to dots in saved column names. Inductive Phase 1 does not use a codebook.
7. Click **Start Coding**. The coded CSV is saved automatically in the
   same folder as the selected data file.

At least one display column must be selected. Coding-entry columns are
always included in the table and are not affected by this selection.

Both the coder and participant ID are required. Leading and trailing
spaces are ignored. Because these values are used in file names, they
cannot contain `< > : " / \ | ? *`.

Use **Change coding activity** above the coding table to return to the
start screen. This reloads the app, so the activity, coder, participant,
and input files can be selected again.

## Prepare the input files

Input files may be stored in any folders and may have any names. CSV
(`.csv`) and Excel (`.xls` or `.xlsx`) files are supported. The app
reports missing required columns before starting the coding table.

The selected data file must contain a participant ID column. If it is
named `Participant_ID`, the app selects it by default; otherwise, the
first data column is selected initially and can be changed. The app
orders the selected data by the chosen participant ID column and `Obs`.

All activities require a selected participant ID column plus `Day`,
`Obs`, and `Time1`. Deductive and inductive Phase 2 coding variables do
not require matching input columns or translations; choose the data to
show using **Display columns**. Inductive Phase 1 also requires `Thought`,
`Activity`, `Location`, `Company`, and `Event`.

The selected activity determines which codebooks are required:

- **Deductive coding** requires one codebook file per manually added
  coding variable.
- **Inductive coding: Phase 1** does not load a codebook.
- **Inductive coding: Phase 2** requires one codebook file per manually
  added coding variable, just like deductive coding.

Codebooks must contain a column named `Code`. For all codebooks,
`Level` is optional. Levels are displayed in different colors when supplied.

### Create a codebook for a variable

Create a spreadsheet with a column headed exactly `Code` and enter one
selectable code per row. For example, a codebook for a variable
named **Emotion** could contain:

```csv
Code
Happiness
Sadness
Anger
```

If your codes have a hierarchy, add an optional column headed exactly
`Level`, using `1`, `2`, or `3` to give each level a distinct display
color. For example:

```csv
Code,Level
Positive emotion,1
Happiness,2
Excitement,2
Negative emotion,1
Sadness,2
```

Save the codebook as a comma-separated CSV (`.csv`) or an Excel file
(`.xls` or `.xlsx`). For Excel, put the codebook on the first worksheet,
with the column headers in the first row. On the app's start screen,
choose **Deductive coding** or **Inductive coding: Phase 2**, enter
**Emotion** under **Coding variable name**, and click **Add variable**.
Then select your file in the
**Emotion codebook** picker. Create and supply a separate codebook for
each variable you add.

## Coder and participant result files

The app creates one CSV result file per coder and participant in the
same folder as the selected data file. Its name combines the input
file's name (without its extension), the participant ID, and the coder:

``` text
<original_name>_<participant_id>_<coder>.csv
```

Each result file contains the selected participant's complete input
data followed by the coding columns. If a merged result file already
exists, it is loaded so coding can continue where it stopped. Otherwise,
the app creates it with empty (`NA`) coding values.

Deductive and inductive Phase 2 results include a `Code_<variable name>`
column for each selected variable. Deductive coding also includes general
and depth comment columns. Inductive Phase 2 keeps the beep and day
familiarization notes and adds a `Proposed_<variable name>` field for each
variable to record proposed new codes.

When resuming with a different variable selection, existing codes are matched
by column name, newly selected variables get empty columns, and saved
columns for removed variables are preserved. Use the same
variable names to resume their coding.

Earlier Phase 2 result files retain their `Existing.event.code` and
`Proposed.event.code` columns. These are preserved in the CSV; new sessions
use the variable-specific columns described above.

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
