# VaP_analysis_code
Analysis code for a three-group cohort study comparing healthy controls (HC),
cerebrovascular disease without parkinsonism (CVNP) and vascular parkinsonism
(VaP), with external validation in UK Biobank (UKB).

## Study overview

- Discovery cohort: hospital-based HC / CVNP / VaP participants.
- External validation: UK Biobank HC / CVNP / VaP participants.
- Primary comparisons: VaP vs HC and VaP vs CVNP.
- Secondary analyses: VaP subtype comparisons and mediation by white matter
  hyperintensity (WMH) burden.

## Repository contents

| File | Description |
|---|---|
| `GDPH_analysis_v2.R` | Discovery cohort pipeline: Table 1, adjusted logistic regression, metadeconfoundR, bootstrap mediation, figures. |
| `UKB_validation_v2.R` | UK Biobank validation pipeline using identical methods. |
| `README.md` | This file. |

The scripts write results to `Output/`, `Figures/`, `Output_UKB/` and
`Figures_UKB/`, which are created automatically.

## Data availability

**Raw participant data are not included in this repository** because they
contain sensitive clinical information and are subject to ethical restrictions.
The scripts expect the following input files in the working directory:

Discovery cohort (`GDPH_analysis_v2.R`):
- `HC data for R revised.xlsx`
- `CVNP patients for R.xlsx`
- `VaP patients for R.xlsx`

UK Biobank validation (`UKB_validation_v2.R`):
- `Z121_only_UKB_controls_for_R.xlsx`
- `I6_withoutG_controls_for_R.xlsx`
- `67 UKB patients for R.xlsx`

Each file must contain the columns referenced in the `vars_*` vectors near the
top of each script (for example `Age`, `BMI`, `Sex`, `Smoking`, `Drinking`,
`Hypertension`, `Diabetes`, inflammatory indices, and imaging variables).
UKB files do not contain medication, subtype, Fazekas grade or NIHSS data.

## Requirements

- R >= 4.2
- Packages: `openxlsx`, `dplyr`, `ggplot2`, `patchwork`, `rstatix`, `car`,
  `mediation`, `metadeconfoundR`

`metadeconfoundR` is installed from GitHub:

```r
remotes::install_github("TillBirkner/metadeconfoundR")

The scripts attempt to install missing packages automatically.

How to run
Place the input Excel files in the working directory.
Open R in that directory and run:
source("GDPH_analysis_v2.R")
source("UKB_validation_v2.R")

Run the discovery script first. The UKB script is independent but uses the same
settings and seed.

Mediation with 5000 bootstrap simulations is computationally intensive. On a
16-core machine, expect tens of minutes.

Analysis pipeline
Data preparation: harmonise the three input sheets, define group labels,
assign VaP subtypes, report missingness, impute within group (median for
continuous, mode for categorical, only if <= 20% missing), log-transform
right-skewed markers and standardise them.

Table 1: three-group baseline comparison with post-hoc tests matched to
the global test (Tukey, Games-Howell or Dunn).

Adjusted logistic regression: VaP vs HC and VaP vs CVNP, adjusted for
age, BMI, sex, smoking, drinking, hypertension, diabetes and medication where
available; per-SD odds ratios for standardised markers.

metadeconfoundR: confounder-robustness of each marker association.

Mediation: bootstrap mediation of age, sex, BMI, smoking and drinking
through blood and imaging markers.

Figures: box plots, adjusted forest plots, mediation forest and
metadeconfoundR heat maps.

Outputs: Excel workbooks with all tables and method logs.

Key methodological notes
Smoking and drinking are collapsed to ever vs never for modelling
(CFG$binarize), assuming code 0 means "never". This must be verified
against the source coding.

A composite monocyte score (MonoScore) is computed as the standardised mean
of the z-scores of monocyte count, MLR, MHR and SIRI.

FDR correction is applied within variable families (demographic, imaging,
inflammatory cell, inflammatory lipid) using Benjamini-Hochberg.

Mediation uses 5000 bootstrap simulations with percentile confidence
intervals and a fixed seed per task.

Known limitations
UK Biobank controls are considerably younger than UKB VaP participants and
have no recorded hypertension or diabetes, so adjusted UKB models against
controls are not reliably estimable. Age- and sex-matched control selection is
recommended before resubmission.

The mediation analysis tests many exposure-mediator combinations and should be
interpreted as exploratory, with FDR correction and explicit reporting of the
number of models tested.

Coding of sex, smoking and drinking must be confirmed against the original data
dictionaries.

Ethics and data use
The study was approved by the relevant institutional ethics committee. Raw data
cannot be shared publicly. De-identified data may be available from the
corresponding author upon reasonable request and subject to institutional
approval.
