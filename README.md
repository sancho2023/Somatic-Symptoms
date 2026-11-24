# Understanding Somatic Symptoms in Relation to Depression, Anxiety, and Stress Among Medical Students

R analysis code for a cross-sectional study of somatic symptom burden (SSS-8) and its association with depression, anxiety and stress (DASS, 18 items) among medical students.

## What the script does

`Somatic_Symptoms.R` runs the full analysis pipeline:

1. **Data preparation** – renames variables, groups age, recodes SSS-8 items (0–4) and computes the SSS-8 total and severity categories (none–minimal, low, medium, high, very high).
2. **DASS subscales** – depression, anxiety and stress scores (6 items each, 0–18) and a global psychological distress score.
3. **Psychometrics** – Cronbach's alpha, KMO, Bartlett's test, parallel analysis and a 3-factor exploratory factor analysis (ML, oblimin).
4. **Descriptive analysis** – frequencies, Shapiro–Wilk tests, Spearman correlations, correlation heatmap, LOESS scatterplots and boxplots/violin plots.
5. **Group comparisons** – `gtsummary` tables by age group, gender and year of study.
6. **Linear regression** – simple and multivariable models for the SSS-8 total, with diagnostics (residual plots, Breusch–Pagan, Durbin–Watson, VIF, Cook's distance, leverage, studentized residuals) and HC3 robust standard errors.
7. **Ordinal logistic regression** – proportional odds models (`MASS::polr`) for SSS-8 severity, reporting odds ratios with 95% CIs.

## Requirements

R (≥ 4.1) and the packages loaded at the top of the script, mainly: `tidyverse`, `psych`, `GPArotation`, `gtsummary`, `janitor`, `skimr`, `Hmisc`, `MASS`, `parameters`, `lmtest`, `car`, `reshape2`.

## Data

The participant dataset (`Understanding Somatic Symptoms.csv`) is **not** included in this repository to protect confidentiality.

## Author

Sancho Xavier – School of Public Health, University of São Paulo (USP)
ORCID: [0000-0001-9493-4098](https://orcid.org/0000-0001-9493-4098)
