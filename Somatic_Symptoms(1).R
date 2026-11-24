# =============================================================================
# Understanding Somatic Symptoms in Relation to Depression, Anxiety, and Stress
# Among Medical Students

# Description:
#   Full analysis pipeline for the study. The script
#     1. prepares the data and builds the SSS-8 and DASS scores,
#     2. assesses reliability and factor structure of the DASS items,
#     3. runs descriptive analyses, correlations and group comparisons,
#     4. fits linear regression models for the SSS-8 total score
#        (with diagnostics and robust standard errors), and
#     5. fits ordinal logistic regression models for SSS-8 severity.
#
# Input : "Understanding Somatic Symptoms.csv" (not shared, participant data)
# Output: TIFF figures (600 dpi) saved in the working directory
# =============================================================================


# 0. Working directory --------------------------------------------------------

# Set the working directory to the folder that contains the data file
# (or open the RStudio project in that folder). Example:
# setwd("path/to/project/folder")


# 1. Packages -----------------------------------------------------------------

# Packages attached with library():
library(tidyverse)   # data import (readr), wrangling (dplyr) and plots (ggplot2)
library(skimr)       # quick overview of the dataset (skim)
library(janitor)     # frequency tables with percentages (tabyl)
library(psych)       # reliability, KMO, Bartlett, factor analysis, correlations
library(GPArotation) # oblimin rotation used by psych::fa()
library(gtsummary)   # publication-ready summary and regression tables
library(parameters)  # model coefficients with robust (HC3) standard errors
library(lmtest)      # Breusch-Pagan test for heteroscedasticity
library(car)         # VIF, ncvTest, Durbin-Watson test, influence plots

# Packages called with "::" only (must be installed, but are not attached
# to avoid masking functions from the tidyverse):
#   Hmisc    -> Hmisc::rcorr()   (correlation matrix with p-values)
#   reshape2 -> reshape2::melt() (correlation matrix to long format)
#   MASS     -> MASS::polr()     (proportional odds ordinal regression)
#
# Also required (used internally by the packages above):
#   broom.helpers and cardx (gtsummary), sandwich (robust SEs in parameters)
#
# To install everything at once:
# install.packages(c("tidyverse", "skimr", "janitor", "psych", "GPArotation",
#                    "gtsummary", "parameters", "lmtest", "car", "Hmisc",
#                    "reshape2", "MASS", "broom.helpers", "cardx", "sandwich"))


# 2. Data import --------------------------------------------------------------

# Read the raw survey data
raw_data <- read_csv("Understanding Somatic Symptoms.csv")

# Overview of variable types, missing values and distributions
skim(raw_data)

# Original column names (as exported from the online form)
names(raw_data)


# 3. Renaming variables -------------------------------------------------------

# Work on a copy so the raw data stay untouched
som_data <- raw_data

# Give every column a short, informative name.
# SSS  = Somatic Symptom Scale-8
# DASS = Depression Anxiety Stress Scales (18 items, 6 per subscale)
# The order below must match the column order of the CSV file.
names(som_data) <- c(
  # Identification and sociodemographic variables
  "timestamp",
  "age",
  "gender",
  "study_year",
  "recruitment_source",

  # Somatic symptoms (SSS-8)
  "sss_stomach_bowel",
  "sss_back_pain",
  "sss_headache",
  "sss_joint_pain",
  "sss_chest_breath",
  "sss_dizziness",
  "sss_fatigue",
  "sss_sleep",

  # DASS items (d = depression, a = anxiety, s = stress)
  "dass_s1_hard_to_wind_down",
  "dass_a1_dry_mouth",
  "dass_d1_no_positive_feeling",
  "dass_d2_no_initiative",
  "dass_s2_overreact",
  "dass_a2_trembling",
  "dass_s3_nervous_energy",
  "dass_a3_panic_fool",
  "dass_d3_nothing_forward",
  "dass_s4_agitated",
  "dass_s5_difficult_relax",
  "dass_s6_intolerant",
  "dass_a4_close_panic",
  "dass_d4_no_enthusiasm",
  "dass_d5_not_worth",
  "dass_d6_life_meaningless",
  "dass_a5_heart_awareness",
  "dass_a6_scared_no_reason",

  # Final variables
  "comments",
  "participant_id"
)

# Check the renamed dataset
skim(som_data)


# 4. Data wrangling -----------------------------------------------------------

## 4.1 Age ----

# Frequency of the age categories, including missing values
table(som_data$age, useNA = "ifany")

# Remove participants with missing age
som_data <- som_data |>
  filter(!is.na(age))

# Collapse the two oldest categories (few participants) into "≥41 years"
som_data$age_group <- case_when(
  som_data$age == "≤21 years"                    ~ "≤21 years",
  som_data$age == "21-40 years"                  ~ "21-40 years",
  som_data$age %in% c("41-60 years", ">60 years") ~ "≥41 years"
)

table(som_data$age_group)

## 4.2 Year of study ----

table(som_data$study_year)


# 5. Somatic Symptom Scale-8 (SSS-8) ------------------------------------------

# Names of the 8 SSS-8 items
sss_items <- c(
  "sss_stomach_bowel",
  "sss_back_pain",
  "sss_headache",
  "sss_joint_pain",
  "sss_chest_breath",
  "sss_dizziness",
  "sss_fatigue",
  "sss_sleep"
)

# Check the original (text) response options of one item
table(som_data$sss_stomach_bowel)

# Recode the text responses into the standard 0-4 SSS-8 scoring
som_data[sss_items] <- lapply(
  som_data[sss_items],
  function(x) dplyr::recode(
    x,
    "Not at all"   = 0,
    "A little bit" = 1,
    "Somewhat"     = 2,
    "Quite a bit"  = 3,
    "Very much"    = 4
  )
)

# Make sure all items are stored as numeric
som_data[sss_items] <- lapply(som_data[sss_items], as.numeric)

glimpse(som_data)

# Total score (range 0-32). na.rm = FALSE: a participant with any missing
# item gets a missing total score
som_data$sss_total <- rowSums(som_data[sss_items], na.rm = FALSE)

summary(som_data$sss_total)

# Severity categories according to the SSS-8 cut-offs:
# 0-3 none to minimal, 4-7 low, 8-11 medium, 12-15 high, 16-32 very high
som_data$sss_severity <- cut(
  som_data$sss_total,
  breaks = c(-Inf, 3, 7, 11, 15, 32),
  labels = c("None to minimal", "Low", "Medium", "High", "Very high"),
  ordered_result = TRUE   # ordered factor, needed for ordinal regression
)

# Check the results
table(som_data$sss_severity)
psych::describe(som_data$sss_total)

# Internal consistency of the SSS-8 (Cronbach's alpha)
psych::alpha(som_data[sss_items])


# 6. DASS subscales -----------------------------------------------------------

# DASS items are assumed to be stored as numbers (0-3) in the CSV file
table(som_data$dass_s1_hard_to_wind_down)

# Items of each subscale
dep_items <- c(                 # Depression
  "dass_d1_no_positive_feeling",
  "dass_d2_no_initiative",
  "dass_d3_nothing_forward",
  "dass_d4_no_enthusiasm",
  "dass_d5_not_worth",
  "dass_d6_life_meaningless"
)

anx_items <- c(                 # Anxiety
  "dass_a1_dry_mouth",
  "dass_a2_trembling",
  "dass_a3_panic_fool",
  "dass_a4_close_panic",
  "dass_a5_heart_awareness",
  "dass_a6_scared_no_reason"
)

stress_items <- c(              # Stress
  "dass_s1_hard_to_wind_down",
  "dass_s2_overreact",
  "dass_s3_nervous_energy",
  "dass_s4_agitated",
  "dass_s5_difficult_relax",
  "dass_s6_intolerant"
)

# All 18 DASS items
dass18_items <- c(dep_items, anx_items, stress_items)

# Subscale scores: sum of 6 items, each 0-3, so each score ranges 0-18
som_data$depression_score <- rowSums(som_data[dep_items],    na.rm = FALSE)
som_data$anxiety_score    <- rowSums(som_data[anx_items],    na.rm = FALSE)
som_data$stress_score     <- rowSums(som_data[stress_items], na.rm = FALSE)

# Global psychological distress score: sum of the 18 items (range 0-54)
som_data$psychological_distress <- rowSums(som_data[dass18_items], na.rm = FALSE)

summary(som_data$depression_score)
summary(som_data$anxiety_score)
summary(som_data$stress_score)
summary(som_data$psychological_distress)


# 7. Reliability and factor structure of the DASS -----------------------------

## 7.1 Cronbach's alpha ----

# Interpretation: >= 0.90 excellent; 0.80-0.89 very good;
#                 0.70-0.79 acceptable; < 0.70 insufficient

alpha_total  <- psych::alpha(som_data[dass18_items])  # full scale (18 items)
alpha_dep    <- psych::alpha(som_data[dep_items])     # depression
alpha_anx    <- psych::alpha(som_data[anx_items])     # anxiety
alpha_stress <- psych::alpha(som_data[stress_items])  # stress

# Raw alpha of each scale
alpha_total$total$raw_alpha
alpha_dep$total$raw_alpha
alpha_anx$total$raw_alpha
alpha_stress$total$raw_alpha

## 7.2 Suitability of the data for factor analysis ----

# Kaiser-Meyer-Olkin measure of sampling adequacy
# Interpretation: >= 0.90 marvelous; 0.80-0.89 meritorious; 0.70-0.79 middling;
#                 0.60-0.69 mediocre; < 0.60 unacceptable (Kaiser, 1974)
KMO(som_data[dass18_items])

# Bartlett's test of sphericity (H0: the correlation matrix is an identity matrix)
cortest.bartlett(
  cor(som_data[dass18_items]),
  n = nrow(som_data)
)

## 7.3 Number of factors to retain ----

# Parallel analysis: compares observed eigenvalues with those from random data.
# Expected result: three factors (depression, anxiety and stress)
fa.parallel(
  som_data[dass18_items],
  fa     = "fa",   # factor analysis (not principal components)
  fm     = "ml",   # maximum likelihood extraction
  n.iter = 100     # number of simulated datasets
)

## 7.4 Exploratory factor analysis (EFA) ----

# Three-factor model, maximum likelihood, oblique rotation (factors may correlate)
efa_3f <- fa(
  som_data[dass18_items],
  nfactors = 3,
  rotate   = "oblimin",
  fm       = "ml"
)

# Loadings sorted by factor, hiding loadings below 0.30
print(efa_3f, sort = TRUE, cutoff = 0.30)

# Variance explained by each factor
efa_3f$Vaccounted

# Correlations between factors
efa_3f$Phi

# Factor scores for each participant
efa_scores <- factor.scores(som_data[dass18_items], efa_3f)
efa_scores


# 8. Descriptive analysis -----------------------------------------------------

## 8.1 Sociodemographic characteristics ----

# Age (original categories and grouped)
som_data |> tabyl(age)       |> adorn_pct_formatting()
som_data |> tabyl(age_group) |> adorn_pct_formatting()

# Gender
som_data |> tabyl(gender) |> adorn_pct_formatting()

# Year of study
som_data |> tabyl(study_year) |> adorn_pct_formatting()

## 8.2 Scores: normality (Shapiro-Wilk) and summary statistics ----

# SSS-8 total score
shapiro.test(som_data$sss_total)
summary(som_data$sss_total)

# Depression score
shapiro.test(som_data$depression_score)
summary(som_data$depression_score)

# Anxiety score
shapiro.test(som_data$anxiety_score)
summary(som_data$anxiety_score)

# Stress score
shapiro.test(som_data$stress_score)
summary(som_data$stress_score)

# Global psychological distress score
shapiro.test(som_data$psychological_distress)
summary(som_data$psychological_distress)

## 8.3 SSS-8 severity categories ----

som_data |> tabyl(sss_severity) |> adorn_pct_formatting()


# 9. Correlations -------------------------------------------------------------

# Scores to correlate
score_vars <- som_data[, c(
  "sss_total",
  "depression_score",
  "anxiety_score",
  "stress_score"
)]

# Check classes and missing values before computing correlations
str(score_vars)
colSums(is.na(score_vars))

## 9.1 Spearman correlation matrix (scores are not normally distributed) ----

cor_mat <- cor(
  score_vars,
  method = "spearman",
  use    = "pairwise.complete.obs"
)

cor_mat

# Same correlations with p-values and n
Hmisc::rcorr(as.matrix(score_vars), type = "spearman")

# Alternative with psych (no adjustment for multiple testing)
corr <- psych::corr.test(score_vars, method = "spearman", adjust = "none")
corr     # coefficients
corr$p   # p-values
corr$n   # sample size

## 9.2 Correlation heatmap ----

# Use readable labels for the figure only
labels_fig <- c("SSS-8", "Depression", "Anxiety", "Stress")
colnames(cor_mat) <- labels_fig
rownames(cor_mat) <- labels_fig

# Long format: one row per pair of variables (Var1, Var2, value)
cor_long <- reshape2::melt(cor_mat)

# Save the heatmap as a 600 dpi TIFF
tiff("Heatmap_somatic_symptoms.tiff",
     width = 7, height = 6, units = "in",
     res = 600, compression = "lzw")

p_heatmap <- ggplot(cor_long, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white", linewidth = 0.5) +            # one tile per pair
  geom_text(aes(label = sprintf("%.2f", value)),           # print coefficient
            size = 5, fontface = "bold") +
  scale_fill_gradient2(                                    # blue (-) to red (+)
    low = "#2166AC", mid = "white", high = "#B2182B",
    midpoint = 0, limits = c(-1, 1),
    name = "Spearman\nρ"
  ) +
  labs(x = NULL, y = NULL) +
  coord_fixed() +                                          # square tiles
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    legend.position = "right"
  )

print(p_heatmap)   # print() is needed so the plot is written when sourcing
dev.off()

## 9.3 Scatterplots with LOESS smoothing ----

# Helper function: scatterplot of the SSS-8 total against one DASS score,
# with a LOESS curve and its 95% confidence band, saved as a TIFF file
save_scatter <- function(x_var, x_label, file_name) {
  tiff(file_name, width = 7, height = 6, units = "in",
       res = 600, compression = "lzw")

  p <- ggplot(som_data, aes(x = .data[[x_var]], y = sss_total)) +
    geom_point(alpha = 0.5, size = 2) +
    geom_smooth(method = "loess", se = TRUE, linewidth = 1.1) +
    labs(x = x_label, y = "SSS-8 total score") +
    theme_classic(base_size = 14)

  print(p)
  dev.off()
}

save_scatter("depression_score", "Depression score", "Figure_Depression_SSS.tiff")
save_scatter("anxiety_score",    "Anxiety score",    "Figure_Anxiety_SSS.tiff")
save_scatter("stress_score",     "Stress score",     "Figure_Stress_SSS.tiff")


# 10. SSS-8 total score by group ----------------------------------------------

## 10.1 Boxplots ----

# Helper function: boxplot of the SSS-8 total score by a grouping variable
save_boxplot <- function(group_var, x_label, file_name, box_width = 0.70) {
  tiff(file_name, width = 7, height = 6, units = "in",
       res = 600, compression = "lzw")

  p <- ggplot(som_data,
              aes(x = .data[[group_var]], y = sss_total,
                  fill = .data[[group_var]])) +
    geom_boxplot(width = box_width) +
    labs(x = x_label, y = "SSS-8 total score") +
    theme_classic(base_size = 14) +
    theme(legend.position = "none")   # colours already shown on the x-axis

  print(p)
  dev.off()
}

save_boxplot("gender",     "Gender",        "Figure_SSS_totalScore_Gender.tiff", 0.60)
save_boxplot("study_year", "Year of study", "Figure_SSS_totalScore_YearOfStudy.tiff")
save_boxplot("age_group",  "Age group",     "Figure_SSS_totalScore_Age.tiff")

## 10.2 Violin plot by gender (alternative to the boxplot) ----

tiff("Figure_SSS_totalScore_Gender_violin.tiff",
     width = 7, height = 6, units = "in",
     res = 600, compression = "lzw")

p_violin <- ggplot(som_data, aes(x = gender, y = sss_total, fill = gender)) +
  geom_violin(trim = FALSE, alpha = 0.5) +                       # distribution
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.9) +  # median, IQR
  labs(x = "Gender", y = "SSS-8 total score") +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")

print(p_violin)
dev.off()

## 10.3 Group comparisons (median and IQR, nonparametric tests) ----

# add_p() uses the Wilcoxon rank-sum test for 2 groups and the
# Kruskal-Wallis test for 3 or more groups

score_list <- c("sss_total", "depression_score", "anxiety_score", "stress_score")

# By age group
tbl_summary(
  som_data,
  by        = age_group,
  include   = all_of(score_list),
  statistic = all_continuous() ~ "{median} ({p25}, {p75})"
) |>
  add_p()

# By gender
tbl_summary(
  som_data,
  by        = gender,
  include   = all_of(score_list),
  statistic = all_continuous() ~ "{median} ({p25}, {p75})"
) |>
  add_p()

# By year of study
tbl_summary(
  som_data,
  by        = study_year,
  include   = all_of(score_list),
  statistic = all_continuous() ~ "{median} ({p25}, {p75})"
) |>
  add_p()


# 11. Linear regression: SSS-8 total score ------------------------------------

## 11.1 Simple (univariable) models ----

# For each model: model summary, formatted table and coefficients with
# heteroscedasticity-robust (HC3) standard errors

# Depression
m1 <- lm(sss_total ~ depression_score, data = som_data)
summary(m1)
tbl_regression(m1, intercept = TRUE)
model_parameters(m1, vcov = "HC3")

# Anxiety
m2 <- lm(sss_total ~ anxiety_score, data = som_data)
summary(m2)
tbl_regression(m2, intercept = FALSE)
model_parameters(m2, vcov = "HC3")

# Stress
m3 <- lm(sss_total ~ stress_score, data = som_data)
summary(m3)
tbl_regression(m3, intercept = FALSE)
model_parameters(m3, vcov = "HC3")

# Age group
m4 <- lm(sss_total ~ age_group, data = som_data)
summary(m4)
tbl_regression(m4, intercept = FALSE)
model_parameters(m4, vcov = "HC3")

# Gender
m5 <- lm(sss_total ~ gender, data = som_data)
summary(m5)
tbl_regression(m5, intercept = FALSE)
model_parameters(m5, vcov = "HC3")

# Year of study
m6 <- lm(sss_total ~ study_year, data = som_data)
summary(m6)
tbl_regression(m6, intercept = FALSE)
model_parameters(m6, vcov = "HC3")

## 11.2 Multivariable models ----

# mult1: DASS subscales only
mult1 <- lm(
  sss_total ~ depression_score + anxiety_score + stress_score,
  data = som_data
)
summary(mult1)
tbl_regression(mult1, intercept = FALSE)

# mult2: DASS subscales + all sociodemographic variables
mult2 <- lm(
  sss_total ~ depression_score + anxiety_score + stress_score +
    age_group + gender + study_year,
  data = som_data
)
summary(mult2)
tbl_regression(mult2, intercept = FALSE)

# mult3: reduced model (without year of study)
mult3 <- lm(
  sss_total ~ depression_score + anxiety_score + stress_score +
    age_group + gender,
  data = som_data
)
summary(mult3)
tbl_regression(mult3, intercept = FALSE)

## 11.3 Model comparison ----

# Lower AIC/BIC = better balance between fit and complexity
AIC(mult1, mult2, mult3)
BIC(mult1, mult2, mult3)

# Residual standard error of each model
summary(mult1)$sigma
summary(mult2)$sigma
summary(mult3)$sigma


# 12. Regression assumptions --------------------------------------------------

## 12.1 Standard diagnostic plots ----

# Residuals vs fitted, normal Q-Q, scale-location and residuals vs leverage
save_lm_diagnostics <- function(model, file_name, size = 8, unit = "in") {
  tiff(file_name, width = size, height = size, units = unit,
       res = 600, compression = "lzw")
  par(mfrow = c(2, 2))   # 2 x 2 panel layout
  plot(model)
  dev.off()
}

save_lm_diagnostics(mult1, "diagnostic_plots_mult1.tiff")
save_lm_diagnostics(mult2, "diagnostic_plots_mult2.tiff")
save_lm_diagnostics(mult3, "diagnostic_plots_mult3.tiff")

# Same panel for mult1 in the supplementary-figure size (24 x 24 cm)
save_lm_diagnostics(mult1, "Figure_S1_Model1_Diagnostics.tiff", size = 24, unit = "cm")

## 12.2 Homoscedasticity ----

# Breusch-Pagan test (H0: constant residual variance)
bptest(mult1)
bptest(mult2)
bptest(mult3)

# Non-constant variance score test (car), as a check
ncvTest(mult1)

## 12.3 Multicollinearity ----

# Variance inflation factors (GVIF for categorical variables);
# values above 5 (or 10) suggest problematic collinearity
vif(mult1)
vif(mult2)
vif(mult3)

## 12.4 Influential observations and summary of all checks ----

# Models to check, with the labels used in the console output
models <- list(
  "Model I"   = mult1,
  "Model II"  = mult2,
  "Model III" = mult3
)

# Common cut-off for Cook's distance: 4 / n
cook_cutoff <- 4 / nrow(som_data)

for (i in seq_along(models)) {

  model <- models[[i]]
  name  <- names(models)[i]

  cat("\n========================================\n")
  cat(name, "\n")
  cat("========================================\n")

  # Influence plot: studentized residuals vs leverage, bubble size = Cook's D
  influencePlot(model, main = paste("Influence Plot -", name), sub = "")

  # Cook's distance with the 4/n reference line
  plot(cooks.distance(model), type = "h",
       main = paste("Cook's Distance -", name),
       ylab = "Cook's distance", xlab = "Observation")
  abline(h = cook_cutoff, col = "red", lty = 2)

  # Leverage (hat values) with the 2 x mean reference line
  plot(hatvalues(model), type = "h",
       main = paste("Leverage -", name),
       ylab = "Hat values", xlab = "Observation")
  abline(h = 2 * mean(hatvalues(model)), col = "blue", lty = 2)

  # Studentized residuals with the +/- 3 reference lines
  plot(rstudent(model), type = "h",
       main = paste("Studentized Residuals -", name),
       ylab = "Studentized residual", xlab = "Observation")
  abline(h = c(-3, 3), col = "red", lty = 2)

  # List the observations above each threshold
  cat("\nObservations with Cook's distance >", round(cook_cutoff, 4), "\n")
  print(which(cooks.distance(model) > cook_cutoff))

  cat("\nObservations with high leverage (> 2 x mean hat value)\n")
  print(which(hatvalues(model) > 2 * mean(hatvalues(model))))

  cat("\nObservations with |studentized residual| > 3\n")
  print(which(abs(rstudent(model)) > 3))

  # Assumption tests
  cat("\nBreusch-Pagan test\n")
  print(bptest(model))

  cat("\nDurbin-Watson test (independence of residuals)\n")
  print(durbinWatsonTest(model))

  cat("\nVIF\n")
  print(vif(model))

  cat("\nShapiro-Wilk test of the standardized residuals\n")
  print(shapiro.test(rstandard(model)))
}

## 12.5 Supplementary figures: influence diagnostics ----

# Saves four TIFF files per model (influence plot, Cook's distance,
# leverage and studentized residuals)
save_influence_figures <- function(model, plot_title, file_suffix) {

  # Shared settings for all four figures
  open_tiff <- function(prefix) {
    tiff(paste0(prefix, "_", file_suffix, ".tiff"),
         width = 18, height = 16, units = "cm",
         res = 600, compression = "lzw")
  }

  # Influence plot
  open_tiff("Figure_S1_Influence")
  influencePlot(model, main = plot_title, sub = "")
  dev.off()

  # Cook's distance
  open_tiff("Figure_S2_Cooks")
  plot(cooks.distance(model), type = "h",
       ylab = "Cook's distance", xlab = "Observation")
  abline(h = 4 / nrow(som_data), col = "red", lty = 2, lwd = 2)
  dev.off()

  # Leverage
  open_tiff("Figure_S3_Leverage")
  plot(hatvalues(model), type = "h",
       ylab = "Leverage", xlab = "Observation")
  abline(h = 2 * mean(hatvalues(model)), col = "blue", lty = 2, lwd = 2)
  dev.off()

  # Studentized residuals
  open_tiff("Figure_S4_Residuals")
  plot(rstudent(model), type = "h",
       ylab = "Studentized residual", xlab = "Observation")
  abline(h = c(-3, 3), col = "red", lty = 2, lwd = 2)
  dev.off()
}

# Titles follow the model numbering used in the manuscript
save_influence_figures(mult1, "Model II",  "Model1")
save_influence_figures(mult2, "Model III", "Model2")
save_influence_figures(mult3, "Model IV",  "Model4")

## 12.6 Robust standard errors ----

# Residuals showed heteroscedasticity, so coefficients are reported with
# HC3 heteroscedasticity-consistent standard errors
model_parameters(mult1, vcov = "HC3")   # Model II
model_parameters(mult2, vcov = "HC3")   # Model III
model_parameters(mult3, vcov = "HC3")   # Model IV


# 13. Ordinal logistic regression: SSS-8 severity -----------------------------

# Proportional odds models (MASS::polr) for the ordered SSS-8 severity
# categories. Odds ratios > 1 mean higher odds of a more severe category.

## 13.1 Helper function to extract results ----

# Returns a table with beta, odds ratio, 95% CI (profile likelihood) and
# p-value (Wald test) for every coefficient of a polr model
extract_polr <- function(model) {

  s <- summary(model)

  # Regression coefficients (log-odds)
  beta <- coef(model)

  # Odds ratios
  OR <- exp(beta)

  # 95% confidence intervals on the odds-ratio scale
  CI <- exp(confint(model))

  # With a single predictor, confint() returns a vector: turn it into a matrix
  if (is.null(dim(CI))) {
    CI <- matrix(CI, nrow = 1,
                 dimnames = list(names(beta), c("2.5 %", "97.5 %")))
  }

  # Two-sided p-values from the t (Wald) statistics; the first rows of the
  # coefficient table are the predictors, the remaining rows are the cut-points
  p <- 2 * pnorm(abs(s$coefficients[seq_along(beta), "t value"]),
                 lower.tail = FALSE)

  # Formatted results table
  data.frame(
    Variable = names(beta),
    Beta     = round(beta, 3),
    OR       = round(OR, 2),
    Lower95  = round(CI[, 1], 2),
    Upper95  = round(CI[, 2], 2),
    p        = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)),
    row.names = NULL
  )
}

## 13.2 Univariable models ----

# Depression
mod_dep <- MASS::polr(sss_severity ~ depression_score, data = som_data, Hess = TRUE)
extract_polr(mod_dep)

# Anxiety
mod_anx <- MASS::polr(sss_severity ~ anxiety_score, data = som_data, Hess = TRUE)
extract_polr(mod_anx)

# Stress
mod_stress <- MASS::polr(sss_severity ~ stress_score, data = som_data, Hess = TRUE)
extract_polr(mod_stress)

# Age group
mod_age <- MASS::polr(sss_severity ~ age_group, data = som_data, Hess = TRUE)
extract_polr(mod_age)

# Gender
mod_gender <- MASS::polr(sss_severity ~ gender, data = som_data, Hess = TRUE)
extract_polr(mod_gender)

# Year of study
mod_year <- MASS::polr(sss_severity ~ study_year, data = som_data, Hess = TRUE)
extract_polr(mod_year)

## 13.3 Multivariable model ----

# All DASS subscales and sociodemographic variables together
mod_multi <- MASS::polr(
  sss_severity ~ depression_score + anxiety_score + stress_score +
    age_group + gender + study_year,
  data = som_data,
  Hess = TRUE
)

result_multi <- extract_polr(mod_multi)
result_multi

# End of script ----------------------------------------------------------------
