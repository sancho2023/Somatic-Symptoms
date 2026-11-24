
# Abrindo diretorio -------------------------------------------------------

# Defina o diretório do projeto (ou abra o .Rproj) antes de rodar:
# setwd("~/Paper 2026/Understanding Somatic Symptoms in Relation to Depression, Anxiety, and Stress Among Medical Students")

# Carregando os pacotes ---------------------------------------------------

library(tidyverse) # Core package for data wrangling, includes dplyr, ggplot2, etc.
library(tidymodels)
library(rsample)
library(ROCR)      # Para a curva ROC
library(survivalROC) # Curva ROC para sobrevida
library(timeROC)    # IC 95%
library(rms)             # Calibração
# source("stdca.R")  # não utilizado neste script
library(janitor)   # Simplifies cleaning column names, tables, and data summaries
library(foreign)   # Read/write data from various statistical software (SPSS, Stata, etc.)
library(survival)  # Survival analysis and related models (Kaplan-Meier, Cox)
library(survminer)  # Visualizations for survival analysis results
library(cmprsk)
library(coin)       # Conditional inference procedures (nonparametric tests)
library(magrittr)
library(broom)      # Tidies statistical model outputs into data frames
library(RcmdrMisc)  # Extra functions for the Rcmdr GUI
library(finalfit)    # Streamlined model fitting and table generation
library(openxlsx)
library(flextable)
library(gtsummary)  # Create publication-ready summary tables (especially for models)
#install.packages("TH.data") # Para rodas outros testes de associacao (hipotese)
library(TH.data)
library(GGally) ## Multicolinearidade
library(writexl) # Work with Excel files (.xlsx)
library(ggthemes)
# install.packages("survivalAnalysis")
# library(survivalAnalysis) # análise de dados de sobrevivência, podendo ajudar a Calcular a taxa de incidência instantânea
library(glmnet)
library("flexsurv")# estimate parametric survival curves.
library(skimr)

# Instalando o pacote de personalidade psicologico

# install.packages("psych")
library(psych)
library(readxl)
# install.packages(c("psych","GPArotation"))
library(GPArotation)

#

library(Hmisc)
library(corrplot)


# Carregando o banco de dados ---------------------------------------------

dados_som <- read_csv("Understanding Somatic Symptoms.csv")

# visualizando os dados gerais

skim(dados_som)

names(dados_som)

# Criando copia do banco antes de renomear as variaveis

dados_som_ren <- dados_som

# Renomeando todas as variaveis do banco de dados
# SSS = Somatic Symptom Scale
# DASS = Depression Anxiety Stress Scales

names(dados_som_ren) <- c(
  # Identificação
  "timestamp",
  "age",
  "gender",
  "study_year",
  "recruitment_source",
  
  # Somatic Symptoms (SSS-8)
  "sss_stomach_bowel",
  "sss_back_pain",
  "sss_headache",
  "sss_joint_pain",
  "sss_chest_breath",
  "sss_dizziness",
  "sss_fatigue",
  "sss_sleep",
  
  # DASS-21
  "dass_s1_hard_to_wind_down",          # Stress Item 1
  "dass_a1_dry_mouth",                  # Anxiety Item 1
  "dass_d1_no_positive_feeling",        # Depression Item 1
  "dass_d2_no_initiative",              # Depression Item 2
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
  
  # Variáveis finais
  "comments",
  "participant_id"
)

# Verificando todo o banco de dados

skim(dados_som_ren)

# Data wrangling

# Age

table(dados_som_ren$age)

# removendo dados faltantes

table(dados_som_ren$age, useNA = "ifany")

dados_som_ren <- dados_som_ren |> 
  filter(!is.na(age))

dados_som_ren$age_group <- case_when(
  dados_som_ren$age == "≤21 years" ~ "≤21 years",
  dados_som_ren$age == "21-40 years" ~ "21-40 years",
  dados_som_ren$age %in% c("41-60 years", ">60 years") ~ "≥41 years"
)

table(dados_som_ren$age_group)

# Current year of study

table(dados_som_ren$study_year)


# Sintomas somáticos ----------------------------------------------------

# SSS-8 (Somatic Symptom Scale–8)

names(dados_som_ren)

table(dados_som_ren$sss_stomach_bowel) # verificando

# Recodificar todos os 8 itens

itens_sss <- c(
  "sss_stomach_bowel",
  "sss_back_pain",
  "sss_headache",
  "sss_joint_pain",
  "sss_chest_breath",
  "sss_dizziness",
  "sss_fatigue",
  "sss_sleep"
)

dados_som_ren[itens_sss] <- lapply(
  dados_som_ren[itens_sss],
  function(x) dplyr::recode(
    x,
    "Not at all"   = 0,
    "A little bit" = 1,
    "Somewhat"     = 2,
    "Quite a bit"  = 3,
    "Very much"    = 4
  )
)

# Transformando em numerico

dados_som_ren[itens_sss] <- lapply(
  dados_som_ren[itens_sss],
  as.numeric
)

glimpse(dados_som_ren)

# Calcular o escore total

dados_som_ren$sss_total <- rowSums(
  dados_som_ren[itens_sss],
  na.rm = FALSE
)

summary(dados_som_ren$sss_total)

# Criar a classificação da SSS-8

dados_som_ren$sss_severity <- cut(
  dados_som_ren$sss_total,
  breaks = c(-Inf, 3, 7, 11, 15, 32),
  labels = c(
    "None to minimal",
    "Low",
    "Medium",
    "High",
    "Very high"
  ),
  ordered_result = TRUE
)

# Conferindo os resultados

summary(dados_som_ren$sss_total)

table(dados_som_ren$sss_severity)

psych::describe(dados_som_ren$sss_total)

psych::alpha(dados_som_ren[itens_sss]) # Calcular a confiabilidade :: Cronbach's alpha

# Verificando a possibilidade de criar os grupos :: Depressao, ansiedade e stress

table(dados_som_ren$dass_s1_hard_to_wind_down)

# Grupos

dep_itens <- c(                 # Depressão
  "dass_d1_no_positive_feeling",
  "dass_d2_no_initiative",
  "dass_d3_nothing_forward",
  "dass_d4_no_enthusiasm",
  "dass_d5_not_worth",
  "dass_d6_life_meaningless"
)


anx_itens <- c(                  # Ansiedade
  "dass_a1_dry_mouth",
  "dass_a2_trembling",
  "dass_a3_panic_fool",
  "dass_a4_close_panic",
  "dass_a5_heart_awareness",
  "dass_a6_scared_no_reason"
)


stress_itens <- c(                # Stress
  "dass_s1_hard_to_wind_down",
  "dass_s2_overreact",
  "dass_s3_nervous_energy",
  "dass_s4_agitated",
  "dass_s5_difficult_relax",
  "dass_s6_intolerant"
)

# Criando os scores :: variando de 0-18

# Depressao

dados_som_ren$depression_score <- rowSums(
  dados_som_ren[dep_itens],
  na.rm = FALSE
)



summary(dados_som_ren$depression_score)

 # Ansiedade

dados_som_ren$anxiety_score <- rowSums(
  dados_som_ren[anx_itens],
  na.rm = FALSE
)

summary(dados_som_ren$anxiety_score)

# Stress

dados_som_ren$stress_score <- rowSums(
  dados_som_ren[stress_itens],
  na.rm = FALSE
)

summary(dados_som_ren$stress_score)


# Score global :: escore global dos 18 itens

todos_itens <- c(dep_itens, anx_itens, stress_itens)

dados_som_ren$psychological_distress <- rowSums(
  dados_som_ren[todos_itens],
  na.rm = FALSE
)

summary(dados_som_ren$psychological_distress)

# Aplicando a analise para verificar a consistencia interna e analise fatorial :: estrutura fatorial

# Criar o vetor com os 18 itens

itens_dass18 <- c(
  dep_itens,
  anx_itens,
  stress_itens
)

# Cronbach's alpha

# escala completa

alpha_total <- psych::alpha(dados_som_ren[itens_dass18])

alpha_total

# PAra cada dominio separadamente

alpha_dep <- psych::alpha(dados_som_ren[dep_itens]) # Depressao

alpha_dep


alpha_anx <- psych::alpha(dados_som_ren[anx_itens]) # Ansiedade


alpha_anx

alpha_stress <- psych::alpha(dados_som_ren[stress_itens]) # Stress

alpha_stress


# Verificando a consistencia interna : 

# Cronbach's α	Interpretação
# ≥ 0.90	Excelente
# 0.80–0.89	Muito boa
# 0.70–0.79	Aceitável
# < 0.70	Insuficiente

alpha_total$total$raw_alpha


# Verificando a adequação para análise fatorial

KMO(dados_som_ren[itens_dass18]) # kmd

cortest.bartlett(
  cor(dados_som_ren[itens_dass18]),
  n = nrow(dados_som_ren)
)

# KMO global = 0,95 → considerado "marvelous" segundo a classificação de Kaiser (1974).

# Interpretação:
  
# KMO	Interpretação
# ≥ 0,90	Excelente ("marvelous")
# 0,80–0,89	Muito bom
# 0,70–0,79	Bom
# 0,60–0,69	Aceitável
# < 0,60	Inadequado

# Numeros de fatores

fa.parallel(
  dados_som_ren[itens_dass18],
  fa = "fa",
  fm = "ml",
  n.iter = 100
)


# Isso significa que, estatisticamente, seus 18 itens se organizam melhor em três fatores latentes.

# Esse resultado é exatamente o que esperávamos teoricamente:
  
# Depressão
# Ansiedade
# Estresse

# Análise Fatorial Exploratória (EFA)

efa_3f <- fa(
  dados_som_ren[itens_dass18],
  nfactors = 3,
  rotate = "oblimin",
  fm = "ml"
)

print(
  efa_3f,
  sort = TRUE,
  cutoff = 0.30
)

# Variância explicada
efa_3f$Vaccounted

# Correlação entre fatores

efa_3f$Phi

# Apha por dominios

alpha_dep$total$raw_alpha
alpha_anx$total$raw_alpha
alpha_stress$total$raw_alpha

# Criar os escores fatoriais

efa_scores <- factor.scores(
  dados_som_ren[itens_dass18],
  efa_3f
)

efa_scores


# Análise descritiva ------------------------------------------------------


# Idade

class(dados_som_ren$age)
# dados_som_ren$age <- as.numeric(dados_som_ren$age)

summary(dados_som_ren$age)

dados_som_ren |> 
  tabyl(age_group) |> adorn_pct_formatting()

# Gender

dados_som_ren |> 
  tabyl(gender) |> adorn_pct_formatting()

# Years of study

dados_som_ren |> 
  tabyl(study_year) |> adorn_pct_formatting()

# Psychological distress

# Total Somatic Symptom Scale

shapiro.test(dados_som_ren$sss_total)

summary(dados_som_ren$sss_total)

# Drepression score

shapiro.test(dados_som_ren$depression_score)

summary(dados_som_ren$depression_score)

# Anxiety score

shapiro.test(dados_som_ren$anxiety_score)

summary(dados_som_ren$anxiety_score)

# Stress score

shapiro.test(dados_som_ren$stress_score)

summary(dados_som_ren$stress_score)

# psychological_distress

shapiro.test(dados_som_ren$psychological_distress)

summary(dados_som_ren$psychological_distress)


# Categories of somatic symptoms

dados_som_ren |> 
  tabyl(sss_severity) |> adorn_pct_formatting()

table(dados_som_ren$sss_severity)

# Testes de correlação

vars <- c(
  "sss_total",
  "depression_score",
  "anxiety_score",
  "stress_score"
)

cor(dados_som_ren[vars], method = "spearman")

# coeficientes com p-valor

Hmisc::rcorr(
  as.matrix(dados_som_ren[vars]),
  type = "spearman"
)

# Calculando as correlacoes

# Verificar as classes das variaveis

sapply(dados_som_ren[, c(
  "sss_total",
  "depression_score",
  "anxiety_score",
  "stress_score"
)], class)

# Recriar nova estrutura

vars <- dados_som_ren[, c(
  "sss_total",
  "depression_score",
  "anxiety_score",
  "stress_score"
)]

str(vars)

# Dados ausentes

colSums(is.na(vars))

# Primeira tentativa
cor(
  vars,
  method = "spearman",
  use = "pairwise.complete.obs"
)

#

# Matriz de correlação

cor_mat <- cor(
  vars,
  method = "spearman",
  use = "pairwise.complete.obs"
)

# Converter para formato longo
# install.packages("reshape2")

library(reshape2)

cor_long <- reshape2::melt(cor_mat)

# Heatmap

tiff("Heatmap_somatic_symptoms.tiff",
     width = 7,
     height = 6,
     units = "in",
     res = 600,
     compression = "lzw")

ggplot(cor_long,
       aes(Var1, Var2, fill = value)) +
  
  geom_tile(color = "white", linewidth = 0.5) +
  
  geom_text(aes(label = sprintf("%.2f", value)),
            size = 5,
            fontface = "bold") +
  
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-1,1),
    name = "Spearman\nρ"
  ) +
  
  labs(
    x = NULL,
    y = NULL
  ) +
  
  coord_fixed() +
  
  theme_classic(base_size = 14) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      face = "bold"
    ),
    axis.text.y = element_text(face = "bold"),
    legend.position = "right"
  )

dev.off()

# Renomear as variaveis

# Selecionar variáveis
vars <- dados_som_ren[, c(
  "sss_total",
  "depression_score",
  "anxiety_score",
  "stress_score"
)]

# Matriz de correlação
cor_mat <- cor(
  vars,
  method = "spearman",
  use = "pairwise.complete.obs"
)

# Renomear APENAS para a figura
colnames(cor_mat) <- c(
  "SSS-8",
  "Depression",
  "Anxiety",
  "Stress"
)


rownames(cor_mat) <- c(
  "SSS-8",
  "Depression",
  "Anxiety",
  "Stress"
)



# Usando Psych corr

vars <- dados_som_ren[, c(
  "sss_total",
  "depression_score",
  "anxiety_score",
  "stress_score"
)]

corr <- psych::corr.test(
  vars,
  method = "spearman",
  adjust = "none"
)

corr

# p-valor
corr$p

# n

corr$n


# Scatterplots com LOESS

# SSS × Depressão

tiff("Figure_Depression_SSS.tiff",
     width = 7,
     height = 6,
     units = "in",
     res = 600,
     compression = "lzw")

ggplot(dados_som_ren,
       aes(x = depression_score,
           y = sss_total)) +
  
  geom_point(alpha = .50,
             size = 2) +
  
  geom_smooth(method = "loess",
              se = TRUE,
              linewidth = 1.1) +
  
  labs(
    x = "Depression score",
    y = "SSS-8 total score"
  ) +
  
  theme_classic(base_size = 14)

dev.off()

# Ansiedade

tiff("Figure_Ansiedade_SSS.tiff",
     width = 7,
     height = 6,
     units = "in",
     res = 600,
     compression = "lzw")


ggplot(dados_som_ren,
       aes(anxiety_score,
           sss_total)) +
  
  geom_point(alpha=.5,size=2)+
  
  geom_smooth(
    method="loess",
    se=TRUE,
    linewidth=1.1)+
  
  labs(
    x="Anxiety score",
    y="SSS-8 total score")+
  
  theme_classic(base_size=14)

dev.off()

# Estresse

tiff("Figure_stresse_SSS.tiff",
     width = 7,
     height = 6,
     units = "in",
     res = 600,
     compression = "lzw")

ggplot(dados_som_ren,
       aes(stress_score,
           sss_total))+
  
  geom_point(alpha=.5,size=2)+
  
  geom_smooth(
    method="loess",
    se=TRUE,
    linewidth=1.1)+
  
  labs(
    x="Stress score",
    y="SSS-8 total score")+
  
  theme_classic(base_size=14)

dev.off()


# Boxplots (comparando grupos)

# Sexo

tiff("Figure_SSS_totalScore_Gender.tiff",
     width = 7,
     height = 6,
     units = "in",
     res = 600,
     compression = "lzw")


ggplot(
  dados_som_ren,
  aes(gender,
      sss_total,
      fill=gender))+
  
  geom_boxplot(width=.60)+
  
  labs(
    x="Gender",
    y="SSS-8 total score")+
  
  theme_classic(base_size=14)+
  
  theme(
    legend.position="none")

dev.off()


# Outra sugestao para sexo

tiff("Figure_SSS_totalScore_Gender_outraForma.tiff",
     width = 7,
     height = 6,
     units = "in",
     res = 600,
     compression = "lzw")

ggplot(
  dados_som_ren,
  aes(x = gender,
      y = sss_total,
      fill = gender)
) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.9) +
  labs(
    x = "Gender",
    y = "SSS-8 total score"
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")

dev.off()

# Ano do curso

tiff("Figure_SSS_totalScore_anoDoCurso.tiff",
     width = 7,
     height = 6,
     units = "in",
     res = 600,
     compression = "lzw")

ggplot(
  dados_som_ren,
  aes(study_year,
      sss_total,
      fill=study_year))+
  
  geom_boxplot(width=.70)+
  
  labs(
    x="Year of study",
    y="SSS-8 total score")+
  
  theme_classic(base_size=14)+
  
  theme(
    legend.position="none")

dev.off()

# Idade

tiff("Figure_SSS_totalScore_Idade.tiff",
     width = 7,
     height = 6,
     units = "in",
     res = 600,
     compression = "lzw")

ggplot(
  dados_som_ren,
  aes(age_group,
      sss_total,
      fill=age_group))+
  
  geom_boxplot(width=.70)+
  
  labs(
    x="Age group",
    y="SSS-8 total score")+
  
  theme_classic(base_size=14)+
  
  theme(
    legend.position="none")

dev.off()


# Testes de comparacao

# Idade

tbl_summary(
  dados_som_ren,
  by = age_group,
  include = c(
    sss_total,
    depression_score,
    anxiety_score,
    stress_score
  ),
  statistic = all_continuous() ~ "{median} ({p25}, {p75})"
) %>%
  add_p()


# Sexo

tbl_summary(
  dados_som_ren,
  by = gender,
  include = c(
    sss_total,
    depression_score,
    anxiety_score,
    stress_score
  ),
  statistic = all_continuous() ~ "{median} ({p25}, {p75})"
) %>%
  add_p()

# Ano do curso

tbl_summary(
  dados_som_ren,
  by = study_year,
  include = c(
    sss_total,
    depression_score,
    anxiety_score,
    stress_score
  ),
  statistic = all_continuous() ~ "{median} ({p25}, {p75})"
) %>%
  add_p()


# Regressao linear --------------------------------------------------------

# Simples

# Depressao

m1 <- lm(sss_total ~ depression_score, data = dados_som_ren) # com depressao

summary(m1)

# tidy(
#   m1,
#   conf.int = TRUE,
#   conf.level = 0.95
# )


tbl_regression(
  m1,
  intercept = TRUE
)


# 

model_parameters(
  m1,
  vcov = "HC3"
)

# Ansiedade

m2 <- lm(sss_total ~ anxiety_score, data = dados_som_ren)

summary(m2)

tbl_regression(
  m2,
  intercept = F
)

#

model_parameters(
  m2,
  vcov = "HC3"
)


# Estress

m3 <- lm(sss_total ~ stress_score, data = dados_som_ren)

summary(m3)

tbl_regression(
  m3,
  intercept = F
)


#

model_parameters(
  m3,
  vcov = "HC3"
)

# Idade

m4 <- lm(sss_total ~ age_group, data = dados_som_ren)

summary(m4)

tbl_regression(
  m4,
  intercept = F
)

#

model_parameters(
  m4,
  vcov = "HC3"
)

# Sexo

m5 <- lm(sss_total ~ gender, data = dados_som_ren)

summary(m5)

tbl_regression(
  m5,
  intercept = F
)

#

model_parameters(
  m5,
  vcov = "HC3"
)

# Ano de estudo

m6 <- lm(sss_total ~ study_year, data = dados_som_ren)

summary(m6)

tbl_regression(
  m6,
  intercept = F
)

#

model_parameters(
  m6,
  vcov = "HC3"
)

# Multivariado somente com as somaticas

mult1 <- lm(
  sss_total ~ depression_score +
    anxiety_score +
    stress_score,
  data = dados_som_ren
)

summary(mult1)

tbl_regression(
  mult1,
  intercept = F
)

# Modelo todo

mult2 <- lm(
  sss_total ~ depression_score +
    anxiety_score +
    stress_score +
    age_group +
    gender +
    study_year,
  data = dados_som_ren
)

summary(mult2)

tbl_regression(
  mult2,
  intercept = F
)

# Reduzido

mult3 <- lm(
  sss_total ~ depression_score +
    anxiety_score +
    stress_score +
    age_group +
    gender,
  data = dados_som_ren
)

summary(mult3)

tbl_regression(
  mult3,
  intercept = F
)

# AIC

AIC(mult1, mult2, mult3)

# BIC

BIC(mult1, mult2, mult3)

# Residuos

summary(mult1)$sigma
summary(mult2)$sigma
summary(mult3)$sigma


# Verificando as suposicoes -----------------------------------------------

# Normalidade dos resíduos

tiff(
  filename = "diagnostic_plots_mult1.tiff",
  width = 8,
  height = 8,
  units = "in",
  res = 600,
  compression = "lzw"
)

par(mfrow = c(2, 2))
plot(mult1)

dev.off()

# Modelo 2

tiff(
  filename = "diagnostic_plots_mult2.tiff",
  width = 8,
  height = 8,
  units = "in",
  res = 600,
  compression = "lzw"
)

par(mfrow = c(2, 2))
plot(mult2)

dev.off()

# Modelo 3

tiff(
  filename = "diagnostic_plots_mult3.tiff",
  width = 8,
  height = 8,
  units = "in",
  res = 600,
  compression = "lzw"
)

par(mfrow = c(2, 2))
plot(mult3)

dev.off()


# Homocedasticidade

library(lmtest)

bptest(mult1)
bptest(mult2)
bptest(mult3)

# Ou

library(car)

ncvTest(mult1)

# Multicolinearidade

vif(mult1)

vif(mult2)

vif(mult3)

# Influencia das observacoes 

influencePlot(mult1, mult2, mult3)

plot(cooks.distance(mult1), type = "h")
abline(h = 4/nrow(dados_som_ren), col = "red", lty = 2)

# gráficos de influência, Cook's distance, leverage e resíduos studentizados para mult1, mult2 e mult3

# Lista dos modelos

modelos <- list(
  "Model I"   = mult1,
  "Model II"  = mult2,
  "Model III" = mult3
)

# Valor de referência para Cook's distance
cook.cutoff <- 4 / nrow(dados_som_ren)

for(i in seq_along(modelos)){
  
  modelo <- modelos[[i]]
  nome   <- names(modelos)[i]
  
  cat("\n=============================\n")
  cat(nome, "\n")
  cat("=============================\n")
  
  ## Influence plot
  influencePlot(
    modelo,
    main = paste("Influence Plot -", nome),
    id.method = "identify",
    sub = ""
  )
  
  ## Cook's distance
  plot(
    cooks.distance(modelo),
    type = "h",
    main = paste("Cook's Distance -", nome),
    ylab = "Cook's distance",
    xlab = "Observation"
  )
  abline(h = cook.cutoff, col = "red", lty = 2)
  
  ## Leverage
  plot(
    hatvalues(modelo),
    type = "h",
    main = paste("Leverage -", nome),
    ylab = "Hat values",
    xlab = "Observation"
  )
  abline(h = 2 * mean(hatvalues(modelo)),
         col = "blue",
         lty = 2)
  
  ## Studentized residuals
  plot(
    rstudent(modelo),
    type = "h",
    main = paste("Studentized Residuals -", nome),
    ylab = "Studentized residual",
    xlab = "Observation"
  )
  abline(h = c(-3, 3),
         col = "red",
         lty = 2)
  
  ## Observações influentes
  cat("\nObservações com Cook's distance >",
      round(cook.cutoff,4), "\n")
  print(which(cooks.distance(modelo) > cook.cutoff))
  
  cat("\nObservações com leverage elevado\n")
  print(which(hatvalues(modelo) >
                2*mean(hatvalues(modelo))))
  
  cat("\nObservações com |resíduo studentizado| > 3\n")
  print(which(abs(rstudent(modelo)) > 3))
  
}

# Ainda melhor


modelos <- list(
  "Model I"   = mult1,
  "Model II"  = mult2,
  "Model III" = mult3
)

for(i in seq_along(modelos)){
  
  modelo <- modelos[[i]]
  nome   <- names(modelos)[i]
  
  cat("\n========================================\n")
  cat(nome,"\n")
  cat("========================================\n")
  
  cat("\nBreusch-Pagan\n")
  print(bptest(modelo))
  
  cat("\nDurbin-Watson\n")
  print(durbinWatsonTest(modelo))
  
  cat("\nVIF\n")
  print(vif(modelo))
  
  cat("\nNormalidade dos resíduos\n")
  print(shapiro.test(rstandard(modelo)))
  
}

# Gráfico

## Influence plot :: Model 1

tiff("Figure_S1_Influence_Model1.tiff",
     width = 18, height = 16,
     units = "cm",
     res = 600,
     compression = "lzw")

influencePlot(mult1,
              main = "Model II",
              id.method = "identify",
              sub = "")

dev.off()


## Cook's distance

tiff("Figure_S2_Cooks_Model1.tiff",
     width = 18, height = 16,
     units = "cm",
     res = 600,
     compression = "lzw")

plot(cooks.distance(mult1),
     type = "h",
     ylab = "Cook's distance",
     xlab = "Observation")

abline(h = 4/nrow(dados_som_ren),
       col = "red",
       lty = 2,
       lwd = 2)

dev.off()


## Leverage

tiff("Figure_S3_Leverage_Model1.tiff",
     width = 18, height = 16,
     units = "cm",
     res = 600,
     compression = "lzw")

plot(hatvalues(mult1),
     type = "h",
     ylab = "Leverage",
     xlab = "Observation")

abline(h = 2*mean(hatvalues(mult1)),
       col = "blue",
       lty = 2,
       lwd = 2)

dev.off()


## Studentized residuals
tiff("Figure_S4_Residuals_Model1.tiff",
     width = 18, height = 16,
     units = "cm",
     res = 600,
     compression = "lzw")

plot(rstudent(mult1),
     type = "h",
     ylab = "Studentized residual",
     xlab = "Observation")

abline(h = c(-3,3),
       col = "red",
       lty = 2,
       lwd = 2)

dev.off()

# uma unica figura

tiff("Figure_S1_Model1_Diagnostics.tiff",
     width = 24,
     height = 24,
     units = "cm",
     res = 600,
     compression = "lzw")

par(mfrow = c(2,2))

plot(mult1)

dev.off()

# Model II

tiff("Figure_S1_Influence_Model2.tiff",
     width = 18, height = 16,
     units = "cm",
     res = 600,
     compression = "lzw")

influencePlot(mult2,
              main = "Model III",
              id.method = "identify",
              sub = "")

dev.off()


## Cook's distance

tiff("Figure_S2_Cooks_Model2.tiff",
     width = 18, height = 16,
     units = "cm",
     res = 600,
     compression = "lzw")

plot(cooks.distance(mult2),
     type = "h",
     ylab = "Cook's distance",
     xlab = "Observation")

abline(h = 4/nrow(dados_som_ren),
       col = "red",
       lty = 2,
       lwd = 2)

dev.off()


## Leverage

tiff("Figure_S3_Leverage_Model2.tiff",
     width = 18, height = 16,
     units = "cm",
     res = 600,
     compression = "lzw")

plot(hatvalues(mult2),
     type = "h",
     ylab = "Leverage",
     xlab = "Observation")

abline(h = 2*mean(hatvalues(mult2)),
       col = "blue",
       lty = 2,
       lwd = 2)

dev.off()


## Studentized residuals
tiff("Figure_S4_Residuals_Model2.tiff",
     width = 18, height = 16,
     units = "cm",
     res = 600,
     compression = "lzw")

plot(rstudent(mult2),
     type = "h",
     ylab = "Studentized residual",
     xlab = "Observation")

abline(h = c(-3,3),
       col = "red",
       lty = 2,
       lwd = 2)

dev.off()

# Modelo 3

tiff("Figure_S1_Influence_Model4.tiff",
     width = 18, height = 16,
     units = "cm",
     res = 600,
     compression = "lzw")

influencePlot(mult3,
              main = "Model IV",
              id.method = "identify",
              sub = "")

dev.off()


## Cook's distance

tiff("Figure_S2_Cooks_Model4.tiff",
     width = 18, height = 16,
     units = "cm",
     res = 600,
     compression = "lzw")

plot(cooks.distance(mult3),
     type = "h",
     ylab = "Cook's distance",
     xlab = "Observation")

abline(h = 4/nrow(dados_som_ren),
       col = "red",
       lty = 2,
       lwd = 2)

dev.off()


## Leverage

tiff("Figure_S3_Leverage_Model4.tiff",
     width = 18, height = 16,
     units = "cm",
     res = 600,
     compression = "lzw")

plot(hatvalues(mult3),
     type = "h",
     ylab = "Leverage",
     xlab = "Observation")

abline(h = 2*mean(hatvalues(mult3)),
       col = "blue",
       lty = 2,
       lwd = 2)

dev.off()


## Studentized residuals
tiff("Figure_S4_Residuals_Model4.tiff",
     width = 18, height = 16,
     units = "cm",
     res = 600,
     compression = "lzw")

plot(rstudent(mult3),
     type = "h",
     ylab = "Studentized residual",
     xlab = "Observation")

abline(h = c(-3,3),
       col = "red",
       lty = 2,
       lwd = 2)

dev.off()


# Utilizar erros-padrão robustos à heterocedasticidade (HC3 ou HC4)

# Modelo univariado

# install.packages("parameters")

library(parameters)

# Modelo II

model_parameters(
  mult1,
  vcov = "HC3"
)

# Modelo III

model_parameters(
  mult2,
  vcov = "HC3"
)


# Modelo IV

model_parameters(
  mult3,
  vcov = "HC3"
)



# Regressao logistica -----------------------------------------------------

# Modelo Univarido

# Depressao 

mod_dep <- MASS::polr(
  sss_severity ~ depression_score,
  data = dados_som_ren,
  Hess = TRUE
)

# Resumo
s <- summary(mod_dep)

# Coeficientes
beta <- coef(mod_dep)

# OR
OR <- exp(beta)

# IC95%
IC <- exp(confint(mod_dep))

# p-valores
p <- 2 * pnorm(abs(s$coefficients[1:length(beta), "t value"]),
               lower.tail = FALSE)

# Tabela (um único preditor: confint() devolve um vetor)
resultado <- data.frame(
  Variable = "Depression score",
  Beta = beta,
  OR = OR,
  Lower95 = IC[1],
  Upper95 = IC[2],
  p = p,
  row.names = NULL
)

resultado


# ANsiedade


mod_anx <- MASS::polr(
  sss_severity ~ anxiety_score,
  data = dados_som_ren,
  Hess = TRUE
)

# Resumo
s <- summary(mod_anx)

# Coeficiente
beta <- coef(mod_anx)

# OR
OR <- exp(beta)

# IC95% (um único preditor: confint() devolve um vetor)
IC <- exp(confint(mod_anx))

# p-valor
p <- 2 * pnorm(abs(s$coefficients[1, "t value"]), lower.tail = FALSE)

# Tabela
resultado <- data.frame(
  Variable = "Anxiety score",
  Beta = beta,
  OR = OR,
  Lower95 = IC[1],
  Upper95 = IC[2],
  p = p,
  row.names = NULL
)

resultado


# Stress

mod_stree <- MASS::polr(
  sss_severity ~ stress_score,
  data = dados_som_ren,
  Hess = TRUE
)

# Resumo
s <- summary(mod_stree)

# Coeficiente
beta <- coef(mod_stree)

# OR
OR <- exp(beta)

# IC95%
IC <- exp(confint(mod_stree))

# p-valor
p <- 2 * pnorm(abs(s$coefficients[1, "t value"]), lower.tail = FALSE)

# Tabela
resultado <- data.frame(
  Variable = "Stress score",
  Beta = beta,
  OR = OR,
  Lower95 = IC[1],
  Upper95 = IC[2],
  p = p,
  row.names = NULL
)

resultado

# Idade

mod_age <- MASS::polr(
  sss_severity ~ age_group,
  data = dados_som_ren,
  Hess = TRUE
)

# Resumo
s <- summary(mod_age)

# Coeficientes
beta <- coef(mod_age)

# OR
OR <- exp(beta)

# IC95%
IC <- exp(confint(mod_age))

# p-valores
p <- 2 * pnorm(abs(s$coefficients[1:length(beta), "t value"]),
               lower.tail = FALSE)

# Tabela
resultado <- data.frame(
  Variable = names(beta),
  Beta = beta,
  OR = OR,
  Lower95 = IC[,1],
  Upper95 = IC[,2],
  p = p,
  row.names = NULL
)

resultado


# Sexo


mod_gender <- MASS::polr(
  sss_severity ~ gender,
  data = dados_som_ren,
  Hess = TRUE
)

# Resumo
s <- summary(mod_gender)

# Coeficientes
beta <- coef(mod_gender)

# OR
OR <- exp(beta)

# IC95%
IC <- exp(confint(mod_gender))

# p-valores
p <- 2 * pnorm(abs(s$coefficients[1:length(beta), "t value"]),
               lower.tail = FALSE)

# Tabela
resultado <- data.frame(
  Variable = names(beta),
  Beta = beta,
  OR = OR,
  Lower95 = IC[,1],
  Upper95 = IC[,2],
  p = p,
  row.names = NULL
)

resultado


# Anos de estudo

mod_year <- MASS::polr(
  sss_severity ~ study_year,
  data = dados_som_ren,
  Hess = TRUE
)

# Resumo
s <- summary(mod_year)

# Coeficientes
beta <- coef(mod_year)

# OR
OR <- exp(beta)

# IC95%
IC <- exp(confint(mod_year))

# p-valores
p <- 2 * pnorm(abs(s$coefficients[1:length(beta), "t value"]),
               lower.tail = FALSE)

# Tabela
resultado <- data.frame(
  Variable = names(beta),
  Beta = beta,
  OR = OR,
  Lower95 = IC[,1],
  Upper95 = IC[,2],
  p = p,
  row.names = NULL
)

resultado


# De outra forma


#---------------------------------------------------------
# Função para extrair resultados de modelos polr()
#---------------------------------------------------------

extrair_polr <- function(modelo){
  
  s <- summary(modelo)
  
  # Coeficientes
  beta <- coef(modelo)
  
  # Odds Ratios
  OR <- exp(beta)
  
  # Intervalo de confiança
  IC <- exp(confint(modelo))
  
  # Se houver apenas um coeficiente, transformar em matriz
  if(is.null(dim(IC))){
    IC <- matrix(IC, nrow = 1,
                 dimnames = list(names(beta), c("2.5 %","97.5 %")))
  }
  
  # p-valores
  p <- 2 * pnorm(abs(s$coefficients[1:length(beta), "t value"]),
                 lower.tail = FALSE)
  
  resultado <- data.frame(
    Variable = names(beta),
    Beta = round(beta, 3),
    OR = round(OR, 2),
    Lower95 = round(IC[,1], 2),
    Upper95 = round(IC[,2], 2),
    p = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)),
    row.names = NULL
  )
  
  return(resultado)
}

# Depressao

mod_dep <- MASS::polr(
  sss_severity ~ depression_score,
  data = dados_som_ren,
  Hess = TRUE
)

extrair_polr(mod_dep)

# Ansiedade

extrair_polr(mod_anx)

# Stress

extrair_polr(mod_stree)

# IDade

extrair_polr(mod_age)

# Sexo

extrair_polr(mod_gender)

# Ano de estudo

extrair_polr(mod_year)


# Modelo multivariado

mod_multi <- MASS::polr(
  sss_severity ~ depression_score +
    anxiety_score +
    stress_score +
    age_group +
    gender +
    study_year,
  data = dados_som_ren,
  Hess = TRUE
)


resultado_multi <- extrair_polr(mod_multi)

resultado_multi
