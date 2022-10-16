#------------------------------------------------------------------------------#
# Project:        MIT Policy Hackathon 2022 - Logistics Challenge
# Goal:           Estimate a probit model for likelihood of being evicted
#   
# Author:         Regina Isabel Medina
# Email:          regimedina19@gmail.com
# 
# Created on:     October 15th, 2022 
# Updated on:     October 15th, 2022 
#------------------------------------------------------------------------------#

# Microdata source: 
# https://www.census.gov/programs-surveys/household-pulse-survey/datasets.html#phase3.6


# 0. Initial set up ------------------------------------------------------------

Sys.setlocale("LC_TIME", "es_ES")

# Opciones globales
options(dplyr.summarise.inform = FALSE) # Silence dplyr's .group message
options(encoding = "UTF-8") # Establecer encoding
options(scipen=999) # Desactivar notación científica 

# Install packages
# devtools::install_github('IQSS/Zelig') # Probit modeling with survey data

# Load packages
require(pacman)
p_load(readr, readxl, tidyverse, dplyr, lubridate, openxlsx, janitor, 
       Zelig, lme4, beepr)

# Clean workspace
rm(list=ls())

# Define path functions
paste_inp <- function(x){paste0("02_raw_data/03_household_pulse_survey/"  , x)}
paste_out <- function(x){paste0("03_clean_data/03_household_pulse_survey/", x)}
paste_fig <- function(x){paste0("04_figures/03_household_pulse_survey/"   , x)}


# 1. Load data -----------------------------------------------------------------

# Import data on likelihood of being evicted from the Census Data 
df_raw1 <- read_xlsx(paste_inp("pulse2022_puf_49.xlsx"), skip = 0)
# df_raw2 <- read_xlsx(paste_inp("pulse2022_repwgt_puf_49.xlsx"), skip = 0)


# 2. Data wrangling ------------------------------------------------------------

## 2.0. Data dictionary --------------------------------------------------------

# ---- Relevant socio-demographic variables
# TBIRTH_YEAR (estimated with): age, 
# EGENID_BIRTH:                 sex at birth,  
# GENID_DESCRIBE:               gender identity, 
# SEXUAL_ORIENTATION:           sexual orientation, 
# RHISPANIC & RRACE:            race, 
# EEDUC:                        education, 
# MS:                           marital status, 
# THHLD_NUMPER:                 household size, 
# THHLD_NUMKID:                 presence of children under 18 in the household, 
# KIDS_LT5Y:                    number of toddlers
# WRKLOSSRV:                    loss of employment income in last 4 weeks, 
# ANYWORK:                      employment in the last 7 days, 
# INCOME:                       household income, 
# SPND_SRCRV1-SPND_SRCRV11:     income sources alternative to employment income, 
# ACTVDUTY1-ACTVDUTY5:          military duty, 
# SEEING:                       difficulty seeing, 
# HEARING:                      difficulty hearing, 
# REMEMBERING:                  difficulty remembering or concentrating, 
# MOBILITY:                     difficulty walking of climbing stairs,
# SELFCARE:                     difficulty with self-care, 
# UNDERSTAND:                   difficulty understanding or being understood 

# ---- Rent related data
# TENURE:     housing owned or rented 
# TRENTAMT:   rent paid
# RENTCHNG:   changes to rent amount 
# LIVQTRRV:   building type 
# RENTCUR:    Caught up on rent 
# RENTASSIST: Application for emergency rental assistance
# EVICT:      Eviction in next two months

# ---- Survey design variables 
# EST_ST:     state
# PWEIGHT:    person level weight 
# HWEIGHT:    household level weight 
# REGION:     region

## 2.1. Cleaning ---------------------------------------------------------------

table(df_raw1$WRKLOSSRV)
table(df_raw1$THHLD_NUMADLT)

df_clean <- df_raw1 |>
  clean_names()     |>
  # Codify missing values
  mutate_if(is.numeric, .funs = ~na_if(abs(.),  88)) |>
  mutate_if(is.numeric, .funs = ~na_if(abs(.),  99)) |>
  # Filter for target population (people who rent)
  # filter(tenure == 3) |>
  # Sociodemographic cleaning 
  rename(
    n_people = thhld_numper, 
    n_adults = thhld_numadlt, 
    n_kids = thhld_numkid)  |>
  mutate(
    age         = 2022-tbirth_year, 
    non_white   = if_else(rrace                != 1, 1, 0), 
    white       = if_else(rrace                == 1, 1, 0), 
    black       = if_else(rrace                == 2, 1, 0), 
    asian       = if_else(rrace                == 3, 1, 0), 
    hispanic    = if_else(rhispanic            == 2, 1, 0), 
    female      = if_else(egenid_birth         == 2, 1, 0),
    trans       = if_else(genid_describe       == 3, 1, 0), 
    non_het     = if_else(sexual_orientation   != 2, 1, 0), 
    lgbt        = if_else(trans == 1 | non_het == 1, 1, 0), 
    kids        = if_else(n_kids != 0, 1, 0), 
    hs_graduate = if_else(eeduc >= 3, 1, 0), 
    college     = if_else(eeduc >= 5, 1, 0), 
    single = if_else(ms != 1, 1, 0), 
    unemployment_month = if_else(wrklossrv == 1, 1, 0), 
    unemployment_week  = if_else(anywork   == 1, 1, 0), 
    military = case_when(
      actvduty1 == 1 ~ 1, 
      actvduty2 == 1 ~ 1, 
      actvduty3 == 1 ~ 1, 
      actvduty4 == 1 ~ 1, 
      actvduty5 == 1 ~ 1, 
      T ~ 0), 
    dis_seeing      = if_else(seeing      >= 2, 1, 0), 
    dis_hearing     = if_else(hearing     >= 2, 1, 0), 
    dis_remembering = if_else(remembering >= 2, 1, 0), 
    dis_mobility    = if_else(mobility    >= 2, 1, 0), 
    dis_selfcare    = if_else(selfcare    >= 2, 1, 0), 
    dis_understand  = if_else(understand  >= 2, 1, 0), 
    disability_phys = if_else(
      (dis_seeing == 1 | dis_hearing == 1 |  dis_mobility == 1), 1, 0), 
    disability_mental = if_else(
    (dis_remembering == 1 | dis_selfcare == 1 | dis_understand == 1), 1, 0))  |>
  # Renting cleaning
  rename(monthly_rent = trentamt)  |> 
  mutate(
    renter = if_else(tenure == 3, 1, 0), 
    rent_increase = if_else(rentchng >= 3, 1, 0), 
    rent_delay = if_else(rentcur == 2, 1, 0), 
    rent_delay_months = if_else(renter == 1, tmnthsbhnd, 0), 
    rent_assistance = if_else(rentassist == 1, 1, 0), 
    eviction_likelihood = case_when(
      evict == 1 ~ 1, 
      evict == 2 ~ 0.75, 
      evict == 3 ~ 0.25, 
      evict == 4 ~ 0))

sum(is.na(df_clean$trans))
table(df_clean$age)

# single caregiver (single*kids)

## 3. Modeling -----------------------------------------------------------------

table(df_clean$evicion)

df_renters <- df_clean |>
  filter(renter == 1)

# Model 
m1 <- glm(
  eviction_likelihood ~ age + female + white + black + asian + hispanic + non_het + trans + single + kids + college + unemployment_month + military + disability_phys + disability_mental + rent_assistance + rent_increase + rent_delay_months + (single*kids), 
    family = binomial(link = "probit"), 
    weights = df_clean$pweight, 
    data = df_clean, 
    # na.rm = T
    )

stargazer::stargazer(m1)
summary(m1)

# Coefficients 
df_coefficients <- as.data.frame(m1$coefficients) |>
  rownames_to_column()                            |>
  rename(variable = 1, coefficient = 2)           




## 4. Figures ------------------------------------------------------------------

## 4.1. Set up -----------------------------------------------------------------

# ---- Tema
tema <-  theme_minimal() +
  theme(
    text             = element_text(family = "Fira Sans", color = "black"),
    plot.title       = element_text(size = 10, face = "bold", hjust = 0.5, margin = margin(10,5,5,5), family="Fira Sans", color = "black"),
    plot.subtitle    = element_text(size = 8, color = "#666666", hjust = 0.5, margin = margin(5, 5, 5, 5), family="Fira Sans"),
    plot.caption     = element_text(hjust = .5, size = 6, family = "Fira Sans", color = "black"),
    panel.grid       = element_line(linetype = 2),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position  = "top",
    legend.title     = element_text(size = 6, face = "bold", family="Fira Sans"),
    legend.text      = element_text(size = 8, family="Fira Sans"),
    axis.title.x     = element_text(size = 8, hjust = .5, margin = margin(1,1,1,1), family="Fira Sans"),
    axis.title.y     = element_text(size = 10, hjust = .5, margin = margin(1,1,1,1), family="Fira Sans", angle = 90),
    axis.text.y      = element_text(size = 7, family="Fira Sans", angle=0, hjust=1),
    axis.text.x      = element_text(size = 9, family="Fira Sans", angle=0, hjust=.5),
    strip.background = element_rect(fill = "white", colour = NA),
    strip.text.x     = element_text(size = 8, family = "Fira Sans", face = "bold", color = "black"),
    strip.text.y     = element_text(size = 8, family = "Fira Sans", face = "bold", color = "black")) +
  theme(legend.key.size   = unit(1, 'cm'),
        legend.key.height = unit(1, 'cm'),
        legend.key.width  = unit(2, 'cm'))

# ---- Colors vectors (traffic lights)
v_colors <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260") # Green, yellow, orange y red

# ---- Text vectors 
v_caption <- "Source: U.S. Census Bureau Household Pulse Survey, Week 49
Note: Total Population 18 Years and Older
Data processed by ReThinkers team for the Logistics challenge at the MIT Policy Hackathon 2022"

v_empty <- ""

## 4.2. Significan coefficients ------------------------------------------------

df_data <- df_coefficients |>
  filter(!(variable %in% c("military", "female", "(Intercept)", "age", "rent_assistance", "disability_mental"))) |>
  mutate(
    variable = str_replace_all(str_to_sentence(variable), "_", " "), 
    variable = case_when(
      variable == "Unemployment month" ~ "Being unemployed during the last month",
      variable == "Single:kids" ~ "Single caregivers with children",
      variable == "Disability phys" ~ "Physical disability",
      variable == "Rent increase" ~ "Rent increase in the last year",
      variable == "Non het" ~ "Not heterosexual",
      variable == "Single" ~ "Single without children",
      variable == "Kids" ~ "Households with kids and more than one caregiver",
      variable == "Rent delay months" ~ "One extra month in delayed rent payment", 
      T ~ variable
    )
  ) |>
    arrange(coefficient) |>
    mutate(
      variable = forcats::fct_inorder(variable), 
      sign = if_else(coefficient > 0, "positive", "negative"))

# df_personal_traits_coefficients <- df_data
# 
# write.csv(df_personal_traits_coefficients, file = paste_out("df_personal_traits_coefficients.csv"))
# save(df_personal_traits_coefficients, file = paste_out("df_personal_traits_coefficients.RData"))

# Only the plot
load(paste_out("df_personal_traits_coefficients.RData"))

df_data <- df_personal_traits_coefficients |>
  filter(variable != "(Intercept)") |>
  mutate(variable = case_when(
    str_detect(variable, "unemployed") ~ "Faced unemployment in the last month", 
    variable == "Trans"                ~ "Being trans", 
    variable == "Single caregivers with children" ~ "Being a single caregiver of children", 
    str_detect(variable, "disability") ~ "Having a physical disability", 
    str_detect(variable, "Rent")       ~ "Faced a rent increase during the last year", 
    variable == "Black"                ~ "Being Black", 
    str_detect(variable, "One")        ~ "An extra month delay in rent payment", 
    variable == "Hispanic"             ~ "Being Hispanic", 
    variable == "White"                ~ "Being White", 
    variable == "Asian"                ~ "Being Asian", 
    str_detect(variable, "Not")        ~ "Diverse sexual orientation", 
    str_detect(variable, "without")    ~ "Being a single without children", 
    variable == "College"              ~ "Having a college degree", 
    str_detect(variable, "Households") ~ "Having more than one caregiver for children"

      )) |>
  arrange(coefficient) |>
  mutate(
    variable = forcats::fct_inorder(variable), 
    sign = if_else(coefficient > 0, "positive", "negative"), 
    coefficient = coefficient*100,
    coefficient_text = paste0(if_else(sign == "positive", "+", ""), round(coefficient, 1), " pp"))


# Plot
ggplot(
  # Data 
  df_data, 
  # Coordenatas 
       aes(x = coefficient, y = variable, fill = sign)) +
  # Geoms
  geom_col() +
  geom_text(aes(label = coefficient_text), 
            nudge_x = if_else(df_data$coefficient > 0, 5, -5), 
            family = "Fira Sans", size = 3) +
  # Etiquetas
  labs(
    title = "How does the likelihood of being evicted change?", 
    subtitle = "For significant eviction-risk personal factors\n", 
    x = "\nChange in percentage points (pp) on the likelihood of being evicted (form 0 to 100)\n", 
    y = "", 
    caption = v_caption, 
  ) +
  # Scales
  scale_x_continuous(limits = c(-40, 50)) +
  # scale_y_discrete(label = scales::wrap_format(35)) +
  scale_fill_manual(values = c("#34c0c4", "#1183a1")) +
  # Theme
  tema +
  theme(legend.position = "none")

# ---- Save figure
ggsave(paste_fig("01_significan_risk_factors.png"), 
       device = "png", type = "cairo", 
       width = 8, height = 4)

## 4. Predicting risk ----------------------------------------------------------

v_names <- unique(df_coefficients$variable)

v_intercept <- df_coefficients$coefficient[df_coefficients$variable == v_names[1]]

# Users' data
v_age         <- 24 # Numeric variable, ranging from 18 to 99
v_trans       <- 0  # Binary variable,  Trans person == 1, Cis person == 0 
v_unemployed  <- 0  # Unemployed person == 1, Employed person == 0
v_trans       <- 1
v_singe       <- 1
v_children    <- 0

# Coefficients estimated from the model 
coeff_intercept   <- df_coefficients$coefficient[df_coefficients$variable == "(Intercept)"]
coeff_trans       <- df_coefficients$coefficient[df_coefficients$variable == "Trans"]
coeff_unemployed  <- df_coefficients$coefficient[df_coefficients$variable == "Unemployed"]
 

# Estimated risk 
v_personal_risk <- coeff_intercept + (v_age*coeff_age) + (v_trans*coeff_trans) + (number_months*coeff_months) + ((single*kids)*coeff_single_kids)

