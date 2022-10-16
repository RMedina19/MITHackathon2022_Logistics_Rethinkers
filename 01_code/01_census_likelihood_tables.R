#------------------------------------------------------------------------------#
# Project:        MIT Policy Hackathon 2022 - Logistics Challenge
# Goal:           Get likelihood heatmaps from aggregated data 
#   
# Author:         Regina Isabel Medina
# Email:          regimedina19@gmail.com
# 
# Created on:     October 15th, 2022 
# Updated on:     October 15th, 2022 
#------------------------------------------------------------------------------#

# Source: https://www.census.gov/data/tables/2022/demo/hhp/hhp49.html

# 0. Initial set up ------------------------------------------------------------

Sys.setlocale("LC_TIME", "es_ES")

# Opciones globales
options(dplyr.summarise.inform = FALSE) # Silence dplyr's .group message
options(encoding = "UTF-8") # Establecer encoding
options(scipen=999) # Desactivar notación científica 

# Load packages
require(pacman)
p_load(readr, readxl, tidyverse, dplyr, lubridate, openxlsx, beepr)


# Clean workspace
rm(list=ls())


# Define path functions
paste_inp <- function(x){paste0("02_raw_data/01_census_likelihood_eviction/"  , x)}
paste_out <- function(x){paste0("03_clean_data/01_census_likelihood_eviction/", x)}
paste_fig <- function(x){paste0("04_figures/01_census_likelihood_eviction/"   , x)}


# 1. Load data -----------------------------------------------------------------

# Import data on likelihood of being evicted from the Census Data 
df_raw <- read_xlsx(paste_inp("housing3b_week49.xlsx"), skip = 5) |>
  rename(group = 1, total = 2)

# Note: These data are experimental. Users should take caution using estimates
# based on subpopulations of the data – sample sizes may be small and the 
# standard errors may be large.**

# Total Population 18 Years and Older in Renter-Occupied Housing Units, 
# That Are Not Current on Rental Payments



# 1. Process data --------------------------------------------------------------

## 1.1. Clean each group -------------------------------------------------------

# ---- Total population 
df_total <- df_raw                                        |>
  slice(2)                                                |>
  pivot_longer(cols = -c(group, total),                 
               names_to = "likelihood",                 
               values_to = "subtotal")                    |>
  mutate_at(                
    .vars = c("total", "subtotal"),                 
    .funs = ~as.numeric(.))                               |>
  mutate(         
    prob = subtotal/total,         
    category = "Total population")                        |>
  select(category, group, likelihood, subtotal, prob)     |>
  glimpse()

# Verify that the sum of probabilities equals one
sum(df_total$prob)

# ---- Months behind rental payment
df_months <- df_raw                                       |>
  slice(4:13)                                             |>
  pivot_longer(cols = -c(group, total),                 
               names_to = "likelihood",                 
               values_to = "subtotal")                    |>
  mutate_at(                
    .vars = c("total", "subtotal"),                 
    .funs = ~as.numeric(.))                               |>
  mutate(         
    prob = subtotal/total,         
    category = "Months behind on rental payments")        |>
  select(category, group, likelihood, subtotal, prob)     |>
  glimpse()



# ---- Months behind rental payment
df_rental <- df_raw                                       |>
  slice(15:19)                                            |>
  pivot_longer(cols = -c(group, total),                 
               names_to = "likelihood",                 
               values_to = "subtotal")                    |>
  mutate_at(                
    .vars = c("total", "subtotal"),                 
    .funs = ~as.numeric(.))                               |>
  mutate(         
    prob = subtotal/total,         
    category = "Household rental assistance through state or local government")        |>
  select(category, group, likelihood, subtotal, prob)     |>
  glimpse()



# ---- Age
df_age <- df_raw                                          |>
  slice(21:25)                                              |>
  pivot_longer(cols = -c(group, total),                 
               names_to = "likelihood",                 
               values_to = "subtotal")                    |>
  mutate_at(                
    .vars = c("total", "subtotal"),                 
    .funs = ~as.numeric(.))                               |>
  mutate(               
    prob = subtotal/total,              
    category = "Age")                                     |>
  select(category, group, likelihood, subtotal, prob)     |>
  glimpse()

# Verify that the sum of one group equals one
sum(df_age$prob[df_age$group == unique(df_age$group)[1]])

# ---- Sex
df_sex <- df_raw                                          |>
  slice(27:28)                                            |>
  pivot_longer(cols = -c(group, total),                 
               names_to = "likelihood",                 
               values_to = "subtotal")                    |>
  mutate_at(                
    .vars = c("total", "subtotal"),                 
    .funs = ~as.numeric(.))                               |>
  mutate(               
    prob = subtotal/total,              
    category = "Sex at birth")                            |>
  select(category, group, likelihood, subtotal, prob)     |>
  glimpse()


# Verify that the sum of one group equals one
sum(df_sex$prob[df_sex$group == unique(df_sex$group)[1]])

# ---- Gender identity
df_gender <- df_raw                                       |>
  slice(30:34)                                            |>
  pivot_longer(cols = -c(group, total),                 
               names_to = "likelihood",                 
               values_to = "subtotal")                    |>
  mutate_at(                
    .vars = c("total", "subtotal"),                 
    .funs = ~as.numeric(.))                               |>
  mutate(               
    prob = subtotal/total,              
    category = "Gender identity")                         |>
  select(category, group, likelihood, subtotal, prob)     |>
  glimpse()

# ---- Sexual orientation
df_orientation <- df_raw                                  |>
  slice(36:41)                                            |>
  pivot_longer(cols = -c(group, total),                 
               names_to = "likelihood",                 
               values_to = "subtotal")                    |>
  mutate_at(                
    .vars = c("total", "subtotal"),                 
    .funs = ~as.numeric(.))                               |>
  mutate(               
    prob = subtotal/total,              
    category = "Sexual orientation")                      |>
  select(category, group, likelihood, subtotal, prob)     |>
  glimpse()


# ---- LGBT
df_lgbt <- df_raw                                         |>
  slice(43:46)                                            |>
  pivot_longer(cols = -c(group, total),                 
               names_to = "likelihood",                 
               values_to = "subtotal")                    |>
  mutate_at(                
    .vars = c("total", "subtotal"),                 
    .funs = ~as.numeric(.))                               |>
  mutate(               
    prob = subtotal/total,              
    category = "LBGT")                                    |>
  select(category, group, likelihood, subtotal, prob)     |>
  glimpse()

# ---- Race
df_race <- df_raw                                         |>
  slice(47:52)                                            |>
  pivot_longer(cols = -c(group, total),                 
               names_to = "likelihood",                 
               values_to = "subtotal")                    |>
  mutate_at(                
    .vars = c("total", "subtotal"),                 
    .funs = ~as.numeric(.))                               |>
  mutate(               
    prob = subtotal/total,              
    category = "Race")                                    |>
  select(category, group, likelihood, subtotal, prob)     |>
  glimpse()


# Verify that the sum of one group equals one
sum(df_race$prob[df_race$group == unique(df_race$group)[1]])

# ---- Education 
df_educ <- df_raw                                         |>
  slice(54:57)                                            |>
  pivot_longer(cols = -c(group, total),                 
               names_to = "likelihood",                 
               values_to = "subtotal")                    |>
  mutate_at(                
    .vars = c("total", "subtotal"),                 
    .funs = ~as.numeric(.))                               |>
  mutate(               
    prob = subtotal/total,              
    category = "Education")                               |>
  select(category, group, likelihood, subtotal, prob)     |>
  glimpse()


# Verify that the sum of one group equals one
sum(df_educ$prob[df_educ$group == unique(df_educ$group)[1]], na.rm = T)


# ---- Marital status                                                  
df_marital <- df_raw                                      |>
  slice(59:63)                                            |>
  pivot_longer(cols = -c(group, total),                 
               names_to = "likelihood",                 
               values_to = "subtotal")                    |>
  mutate_at(                
    .vars = c("total", "subtotal"),                 
    .funs = ~as.numeric(.))                               |>
  mutate(               
    prob = subtotal/total,              
    category = "Marital status")                          |>
  select(category, group, likelihood, subtotal, prob)     |>
  glimpse()


# Verify that the sum of one group equals one
sum(df_marital$prob[df_marital$group == unique(df_marital$group)[1]], na.rm = T)

# ---- Household size  
df_household <- df_raw                                    |>
  slice(65:71)                                            |>
  pivot_longer(cols = -c(group, total),                 
               names_to = "likelihood",                 
               values_to = "subtotal")                    |>
  mutate_at(                
    .vars = c("total", "subtotal"),                 
    .funs = ~as.numeric(.))                               |>
  mutate(               
    prob = subtotal/total,              
    category = "Household size")                          |>
  select(category, group, likelihood, subtotal, prob)     |>
  glimpse()


# Verify that the sum of one group equals one
sum(df_household$prob[df_household$group == unique(df_household$group)[1]], na.rm = T)


# ---- Children
df_children <- df_raw                                     |>
  slice(73:74)                                            |>
  pivot_longer(cols = -c(group, total),                 
               names_to = "likelihood",                 
               values_to = "subtotal")                    |>
  mutate_at(                
    .vars = c("total", "subtotal"),                 
    .funs = ~as.numeric(.))                               |>
  mutate(               
    prob = subtotal/total,              
    category = "Presence of children")                    |>
  select(category, group, likelihood, subtotal, prob)     |>
  glimpse()


# Verify that the sum of one group equals one
sum(df_children$prob[df_children$group == unique(df_children$group)[1]], na.rm = T)


# ---- Unemployment
df_unemployment <- df_raw                                 |>
  slice(76:78)                                            |>
  pivot_longer(cols = -c(group, total),                 
               names_to = "likelihood",                 
               values_to = "subtotal")                    |>
  mutate_at(                
    .vars = c("total", "subtotal"),                 
    .funs = ~as.numeric(.))                               |>
  mutate(               
    prob = subtotal/total,              
    category = "Loss of employment income in last 4 weeks")            |>
  select(category, group, likelihood, subtotal, prob)     |>
  glimpse()


# Verify that the sum of one group equals one
sum(df_unemployment$prob[df_unemployment$group == unique(df_unemployment)[1]], na.rm = T)


# ---- Employment
df_employment <- df_raw                                   |>
  slice(80:82)                                            |>
  pivot_longer(cols = -c(group, total),                 
               names_to = "likelihood",                 
               values_to = "subtotal")                    |>
  mutate_at(                
    .vars = c("total", "subtotal"),                 
    .funs = ~as.numeric(.))                               |>
  mutate(               
    prob = subtotal/total,              
    category = "Respondent employed in the last 7 days")  |>
  select(category, group, likelihood, subtotal, prob)     |>
  glimpse()


# Verify that the sum of one group equals one
sum(df_unemployment$prob[df_unemployment$group == unique(df_unemployment)[1]], na.rm = T)


# ---- Household income
df_income <- df_raw                                       |>
  slice(84:92)                                            |>
  pivot_longer(cols = -c(group, total),                 
               names_to = "likelihood",                 
               values_to = "subtotal")                    |>
  mutate_at(                
    .vars = c("total", "subtotal"),                 
    .funs = ~as.numeric(.))                               |>
  mutate(               
    prob = subtotal/total,              
    category = "Household income")                        |>
  select(category, group, likelihood, subtotal, prob)     |>
  glimpse()


# Verify that the sum of one group equals one
sum(df_income$prob[df_income$group == unique(df_income)[1]], na.rm = T)

# ---- Money sources
df_money <- df_raw                                        |>
  slice(94:105)                                            |>
  pivot_longer(cols = -c(group, total),                 
               names_to = "likelihood",                 
               values_to = "subtotal")                    |>
  mutate_at(                
    .vars = c("total", "subtotal"),                 
    .funs = ~as.numeric(.))                               |>
  mutate(               
    prob = subtotal/total,              
    category = "Income sources used in the last 7 days to meet spending needs")                           |>
  select(category, group, likelihood, subtotal, prob)     |>
  glimpse()


# ---- Active duty military
df_military <- df_raw                                     |>
  slice(107:112)                                            |>
  pivot_longer(cols = -c(group, total),                 
               names_to = "likelihood",                 
               values_to = "subtotal")                    |>
  mutate_at(                
    .vars = c("total", "subtotal"),                 
    .funs = ~as.numeric(.))                               |>
  mutate(               
    prob = subtotal/total,              
    category = "Military duty")                           |>
  select(category, group, likelihood, subtotal, prob)     |>
  glimpse()



# ---- Difficulty seeing
df_seeing <- df_raw                                       |>
  slice(114:118)                                            |>
  pivot_longer(cols = -c(group, total),                 
               names_to = "likelihood",                 
               values_to = "subtotal")                    |>
  mutate_at(                
    .vars = c("total", "subtotal"),                 
    .funs = ~as.numeric(.))                               |>
  mutate(               
    prob = subtotal/total,              
    category = "Difficulty seeing")                       |>
  select(category, group, likelihood, subtotal, prob)     |>
  glimpse()


# ---- Difficulty hearing
df_hearing<- df_raw                                       |>
  slice(120:124)                                            |>
  pivot_longer(cols = -c(group, total),                 
               names_to = "likelihood",                 
               values_to = "subtotal")                    |>
  mutate_at(                
    .vars = c("total", "subtotal"),                 
    .funs = ~as.numeric(.))                               |>
  mutate(               
    prob = subtotal/total,              
    category = "Difficulty hearing")                       |>
  select(category, group, likelihood, subtotal, prob)     |>
  glimpse()


# ---- Difficulty remembering or concentratig
df_remembering <- df_raw                                  |>
  slice(126:130)                                            |>
  pivot_longer(cols = -c(group, total),                 
               names_to = "likelihood",                 
               values_to = "subtotal")                    |>
  mutate_at(                
    .vars = c("total", "subtotal"),                 
    .funs = ~as.numeric(.))                               |>
  mutate(               
    prob = subtotal/total,              
    category = "Difficulty remembering or concentrating") |>
  select(category, group, likelihood, subtotal, prob)     |>
  glimpse()

# ---- Difficulty walking of climbings stairs
df_moving <- df_raw                                       |>
  slice(132:136)                                          |>
  pivot_longer(cols = -c(group, total),                 
               names_to = "likelihood",                 
               values_to = "subtotal")                    |>
  mutate_at(                
    .vars = c("total", "subtotal"),                 
    .funs = ~as.numeric(.))                               |>
  mutate(               
    prob = subtotal/total,              
    category = "Difficulty walking or climbing stairs")   |>
  select(category, group, likelihood, subtotal, prob)     |>
  glimpse()

# ---- Difficulty with self-care
df_care <- df_raw                                         |>
  slice(138:142)                                          |>
  pivot_longer(cols = -c(group, total),                 
               names_to = "likelihood",                 
               values_to = "subtotal")                    |>
  mutate_at(                
    .vars = c("total", "subtotal"),                 
    .funs = ~as.numeric(.))                               |>
  mutate(               
    prob = subtotal/total,              
    category = "Difficulty with self-care")               |>
  select(category, group, likelihood, subtotal, prob)     |>
  glimpse()


# ---- Difficulty understanding of being understood
df_understood <- df_raw                                   |>
  slice(144:148)                                          |>
  pivot_longer(cols = -c(group, total),                 
               names_to = "likelihood",                 
               values_to = "subtotal")                    |>
  mutate_at(                
    .vars = c("total", "subtotal"),                 
    .funs = ~as.numeric(.))                               |>
  mutate(               
    prob = subtotal/total,              
    category = "Difficulty understanding of being understood")   |>
  select(category, group, likelihood, subtotal, prob)     |>
  glimpse()

## 1.2. Join groups ------------------------------------------------------------

df_unified <- df_total |>
  bind_rows(
    df_months, df_rental, df_age, df_sex, df_gender, df_orientation, df_lgbt, 
    df_race, df_educ, df_marital, df_household, df_children, df_unemployment, 
    df_employment, df_income, df_money, df_military, df_seeing, df_hearing, 
    df_remembering, df_moving, df_care, df_understood)  |>
  mutate(likelihood = factor(
    likelihood, levels = c("Did not report", 
      "Not likely at all", "Not very likely", "Somewhat likely", "Very likely")))


df_census_likelihood_eviction <- df_unified

write.csv(df_unified, file = paste_out("df_census_likelihood_eviction.csv"))
save(df_census_likelihood_eviction, file = paste_out("df_census_likelihood_eviction.RData"))


# 2. Figures -------------------------------------------------------------------

## 2.1. Set up -----------------------------------------------------------------

# ---- Tema
tema <-  theme_minimal() +
  theme(
    text             = element_text(family = "Fira Sans", color = "black"),
    plot.title       = element_text(size = 12, face = "bold", hjust = 0.5, margin = margin(10,5,5,5), family="Fira Sans", color = "black"),
    plot.subtitle    = element_text(size = 10, color = "#666666", hjust = 0.5, margin = margin(5, 5, 5, 5), family="Fira Sans"),
    plot.caption     = element_text(hjust = .5, size = 6, family = "Fira Sans", color = "black"),
    panel.grid       = element_line(linetype = 2),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position  = "top",
    legend.title     = element_text(size = 6, face = "bold", family="Fira Sans"),
    legend.text      = element_text(size = 8, family="Fira Sans"),
    axis.title.x     = element_text(size = 10, hjust = .5, margin = margin(1,1,1,1), family="Fira Sans"),
    axis.title.y     = element_text(size = 10, hjust = .5, margin = margin(1,1,1,1), family="Fira Sans", angle = 90),
    axis.text.y      = element_text(size = 8, family="Fira Sans", angle=0, hjust=.5),
    axis.text.x      = element_text(size = 8, family="Fira Sans", angle=0, hjust=.5),
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
Note: Total Population 18 Years and Older in Renter-Occupied Housing Units, That Are Not Current on Rental Payments
Data processed by ReThinkers team for the Logistics challenge at the MIT Policy Hackathon 2022"

v_empty <- ""

## 2.1. Total renters ----------------------------------------------------------

# ---- Clean data
df_data <- df_unified                                 |>
  filter(category == unique(df_unified$category)[1])  |>
  glimpse()

# ---- Plot 
ggplot(
  # Data
  df_data, 
  # Coordinates
  aes(x = likelihood, y = group)) +
  # facet_wrap(~ingreso_rango) +
  # Geoms
  geom_tile(aes(fill = prob)) +
  geom_text(aes(label = scales::percent(prob, accuracy = 0.1)), 
            family = "Fira Sans", color = "black", size = 5) +
  # Labels
  labs(
    title    = "Likelihood of eviction", 
    subtitle =  paste0(unique(df_unified$category)[1], "\n"),
    x        = v_empty, 
    y        = v_empty, 
    fill     = "% of renters\nnot current on\nrental payments", 
    caption  = v_caption, 
  ) +
  # Scales
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits=rev) +
  scale_fill_gradient2(label = scales::percent_format(), 
                       high  = v_colors[3], 
                       # mid  = v_colors[2],
                       low   = "white", 
                       # midpoint = 0.25
                       ) +
  # Theme
  tema +
  theme(legend.key.size   = unit(1, 'cm'),
        legend.key.height = unit(1, 'cm'),
        legend.key.width  = unit(2, 'cm'))


# ---- Save figure
ggsave(paste_fig("01_total.png"), 
       device = "png", type = "cairo", 
       width = 6, height = 4)

## 2.2. Loop over categories ---------------------------------------------------

# ---- Loop
for(i in 2:length(unique(df_unified$category))){
  
  # Clean data
  df_data <- df_unified                                 |>
    filter(category == unique(df_unified$category)[i])  |>
    glimpse()
  
  # Plot
  ggplot(
    # Data
    df_data, 
    # Coordinates
    aes(x = likelihood, y = group)) +
    # facet_wrap(~ingreso_rango) +
    # Geoms
    geom_tile(aes(fill = prob)) +
    geom_text(aes(label = scales::percent(prob, accuracy = 0.1)), 
              family = "Fira Sans", color = "black", size = 3) +
    # Labels
    labs(
      title    = "Likelihood of eviction", 
      subtitle =  paste0(unique(df_unified$category)[i], "\n"),
      x        = v_empty, 
      y        = v_empty, 
      fill     = "% of renters\nnot current on\nrental payments", 
      caption  = v_caption, 
    ) +
    # Scales
    scale_x_discrete(position = "top") +
    scale_y_discrete(limits=rev, labels = scales::wrap_format(25)) +
    scale_fill_gradient2(label = scales::percent_format(), 
                         high  = v_colors[3], 
                         # mid  = v_colors[2],
                         low = v_colors[2], 
                         # midpoint = 0.5
                         ) +
    # Theme
    tema 
  
  # Save plot
  ggsave(paste_fig(paste0(str_pad(i, width = 2, pad = "0"), ".png")), 
         device = "png", type = "cairo", 
         width = 6, height = 6)
}

# END. -------------------------------------------------------------------------

