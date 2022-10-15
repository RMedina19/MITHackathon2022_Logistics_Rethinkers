#------------------------------------------------------------------------------#
# Project:        MIT Policy Hackathon 2022 - Logistics Challenge
# Goal:           Estimate likelihood of being evicted based on sociodem data
#   
# Author:         Regina Isabel Medina
# Email:          regimedina19@gmail.com
# 
# Created on:     October 15th, 2022 
# Updated on:     October 15th, 2022 
#------------------------------------------------------------------------------#

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
paste_inp <- function(x){paste0("02_raw_data/" , x)}
paste_out <- function(x){paste0("03_clean_data/01_census_likelihood_eviction/", x)}
paste_fig <- function(x){paste0("04_figures/01_census_likelihood_eviction/"   , x)}


# 1. Load data -----------------------------------------------------------------

# Import data on likelihood of being evicted from the Census Data 
df_raw <- read_xlsx(paste_inp("housing3b_week33.xlsx"), skip = 5) |>
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

# ---- Age
df_age <- df_raw                                          |>
  slice(4:8)                                              |>
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
  slice(10:11)                                            |>
  pivot_longer(cols = -c(group, total),                 
               names_to = "likelihood",                 
               values_to = "subtotal")                    |>
  mutate_at(                
    .vars = c("total", "subtotal"),                 
    .funs = ~as.numeric(.))                               |>
  mutate(               
    prob = subtotal/total,              
    category = "Sex")                                     |>
  select(category, group, likelihood, subtotal, prob)     |>
  glimpse()


# Verify that the sum of one group equals one
sum(df_sex$prob[df_sex$group == unique(df_sex$group)[1]])

# ---- Race
df_race <- df_raw                                         |>
  slice(13:17)                                            |>
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
  slice(19:22)                                            |>
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
  slice(24:28)                                            |>
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
  slice(30:37)                                            |>
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
  slice(38:39)                                            |>
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
  slice(41:43)                                            |>
  pivot_longer(cols = -c(group, total),                 
               names_to = "likelihood",                 
               values_to = "subtotal")                    |>
  mutate_at(                
    .vars = c("total", "subtotal"),                 
    .funs = ~as.numeric(.))                               |>
  mutate(               
    prob = subtotal/total,              
    category = "Unemployment")                            |>
  select(category, group, likelihood, subtotal, prob)     |>
  glimpse()


# Verify that the sum of one group equals one
sum(df_unemployment$prob[df_unemployment$group == unique(df_unemployment)[1]], na.rm = T)


# ---- Household income
df_income <- df_raw                                       |>
  slice(49:57)                                            |>
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
  slice(59:68)                                            |>
  pivot_longer(cols = -c(group, total),                 
               names_to = "likelihood",                 
               values_to = "subtotal")                    |>
  mutate_at(                
    .vars = c("total", "subtotal"),                 
    .funs = ~as.numeric(.))                               |>
  mutate(               
    prob = subtotal/total,              
    category = "Money sources")                           |>
  select(category, group, likelihood, subtotal, prob)     |>
  glimpse()


# ---- Active duty military
df_military <- df_raw                                     |>
  slice(70:75)                                            |>
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
  slice(77:81)                                            |>
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
  slice(82:87)                                            |>
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
  slice(89:93)                                            |>
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
  slice(95:99)                                            |>
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

## 1.2. Join groups ------------------------------------------------------------

df_unified <- df_total |>
  bind_rows(
    df_age, df_sex, df_race, df_educ, df_marital, df_household, df_children, 
    df_unemployment, df_income, df_money, df_military, df_seeing, df_hearing, 
    df_remembering, df_moving)  |>
  mutate(likelihood = factor(
    likelihood, levels = c("Did not report", 
      "Not likely at all", "Not very likely", "Somewhat likely", "Very likely")))

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
v_caption <- "Source: U.S. Census Bureau Household Pulse Survey, Week 33
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
                       high  = v_colors[4], 
                       mid  = v_colors[2],
                       low = v_colors[1], 
                       midpoint = 0.5) +
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
                         high  = v_colors[4], 
                         mid  = v_colors[2],
                         low = v_colors[1], 
                         midpoint = 0.5) +
    # Theme
    tema 
  
  # Save plot
  ggsave(paste_fig(paste0(str_pad(i, width = 2, pad = "0"), ".png")), 
         device = "png", type = "cairo", 
         width = 6, height = 4)
}

