#------------------------------------------------------------------------------#
# Project:        MIT Policy Hackathon 2022 - Logistics Challenge
# Goal:           Estimate a probit model for likelihood of being evicted
#   
# Author:         Regina Isabel Medina   | Data processed by Sanhitha Cherukupally 
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

# Install packages
# devtools::install_github('IQSS/Zelig') # Probit modeling with survey data

# Load packages
require(pacman)
p_load(readr, readxl, tidyverse, dplyr, lubridate, openxlsx, janitor, 
       Zelig, lme4, beepr)

# Clean workspace
rm(list=ls())


# Define path functions
paste_inp <- function(x){paste0("02_raw_data/04_county_level_data/"  , x)}
paste_out <- function(x){paste0("03_clean_data/04_county_level_data/", x)}
paste_fig <- function(x){paste0("04_figures/04_county_level_data/"   , x)}


# 1. Load data -----------------------------------------------------------------

df_data <- read_csv(paste_inp("dataframe.csv"))
