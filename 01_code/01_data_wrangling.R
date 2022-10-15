#------------------------------------------------------------------------------#
# Proyecto:                 MIT Policy Hackathon 2022 - Logistics Challenge
# Objetivo:                 Clean raw data for modeling 
#
# Encargadas:               Regina Isabel Medina
# Correos:                  regimedina19@gmail.com
# 
# Fecha de creación:        October 15th, 2022 
# Última actualización:     October 15th, 2022 
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

Sys.setlocale("LC_TIME", "es_ES")

# Opciones globales
options(dplyr.summarise.inform = FALSE) # Silenciar mensajes de .group en dplyr
options(encoding = "UTF-8") # Establecer encoding
options(scipen=999) # Desactivar notación científica 

# Cargar paquetería 
require(pacman)
p_load(readr, readxl, inegiR, tidyverse, dplyr, lubridate, openxlsx, beepr)


# Vaciar espacio de trabajo 
rm(list=ls())


# Funciones de importación y exportación
paste_inp <- function(x){paste0("02_raw_data/" , x)}
paste_out <- function(x){paste0("03_clean_data/", x)}
