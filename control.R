#----------------------------------------------------------------------------
# Kalman filter examples - control file
#----------------------------------------------------------------------------
# This script sets up environment ect..


setwd("C:/Users/aelde/OneDrive/Documents/GitHub/Kalman-Filter-Examples")

# Ploting and data import / manipulation

library(tidyverse)
library(readxl)
library(dplyr)
library(Quandl)


# KF packages


# Data

Quandl.api_key("x7vhPMRdCbhdzYrvZZHu")

ABS.metadata <- Quandl.datatable("AUSBS/M")

ToT <- Quandl.datatable("AUSBS/D", series_id = ABS.metadata %>% 
                          filter(name == "Terms of trade: Index" & type == "Seasonally Adjusted") %>%
                          select(series_id)   )



