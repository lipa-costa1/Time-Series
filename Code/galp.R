#### Times series project part 2 ############

library("lubridate")
library(stlplus)
library(readxl)
library(latex2exp)
library(forecast)
library(fpp2)
library(ggplot2)
library(Metrics)

# Load the datasets

data_GALP <- read_excel("C:\\Users\\macac\\OneDrive\\Ambiente de Trabalho\\IST_e_afins\\ST\\Projecto\\GALP ENERGIA-NOMprice.xls")
data_GALP <- data_GALP[,c(1,5)]
data_GALP$Date <- as.Date(data_GALP$Date)


# Checking for missing values : 0 NA
sum(is.na(data_GALP))
