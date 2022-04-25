library(tidyverse)
library(lubridate)
library(xts)
library(qrmdata)
library(qrmtools)
library(psych)
library(gmodels)
library(MASS)
library(survival)
library(fitdistrplus)
library(lmtest)
library (fdth)
library(readxl)
library(ggplot2)
library(PASWR2)
library(lattice)
library(descr)
library(openxlsx)

dataf <- read_excel("data/raw/Answers.xlsx")
View(dataf)

##Estimacion de la media del gasto
summary(dataf$gasto)


##Estimacion proporcion de personas que gastan dinero
CantidadDeDineroGastada<- c("Mayor a 0","Igual a 0")
cantidadDePersonas <- c(length(dataf$gasto[dataf$gasto > 0]),length(dataf$gasto[dataf$gasto == 0]))
proporcion=cantidadDePersonas/length(dataf$gasto)
tabla.frec=data.frame(CantidadDeDineroGastada,cantidadDePersonas,proporcion)
tabla.frec


