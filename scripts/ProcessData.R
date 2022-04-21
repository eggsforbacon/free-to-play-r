# Package dependencies 

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

##### Shame doc: everything we don't know where to put but we still need to remember
###### The other games: Wild Rift, Genshin Impact, Brawl Stars, Dofus, Clash of Clans, Mobile Legends, Parchis

dataf <- read_excel("data/raw/Answers.xlsx")
View(dataf)

attach(dataf)

gender <- factor(c(dataf$`¿Cuál es su género?`))
gender
gender_unclass <- unclass(gender)
gender_unclass
