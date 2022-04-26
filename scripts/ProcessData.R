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
library(PASWR2)

##### Shame doc: everything we don't know where to put but we still need to remember
###### The other games: Wild Rift, Genshin Impact, Brawl Stars, Dofus, Clash of Clans, Mobile Legends, Parchis

dataf <- read_excel("data/raw/AnswersDepurated.xlsx")
View(dataf)

## Data quality verification
# boxplot(dataf$gasto)

## Exploratory data analysis for gasto
eda(dataf$gasto)

## Show the proportion of players and non-players

#attach(dataf)
#Players <- table(factor(juegaVideojuegos, labels = c("No juegan", "Sí juegan")))
#barchart(Players)
#pie(Players, main = "Distribución de jugadores y no jugadores")

## Exploratory data analysis
spendings <- dataf$gasto
eda(spendings)

boxplot(dataf$gasto)
boxplot(dataf$edad)

## Estimacion de la media del gasto
summary(dataf$gasto)


## Estimacion proporcion de personas que gastan dinero
CantidadDeDineroGastada<- c("Mayor a 0","Igual a 0")
cantidadDePersonas <- c(length(dataf$gasto[dataf$gasto > 0]),length(dataf$gasto[dataf$gasto == 0]))
proporcion=cantidadDePersonas/length(dataf$gasto)
tabla.frec=data.frame(CantidadDeDineroGastada,cantidadDePersonas,proporcion)
tabla.frec

## Hipotesis para diferencia de medias independientes (I'm sorry Samu but I have to be non-binaryphobic for this point) // torste :c
genderlist <- split(dataf,dataf$genero)
gastoMujeres <- genderlist[[1]][4]
gastoHombres <- genderlist[[2]][4]
mean(gastoMujeres$gasto,na.rm = TRUE)
sd(gastoMujeres$gasto, na.rm = TRUE)
mean(gastoHombres$gasto,na.rm = TRUE)
sd(gastoHombres$gasto, na.rm = TRUE)
var.test(gastoHombres$gasto,gastoMujeres$gasto,alternative = "greater")
t.test(gastoHombres$gasto, gastoMujeres$gasto,alternative = "greater")


## Hipotesis para diferencia de medias dependientes
mean(dataf$tiempoVacaciones,na.rm = TRUE)
sd(dataf$tiempoVacaciones,na.rm = TRUE)
mean(dataf$tiempoAcademico,na.rm = TRUE)
sd(dataf$tiempoAcademico,na.rm = TRUE)
t.test(dataf$tiempoVacaciones,dataf$tiempoAcademico,paired = TRUE, alternative = "greater")

## Hipótesis para diferencia de multiples medias

attach(dataf)
outliers <- boxplot(gasto, plot=FALSE)$out
filtered_dataf <- dataf
filtered_dataf <- filtered_dataf[-which(filtered_dataf$gasto %in% outliers), ]
attach(filtered_dataf)
favGenre <-as.factor(generoFavorito)
boxplot(spendings~generoFavorito)
anova<-aov(lm(spendings ~ favGenre))
summary(anova)
TukeyHSD(anova)

