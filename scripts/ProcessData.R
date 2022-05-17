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
library(gginference)

##### Shame doc: everything we don't know where to put but we still need to remember
###### The other games: Wild Rift, Genshin Impact, Brawl Stars, Dofus, Clash of Clans, Mobile Legends, Parchis


dataf <- read_excel("data/raw/Answers.xlsx")
View(dataf)

## Exploratory data analysis for gasto
eda(dataf$gasto)

##Prueba de hipotesis para la media
t.test(dataf$gasto,mu=50000,alternative="less")

##Prueba de hipotesis para la proporcion
PDataSucces <- filter(dataf, playstore == 1)
binom.test(x = nrow(PDataSucces), n = nrow(dataf),p = 0.5, alternative = "less")


## Hipotesis para diferencia de medias independientes (I'm sorry Samu but I have to be non-binaryphobic for this point) // torste :c
genderlist <- split(dataf,dataf$genero)
gastoMujeres <- genderlist[[1]][4]
gastoHombres <- genderlist[[2]][4]
t.test(gastoHombres$gasto, gastoMujeres$gasto,alternative = "greater")


## Hipotesis para diferencia de medias dependientes
t.test(dataf$tiempoVacaciones,dataf$tiempoAcademico, paired = TRUE, alternative = "greater")

## Chi-square
newdataF <- subset(dataf, gasto > 0 & gasto <= 1000000)
attach(newdataF)

tabla <- table(newdataF$genero)
tabla.1 <- freq(newdataF$genero, plot = FALSE)
tabla.1

puntG <- cut(newdataF$gasto, seq(from = 20, to = 89300, by = 14880), include.lowest = TRUE)
tabla.2 <- freq(ordered(puntG), plot = FALSE)
tabla.2

tabla.3 <- crosstab(newdataF$genero, puntG, plot = TRUE, prop.c = TRUE, prop.r = TRUE, xlab = "Gasto", ylab = "Género")
tabla.3
with(dataf, chisq.test(newdataF$genero, puntG, correct = TRUE))

## Hip�tesis para diferencia de multiples medias
dataf_new <- subset(dataf, gasto>0 & gasto<= 80000)

dataf_new <- dataf[-c(21), ]
dataf_new <- dataf_new[-c(), ]
view(dataf_new)
attach(dataf_new)
spendings_new <- gasto
favGenre <-as.factor(generoFavorito)
boxplot(spendings_new~generoFavorito)
anova<-aov(lm(spendings_new ~ favGenre))
summary(anova)


attach(dataf_new)
gender <- as.factor(genero)
boxplot(spendings_new~genero)
anova <- aov(lm(spendings_new ~ gender))
summary(anova)

## Ninguna es diferente, pero hay muchos datos atipicos

#Regresion
new_Data <- subset(dataf,gasto <= 80000)
attach(new_Data)
plot(tiempoVacaciones,gasto)
cor.test(gasto, tiempoVacaciones)

Regresion <- lm(gasto ~ tiempoVacaciones, data = new_data)
summary(Regresion)

## Independencia
dwtest(Regresion)

## Heterocedasticidad
bptest(Regresion)

## Residuos
Residuos <- residuals(Regresion)

## Normalidad
shapiro.test(Residuos)
