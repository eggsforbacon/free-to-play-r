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

##Prueba de hipotesis para la media
t.test(dataf$gasto,mu=50000,alternative="less")

##Prueba de hipotesis para la proporcion
PDataSucces <- filter(dataf, playstore == 1)
binom.test(x = nrow(PDataSucces), n = nrow(dataf),p = 0.5, alternative = "less")


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

## Chi-square
attach(dataf)

tabla <- table(genero)
tabla.1 <- freq(genero, plot = FALSE)
tabla.1

puntG <- cut(gasto, seq(from = 20, to = 89300, by = 14880), include.lowest = TRUE)
tabla.2 <- freq(ordered(puntG), plot = FALSE)
tabla.2

tabla.3 <- crosstab(genero, puntG, prop.r = FALSE, plot = TRUE, xlab = "Gasto", ylab = "Género")
tabla.3
with(dataf, chisq.test(genero, puntG, correct = TRUE))

## Hipótesis para diferencia de multiples medias

dataf_new <- subset(dataf, gasto>0 & gasto<= 80000)

## Hipotesis para diferencia de multiples medias
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

## Ninguna es diferente, pero hay un chingo de datos atipicos

#Regresion
new_Data <- subset(dataf,gasto <= 80000)
attach(new_Data)
plot(tiempoVacaciones,gasto)
cor.test(gasto, tiempoVacaciones)
Regresion <- lm(gasto ~ tiempoVacaciones, data = new_data)
summary(Regresion)
dwtest(Regresion)
Residuos <- residuals(Regresion)
shapiro.test(Residuos)

Y_estim<-predict(Regresion,list(Tiempo_uso=24), interval = "predict")
Y_estim

Y_estim2<-predict(Regresion,list(Tiempo_uso=24), interval = "confidence")
Y_estim2
