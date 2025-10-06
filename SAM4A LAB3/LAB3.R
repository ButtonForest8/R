
# ------- GRAVITASJONSMODELLEN FOR HANDEL -------


# Setup
library(readxl)
library(tidyverse)
library(fixest) # Disse lab-ene får oss til å importere så mye at Trump hadde klikka helt
library(stargazer)

gravity <- read_csv("gravity.csv")


# Oppgave 1
gravity2 <- gravity %>% mutate(
  ldist = log(distw_harmonic),
  lgdpo = log(gdp_o),
  lgdpd = log(gdp_d),
  lgdpco = log(gdpcap_o),
  lgdpcd = log(gdpcap_d),
  logtrade = log(1+tradeflow_baci)
  )

regression0 <- lm(logtrade ~ lgdpo + lgdpd + lgdpco + lgdpcd + ldist, data = gravity2) # Men hvorfor med GDP/capita?
summary(regression0)

regression1 <- lm(logtrade ~ lgdpo + lgdpd + ldist, data = gravity2) # Her er uten GDP per capita
summary(regression1)

stargazer(regression0, regression1, type = "latex")


# Oppgave 4
gravity3 <- gravity2 %>% mutate(
  Z = contig + comcol + comlang_off + fta_wto)

regression2 <- lm(logtrade ~ lgdpo + lgdpd + lgdpco + lgdpcd + ldist + Z, data = gravity3)
summary(regression2)
stargazer(regression0,regression2,type="latex")


# Oppgave 5 - light work no reaction


# ------- HANDELENS ELASTISITET OVER TID -------


# Setup
gravity_panel <- read_csv("gravity_panel.csv") # WOW 539000 rows! 


# Oppgave 1
gravity3 <- gravity_panel %>% mutate(
  ldist = log(distw_harmonic),
  logtrade = log(1 + tradeflow_baci)
  )

regression3 <- feols(
  logtrade ~ i(year, ldist, ref = 2000) + contig + comcol + comlang_off + fta_wto | iso3_o + iso3_d + year, 
  data = gravity3, 
  vcov = ~ iso3_o + iso3_d
  )

etable(regression3, signif.code = c("*" = 0.1, "**" = 0.05, "***" = 0.01)) #hvorfor gjør vi sånne tryhard ting?
iplot(regression3, xlab = "", main = "Handelselastisitet over tid") #Hva var galt med GGplot2?


# Oppgave 2 
#Mer sensitiv under finanskrisen, en liten oppgang etter men generell stagnering siden vi har nådd grensen på globalisering etc