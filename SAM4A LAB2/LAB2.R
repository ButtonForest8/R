# FORARBEID

#Read packages
packagez <- c("dplyr","haven","ggplot2","tidyr","ymd","stargazer") 
invisible(lapply(packagez, library, character.only=TRUE)) #Looked this up on StackOverflow. Neat alternative to six lines of "library()"

#Read data
kartell_case <- read_dta("kartell_case_SAM4_H25.dta")

#Prepare data for visualization
kartell_case <- kartell_case %>% mutate(dato = ymd(dato))
kartell_case_long <- kartell_case %>% select(dato, bensinpris, plattspris, skatt) %>% tidyr::gather(serie, verdi, bensinpris:skatt, factor_key = T)

#--------------------------------------

# OPPGAVER

#Visualize (Oppgave 1)
ggplot(kartell_case_long, aes(x = dato, y = verdi, color = serie)) + 
  geom_line() +
  labs(title = "Utvikling i pris, varekost og skatt", x = element_blank(), y = "Bensinpris i øre per liter", color = "Serie") +
  theme_minimal() + 
  theme(legend.title = element_blank()) #Wow this turned out really neat. 

#Regression (Oppgave 2)
regression <- lm(bensinpris ~ plattspris + skatt, data = kartell_case)
summary(regression)
stargazer(regression,type="text")

#Expansion (Oppgave 3)
regression2 <- lm(bensinpris ~ plattspris + skatt + jan + feb + mars + apr + may + jun + jul + aug + sep + okt + nov, data = kartell_case) 
summary(regression2)
stargazer(regression2,type="text")

#Oppgave 4
ggplot(kartell_case, aes(dato, bensinpris)) +
  geom_line() +
  geom_vline(xintercept = as.Date("1997-08-27"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("1999-03-01"), linetype = "dashed") + 
  labs(title = "Bensinprisutviklingen gikk mot trenden (altså nedover) mellom 1997 og 1999", x = element_blank(), y = "Bensinpris i øre per liter") +
  theme_minimal()

#Oppgave 5

regression3 <- lm(
  bensinpris ~ plattspris + skatt + jan + feb + mars + apr + may + jun + jul + aug + sep + okt + nov + trendp1 + trendp2 + trendp3, data = kartell_case)
summary(regression3) 
stargazer(regression3,type="text")


#Oppgave 6
regression4 <- lm(
  bensinpris ~ plattspris + skatt + jan + feb + mars + apr + may + jun + jul + aug + sep + okt + nov + trendp1 + trendp2 + trendp3 + priskrig, data = kartell_case)
summary(regression4) 
