#Read packages
packagez <- c("dplyr","haven","ggplot2","tidyr","ymd","stargazer") 
invisible(lapply(packagez, library, character.only=TRUE)) #Looked this up on StackOverflow. Neat alternative to six lines of "library()"

#Read data
kartell_case <- read_dta("kartell_case_SAM4_H25.dta")

#Prepare data for visualization
kartell_case <- kartell_case %>% mutate(dato = ymd(dato))
kartell_case_long <- kartell_case %>% select(dato, bensinpris, plattspris, skatt) %>% tidyr::gather(serie, verdi, bensinpris:skatt, factor_key = T)

#Visualize
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
#This is probably an inefficient way to do this but I will not outsource my thinking to an LLM:
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
kartell_case$trendp1 <- ifelse(kartell_case$dato >= as.Date("1993-01-04") & kartell_case$dato <= as.Date("1997-08-27"),1,0) #Create new dummies
kartell_case$trendp2 <- ifelse(kartell_case$dato >= as.Date("1997-08-28") & kartell_case$dato <= as.Date("1999-03-01"),1,0)
kartell_case$trendp3 <- ifelse(kartell_case$dato >= as.Date("1999-03-02") & kartell_case$dato <= as.Date("2000-03-31"),1,0)


regression3 <- lm(
  bensinpris ~ plattspris + skatt + jan + feb + mars + apr + may + jun + jul + aug + sep + okt + nov + trendp1 + trendp2 + trendp3, data = kartell_case)
summary(regression3) 
# WTF is this supposed to tell me!!!!!?? Why is trendp3 NA? When on earth were we supposed to know all this?
#Ok så jeg googla det nå og det skyldes kolinnearitet (husker du fra MET2?) - vi må ekskludere en dummy siden de ellers adder opp til 1. 
#Men hvordan skal vi tolke alle disse tingene. Skjønner ikke en dritt
stargazer(regression3,type="text")


#Oppgave 6
regression4 <- lm(
  bensinpris ~ plattspris + skatt + jan + feb + mars + apr + may + jun + jul + aug + sep + okt + nov + trendp1 + trendp2 + trendp3 + priskrig, data = kartell_case)
summary(regression4) 

#Skjønner ingenting egentlig.