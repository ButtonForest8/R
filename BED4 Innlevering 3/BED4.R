library(forecast) # for ARIMA
library(tidyverse) # for dplyr og ggplot2
library(writexl) # for å kunne eksportere til .xlsx

load("p_da.Rdata")


# ----- NO3 ----- #

dekomponert_no3 <- stl(no3, s.window = "periodic") # sesong-trend-dekomponering

prognose_no3 <- forecast(dekomponert_no3, method = "arima", h = 24, level = 0.95) # ARIMA

prediksjon_no3 <- data.frame( # opprette dataframe for ryddig eksportdata
    x = 1:24,
    predikert = prognose_no3$mean,
    nedre = prognose_no3$lower,
    ovre = prognose_no3$upper
)

prediksjon_no3 <- prediksjon_no3 %>% rename(
    ovre = X95..1, # den nekter å gi de rette navnene når jeg oppretter dataframen, så 
    nedre = X95.   # vi må rename de her. kvifor? hakkje pipling. men dette funker iaf.
) 

autoplot(prognose_no3, include = 100) + # inkluderer de 100 siste timene for å gi litt kontekst
    theme_linedraw() +
    labs(x = "Days since 1/1/2022",
         y = "€/MW, NO3",
         title = "Forecasted Electricity Prices, NO3, using STL + ARIMA(3,1,3)",
         caption = "Forecasted using price data from previous days in 2022")

write_xlsx(prediksjon_no3, "no3_prediksjon.xlsx") # eksportere data til Excel



# ----- NO5 ----- # (nøyaktig identisk kode, bare at no3 nå er no5). 

dekomponert_no5 <- stl(no5, s.window = "periodic")

prognose_no5 <- forecast(dekomponert_no5, method = "arima", h = 24, level = 0.95)

prediksjon_no5 <- data.frame(
    x = 1:24,
    predikert = prognose_no5$mean,
    nedre = prognose_no5$lower,
    ovre = prognose_no5$upper
)

prediksjon_no5 <- prediksjon_no5 %>% rename(
    nedre = X95.,
    ovre = X95..1
)

autoplot(prognose_no5, include = 100) +
    theme_linedraw() +
    labs(x = "Days since 1/1/2022",
         y = "€/MW, NO5",
         title = "Forecasted Electricity Prices, NO5, using STL + ARIMA(3,1,3)",
         caption = "Forecasted using price data from all previous days in 2022")

write_xlsx(prediksjon_no5, "no5_prediksjon.xlsx")
