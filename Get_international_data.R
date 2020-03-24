# Script from European CDC for download


#these libraries are necessary
library(readxl)

library(httr)

#create the URL where the dataset is stored with automatic updates every day

url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")

#download the dataset from the website to a local temporary file

GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))

#read the Dataset sheet into “R”

data <- read_excel(tf)





# Let's have a look and plot
summary(data)
head(data)
data$`Countries and territories` <- factor(data$`Countries and territories`)

# See which countries are in there
levels(data$`Countries and territories`)

# Load packages
lapply(c("dplyr", "tidyr", "Cairo", "ggplot2", "scales"), require, character.only=TRUE)

## Set ggplot defaults
theme_set(
  theme_minimal() +
    theme(legend.position = "right",
          plot.subtitle = element_text(face = "italic"
                                       , family = "sans"
                                       , size=10))
)


# Quick plot of cases
data %>% 
  mutate(DateRep = as.Date(DateRep)) %>% 
  filter(`Countries and territories` %in% c("United_Kingdom", "Italy", "China")) %>% 
  select(DateRep, Cases, Deaths, Country = `Countries and territories`) %>% 
  pivot_longer(cols = c(Cases, Deaths)
               , values_to = "Count"
               , names_to = "Measure"
               ) %>% 
  ggplot(aes(x=DateRep, y=Count, col=Country, linetype=Measure))+
  geom_line(size=1)+
  labs(title= "International coronavirus infections",
       subtitle = "Data source: Ecdc daily",
       x = "Date", y = "Cases") +
  scale_color_brewer(type="qual")+
  scale_y_continuous(label = comma)+
  scale_x_date(date_breaks = "10 day", date_labels = "%b-%d")+
  theme(axis.text.x = element_text(angle = 45),
        legend.title = element_blank())


# Apply same forecasting methods as national

# Set at tsibble

library(fable)

cases_ts <- data %>% 
  mutate(DateRep = as.Date(DateRep)) %>% 
  #filter(`Countries and territories` %in% c("United_Kingdom", "Italy")) %>%
  filter(`Countries and territories` %in% c("United_Kingdom")) %>% 
  select(DateRep, Cases, Deaths, Country = `Countries and territories`) %>% 
  # pivot_wider(id_cols = DateRep,
  #             names_from = Country,
  #             values_from = c(Cases, Deaths)
  #             ) %>% 
  as_tsibble(index=DateRep,
             key=Country)

# Use autoplot
cases_ts %>% autoplot(Cases)
cases_ts %>% autoplot(Deaths)


# FIt various models, as a shot in the dark (ETS is exponential smoothing)
cases_ts_fit <- cases_ts %>% 
  mutate(dts = Deaths + 1) %>% 
  model( #mean = MEAN(Cases)
    #, naive = NAIVE(Cases)
    #, snaive = SNAIVE(Cases, lag="day")
    #, drift = RW(Cases ~ drift())
    #ets_case = ETS(Cases)
    
    
    #        , arima = ARIMA(Cases ~ error("A")+trend("N")+season("N"))
    #, ses_case = ETS(Cases ~ error("A")+trend("N")+season("N"))
    
    #, sesM_case = ETS(Cases ~ error("A")+trend("M")+season("A"))
    #, sesMN_case = ETS(Cases ~ error("A")+trend("M")+season("N"))
    #, holt_winter_case = ETS(Cases ~ error("A")+trend("A")+season("A"))
    #, holt_winterM_case = ETS(Cases ~ error("A")+trend("M")+season("A"))
    #, holt_winterMN_case = ETS(Cases ~ error("A")+trend("M")+season("N"))
    
     #ets_death = ETS(log(dts))
     ses_death = ETS(log(dts) ~ error("A")+trend("A")+season("N"))
    #, sesM_death = ETS(log(dts) ~ error("A")+trend("M")+season("A"))
    #, sesMN_death = ETS(log(dts) ~ error("A")+trend("M")+season("N"))
    #, holt_winter_death = ETS(log(dts) ~ error("A")+trend("A")+season("A"))
    #, holt_winterM_death = ETS(log(dts) ~ error("A")+trend("M")+season("A"))
    #, holt_winterMN_death = ETS(log(dts) ~ error("A")+trend("M")+season("N"))
    
  )



cases_ts_fit %>% glance()
cases_ts_fit


cases_ts_forcast5d <- cases_ts_fit %>% 
  forecast(h = "5 days")
  


death_pred<-cases_ts_forcast5d %>% autoplot()

cases_ts_forcast5d <- cases_ts_fit %>% 
  forecast(h = "5 days") %>% 
  hilo() %>% 
  tidyr::unnest(cols = c(`80%`, `95%`), names_repair="universal")
  
mutate(Deaths = exp(dts[[2]]))

cases_ts_forcast5d

cases_ts_forcast5d %>% 
  mutate(Deaths = exp(dts) - 1,
         L80 = exp(unlist(.[[5]])[1])-1,
         U80 = exp(unlist(.[[5]])[2])-1,
         L95 = exp(unlist(.[[6]])[1])-1,
         U95 = exp(unlist(.[[6]])[1])-1,
         ) %>% 
  select(DateRep, Deaths, L80, U80, L95, U95)

# Plot




data %>% 
mutate(DateRep = as.Date(DateRep)) %>% 
  filter(`Countries and territories` %in% c("United_Kingdom")) %>% 
  select(Country = `Countries and territories`, DateRep, Deaths) %>% 
  
  pivot_longer(cols = c(Cases, Deaths)
               , values_to = "Count"
               , names_to = "Measure"
  ) %>% 
  ggplot(aes(x=DateRep, y=Count, col=Country, linetype=Measure))+
  geom_line(size=1)+
  labs(title= "International coronavirus infections",
       subtitle = "Data source: Ecdc daily",
       x = "Date", y = "Cases") +
  scale_color_brewer(type="qual")+
  scale_y_continuous(label = comma)+
  scale_x_date(date_breaks = "10 day", date_labels = "%b-%d")+
  theme(axis.text.x = element_text(angle = 45),
        legend.title = element_blank())
