
# Load packages
lapply(c("Cairo", "datapasta", "dplyr", "tsibble", "fable", "fabletools",
         "lubridate", "ggplot2", "tidyr", "scales"),
       require,
       character.only=TRUE)

# Set ggplot defaults
theme_set(
  theme_minimal() +
    theme(legend.position = "right",
          plot.subtitle = element_text(face = "italic"
                                       , family = "sans"
                                       , size=10))
)


# Import and format date, sources from gov.uk/. PHE site on 18/03/2020
cases <- read.csv("DailyConfirmedCases.csv", stringsAsFactors = FALSE)
colnames(cases) <- c("dt", "New", "Cumulative")
cases$dt <- as.Date(cases$dt, "%d/%m/%Y")

# Strip commas out of thousands (Excel, honestly...)
cases$Cumulative <- as.integer(gsub(",", "", cases$Cumulative))


# Quick plot of cases
cases %>% 
  pivot_longer(cols = -dt
               , values_to = "cases"
               , names_to = "type") %>% 
  ggplot(aes(x=dt, y=cases, col=factor(type)))+
  geom_line(size=1.2)+
  labs(title= "UK nationally reported coronavirus infections",
       subtitle = "Data source: PHE / Gov.uk",
       x = "Date", y = "Cases") +
  scale_color_brewer(type="qual")+
  scale_y_continuous(label = comma)+
  scale_x_date(date_breaks = "10 day", date_labels = "%b-%d")+
  theme(axis.text.x = element_text(angle = 45),
        legend.title = element_blank())


# Now try with tsibble and fable

cases_ts <- as_tsibble(cases)

cases_ts %>% autoplot()


cases_ts_fit <- cases_ts %>% 
  model( mean = MEAN(New)
         , naive = NAIVE(New)
         , snaive = SNAIVE(New, lag="day")
         , drift = RW(New ~ drift())
  )

cases_ts_fit %>% glance()
cases_ts_fit


cases_ts_forcast12d <- cases_ts_fit %>% 
  forecast(h = "12 days")

# Plot
cases_ts_forcast12d %>% autoplot()
