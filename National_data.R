
# Load packages
lapply(c("Cairo", "datapasta", "dplyr", "prophet", 
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



# Trying prophet pacakge from Facebook
# Subset and rename for prophet package
cases_prophet <- cases[1:2]
colnames(cases_prophet) <- c("ds", "y")

# Model
prop_lin <- prophet(cases_prophet, growth = "logistic")


# Predict future
future <- make_future_dataframe(prop_lin, periods = 10, freq = "day", include_history = TRUE)

prophet_forcast <- predict(prop_lin, future)

plot(prop_lin, prophet_forcast)



# Manual model





