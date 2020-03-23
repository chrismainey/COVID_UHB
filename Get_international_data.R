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
