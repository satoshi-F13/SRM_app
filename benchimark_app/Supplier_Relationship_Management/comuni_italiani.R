library(dplyr)
library(tidyverse)
library(readxl)
library(janitor)
library(skimr)
library(rjson)

#Load data from local file
df= read_excel("data/comuni-italiani.xls")
colnames(df)
head(df)
skim(df)

#clean the variable names
lookup = c("region_code" = "codice_regione", "province_code" = "codice_provincia_storico_1", "municipality" = "denominazione_in_italiano", "geo_area"="ripartizione_geografica", "region" = "denominazione_regione", "capital_city" = "denominazione_dell_unita_territoriale_sovracomunale_valida_a_fini_statistici" )

df1.2 = df %>% 
  clean_names() %>%  
  select(codice_regione,
         codice_provincia_storico_1,
         denominazione_in_italiano,
         ripartizione_geografica,
         denominazione_regione,
         denominazione_dell_unita_territoriale_sovracomunale_valida_a_fini_statistici) %>% 
  rename(lookup)

unique(df1.2$municipality)
df_mu = df1.2$municipality

write_csv(x = df1.2, file = "data/comuni_italiani.csv")

# geojson file about Italian municipalities as raw

json <- rjson::fromJSON(file="data/comuni.geojson")
g <- list(
  fitbounds = "locations",
  visible = FALSE
)


library(plotly)

it_city <- read_csv(file = "data/it.csv")

fig <- it_city 
fig <- fig %>%
  plot_ly(
    lat = ~lat,
    lon = ~lng,
    mode = 'markers',
    marker = list(color = "green"),
    type = 'scattermapbox',
    hovertext = ~city)
fig <- fig %>%
  layout(
    mapbox = list(
      style = 'open-street-map',
      zoom =4.2,
      center = list(lon = 12, lat = 42))) 

fig
