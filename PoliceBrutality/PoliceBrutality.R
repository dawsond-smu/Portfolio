library(data.table)
library(DT)
library(forcats)
library(ggplot2)
library(haven)
library(Hmisc)
library(plotly)
library(scales)
library(shiny)
library(stringi)
library(dplyr)
library(rsconnect)
library(dplyr)
library(ggplot2)
library(caret)
library(class)
library(GGally)
library(e1071)
library(naivebayes)
library(tidyr)
library(ggpubr)
library(data.table)
library(readr)  # for read_csv
library(knitr)  # for kable
library(RCurl)
install.packages(c("skimr","DataExplorer"))
library(skimr)
library(DataExplorer)
library(usmap)
library(maps)
install.packages("statebins")
library(statebins)


#Test Upload and na.omit() Sample
BeersUrl ="https://raw.githubusercontent.com/dawsond-smu/dawsond-smu.github.io/master/Data/Beers.csv"
Beers <-read_csv(url(BeersUrl))
BreweriesUrl ="https://raw.githubusercontent.com/dawsond-smu/dawsond-smu.github.io/master/Data/Breweries.csv"
Breweries <-read_csv(url(BreweriesUrl))
Beers_Brewery = merge(Beers,Breweries, by.x = "Brewery_id", by.y = "Brew_ID")
Beers_Brewery_Omit = na.omit(Beers_Brewery)


PoliceUrl ="https://raw.githubusercontent.com/dawsond-smu/Portfolio/master/PoliceBrutality/PoliceKillingsUS.csv"
Police <-read_csv(url(PoliceUrl))
RaceUrl ="https://raw.githubusercontent.com/dawsond-smu/Portfolio/master/PoliceBrutality/ShareRaceByCity.csv"
Race <-read_csv(url(RaceUrl))
IncomeUrl ="https://raw.githubusercontent.com/dawsond-smu/Portfolio/master/PoliceBrutality/MedianHouseholdIncome2015.csv"
Income <-read_csv(url(IncomeUrl))
PovertyUrl ="https://raw.githubusercontent.com/dawsond-smu/Portfolio/master/PoliceBrutality/PercentagePeopleBelowPovertyLevel.csv"
Poverty <-read_csv(url(PovertyUrl))
EducationUrl ="https://raw.githubusercontent.com/dawsond-smu/Portfolio/master/PoliceBrutality/PercentOver25CompletedHighSchool.csv"
Education <-read_csv(url(EducationUrl))

skim(Police)
DataExplorer::create_report(Police)
skim(Race)
DataExplorer::create_report(Race)
skim(Income)
DataExplorer::create_report(Income)
skim(Poverty)
DataExplorer::create_report(Poverty)
skim(Education)
DataExplorer::create_report(Education)

head(Police)
summary(Police)
Police_Mod = Police %>% mutate(flee = as.factor(flee), body_camera = as.factor(body_camera), manner_of_death = as.factor(manner_of_death), signs_of_mental_illness = as.factor(signs_of_mental_illness), threat_level = as.factor(threat_level), armed = as.factor(armed), gender = as.factor(gender), race = as.factor(race))
head(Police_Mod)
skim(Police_Mod)
DataExplorer::create_report(Police_Mod)
lmMod <- lm(manner_of_death ~ . , data = Police_Mod)
selectedMod <- step(lmMod)
summary(selectedMod)

str(usmap::us_map())
str(usmap::us_map(regions = "counties"))

plot_usmap(data = statepop, values = "pop_2015", color = "red") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Population (2015)", label = scales::comma
  ) + theme(legend.position = "right")

PoliceByState = Police %>% group_by(state) %>% summarize(N = n()) 
PoliceByState

help(plot_usmap)
plot_usmap(data = PoliceByState, values = "N", labels = TRUE, label_color = "white") + 
  scale_fill_continuous(name = "N", label = scales::comma, type = "viridis") + 
  cale_colour_steps2() + 
  theme(legend.position = "right")












plot_usmap()
plot_usmap(regions = "states")
plot_usmap(regions = "counties")
plot_usmap(regions = "state")
plot_usmap(regions = "county")

# Output is ggplot object so it can be extended
# with any number of ggplot layers
library(ggplot2)
plot_usmap(include = c("CA", "NV", "ID", "OR", "WA")) +
  labs(title = "Western States")

# Color maps with data
plot_usmap(data = statepop, values = "pop_2015")

# Include labels on map (e.g. state abbreviations)
plot_usmap(data = statepop, values = "pop_2015", labels = TRUE)
# Choose color for labels
plot_usmap(data = statepop, values = "pop_2015", labels = TRUE, label_color = "white")

us_states <- map_data("state")
head(us_states)
p <- ggplot(data = us_states,
            mapping = aes(x = long, y = lat,
                          group = group))
p + geom_polygon(fill = "white", color = "black")

p <- ggplot(data = us_states,
            aes(x = long, y = lat,
                group = group, fill = region))
p + geom_polygon(color = "gray90", size = 0.1) + guides(fill = FALSE)

p <- ggplot(data = us_states,
            mapping = aes(x = long, y = lat,
                          group = group, fill = region))
p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(fill = FALSE)

statebins_continuous(state_data = election, state_col = "state",
                     text_color = "white", value_col = "pct_trump",
                     brewer_pal="Reds", font_size = 3,
                     legend_title="Percent Trump")

statebins_continuous(state_data = subset(election, st %nin% "DC"),
                     state_col = "state",
                     text_color = "black", value_col = "pct_clinton",
                     brewer_pal="Blues", font_size = 3,
                     legend_title="Percent Clinton")

opiates
library(viridis)

p0 <- ggplot(data = subset(opiates_map, year > 1999),
             mapping = aes(x = long, y = lat,
                           group = group,
                           fill = adjusted))

p1 <- p0 + geom_polygon(color = "gray90", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) 

p2 <- p1 + scale_fill_viridis_c(option = "plasma")

p2 + theme_map() + facet_wrap(~ year, ncol = 3) +
  theme(legend.position = "bottom",
        strip.background = element_blank()) +
  labs(fill = "Death rate per 100,000 population ",
       title = "Opiate Related Deaths by State, 2000-2014")  
