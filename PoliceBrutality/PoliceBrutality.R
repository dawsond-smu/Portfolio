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



