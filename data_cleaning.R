library(countrycode)
library(RColorBrewer)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(here)
library(readxl)
#library(reshape)
library(scales)
library(devtools)
library(aidtools)
library(ggalluvial)
library(dplyr)

# Read in Data #
data <- read.csv(here("data","PPD7-5-18.csv"), header = TRUE, sep= ",", fill = TRUE)
polity <- read_excel(here("data", "p4v2017.xls"))
sfi <- read_excel(here("data", "SFIv2017.xls"))
fh <- read_excel(here("data", "freedomhouse_FIW.xlsx"), sheet = 2, skip = 3, col_names = FALSE)
ltl <- read_excel(here("data", "q8_focus_country_ltl.xlsx"))

fh_cols <- c("country_name")
for (i in 1972:2017) {
  if (i != 1989){   # Freedom House data does not include 1989
    fh_cols <- append(fh_cols, paste(i, "_PR", sep = ''))
    fh_cols <- append(fh_cols, paste(i, "_CL", sep = ''))
    fh_cols <- append(fh_cols, paste(i, "_Status", sep = ''))
  }
}
colnames(fh) <- fh_cols
fh$country_code <- countrycode(fh$country_name, 'country.name', 'cowc')
fh$country_code[fh$country_code == "Serbia"] <- "SRB"
fh$country_code[fh$country_code == "Micronesia"] <- "FSM"

# map ranking to six categories  
data$performance_cat <-cut(data$six_overall_rating, seq(1,7,1), right = FALSE, labels=c("highly unsatisfactory", "unsatisfactory", 
                                                                                        "marginally unsatisfactory", "marginally satisfactory", 
                                                                                        "satisfactory", "highly satisfactory"))

# generate date and year variables #
data$completion_date <- as.Date(data$completion_date, format = "%d%B%Y") 
data$completion_year <- year(data$completion_date)
data$start_date <- as.Date(data$start_date, format = "%d%B%Y") 
data$start_year <- year(data$start_date)

# generate aggregated sector variable based on OCED CRS Sector Classifications#
data$sector_agg <- substr(as.character(data$mmg_purpose_code), 1, 2)
data$sector_agg_name <- factor(data$sector_agg, 
                               levels = c("11", "12", "13", "14", "15", "16", "21", "22", "23", "24", "25", "31", "32", "33", "41", "43",
                                          "51", "52", "53", "60", "70", "72", "73", "74", "91", "93", "99"), 
                               labels = c("Education", "Health", "Reproductive Health", "Water Supply & Sanitation", "Govt & Civil Society",
                                          "Other Social Inf & Services", "Transport & Storage", "Communications", "Energy", 
                                          "Banking & Financial Services", "Business and Other Services", "Agriculture, Forestry, Fishing", 
                                          "Industry, Mining, Construction", "Trade Policies & Regulations", "General Environmental Protection",
                                          "Other Multisector", "General Budget Support", "Food Aid", "Other Commodity Assistance",
                                          "Action Relating to Debt", "Humanitarian Aid", "Emergency Response", "Reconstruction Relief & Rehabilitation", 
                                          "Disaster Prevention & Preparedness", "Administrative Costs of Donors", 
                                          "Refugees in Donor Countries", "Unspecified"))

# generate unified project cost variable - rescale + convert to USD according to Honig methodology #
data$project_cost <- 0
data$project_cost[data$donor == "AsianDB"] <- data$asdb_approvedamount[data$donor == "AsianDB"]*1000000
#Multiply by 1 million to de-scale
data$project_cost[data$donor == "DFID"] <- data$dfid_projectbudgetcurrent[data$donor == "DFID"]*1.51
#Multiply by 1.51 (GBP-USD exchange rate on April 3, 2013) 
data$project_cost[data$donor == "GFATM"] <- data$gfatm_projectdisbconst_amount[data$donor == "GFATM"]
#No modification necessary  
data$project_cost[data$donor == "GiZ"] <- data$giz_projectsize[data$donor == "GiZ"]*1000*1.3065 
#Multiply by 1 thousand to de-scale multiply by 1.3065 (EUR-USD exchange rate on July 13, 2013)
data$project_cost[data$donor == "IFAD"] <- data$ifad_projectsize[data$donor == "IFAD"]*1000000
#Multiply by 1 million to de-scale
data$project_cost[data$donor == "JICA"] <- data$jica_projectsize[data$donor == "JICA"]*10687
#Multiply by 10687
data$project_cost[data$donor == "KfW"] <- data$kfw_projectsize[data$donor == "KfW"]*1.28
#Multiply by 1.28 (EUR-USD exchange rate on April 4, 2013)
data$project_cost[data$donor == "WB"] <- data$wb_lendingproject_cost[data$donor == "WB"]
#No modification necessary 

# generate region variable #
data$region <- countrycode(data$country_code, 'cowc', 'region')

# manually assign country codes not identified by package #
data$region[data$country_code == "DRV"] <- "Eastern Asia" # Vietnam
data$region[data$country_code == "KOS"] <- "Eastern Europe" # Kosovo
data$region[data$country_code == "YUG"] <- "Eastern Europe"  # Yugoslavia
data$region[data$country_code == "ZAN"] <- "Eastern Africa" # Zanzibar
data$region[data$country_code == "HKG"] <- "Eastern Asia" # Hong Kong
data$region[data$country_code == "WBG"] <- "Western Asia" # West Bank/Gaza
data$region[data$country_code == "MNS"] <- "Caribbean" # Montserrat
data$region[data$country_code == "PIT"] <- "Polynesia" # Pitcarin
data$region[data$country_code == "STH"] <- "Western Africa" # St. Helena
data$region[data$country_code == "SRB"] <- "Eastern Europe" # Serbia
data$region[data$country_code == "TRS"] <- "Southern Africa" #Tristan da Cunha 
data$region[data$country_code == "TUC"] <- "Caribbean" # Turks and Caicos
data$region[data$country_code == "WIN"] <- "Caribbean" #OECS project - Org Eastern Caribbean States

data$ss_africa_dummy <- ifelse(data$region == "Eastern Africa" | data$region == "Middle Africa" | data$region == "Southern Africa" 
                               | data$region == "Western Africa", 1, 0 )
data$ss_africa_dummy <- factor(as.character(data$ss_africa_dummy), levels = c(1, 0), labels = c("Sub-Saharan Africa", "Other Regions"))
data$africa_dummy <- ifelse(data$region == "Eastern Africa" | data$region == "Middle Africa" | data$region == "Southern Africa" 
                            | data$region == "Western Africa" | data$region == "Northern Africa", 1, 0 )
data$africa_dummy <- factor(as.character(data$africa_dummy), levels = c(1, 0), labels = c("Africa", "Other Continents"))


data$country_code <- as.factor(data$country_code)

#Generate country name variable#
data$country_name <- countrycode(data$country_code, 'cowc', 'un.name.en')

# manually assign country codes not identified by package #
data$country_name[data$country_code == "ZAN"] <- "Zanzibar"
data$country_name[data$country_code == "DRV"] <- "Vietnam"
data$country_name[data$country_code == "KOS"] <- "Kosovo"
data$country_name[data$country_code == "YUG"] <- "Yugoslavia"
data$country_name[data$country_code == "ZAN"] <- "Zanzibar"
data$country_name[data$country_code == "HKG"] <- "Hong Kong"
data$country_name[data$country_code == "WBG"] <- "West Bank/Gaza"
data$country_name[data$country_code == "MNS"] <- "Montserrat"
data$country_name[data$country_code == "PIT"] <- "Pitcarin"
data$country_name[data$country_code == "STH"] <- "St. Helena"
data$country_name[data$country_code == "SRB"] <- "Serbia"
data$country_name[data$country_code == "TRS"] <- "Tristan da Cunha"
data$country_name[data$country_code == "TUC"] <- "Turks and Caicos"
#data$country_name[data$country_code == "WIN"] <- NULL 

data <- data %>% 
  mutate(satisfactory = ifelse(six_overall_rating >= 4, 1, 0)) %>%
  mutate(unsatisfactory = ifelse(six_overall_rating < 4, 1, 0)) %>%
  mutate(continent = ifelse(region %in% c("Eastern Asia", "Central Asia", "Southern Asia", 
                                          "Western Asia", "South-Eastern Asia"), "Asia",
                            ifelse(region %in% c("Eastern Africa", "Middle Africa", "Northern Africa", 
                                                 "Western Africa", "Southern Africa"), "Africa",
                                   ifelse(region %in% c("Central America", "Caribbean"), "North America",
                                          ifelse(region %in% c("South America"), "South America", 
                                                 ifelse(region %in% c("Eastern Europe", "Northern Europe",
                                                                      "Southern Europe"), "Europe", 
                                                        "New Zealand + Oceania"))))))

## Merge SFI and PPD data sets ##
data_sfi <- merge(x=data,y=sfi,by.x=c("start_year","country_code"), by.y=c("year","scode"), all.x = TRUE)


## SDG CODING ###

data_sdg <- data %>% rename(aiddata_id_dep = aiddata_id, commitment_amount_usd_constant = project_cost, 
                            coalesced_purpose_code = mmg_purpose_sector, aiddata_id = ppd_project_id)

data_sdg2 <- sdg_coder(data_sdg, coalesced_purpose = TRUE)

data_sdg <- merge(x=data_sdg, y = data_sdg2, by = "aiddata_id", all.x = TRUE)