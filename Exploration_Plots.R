library(countrycode)
library(RColorBrewer)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(here)

# Read in PPD Data #
data <- read.csv(here("data","PPD7-5-18.csv"), header = TRUE, sep= ",", fill = TRUE)

# map ranking to six categories #
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
data$country_name[data$country_code == "WIN"] <- NULL 

### Plot Performance Score Composition by Region ###
data %>%
  filter(completion_year > 2000) %>%
  select(region, performance_cat) %>%
  ggplot(aes(x = fct_infreq(region), fill = fct_rev(performance_cat))) + 
  geom_bar() +
  scale_fill_manual(values = rev(brewer.pal(6, "RdYlGn")), na.value = "grey39") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Region", y = "Total Number of Projects (Completion Date 2000-2015)", fill = "Project Performance Category",
       title = "The Majority of Development Projects are Satisfactory Across Regions", 
       subtitle = "Marginally Satisfactory was the most common ranking across all regions with more than 100 total projects") +
  annotate("text", label = "Source: Project Performance \n Database (Honig 2018)", x = 18, y = 1150, size = 3)
 ggsave(here("output", "Performance_Category_by_Year.pdf", width =30, height = 30, units = "cm"))
  

### Trend in performance by year and sector for Sub-Saharan Africa and other regions ### 
data %>% 
  filter(completion_year > 2000) %>%
  select(completion_year, six_overall_rating, sector_agg_name, ss_africa_dummy) %>%
  group_by(sector_agg_name) %>%
  filter(n() > 40) %>%
  group_by(ss_africa_dummy, sector_agg_name, completion_year) %>%
  summarize(
    avg_performance = mean(six_overall_rating, na.rm = TRUE)
  ) %>%
ggplot(aes(x = completion_year, y = avg_performance, linetype=ss_africa_dummy, color = ss_africa_dummy)) +
  coord_cartesian(ylim = c(1, 6)) +
  geom_line() +  
  facet_wrap(~sector_agg_name, ncol = 4) + 
  geom_smooth(method = "lm") +
  labs(x = "Project Completion Year", y = "Average Performance Ranking", color = "Region", linetype = "Region",
       title = "Sub-Saharan Africa Diverges from Other Regions in Education, Health, and Reproductive Health Performance Trends", 
       subtitle = "Trends over time show significant variance across years, but similar overall average performance across sectors and regions",
       caption = "Source: Project Performance Database (Honig 2018)") 
  ggsave(here("output", "Avg_Perf_Year_Sector_SSAfrica.pdf"), width =30, height = 30, units = "cm")
  
### Performance by Country, Sector, and Spending in the top 10 funded sectors in Eastern Africa ###
  
### Identify top 10 sectors in terms of total spending ###  
data_tt <- data %>% 
  filter(completion_year > 2000 & region == "Eastern Africa") %>%
  select(project_cost, sector_agg_name) %>%
  group_by(sector_agg_name) %>%
  summarize(
    total_spending = sum(project_cost, na.rm = TRUE)
  ) %>% 
  arrange(desc(total_spending))

### Plot average performance of top 10 sectors by country, showing total spending for each sector-country ###    
data %>%
    filter(completion_year > 2000 & region == "Eastern Africa" & sector_agg_name %in% data_tt$sector_agg_name[1:10]) %>%
    select(country_name, six_overall_rating, project_cost, sector_agg_name) %>%
    group_by(country_name, sector_agg_name) %>%
    summarize(
      avg_performance = mean(six_overall_rating, na.rm=TRUE),
      total_spending = sum(project_cost, na.rm = TRUE)
    ) %>%
  ggplot(aes(x= country_name, y = avg_performance, fill = sector_agg_name, size = total_spending)) + 
  geom_point(shape = 21) +
  scale_fill_manual(values = brewer.pal(10, "Spectral")) +
  scale_size_area(max_size = 12) +
  coord_flip() +
  labs(y = "Average Performance (Projects Completed 2000-2015)", x = "Country", fill = "Sector", size = "Total Spending",
     title = "Top-Performing Sectors Vary Across Country and Spending", 
     subtitle = "Education and health perform most consistently across countries") +
    annotate("text", label = "Source: Project\n Performance Database\n (Honig 2018)", x = 17, y = 5.5, size = 2)
  ggsave(here("output", "Avg_Perf_Sector_Country_EAfrica.pdf"), width =30, height = 30, units = "cm")

