library(dplyr)
library(tidyr)

## argiculture data
Agri <- read.csv("data/AEI_NUTRIENTS.csv")
countries <- c("United States", "Russia", "Japan", "France", "Spain", "Australia", "South Africa", "Brazil")
USAAgri_clean <- Agri %>%
  filter(Country%in%countries & Indicator == "Balance per hectare") %>%
  select(-c("COUNTRY", "INDICATOR", "NUTRIENTS", "TIME", "Unit.Code", "PowerCode.Code", "PowerCode", "Reference.Period.Code", "Reference.Period","Flag.Codes", "Flags")) %>%
  rename(Year = Time)

USAAgri_clean <- USAAgri_clean %>%
  pivot_wider(names_from = Nutrients, values_from = Value) %>%
  select(-Unit) %>%
  rename(`Phosphorus (kg)` = Phosphorus, `Nitrogen (kg)` = Nitrogen)


# Greenhouse data

variables <- c("Total emissions including LULUCF", "3 - Agriculture",
               "Total GHG excl. LULUCF, Index 1990=100",
               "Total GHG incl. LULUCF per capita")

Greenhouse <- read.csv("data/AIR_GHG.csv")
greenhouse_cleaned <- Greenhouse %>%
  filter(Country %in% countries, Pollutant == "Greenhouse gases", Variable %in% variables) %>%
  select(-c("COU", "POL", "VAR", "YEA", "Unit.Code", "PowerCode.Code", "PowerCode", "Reference.Period.Code", "Reference.Period", "Flag.Codes", "Flags", "Unit")) %>%
  spread(Variable, Value)

greenhouse_cleaned <- greenhouse_cleaned %>%
  group_by(Country) %>%
  mutate(`Total emissions yearly diff (Tonnes of CO2 equivalent)` = c(0, diff(`Total emissions including LULUCF`)))

write.csv(greenhouse_cleaned,"data/greenhouse_cleaned_before.csv", row.names = FALSE)

# include units in col titles
colnames(greenhouse_cleaned)[4] <- "Agriculture (Tonnes of CO2 equivalent)"
colnames(greenhouse_cleaned)[5] <- "Total emissions including LULUCF (Tonnes of CO2 equivalent)"
colnames(greenhouse_cleaned)[6] <- "Total emissions including LULUCF (Index 1990=100)"
colnames(greenhouse_cleaned)[7] <- "Total emissions including LULUCF per capita (kg/per capita)"

write.csv(greenhouse_cleaned,"data/greenhouse_cleaned.csv", row.names = FALSE)

# min and max years
start_year <- max(min(USAAgri_clean$Year), min(greenhouse_cleaned$Year))

# Filter
USAAgri_clean <- USAAgri_clean[USAAgri_clean$Year >= start_year, ]
greenhouse_cleaned <- greenhouse_cleaned[greenhouse_cleaned$Year >= start_year, ]

country_region_mapping <- data.frame(
  Country = c("United States", "Russia", "Japan", "France", "Spain", "Australia", "South Africa", "Brazil"),
  Region = c("North America", "Europe", "Asia", "Europe", "Europe", "Oceania", "Africa", "South America")
)

# Merge based on the "Year" column
merged_data <- merge(USAAgri_clean, greenhouse_cleaned, by = c("Country", "Year"))
merged_data <- merge(merged_data, country_region_mapping, by = "Country", all.x = TRUE)

merged_data <- merged_data %>%
  select(
    Country,
    Region,
    Year,
    Indicator,
    `Phosphorus (kg)`,
    `Nitrogen (kg)`,
    Pollutant,
    `Total emissions including LULUCF (Tonnes of CO2 equivalent)`,
    `Total emissions yearly diff (Tonnes of CO2 equivalent)`,
    `Agriculture (Tonnes of CO2 equivalent)`,
    `Total emissions including LULUCF (Index 1990=100)`,
    `Total emissions including LULUCF per capita (kg/per capita)`
  )


# new dataframe that computes the avg Total emissions by country
write.csv(merged_data,"data/ghg_and_nutrients_data.csv", row.names = FALSE)

average_data <- aggregate(merged_data$`Total emissions including LULUCF per capita (kg/per capita)`, 
                          by = list(Category = merged_data$Country), 
                          FUN = mean)

colnames(average_data) <- c("Country", "Average Emission Per Capita(Tonnes of CO2 equivalent)")
write.csv(average_data, "data/yearly_avg_capita_by_country.csv", row.names = FALSE)
