# Advanced Visualization in R: Impact of Economic and Environmental Factors on Public Health
# This project analyzes the relationships between economic, environmental, and public health indicators
# using advanced visualization techniques in R.

Sys.setenv(LANG = "en")
options(scipen = 5)
setwd("C:/Users/User/Documents/3 semester/ad vis in r/project/R_Project")
getwd()

# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)
library(corrplot)
library(maps)

# =======================
# 1. Data Loading
# =======================

# Function to load the dataset
load_data <- function() {
  # Load data from the CSV file
  data <- read_csv("world-data-2023.csv")

  # Display an overview of the dataset
  print("Data successfully loaded!")
  glimpse(data)

  return(data)
}

# Load the dataset
data1 <- load_data()
data <- load_data()
summary(data)

# Display the list of column names in the dataset
get_column_names <- function(data) {
  column_names <- names(data)  # Extract column names
  print("List of column names:")
  print(column_names)
  return(column_names)
}

# Apply the function to your dataset
column_names <- get_column_names(data)

# Rename columns to more convenient names
rename_columns <- function(data) {
  data <- data %>%
    rename(
      Country = Country,
      Density = `Density\n(P/Km2)`,
      Abbreviation = Abbreviation,
      AgriculturalLand = `Agricultural Land( %)`,

      LandArea = `Land Area(Km2)`,
      ArmedForces = `Armed Forces size`,
      BirthRate = `Birth Rate`,
      CallingCode = `Calling Code`,

      CapitalCity = `Capital/Major City`,
      CO2Emissions = `Co2-Emissions`,
      CPI = CPI,
      CPIChange = `CPI Change (%)`,

      CurrencyCode = `Currency-Code`,
      FertilityRate = `Fertility Rate`,
      ForestedArea = `Forested Area (%)`,
      GasolinePrice = `Gasoline Price`,

      GDP = GDP,
      PrimaryEducation = `Gross primary education enrollment (%)`,
      TertiaryEducation = `Gross tertiary education enrollment (%)`,
      InfantMortality = `Infant mortality`,

      LargestCity = `Largest city`,
      LifeExpectancy = `Life expectancy`,
      MaternalMortality = `Maternal mortality ratio`,
      MinimumWage = `Minimum wage`,

      OfficialLanguage = `Official language`,
      HealthExpenditure = `Out of pocket health expenditure`,
      PhysiciansPerThousand = `Physicians per thousand`,
      Population = Population,

      LaborForceParticipation = `Population: Labor force participation (%)`,
      TaxRevenue = `Tax revenue (%)`,
      TotalTaxRate = `Total tax rate`,
      UnemploymentRate = `Unemployment rate`,

      UrbanPopulation = `Urban_population`,
      Latitude = Latitude,
      Longitude = Longitude
    )

  print("Column names have been renamed for convenience.")
  return(data)
}

# Apply the function to your dataset
data <- rename_columns(data)

# Check the renamed columns
names(data)

# =======================
# 2. Data Cleaning
# =======================
# Function to clean percentage columns and convert them to numeric format
clean_percentage_columns <- function(data) {
  data <- data %>%
    mutate(
      # Remove '%' and convert columns to numeric
      AgriculturalLand = as.numeric(gsub("%", "", AgriculturalLand)),
      CPIChange = as.numeric(gsub("%", "", CPIChange)),
      ForestedArea = as.numeric(gsub("%", "", ForestedArea)),
      PrimaryEducation = as.numeric(gsub("%", "", PrimaryEducation)),
      TertiaryEducation = as.numeric(gsub("%", "", TertiaryEducation)),
      HealthExpenditure = as.numeric(gsub("%", "", HealthExpenditure)),
      LaborForceParticipation = as.numeric(gsub("%", "", LaborForceParticipation)),
      TaxRevenue = as.numeric(gsub("%", "", TaxRevenue)),
      TotalTaxRate = as.numeric(gsub("%", "", TotalTaxRate)),
      UnemploymentRate = as.numeric(gsub("%", "", UnemploymentRate))
    )

  # Print a success message
  print("Percentage columns cleaned and converted to numeric.")
  return(data)
}

# Apply the function to clean the percentage columns
data <- clean_percentage_columns(data)

# Function to clean dollar symbols and commas, then convert to numeric
clean_dollar_columns <- function(data) {
  data <- data %>%
    mutate(
      # Remove '$' and ',' from the columns and convert to numeric
      GasolinePrice = as.numeric(gsub("[\\$,]", "", GasolinePrice)),
      GDP = as.numeric(gsub("[\\$,]", "", GDP)),
      MinimumWage = as.numeric(gsub("[\\$,]", "", MinimumWage))
    )

  # Print a success message
  print("Dollar symbols and commas removed, columns converted to numeric.")
  return(data)
}

# Apply the function to clean the dollar columns
data <- clean_dollar_columns(data)

# Verify the changes
head(data[c("GasolinePrice", "GDP", "MinimumWage")])

# ======================== HANDLING MISSING VALUES================================
# Check the number of missing values in each column
check_missing_values <- function(data) {
  missing_summary <- colSums(is.na(data))
  print("Number of missing values in each column:")
  print(missing_summary)
  return(missing_summary)
}

# Apply the function to your dataset
missing_values <- check_missing_values(data)

# Function to handle missing values, remove rows with missing Latitude/Longitude, and round numeric columns
handle_missing_values <- function(data) {
  data <- data %>%
    # Remove rows with missing Latitude or Longitude
    filter(!is.na(Latitude) & !is.na(Longitude)) %>%
    mutate(
      # Fill numeric columns with mean or median and round to 2 decimal places
      GDP = ifelse(is.na(GDP), round(median(GDP, na.rm = TRUE), 2), round(GDP, 2)),
      GasolinePrice = ifelse(is.na(GasolinePrice), round(mean(GasolinePrice, na.rm = TRUE), 2), round(GasolinePrice, 2)),
      BirthRate = ifelse(is.na(BirthRate), round(median(BirthRate, na.rm = TRUE), 2), round(BirthRate, 2)),
      UrbanPopulation = ifelse(is.na(UrbanPopulation), round(median(UrbanPopulation, na.rm = TRUE), 2), round(UrbanPopulation, 2)),
      LaborForceParticipation = ifelse(is.na(LaborForceParticipation), round(median(LaborForceParticipation, na.rm = TRUE), 2), round(LaborForceParticipation, 2)),
      TaxRevenue = ifelse(is.na(TaxRevenue), round(mean(TaxRevenue, na.rm = TRUE), 2), round(TaxRevenue, 2)),
      TotalTaxRate = ifelse(is.na(TotalTaxRate), round(median(TotalTaxRate, na.rm = TRUE), 2), round(TotalTaxRate, 2)),
      ArmedForces = ifelse(is.na(ArmedForces), round(median(ArmedForces, na.rm = TRUE), 2), round(ArmedForces, 2)),

      # CO2Emissions: Keep as integer
      CO2Emissions = ifelse(is.na(CO2Emissions), as.integer(mean(CO2Emissions, na.rm = TRUE)), as.integer(CO2Emissions)),

      FertilityRate = ifelse(is.na(FertilityRate), round(median(FertilityRate, na.rm = TRUE), 2), round(FertilityRate, 2)),
      InfantMortality = ifelse(is.na(InfantMortality), round(median(InfantMortality, na.rm = TRUE), 2), round(InfantMortality, 2)),
      LifeExpectancy = ifelse(is.na(LifeExpectancy), round(median(LifeExpectancy, na.rm = TRUE), 2), round(LifeExpectancy, 2)),
      HealthExpenditure = ifelse(is.na(HealthExpenditure), round(median(HealthExpenditure, na.rm = TRUE), 2), round(HealthExpenditure, 2)),
      PrimaryEducation = ifelse(is.na(PrimaryEducation), round(median(PrimaryEducation, na.rm = TRUE), 2), round(PrimaryEducation, 2)),
      TertiaryEducation = ifelse(is.na(TertiaryEducation), round(median(TertiaryEducation, na.rm = TRUE), 2), round(TertiaryEducation, 2)),

      # Newly added numeric columns with rounding
      LandArea = ifelse(is.na(LandArea), round(median(LandArea, na.rm = TRUE), 2), round(LandArea, 2)),
      AgriculturalLand = ifelse(is.na(AgriculturalLand), round(mean(AgriculturalLand, na.rm = TRUE), 2), round(AgriculturalLand, 2)),
      CPI = ifelse(is.na(CPI), round(mean(CPI, na.rm = TRUE), 2), round(CPI, 2)),
      CPIChange = ifelse(is.na(CPIChange), round(mean(CPIChange, na.rm = TRUE), 2), round(CPIChange, 2)),
      ForestedArea = ifelse(is.na(ForestedArea), round(mean(ForestedArea, na.rm = TRUE), 2), round(ForestedArea, 2)),
      MaternalMortality = ifelse(is.na(MaternalMortality), round(median(MaternalMortality, na.rm = TRUE), 2), round(MaternalMortality, 2)),
      MinimumWage = ifelse(is.na(MinimumWage), round(median(MinimumWage, na.rm = TRUE), 2), round(MinimumWage, 2)),
      PhysiciansPerThousand = ifelse(is.na(PhysiciansPerThousand), round(median(PhysiciansPerThousand, na.rm = TRUE), 2), round(PhysiciansPerThousand, 2)),
      Population = ifelse(is.na(Population), round(median(Population, na.rm = TRUE), 2), round(Population, 2)),
      UnemploymentRate = ifelse(is.na(UnemploymentRate), round(mean(UnemploymentRate, na.rm = TRUE), 2), round(UnemploymentRate, 2)),

      # Fill categorical columns with "Unknown"
      Country = ifelse(is.na(Country), "Unknown", Country),
      CapitalCity = ifelse(is.na(CapitalCity), "Unknown", CapitalCity),
      LargestCity = ifelse(is.na(LargestCity), "Unknown", LargestCity),
      OfficialLanguage = ifelse(is.na(OfficialLanguage), "Unknown", OfficialLanguage),
      CurrencyCode = ifelse(is.na(CurrencyCode), "Unknown", CurrencyCode),
      Abbreviation = ifelse(is.na(Abbreviation), "Unknown", Abbreviation),
      CallingCode = ifelse(is.na(CallingCode), "Unknown", CallingCode)
    )

  print("Missing values handled successfully, rows with missing Latitude/Longitude removed, numeric values rounded, and CO2Emissions set as integers.")
  return(data)
}

# Apply the function to handle missing values and round numeric columns
data <- handle_missing_values(data)

# Verify the result
check_missing_values(data)

# ============================ Detect Columns with Unusual Values =========================

# Function to detect columns with unusual characters
detect_weird_values <- function(data) {
  # List to store results
  weird_columns <- list()

  # Iterate over each column
  for (col in names(data)) {
    # Check if the column is categorical (character or factor)
    if (is.character(data[[col]]) || is.factor(data[[col]])) {
      # Use a regular expression to find non-alphanumeric characters
      weird_values <- data[[col]][grepl("[^A-Za-z0-9\\s]", data[[col]])]

      # If unusual characters are found, store them in the list
      if (length(weird_values) > 0) {
        weird_columns[[col]] <- unique(weird_values)
      }
    }
  }

  # Print the columns with weird values
  if (length(weird_columns) > 0) {
    print("Columns with unusual values and their unique entries:")
    print(weird_columns)
  } else {
    print("No unusual values detected in categorical columns.")
  }

  return(weird_columns)
}

# Apply the function to your dataset
weird_columns <- detect_weird_values(data)


# Function to replace corrupted values with correct ones
replace_corrupted_values <- function(data) {
  # Replace values in CapitalCity
  data$CapitalCity <- recode(data$CapitalCity,
                             "Bras���" = "Brasília",
                             "Bogot�" = "Bogotá",
                             "San Jos������" = "San José",
                             "Reykjav��" = "Reykjavík",
                             "Mal�" = "Malé",
                             "Chi����" = "Chişinău",
                             "Asunci��" = "Asunción",
                             "Lom�" = "Lomé",
                             "Nuku����" = "Nukuʻalofa",
                             "Yaound�" = "Yaoundé" # Added missing value
  )

  # Replace values in LargestCity
  data$LargestCity <- recode(data$LargestCity,
                             "S����" = "São Paulo",
                             "Bogot�" = "Bogotá",
                             "San Jos������" = "San José",
                             "Statos�������" = "Stratos",
                             "Reykjav��" = "Reykjavík",
                             "Mal�" = "Malé",
                             "Chi����" = "Chişinău",
                             "S�����" = "São Luís",
                             "Z���" = "Zürich",
                             "Lom�" = "Lomé",
                             "Nuku����" = "Nukuʻalofa"
  )

  print("Corrupted values replaced with correct ones.")
  return(data)
}

# Apply the function to your dataset
data <- replace_corrupted_values(data)

# Check the result
unique(data$CapitalCity)
unique(data$LargestCity)

# ====================== Convert Columns to Appropriate Formats ========================

# Function to convert columns to appropriate formats
convert_column_types <- function(data) {
  data <- data %>%
    mutate(
      # Convert to numeric
      GDP = as.numeric(GDP),
      GasolinePrice = as.numeric(GasolinePrice),
      BirthRate = as.numeric(BirthRate),
      UrbanPopulation = as.numeric(UrbanPopulation),
      LaborForceParticipation = as.numeric(LaborForceParticipation),
      TaxRevenue = as.numeric(TaxRevenue),
      TotalTaxRate = as.numeric(TotalTaxRate),
      ArmedForces = as.numeric(ArmedForces),
      CO2Emissions = as.integer(CO2Emissions), # Integer for whole numbers
      FertilityRate = as.numeric(FertilityRate),
      InfantMortality = as.numeric(InfantMortality),
      LifeExpectancy = as.numeric(LifeExpectancy),
      HealthExpenditure = as.numeric(HealthExpenditure),
      PrimaryEducation = as.numeric(PrimaryEducation),
      TertiaryEducation = as.numeric(TertiaryEducation),
      LandArea = as.numeric(LandArea),
      AgriculturalLand = as.numeric(AgriculturalLand),
      CPI = as.numeric(CPI),
      CPIChange = as.numeric(CPIChange),
      ForestedArea = as.numeric(ForestedArea),
      MaternalMortality = as.numeric(MaternalMortality),
      MinimumWage = as.numeric(MinimumWage),
      PhysiciansPerThousand = as.numeric(PhysiciansPerThousand),
      Population = as.numeric(Population),
      UnemploymentRate = as.numeric(UnemploymentRate),

      # Convert to character
      Abbreviation = as.character(Abbreviation),
      CurrencyCode = as.character(CurrencyCode),
      CallingCode = as.character(CallingCode),
      CapitalCity = as.character(CapitalCity),
      LargestCity = as.character(LargestCity),

      # Convert to factor
      Country = as.factor(Country),
      OfficialLanguage = as.factor(OfficialLanguage)
    )

  print("Columns converted to appropriate formats.")
  return(data)
}

# Apply the function to your dataset
data <- convert_column_types(data)

# Check the structure of the dataset
str(data)

# =========== Export the Dataset to CSV ===================================

# Check for missing values in the entire dataset
check_missing_values <- function(data) {
  missing_summary <- colSums(is.na(data))
  print("Number of missing values in each column:")
  return(missing_summary)
}

missing_values <- check_missing_values(data)

str(data)
head(data)
summary(data)

# Export the dataset to a CSV file
write.csv(data, "cleaned_dataset.csv", row.names = FALSE)




# ============== Analyze Each Research Question ==================================

# Load the new dataset
new_data <- read.csv("cleaned_dataset.csv")

# Preview the data
head(new_data)


# Subset for economic prosperity and health outcomes
economic_health_data <- new_data %>%
  select(Country, GDP, MinimumWage, LifeExpectancy, InfantMortality)

# Subset for CO2 emissions and public health outcomes
co2_health_data <- new_data %>%
  select(Country, CO2Emissions, LifeExpectancy, InfantMortality)

# Subset for healthcare metrics and life expectancy
healthcare_data <- new_data %>%
  select(Country, PhysiciansPerThousand, HealthExpenditure, LifeExpectancy)

# ============== RQ1============================================================
# How does economic prosperity, measured through GDP and minimum wage, influence life
# expectancy and infant mortality?

# Scatter plot for GDP vs Life Expectancy
ggplot(economic_health_data, aes(x = GDP, y = LifeExpectancy)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "GDP vs Life Expectancy",
       x = "GDP (in USD)",
       y = "Life Expectancy (in years)") +
  theme_minimal()

# Correlation analysis
cor(economic_health_data$GDP, economic_health_data$LifeExpectancy, use = "complete.obs")

# ============== RQ2 ============================================================
# What is the relationship between CO2 emissions and public health outcomes?

# Scatter plot for CO2 Emissions vs Life Expectancy
ggplot(co2_health_data, aes(x = CO2Emissions, y = LifeExpectancy)) +
  geom_point(alpha = 0.6, color = "green") +
  geom_smooth(method = "lm", color = "darkgreen", se = TRUE) +
  labs(title = "CO2 Emissions vs Life Expectancy",
       x = "CO2 Emissions (tons)",
       y = "Life Expectancy (in years)") +
  theme_minimal()

# Correlation analysis
cor(co2_health_data$CO2Emissions, co2_health_data$LifeExpectancy, use = "complete.obs")

# ============== RQ3 ============================================================
# How do healthcare metrics (such as availability of physicians and out-of-pocket health
# expenditures) correlate with life expectancy in different countries?

# Scatter plot for Physicians per Thousand vs Life Expectancy
ggplot(healthcare_data, aes(x = PhysiciansPerThousand, y = LifeExpectancy)) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_smooth(method = "lm", color = "darkblue", se = TRUE) +
  labs(title = "Physicians per Thousand vs Life Expectancy",
       x = "Physicians per Thousand",
       y = "Life Expectancy (in years)") +
  theme_minimal()

# Correlation analysis
cor(healthcare_data$PhysiciansPerThousand, healthcare_data$LifeExpectancy, use = "complete.obs")

# ============== Geospatial Analysis ============================================================

# Geospatial visualization for Life Expectancy
world_map <- map_data("world") # Load world map data

ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightgray") +
  geom_point(data = new_data, aes(x = Longitude, y = Latitude, color = LifeExpectancy), size = 3) +
  scale_color_gradient(low = "red", high = "green") +
  labs(title = "Global Life Expectancy",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()


# ============== Shokoufeh ============================================================

# Subset for CO2 emissions and public health outcomes
co2_health_data <- new_data %>%
  select(Country, Density, CO2Emissions, AgriculturalLand , BirthRate , ForestedArea , GasolinePrice, 
         InfantMortality, MaternalMortality , HealthExpenditure , PhysiciansPerThousand ,Population ,
         UrbanPopulation,  
         Latitude  , Longitude, LifeExpectancy, InfantMortality)
co2_cause_data <- new_data %>% select(Country, Density, CO2Emissions, AgriculturalLand  , ForestedArea , GasolinePrice, 
                                      Population ,
                                      UrbanPopulation,  
                                      Latitude  , Longitude)
co2_effect_data <- new_data %>%
  select(Country, CO2Emissions , BirthRate , 
         InfantMortality, MaternalMortality , HealthExpenditure , PhysiciansPerThousand ,
        Latitude  , Longitude, LifeExpectancy, InfantMortality)



################### "Lower Triangular Correlation Bubble Chart: CO2 Emissions Causes and Effects"#############


library(ggplot2)
library(reshape2)
library(dplyr)



# Compute correlation matrix
cor_matrix <- cor(co2_health_data %>% select(-Country, -Latitude, -Longitude), use = "complete.obs")


# Melt the correlation matrix for ggplot
cor_melt <- melt(cor_matrix)

# Filter to show only the lower triangular part
cor_melt <- cor_melt %>% 
  mutate(Var1 = as.character(Var1), Var2 = as.character(Var2)) %>% 
  filter(as.numeric(factor(Var1)) > as.numeric(factor(Var2)))

# Plot correlation bubble chart
ggplot(cor_melt, aes(x = Var1, y = Var2, size = abs(value), fill = value)) +
  geom_point(shape = 21, color = "white") +
  geom_text(
    aes(label = ifelse(abs(value) > 0.7, sprintf("%.2f", value), "")),
    color = "black", size = 3
  ) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limit = c(-1, 1), name = "Corr") +
  scale_size(range = c(1, 10), name = "Abs(Corr)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Lower Triangular Correlation Bubble Chart: CO2 Emissions Causes and Effects",
    x = NULL,
    y = NULL
  )


############################## Global CO2 Emissions with Urban Population ###########

library(ggplot2)
library(maps)

# Load world map
world_map <- map_data("world")

# Plot CO2 emissions and Gasoline Price
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
  geom_point(data = co2_health_data, aes(x = Longitude, y = Latitude, size = CO2Emissions, color = UrbanPopulation), alpha = 0.7) +
  scale_size_continuous(range = c(1, 15), name = "CO2 Emissions (kt)") +
  scale_color_gradient(low = "blue", high = "red", name = "Urban Population") +
  labs(
    title = "Global CO2 Emissions with Urban Population",
    subtitle = "Bubble size represents CO2 emissions",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

###################### Bubble size represents CO2 emissions; Top 10 countries labeled ###########
library(ggplot2)
library(maps)
library(dplyr)

# Load world map
world_map <- map_data("world")

# Select the top 10 countries by CO2 emissions
top_countries <- co2_health_data %>%
  arrange(desc(CO2Emissions)) %>%
  slice(1:10)

# Plot CO2 emissions and Gasoline Price
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
  geom_point(data = co2_health_data, aes(x = Longitude, y = Latitude, size = CO2Emissions, color = UrbanPopulation), alpha = 0.7) +
  geom_text(data = top_countries, aes(x = Longitude, y = Latitude, label = Country), 
            size = 3, vjust = -1, color = "black") + # Add country names
  scale_size_continuous(range = c(1, 15), name = "CO2 Emissions (kt)") +
  scale_color_gradient(low = "blue", high = "red", name = "Urban Population") +
  labs(
    title = "Global CO2 Emissions with Urban Population",
    subtitle = "Bubble size represents CO2 emissions; Top 10 countries labeled",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

############### Bump plot: Comparison of Top 10 Countries: Urban Population vs CO2 Emissions ###########

library(ggplot2)
library(dplyr)
library(tidyr)

# Rank countries by UrbanPopulation and CO2Emissions
ranked_data <- co2_health_data %>%
  mutate(
    Urban_Rank = dense_rank(desc(UrbanPopulation)),
    CO2_Rank = dense_rank(desc(CO2Emissions))
  ) %>%
  filter(Urban_Rank <= 10 | CO2_Rank <= 10)  # Keep only top 10 in either metric

# Reshape data for bump chart
bump_data <- ranked_data %>%
  pivot_longer(
    cols = c(Urban_Rank, CO2_Rank),
    names_to = "Metric",
    values_to = "Rank"
  ) %>%
  mutate(
    Metric = ifelse(Metric == "Urban_Rank", "Urban Population", "CO2 Emissions"),
    Metric = factor(Metric, levels = c("Urban Population", "CO2 Emissions"))
  )

# Create the bump chart
ggplot(bump_data, aes(x = Metric, y = Rank, group = Country, color = Country)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text(data = bump_data %>% filter(Metric == "Urban Population"), 
            aes(label = Country), hjust = 1.2, size = 3, show.legend = FALSE) +
  geom_text(data = bump_data %>% filter(Metric == "CO2 Emissions"), 
            aes(label = Country), hjust = -0.2, size = 3, show.legend = FALSE) +
  scale_y_reverse(breaks = 1:10) +
  labs(
    title = "Comparison of Top 10 Countries: Urban Population vs CO2 Emissions",
    x = "",
    y = "Rank",
    color = "Country"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  )


########
library(ggplot2)
library(dplyr)
library(tidyr)

# Rank countries by UrbanPopulation and CO2Emissions
ranked_data <- co2_health_data %>%
  mutate(
    Urban_Rank = dense_rank(desc(UrbanPopulation)),
    CO2_Rank = dense_rank(desc(CO2Emissions))
  ) %>%
  filter(Urban_Rank <= 10 | CO2_Rank <= 10)  # Keep only top 10 in either metric

# Reshape data for bump chart
bump_data <- ranked_data %>%
  pivot_longer(
    cols = c(Urban_Rank, CO2_Rank),
    names_to = "Metric",
    values_to = "Rank"
  ) %>%
  mutate(
    Metric = ifelse(Metric == "Urban_Rank", "Urban Population", "CO2 Emissions"),
    Metric = factor(Metric, levels = c("Urban Population", "CO2 Emissions"))
  )

# Create the bump chart with ForestedArea as color
ggplot(bump_data, aes(x = Metric, y = Rank, group = Country, color = ForestedArea)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text(data = bump_data %>% filter(Metric == "Urban Population"), 
            aes(label = Country), hjust = 1.2, size = 3, show.legend = FALSE) +
  geom_text(data = bump_data %>% filter(Metric == "CO2 Emissions"), 
            aes(label = Country), hjust = -0.2, size = 3, show.legend = FALSE) +
  scale_y_reverse(breaks = 1:10) +
  scale_color_gradient(low = "yellow", high = "darkgreen", name = "Forested Area (%)") +
  labs(
    title = "Comparison of Top 10 Countries: Urban Population vs CO2 Emissions",
    x = "",
    y = "Rank"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  )

#################
library(ggplot2)
library(dplyr)
library(tidyr)

# Rank countries by UrbanPopulation and CO2Emissions
ranked_data <- co2_health_data %>%
  mutate(
    Urban_Rank = dense_rank(desc(UrbanPopulation)),
    CO2_Rank = dense_rank(desc(CO2Emissions))
  ) %>%
  filter(Urban_Rank <= 10 | CO2_Rank <= 10)  # Keep only top 10 in either metric

# Reshape data for bump chart
bump_data <- ranked_data %>%
  pivot_longer(
    cols = c(Urban_Rank, CO2_Rank),
    names_to = "Metric",
    values_to = "Rank"
  ) %>%
  mutate(
    Metric = ifelse(Metric == "Urban_Rank", "Urban Population", "CO2 Emissions"),
    Metric = factor(Metric, levels = c("Urban Population", "CO2 Emissions"))
  )

# Create the bump chart with ForestedArea as color and GasolinePrice as size
ggplot(bump_data, aes(x = Metric, y = Rank, group = Country, color = ForestedArea, size = GasolinePrice)) +
  geom_line(size = 1.2) +
  geom_point(alpha = 0.8) +
  geom_text(data = bump_data %>% filter(Metric == "Urban Population"), 
            aes(label = Country), hjust = 1.2, size = 3, show.legend = FALSE) +
  geom_text(data = bump_data %>% filter(Metric == "CO2 Emissions"), 
            aes(label = Country), hjust = -0.2, size = 3, show.legend = FALSE) +
  scale_y_reverse(breaks = 1:10) +
  scale_color_gradient(low = "yellow", high = "darkgreen", name = "Forested Area (%)") +
  scale_size_continuous(range = c(2, 10), name = "Gasoline Price (USD)") +
  labs(
    title = "Comparison of Top 10 Countries: Urban Population vs CO2 Emissions",
    x = "",
    y = "Rank"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  )



#################  Raincloud Plot of Gasoline Price by CO2 Emissions Category #############
# Install and load the necessary libraries
#install.packages("ggplot2")
#install.packages("ggdist")
library(ggplot2)
library(ggdist)
library(rugarch)
library(parallel)
# Categorize CO2 emissions into Low, Medium, and High
low_threshold <- quantile(co2_health_data$CO2Emissions, 0.33)
high_threshold <- quantile(co2_health_data$CO2Emissions, 0.66)

co2_health_data$CO2_Category <- cut(co2_health_data$CO2Emissions,
                                    breaks = c(-Inf, low_threshold, high_threshold, Inf),
                                    labels = c("Low", "Medium", "High"),
                                    right = FALSE)
ggsave("raincloud_plot.png", width = 3, height = 3)

# Create the raincloud plot for GasolinePrice
ggplot(co2_health_data, aes(x = CO2_Category, y = GasolinePrice, fill = CO2_Category)) +
  
  # Add half-violin plot from {ggdist} package
  stat_halfeye(
    adjust = 0.3,
    justification = -0.1,
    .width = 0,
    point_colour = NA
  ) +
  
  # Add boxplot to show the central tendency and spread
  geom_boxplot(
    width = 0.12,
    outlier.color = NA,
    alpha = 0.5
  ) +
  

  # Set the plot labels and theme
  labs(title = "Raincloud Plot of Gasoline Price by CO2 Emissions Category",
       x = "CO2 Emissions Category",
       y = "Gasoline Price") +
  theme_minimal()
############################




######################## "Raincloud Plot of Forest Area by CO2 Emissions Category"
library(ghibli)
library(ggdist)

ggplot(co2_health_data, aes(x = CO2_Category, y = ForestedArea, fill = CO2_Category)) +
  # Set manual color palette for CO2_Category
  scale_fill_manual(values = c("Low" = "green", "Medium" = "yellow", "High" = "red")) +
  
  # Add boxplot
  geom_boxplot(width = 0.1) +
  
  # Customize theme
  theme_classic(base_size = 18, base_family = "serif") +
  theme(
    text = element_text(size = 18),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, color = "black"),
    axis.text.y = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  ) +
  
  # Customize y-axis
  scale_y_continuous(expand = c(0, 0)) +
  
  # Add dots
  stat_dots(
    side = "left",
    justification = 1.12,
    binwidth = NA
  ) +
  
  # Add half-violin plot
  stat_halfeye(
    adjust = 0.5,
    width = 0.6,
    justification = -0.2,
    .width = 0,
    point_colour = NA
  )+
# Set the plot labels and theme
labs(title = "Raincloud Plot of Forest Area by CO2 Emissions Category",
     x = "CO2 Emissions Category",
     y = "Forest Area") +
  theme_minimal()


################################## Impact Analysis with Boxplots

# Reshape effect data for visualization
library(tidyr)
effect_long <- co2_effect_data %>%
  select(Country, CO2Emissions, LifeExpectancy, InfantMortality, MaternalMortality) %>%
  pivot_longer(cols = c(LifeExpectancy, InfantMortality, MaternalMortality), names_to = "Effect", values_to = "Value")

# Boxplot
ggplot(effect_long, aes(x = Effect, y = Value, fill = CO2Emissions)) +
  geom_boxplot(outlier.color = "red", alpha = 0.7) +
  scale_fill_gradient(low = "blue", high = "red", name = "CO2 Emissions (kt)") +
  theme_minimal() +
  labs(
    title = "Impact of CO2 Emissions on Health Metrics",
    x = "Health Metric",
    y = "Value"
  )








