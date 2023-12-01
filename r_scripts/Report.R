#### `r_scripts/Report.R`
#### by: UUN S2616861
#### 04.12.2023
### Dependencies ----
## Tidyverse, gt, webshot2, janitor, here and OpenAI
{
  # Loading packages
  pacman::p_load(tidyverse, openai, gridExtra, gt, webshot2, janitor, here)
  
  ## Set variables
  OPENAI_API_KEY = Sys.getenv("OPENAI_API_KEY") # Setting API key for OpenAI
  OPENAI_API_KEY = ""
  YEAR = 2019 # Setting year for analysis
  CITY = "Milano" # Setting city for analysis
  
  # List files in working directory
  list.files("./")
}



### Reading data and tidying ----
## Processing OECD Health data of respiratory diseases
{
  # Source: https://stats.oecd.org/viewhtml.aspx?datasetcode=HEALTH_PROC&lang=en#
  # 10.11.2023 - all indicators
  oecd_data_raw <- read_csv(here("raw_data/", "HEALTH_PROC_10112023095631477.csv"))
  
  # Eyeballing data
  oecd_data_raw %>% glimpse() # Country names are in English language, `Türkiye` is used instead of `Turkey` see
  # https://www.theguardian.com/world/2022/jun/03/turkey-changes-name-to-turkiye-as-other-name-is-for-the-birds
  oecd_data_raw %>% skimr::skim()
  oecd_data_raw %>% tabyl(Country, Year, show_na = FALSE) %>% # Shows 2018 the highest number of observations
    adorn_totals("row") %>%
    adorn_percentages("row") %>%
    adorn_pct_formatting(digits = 1) %>%
    adorn_ns %>%
    adorn_title %>%
    head(10)
  
  # Preparing the OECD data
  oecd_data_tidy <- oecd_data_raw %>%
    filter(Variable == "Diseases of the respiratory system") %>%
    filter(Measure == "Number") %>%
    select(Year, Country, Variable, Value) %>%
    arrange(Year) %>%
    pivot_wider(names_from = Variable, values_from = Value) %>% 
    filter(Year == YEAR) %>%
    select(Country, starts_with("Diseases")) %>%
    mutate(Country = Country %>%
      factor())
}

## Processing air pollution data
{
  # Source: https://github.com/dw-data/edjnet-pm2p5)
  # Reading data 
  air_pollution_data_raw <- read_csv(here("raw_data/", "CAMS-Europe-Renalaysis-Countries-Yearly-2018-2022.csv"))
  air_pollution_cities_data_raw <- read_csv(here("raw_data/", "CAMS-Europe-Renalysis-Yearly-2018-2022.csv"))
  
  # Eyeballing data
  air_pollution_data_raw %>% glimpse() # Country names are each in their local language
  air_pollution_data_raw %>% skimr::skim()
  air_pollution_data_raw %>% tabyl(`Name (latin characters)`, Year, show_na = FALSE) %>% # Shows for all years (2018 - 2022) the same amount of observations
    adorn_totals("row") %>%
    adorn_percentages("row") %>%
    adorn_pct_formatting(digits = 1) %>%
    adorn_ns %>%
    adorn_title %>%
    head(10)
  air_pollution_cities_data_raw %>% glimpse() # Country names are each in their short code
  air_pollution_cities_data_raw %>% skimr::skim()
  air_pollution_cities_data_raw %>% tabyl(`Name (latin characters)`, Day, show_na = FALSE) %>% # Shows for all years (2018 - 2022) the same amount of observations
    adorn_totals("row") %>%
    adorn_percentages("row") %>%
    adorn_pct_formatting(digits = 1) %>%
    adorn_ns %>%
    adorn_title %>%
    head(10)
  
  # Preparing the air pollution data
  air_pollution_data_tidy <- air_pollution_data_raw %>%
    select(Year, "Name (latin characters)", "Yearly PM 2.5 average (µg/m³)", "Population estimate (GHSL 2020)") %>%
    rename(Country = "Name (latin characters)", PM2.5 = "Yearly PM 2.5 average (µg/m³)", Pop = "Population estimate (GHSL 2020)") %>%
    filter(Year == YEAR) %>%
    select(Country, PM2.5, Pop) %>%
    mutate(Country = Country %>%
      factor())
  
  # Preparing the air pollution data for cities
  air_pollution_cities_data_tidy <- air_pollution_cities_data_raw %>%
  select(Day, `Country code`, "Name (latin characters)", "Daily PM 2.5 average (µg/m³)") %>%
    rename(Code = `Country code`, City = "Name (latin characters)", PM2.5 = "Daily PM 2.5 average (µg/m³)") %>%
    filter(Day == paste0(YEAR, "-12-31")) %>%
    select(City, Code, PM2.5) %>%
    mutate(City = City %>%
             factor(),
           Code = Code %>%
             factor())
  # Storing PM2.5 value for the selected city
  city_pm <- air_pollution_cities_data_tidy %>%
    filter(City == CITY) %>%
    select(PM2.5) %>%
    as.numeric()
}

## Processing overweight / obesity data ----
{
  # Source: https://data.oecd.org/healthrisk/overweight-or-obese-population.htm
  # Reading data
  obesity_data_raw <- read_csv(here("raw_data/", "DP_LIVE_02112023125750641.csv"))
  
  # Eyeballing data
  obesity_data_raw %>% glimpse() # Country names are in a 3-letter country-code format
  obesity_data_raw %>% skimr::skim()
  obesity_data_raw %>% tabyl(LOCATION, TIME, show_na = FALSE) %>% # Shows 2019 the highest number of observations
    adorn_totals("row") %>%
    adorn_percentages("row") %>%
    adorn_pct_formatting(digits = 1) %>%
    adorn_ns %>%
    adorn_title %>%
    head(10)
  
  # Preparing the obesity data
  obesity_data_tidy <- obesity_data_raw %>%
    filter(TIME == YEAR) %>%
    select(LOCATION, Value) %>%
    rename(Country = LOCATION) %>%
    group_by(Country) %>%
    summarise(Value = mean(Value, na.rm = TRUE)) %>%
    mutate(Country = Country %>%
           factor())
}

## Processing smoke data
{
  # Source: https://www.statista.com/statistics/433390/individuals-who-currently-smoke-cigarettes-in-european-countries/
  # Reading data 
  smoking_data_raw <- read_csv(here("raw_data/", "statistic_id433390_current-smokers-in-europe-2020-by-country.csv"))
  
  # Eyeballing data
  smoking_data_raw %>% glimpse()
  
  # Preparing the obesity data
  smoking_data_tidy <- smoking_data_raw %>%
    mutate(Country = Country %>%
             factor())
}

## Processing median age data
{
  # Source: https://www.cia.gov/the-world-factbook/field/median-age/country-comparison/
  # Reading data
  age_data_raw <- read_csv(here("raw_data/", "cia_factbook_median_age.csv"))
  
  # Eyeballing data
  age_data_raw %>% glimpse()
  
  # Preparing the obesity data
  age_data_tidy <- age_data_raw %>%
    select(name, value) %>%
    rename(Country = name, `Median age` = value) %>%
    mutate(Country = Country %>%
             factor())
  
  # Remove unused data / values from Global Environment
  rm(list = c("air_pollution_data_raw", "obesity_data_raw", "oecd_data_raw", "air_pollution_cities_data_raw", "air_pollution_cities_data_tidy", "smoking_data_raw", "age_data_raw"))
}



### Checking common countries in the datasets ----
## ChatGPT or loading existing data
if (nchar(OPENAI_API_KEY) == 51) {
  message("API key is valid, requesting data from OpenAI")

  ## Filter unique countries and continents
  # Filter dataset `oecd_data_tidy`
  countries_oecd <- oecd_data_tidy %>%
    select(Country) %>%
    distinct() %>%
    arrange(Country)
  
  # Filter dataset `air_pollution_data_tidy`
  countries_airpol <- air_pollution_data_tidy %>%
    select(Country) %>%
    distinct() %>%
    arrange(Country)
  
  # Filter dataset `obesity_data_tidy`
  countries_obesity <- obesity_data_tidy %>%
    select(Country) %>%
    distinct() %>%
    arrange(Country)
  
  # Filter dataset `smoking_data_tidy`
  countries_smoking <- smoking_data_tidy %>%
    select(Country) %>%
    distinct() %>%
    arrange(Country)
  
  # Filter dataset `age_data_tidy`
  countries_age <- age_data_tidy %>%
    select(Country) %>%
    distinct() %>%
    arrange(Country)
  
  ## Convert country tibbles to chr
  # Create empty vector
  countries <- character()
  # OECD data,
  countries <- append(countries, list(levels(countries_oecd$Country)))
  # air pollution data
  countries <- append(countries, list(levels(countries_airpol$Country)))
  # smoking data,
  countries <- append(countries, list(levels(countries_smoking$Country)))
  # age data,
  countries <- append(countries, list(levels(countries_age$Country)))
  # OECD obesity data
  countries <- append(countries, list(levels(countries_obesity$Country)))
  
  ## Request AI completion to standardize the country names
  # Loop through OpenAI ChatGPT completion
  countries_cleaned <- vector()
  check <- character()
  country <- character()
  continent_list <- ""
  for (i in 1:5) {
    # Remove unneeded character from country lists
    countries[i] <- countries[i] %>% gsub("\n", "", .) %>% gsub('\\"', "", .) %>% gsub("c(", "", ., fixed = TRUE) %>% gsub(")", "", ., fixed = TRUE)
    
    # Request AI completion, as long as output has same length as input
    while (length(strsplit(as.character(countries[i]), ", ")[[1]]) != length(strsplit(as.character(countries_cleaned[i]), ", ")[[1]])) {
      print("Input")
      print(paste(countries[i], collapse = ", "))
      print(paste0("Countries: ", length(strsplit(as.character(countries[i]), ", ")[[1]])))
      request <- create_chat_completion(model = "gpt-3.5-turbo", openai_api_key = OPENAI_API_KEY, messages = list(
        list("role" = "system", "content" = "You are a translator of country names or descriptions which comes often in national language, several anmes or abbrevations, into a single standard english country name. Use `Türkiye` instead of `Turkey`."),
        list("role" = "user", "content" = "I will give you a list of countries and I need you to reply just the english country names, comma seperated, without any further content! Please give me an sample output of '
                                           Türkiye, Danmark, DEU, Schweiz/Suisse/Svizzera, Shqipëria'."),
        list("role" = "assistant", "content" = "Türkiye, Denmark, Germany, Switzerland, Albania"),
        list("role" = "user", "content" = "Exactly, like that! Now I give you the list. `Türkiye` is the new name for formerly `Turkey`"),
        list("role" = "user", "content" = paste(countries[i], collapse = ", "))))
      countries_cleaned[i] <- request$choices$message.content
      print("Output")
      print(countries_cleaned[i])
      print(paste0("Countries: ", length(strsplit(as.character(countries_cleaned[i]), ", ")[[1]])))
      print("------------------")
    }
    
    # From chr to vector
    check <- append(check, list(strsplit(as.character(countries[i]), split = ", ")[[1]]))
    country <- append(country, list(strsplit(countries_cleaned[i], split = ", ")[[1]]))
    
    # Continent list
    if (i == 5) {
      # Loop as long number of countries are not equal to number continents
      while (length(unlist(country[i])) != length(continent_list)) {
        print("Input")
        print(paste(countries_cleaned[i], collapse = ", "))
        print(paste0("Country list length: ", length(unlist(country[i]))))
        request <- create_chat_completion(model = "gpt-3.5-turbo", openai_api_key = OPENAI_API_KEY, messages = list(
          list("role" = "system", "content" = "You convert country names to the english continent names they are on."),
          list("role" = "user", "content" = "I will give you a list of countries and I need you to print out the english continent names, comma seperate all countries, without any further content! Please give me an sample output of 'Germany, Mexico, Japan, Chile'."),
          list("role" = "assistant", "content" = "Europe, North Amercia, Asia, South America"),
          list("role" = "user", "content" = "Exactly, like that! Now I give you the list."),
          list("role" = "user", "content" = paste(countries_cleaned[i], collapse = ", "))))
        continent_list <- request$choices$message.content
        continent_list <- strsplit(continent_list, split = ", ")[[1]]
        print("Output")
        print(paste(continent_list, collapse = ", "))
        print(paste0("Continent list length: ", length(continent_list)))
      }
      
      # Find common countries in the lists
      country_list <- intersect(intersect(intersect(intersect(unlist(country[1]), unlist(country[2])), unlist(country[3])), unlist(country[4])), unlist(country[5]))
      
      # Save lists
      saveRDS(country_list, here("processed_data/", "country_list.rds"))
      saveRDS(continent_list, here("processed_data/", "continent_list.rds"))
      saveRDS(check, here("processed_data/", "check.rds"))
      saveRDS(country, here("processed_data/", "country.rds"))
    }
  }
} else {
  message("API key is invalid, loading data from disk ...")

  # Load lists, if API key is missing
  country_list <- readRDS(here("processed_data/", "country_list.rds"))
  continent_list <- readRDS(here("processed_data/", "continent_list.rds"))
  check <- readRDS(here("processed_data/", "check.rds"))
  country <- readRDS(here("processed_data/", "country.rds"))
}

## Show common countries
{
  print("############################")
  print(paste0("### Common countries: ", length(country_list), " ###"))
  print("############################")

  # Remove unused data / values from Global Environment
  rm(list = c("i", "request", "countries" ,"countries_cleaned", "countries_airpol", "countries_obesity", "countries_oecd", "countries_smoking", "countries_age"))
}



### Cleaning country descriptions ----
{
  ## Create dataset `oecd_data_plot`
  # Standardize country names ...
  oecd_plot <- oecd_data_tidy
  for (i in 1:length(unlist(country[1]))) {
    oecd_plot <- oecd_plot %>%
      mutate(Country = if_else(Country == unlist(check[1])[i], unlist(country[1])[i], Country))
  }
  # ... and filter for common countries in datasets
  oecd_plot <- oecd_plot %>%
    filter(Country %in% country_list)
  
  ## Create dataset `air_pollution_plot`
  # Standardize country names ...
  air_pollution_plot <- air_pollution_data_tidy
  for (i in 1:length(unlist(country[2]))) {
    air_pollution_plot <- air_pollution_plot %>%
      mutate(Country = if_else(Country == unlist(check[2])[i], unlist(country[2])[i], Country))
  }
  # ... and filter for common countries in datasets
  air_pollution_plot <- air_pollution_plot %>%
    filter(Country %in% country_list)
  
  ## Create dataset `smoking_data_plot`
  # Standardize country names ...
  smoking_plot <- smoking_data_tidy
  for (i in 1:length(unlist(country[3]))) {
    smoking_plot <- smoking_plot %>%
      mutate(Country = if_else(Country == unlist(check[3])[i], unlist(country[3])[i], Country))
  }
  # ... and filter for common countries in datasets
  smoking_plot <- smoking_plot %>%
    filter(Country %in% country_list)
  
  ## Create dataset `age_data_plot`
  # Standardize country names ...
  age_plot <- age_data_tidy
  for (i in 1:length(unlist(country[4]))) {
    smoking_plot <- smoking_plot %>%
      mutate(Country = if_else(Country == unlist(check[4])[i], unlist(country[4])[i], Country))
  }
  # ... and filter for common countries in datasets
  age_plot <- age_plot %>%
    filter(Country %in% country_list)
  
  ## Create dataset `obesity_plot`
  # Standardize country names
  obesity_plot <- obesity_data_tidy
  for (i in 1:length(unlist(country[5]))) {
    obesity_plot <- obesity_plot %>%
      mutate(Country = if_else(Country == unlist(check[5])[i], unlist(country[5])[i], Country))
  }
  # ... and filter for common countries in datasets
  obesity_plot <- obesity_plot %>%
    filter(Country %in% country_list)
  
  # Remove unused data / values from Global Environment
  rm(list = c("air_pollution_data_tidy", "obesity_data_tidy", "oecd_data_tidy", "smoking_data_tidy", "i", "check", "country", "country_list", "continent_list"))
}



### Preparation of the plotting datasets ----
{
  ## Combining the datasets
  # Joining OECD data and air pollution data
  data <- left_join(oecd_plot, air_pollution_plot, by = "Country")
  
  # Joining obesity data
  data <- left_join(data, obesity_plot, by = "Country")
  
  # Joining smoking data
  data <- left_join(data, smoking_plot, by = "Country")
  
  # Joining age data
  data <- left_join(data, age_plot, by = "Country")
  
  ## Prepare the dataset for plotting
  # Calculate the diseases per 100.000
  data_plot <- data %>%
    mutate("Diseases of the respiratory system" = `Diseases of the respiratory system` / Pop * 100000) %>%
    rename("Resp. diseases per 100.000" = `Diseases of the respiratory system`) %>%
    rename("Obesity" = Value) %>%
    rename("Smoker" = `Percentage of smokers`)
  
  # Remove unused data / values from Global Environment
  rm(list = c("data", "oecd_plot", "air_pollution_plot", "obesity_plot", "smoking_plot", "age_plot"))
}



### Visualisation ----
{
  ## Figure A1: Point figure with a linear regression line
  # Calculate dependence of respiratory diseases in correlation to air pollution
  model = lm(`Resp. diseases per 100.000` ~ PM2.5, data = data_plot)
  p = round(summary(model)$coefficients[,4][2], digits = 3)
  
  # Predict resp. diseases per 100,000 for selected city
  city_value <- data.frame(PM2.5 = city_pm)
  city_resp_diseases <- as.numeric(predict(model, newdata = city_value))
  data_plot_with_city <- data_plot %>%
    add_row(Country = CITY, `Resp. diseases per 100.000` = city_resp_diseases, PM2.5 = city_pm, `Median age` = NA)
  
  # Show correlation between respiratory diseases and air pollution, plus selected city
  p1a <- data_plot_with_city %>%
    ggplot(aes(x = PM2.5, y = `Resp. diseases per 100.000`, color = Country)) +
    geom_point(aes(color = Country, size = `Resp. diseases per 100.000`)) +
    geom_smooth(method = "lm", se = FALSE, color = "gray") +
    geom_text(aes(label = Country, color = `Resp. diseases per 100.000` > 2000), hjust = -0.1, vjust = 1.5) +
    labs(title = paste0("Figure A1: Respiratory diseases in ", length(data_plot$Country)," european countries"),
         subtitle = paste0("in correlation to air pollution (year ", YEAR, "), plus prediction of ", CITY),
         x = "Air pollution (particulate matter as PM2.5)", y = "Respiratory diseases (per 100.000)", 
         caption = "Data source: OECD health data & https://github.com/dw-data/edjnet-pm2p5") +
    scale_y_continuous(labels = scales::label_comma()) +
    expand_limits(x = c(1, 30), y = c(660, 2400)) +
    coord_cartesian(expand = FALSE) +
    annotate("text", x = 15, y = 1580, label = paste0("p-value = ", p)) +
    annotate("text", x = 25, y = 1500, label = paste0(round(city_resp_diseases, digits = 0), " per 100,000")) +
    annotate("text", x = 25, y = 1400, label = "predicted resp. diseases") +
    annotate("text", x = 25, y = 1300, label = "(1,37m total population)") +
    theme_bw() +
    theme(legend.position = "none", 
          plot.caption = element_text(face = "italic", hjust = 0),
          panel.grid.major = element_line(colour = "lightgrey", linetype = "dashed"),
          plot.title = element_text(size = 16, colour = "#2b2828", hjust = 0),
          plot.subtitle = element_text(size = 12, colour = "#2b2828", hjust = 0),
          axis.text.y = element_text(size = 10, colour = "#2b2828"),
          axis.text.x = element_text(size = 10, colour = "#2b2828", angle = 90, vjust = 0.5),
          axis.title = element_text(size = 14, hjust = 0.5, colour = "#2b2828"))

  ## Figure A2: Point figure with a linear regression line
  # Calculate dependence of respiratory diseases in correlation to obesity
  model = lm(`Resp. diseases per 100.000` ~ Obesity, data = data_plot)
  p = round(summary(model)$coefficients[,4][2], digits = 3)
  
  # Correlation between respiratory diseases and overweight & obesity
  p1b <- data_plot %>%
    ggplot(aes(x = Obesity, y = `Resp. diseases per 100.000`, color = Country)) +
    geom_point(aes(color = Country, size = `Resp. diseases per 100.000`)) +
    geom_text(aes(label = Country, color = `Resp. diseases per 100.000` > 2000), hjust = -0.1, vjust = 1.5) +
    geom_smooth(method = "lm", se = FALSE, color = "gray") +
    annotate("text", x = 65, y = 1900, label = paste0("p-value = ", p)) +
    labs(title = paste0("Figure A2: Respiratory diseases in ", length(data_plot$Country), " european countries"),
         subtitle = paste0("in correlation to obesity (year ", YEAR, ")"),
         x = "Obesity (in percentage)", y = "Respiratory diseases (per 100.000)", 
         caption = "Data source: OECD health data") +
    scale_x_continuous(labels = function(x) paste0(x, '%')) +
    scale_y_continuous(labels = scales::label_comma()) +
    expand_limits(x = c(45, 70), y = c(500, 2400)) +
    coord_cartesian(expand = FALSE) +
    theme_bw() +
    theme(legend.position = "none", 
          plot.caption = element_text(face = "italic",hjust = 0),
          panel.grid.major = element_line(colour = "lightgrey", linetype = "dashed"),
          plot.title = element_text(size = 16, colour = "#2b2828", hjust = 0),
          plot.subtitle = element_text(size = 12, colour = "#2b2828", hjust = 0),
          axis.text.y = element_text(size = 10, colour = "#2b2828"),
          axis.text.x = element_text(size = 10, colour = "#2b2828", angle = 90, vjust = 0.5),
          axis.title = element_text(size = 14, colour = "#2b2828"))

  ## Figure A3: Point figure with a linear regression line
  # Calculate dependence of respiratory diseases in correlation to smoking
  model = lm(`Resp. diseases per 100.000` ~ Smoker, data = data_plot)
  p = round(summary(model)$coefficients[,4][2], digits = 3)
  
  # Correlation between respiratory diseases and smoking
  p1c <- data_plot %>%
    ggplot(aes(x = Smoker, y = `Resp. diseases per 100.000`, color = Country)) +
    geom_point(aes(color = Country, size = `Resp. diseases per 100.000`)) +
    geom_text(aes(label = Country, color = `Resp. diseases per 100.000` > 2000), hjust = -0.1, vjust = 1.5) +
    geom_smooth(method = "lm", se = FALSE, color = "gray") +
    annotate("text", x = 35, y = 1800, label = paste0("p-value = ", p)) +
    labs(title = paste0("Figure A3: Respiratory diseases in ", length(data_plot$Country), " european countries"),
         subtitle = paste0("in correlation to smoking (year ", YEAR, ")"),
         x = "Smoker (in percentage)", y = "Respiratory diseases (per 100.000)", 
         caption = "Data source: https://www.statista.com/statistics/433390/individuals-who-currently-smoke-cigarettes-in-european-countries/") +
    scale_x_continuous(labels = function(x) paste0(x, '%')) +
    scale_y_continuous(labels = scales::label_comma()) +
    expand_limits(x = c(15, 40), y = c(500, 2400)) +
    coord_cartesian(expand = FALSE) +
    theme_bw() +
    theme(legend.position = "none", 
          plot.caption = element_text(face = "italic",hjust = 0),
          panel.grid.major = element_line(colour = "lightgrey", linetype = "dashed"),
          plot.title = element_text(size = 16, colour = "#2b2828", hjust = 0),
          plot.subtitle = element_text(size = 12, colour = "#2b2828", hjust = 0),
          axis.text.y = element_text(size = 10, colour = "#2b2828"),
          axis.text.x = element_text(size = 10, colour = "#2b2828", angle = 90, vjust = 0.5),
          axis.title = element_text(size = 14, colour = "#2b2828"))
  
  ## Figure A4: Point figure with a linear regression line
  # Calculate dependence of respiratory diseases in correlation to smoking
  model = lm(`Resp. diseases per 100.000` ~ `Median age`, data = data_plot)
  p = round(summary(model)$coefficients[,4][2], digits = 3)
  
  # Correlation between respiratory diseases and age
  p1d <- data_plot %>%
    ggplot(aes(x = `Median age`, y = `Resp. diseases per 100.000`, color = Country)) +
    geom_point(aes(color = Country, size = `Resp. diseases per 100.000`)) +
    geom_text(aes(label = Country, color = `Resp. diseases per 100.000` > 2000), hjust = -0.1, vjust = 1.5) +
    geom_smooth(method = "lm", se = FALSE, color = "gray") +
    annotate("text", x = 40, y = 1300, label = paste0("p-value = ", p)) +
    labs(title = paste0("Figure A4: Respiratory diseases in ", length(data_plot$Country), " european countries"),
         subtitle = paste0("in correlation to age (year ", YEAR, ")"),
         x = "Median Age (in years)", y = "Respiratory diseases (per 100.000)", 
         caption = "Data source: https://www.cia.gov/the-world-factbook/field/median-age/country-comparison/") +
    scale_x_continuous(labels = function(x) paste0(x, 'yrs')) +
    scale_y_continuous(labels = scales::label_comma()) +
    expand_limits(x = c(38.5, 47), y = c(500, 2400)) +
    coord_cartesian(expand = FALSE) +
    theme_bw() +
    theme(legend.position = "none", 
          plot.caption = element_text(face = "italic",hjust = 0),
          panel.grid.major = element_line(colour = "lightgrey", linetype = "dashed"),
          plot.title = element_text(size = 16, colour = "#2b2828", hjust = 0),
          plot.subtitle = element_text(size = 12, colour = "#2b2828", hjust = 0),
          axis.text.y = element_text(size = 10, colour = "#2b2828"),
          axis.text.x = element_text(size = 10, colour = "#2b2828", angle = 90, vjust = 0.5),
          axis.title = element_text(size = 14, colour = "#2b2828"))

  ## Combine the plots
  combined_plot <- grid.arrange(p1a, p1b, p1c, p1d, nrow = 4)
  
  # Save the combined plot
  ggsave(combined_plot, file = here("figures/", "Plot_A_Combined.png"), width = 7, height = 20)
  
  # Remove unused data / values from Global Environment
  rm(list = c("p", "model", "CITY", "city_pm", "city_value", "city_resp_diseases", "data_plot_with_city", "data_plot", "p1a", "p1b", "p1c", "p1d"))
}

# Show figure
combined_plot

# Remove unused data / values from Global Environment
rm("combined_plot")

## More data from countries on other continents needed!
{
  # Source: https://ourworldindata.org/obesity
  obesity_world_data_raw <- read_csv(here("raw_data/", "share-of-deaths-obesity.csv"))
  obesity_world_data_tidy <- obesity_world_data_raw %>%
    filter(Year == YEAR) %>%
    drop_na(Code) %>%
    filter(str_length(Code) <= 3) %>%
    rename("Country" = Code, "Obesity" = starts_with("Share")) %>%
    select(Country, Obesity)
  
  ## ChatGPT or loading existing data
  if (nchar(OPENAI_API_KEY) == 51) {
    message("API key is valid, requesting data from OpenAI")
  
    # OpenAI ChatGPT API usage to map continents to countries
    convert_codes <- function(mode, data) {
      output <- vector()
      cat("Converting codes: ")
      for (country in data) {
        if (mode == "Country") {
          request <- create_chat_completion(model = "gpt-3.5-turbo", openai_api_key = OPENAI_API_KEY, messages = list(
              list("role" = "system", "content" = "You convert a country code to the english country name. Use `Unknown` for unknown country codes."),
              list("role" = "user", "content" = "Please convert 'DEU'."),
              list("role" = "assistant", "content" = "Germany"),
              list("role" = "user", "content" = "Exactly, like that! Now I give you another country code."),
              list("role" = "user", "content" = country)))
          output <- append(output, request$choices$message.content)
        }
        if (mode == "Continent") {
          request <- create_chat_completion(model = "gpt-3.5-turbo", openai_api_key = OPENAI_API_KEY, messages = list(
              list("role" = "system", "content" = "You convert a country code to the english continent name it is located on. Use `Unknown` for unknown country codes or one of these 6 different continent names `Africa`, `Asia`, `Europe`, `North America`, `South America`, `Oceania`.)"),
              list("role" = "user", "content" = "Please convert 'DEU'."),
              list("role" = "assistant", "content" = "Europe"),
              list("role" = "user", "content" = "Exactly, like that! Now I give you another country code."),
              list("role" = "user", "content" = country)))
          output <- append(output, request$choices$message.content)
        }
        cat("#")
      }
      return(output)
    }
    
    # Convert country codes to continents
    continents_world <- convert_codes("Continent", data = obesity_world_data_tidy$Country)
    saveRDS(continents_world, here("processed_data/", "continents.rds"))
    
    # Convert country codes to country names
    countries_world <- convert_codes("Country", data = obesity_world_data_tidy$Country)
    saveRDS(countries_world, here("processed_data/", "countries.rds"))
  } else {
    message("API key is invalid, loading existing data")
    
    # Load saved data (ChatGPT requests are charged!)
    continents_world <- readRDS(here("processed_data/", "continents.rds"))
    countries_world <- readRDS(here("processed_data/", "countries.rds"))
  }
  
  # Creating dataset for plotting
  obesity_world_plot <- obesity_world_data_tidy %>%
    mutate(Country = countries_world %>%
      factor()) %>%
    mutate(Continent = continents_world %>%
      factor()) %>%
    filter(Country != "Unknown") %>%
    filter(Continent != "Unknown")
  
  # Remove unused data / values from Global Environment
  rm(list = c("obesity_world_data_raw", "obesity_world_data_tidy", "continents_world", "countries_world", "OPENAI_API_KEY", "convert_codes"))
  
  ## Checking new data
  # Show levels (Continents)
  obesity_world_plot %>%
    pull(Continent) %>%
    levels()
  
  # Show count of countries per continent
  obesity_world_plot %>%
    select(Continent) %>%
    summary()
  
  # Count African countries
  afr_countries <- obesity_world_plot %>%
    filter(Continent == "Africa") %>%
    select(Continent) %>%
    count()
  
  # Count European countries
  eur_countries <- obesity_world_plot %>%
    filter(Continent == "Europe") %>%
    select(Continent) %>%
    count()
}

## Table A1: Table with 6 columns (grouped by continents)
{
  # Show overview over countries with deaths related to obesity
  t1 <- obesity_world_plot %>%
    mutate(Country = if_else(Obesity < 7,  md(paste0(Country, ' (<span style="color:green">', round(Obesity, digits = 1), '%</span>)')), 
                     if_else(Obesity < 15, md(paste0(Country, ' (<span style="color:#ff8c00">', round(Obesity, digits = 1), '%</span>)')), 
                                           md(paste0("**", Country, ' (<span style="color:red">', round(Obesity, digits = 1), '%</span>)**'))))) %>%
    arrange(Obesity) %>%
    select(Country, Continent) %>%
    pivot_wider(names_from = Continent, values_from = Country) %>%
    gt() %>%
      tab_header(title = "Table A1: Percentage of deaths attributed to obesity",
                 subtitle = paste0("for ", length(obesity_world_plot$Country), " countries, grouped by continents (year ", YEAR, ")")) %>%
      tab_source_note(source_note = md("*Data source: https://ourworldindata.org/obesity*")) %>%
      tab_footnote(footnote = paste0("EUROPE includes ", eur_countries, " countries (avg. age 42.5yrs), AFRICA includes ", afr_countries, " countries (avg. age 19.7yrs)"),
                   locations = cells_column_labels(columns = c(Europe, Africa))) %>%
      cols_move(columns = Africa, after = Asia) %>%  
      cols_move(columns = `North America`, after = Europe) %>%
      cols_move(columns = Oceania, after = `South America`) %>%
      cols_label(Africa = md("**AFRICA**"), Asia = md("**ASIA**"), `North America` = md("**NORTH AMERICA**"),
                 Europe = md("**EUROPE**"), `South America` = md("**SOUTH AMERICA**"), Oceania = md("**OCEANIA**")) %>%
      cols_align(align = "left", columns = everything()) %>%
      tab_spanner(label = md("*Focus continents*"), columns = c(Africa, Europe))
  
  # Save the table
  gtsave(t1, filename = "Table_A1_Deaths_Obesity.pdf", path = here("tables/"))
}

# Show figure
t1

# Remove unused data / values from Global Environment
rm(list = c("t1", "eur_countries", "afr_countries"))

## Figure B: Boxplot, grouped by continents
{
  # Rename `obesity` to `Risk of death`
  obesity_world_plot <- obesity_world_plot %>%
    rename("Risk of death" = "Obesity")
  
  # Show deaths attributed to obesity worldwide
  p2 <- obesity_world_plot %>%
    ggplot(aes(x = Continent, y = `Risk of death`)) +
    geom_boxplot(aes(color = Continent)) +
    labs(title = "Figure B: Risk of death attributed to obesity",
         subtitle = paste0("for ", length(obesity_world_plot$Country), " countries grouped by continents (year ", YEAR, ")"),
         x = "", y = "Risk of death due to obesity (in percentage)",  
         caption = "Data source: https://ourworldindata.org/obesity") +
    theme_bw() +
    theme(legend.position = "none", 
          plot.caption = element_text(face = "italic", hjust = 0),
          plot.background = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), panel.border = element_blank(),
          plot.title = element_text(size = 16, colour = "#2b2828", hjust = 0),
          axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
          axis.text.x = element_text(size = 10, colour = "#2b2828", angle = 90, vjust = 0.5),
          axis.text.y = element_text(size = 10, colour = "#2b2828"),
          axis.title = element_text(size = 14, hjust = 0.5, colour = "#2b2828"))
  
  # Save the table
  ggsave(p2, file = here("figures/", "Plot_B_Deaths_Obesity.png"), width = 7, height = 5)
}

# Show figure
p2

# Remove unused data / values from Global Environment
rm("p2")

## Table A2: Table grouped by risks and age groups
{
  # Add avg. age per country
  obesity_world_plot2 <- left_join(obesity_world_plot, age_data_tidy, by = "Country")
  
  # Convert variable `Risk of death` values from numeric to categorical
  obesity_world_plot2 <- obesity_world_plot2 %>%
    mutate(`Risk of death` = cut(`Risk of death`, breaks = c(0, 7, 15, 100), labels = c("Low", "Medium", "High"), include.lowest = TRUE)) %>%
    mutate(`Median age` = cut(`Median age`, breaks = c(0, 25, 40, 100), labels = c("Young", "Middle", "Old"), include.lowest = TRUE)) %>%
    rename("Age" = "Median age") %>%
    drop_na(Age)
  
  # Calculate highest risk group
  len_list <- obesity_world_plot2 %>%
    filter(`Risk of death` == "High" & Age == "Old") %>%
    count()
  
  # Show overview of risk of death related to obesity and age group
  t2 <- obesity_world_plot2 %>%
    arrange(`Risk of death`) %>%
    mutate(Country = if_else(`Risk of death` == "Low",  md(paste0('<span style="color:green">', Country, ' </span>')), 
                     if_else(`Risk of death` == "Medium", md(paste0('<span style="color:#ff8c00">', Country, '</span>')), 
                     if_else(Age == "Old", md(paste0('**<span style="color:darkred">', Country, '</span>**')),
                                                        md(paste0('**<span style="color:red">', Country, '</span>**'))))),
           Age = toupper(Age)) %>%
    select(Country, `Risk of death`, Age) %>%
    pivot_wider(names_from = `Risk of death`, values_from = Country) %>%
    gt() %>%
      tab_header(title = "Table A2: Risk of death due to obesity",
                 subtitle = paste0("for ", length(obesity_world_plot2$Country), " countries, grouped by risk and age (year ", YEAR, ")")) %>%
      tab_source_note(source_note = md("*Data sources: https://ourworldindata.org/obesity*, https://en.wikipedia.org/wiki/List_of_countries_by_median_age")) %>%
      tab_footnote(footnote = paste0("Legend: `YOUNG` ≙ under 25yrs avg. age; `MIDDLE` ≙ 25 - 40yrs avg. age; `OLD` ≙ over 40yrs avg. age."),
                 locations = cells_column_labels(columns = Age)) %>%  
      tab_footnote(footnote = paste0("Subgroup of high risk & and high avg. age includes ", len_list, " countries."),
                 locations = cells_column_labels(columns = High)) %>%
      cols_move(columns = Low, after = Age) %>%
      cols_label(Age = md("**AGE**"), Low = md("*LOW RISK*"), Medium = md("*MEDIUM RISK*"), High = md("*HIGH RISK*")) %>%
      cols_align(align = "left", columns = everything()) %>%
      tab_spanner(label = md("*Focus countries*"), columns = High)

  # Save the table
  gtsave(t2, filename = "Table_A2_Deaths_Risk.pdf", path = here("tables/"))
  
}

# Show figure
t2

# Remove unused data / values from Global Environment
rm(list = c("t2", "obesity_world_plot", "obesity_world_plot2", "age_data_tidy", "YEAR", "len_list"))

## Session Info
sessionInfo()
