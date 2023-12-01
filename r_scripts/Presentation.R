#### `r_scripts/Presentation.R`
#### by: Benjamin Gross, UUN S2616861
#### 04.12.2023
### Dependencies ----
## Setup Tidyverse and OpenAI
{
  # Loading libraries
  pacman::p_load(tidyverse, openai, here)
  
  ## Set variables
  OPENAI_API_KEY = Sys.getenv("OPENAI_API_KEY") # Setting API key for OpenAI
  YEAR = 2019 # Setting year for analysis
  
  # List files in working directory
  list.files("./")
}



### Reading data and tidying ----
## Processing OECD Health data of respiratory diseases
## Source: https://stats.oecd.org/viewhtml.aspx?datasetcode=HEALTH_PROC&lang=en#
# 10.11.2023 - all indicators
{
  oecd_data_raw <- read_csv(here("raw_data/", "HEALTH_PROC_10112023095631477.csv"))
  
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
  
  ## Processing air pollution data of respiratory diseases 
  ## Source: https://github.com/dw-data/edjnet-pm2p5)
  # Reading data 
  air_pollution_data_raw <- read_csv(here("raw_data/", "CAMS-Europe-Renalaysis-Countries-Yearly-2018-2022.csv"))
  
  # Preparing the air pollution data
  air_pollution_data_tidy <- air_pollution_data_raw %>%
    select(Year, "Name (latin characters)", "Yearly PM 2.5 average (µg/m³)", "Population estimate (GHSL 2020)") %>%
    rename(Country = "Name (latin characters)", PM2.5 = "Yearly PM 2.5 average (µg/m³)", Pop = "Population estimate (GHSL 2020)") %>%
    filter(Year == YEAR) %>%
    select(Country, PM2.5, Pop) %>%
    mutate(Country = Country %>%
      factor())
  
  ## Processing overweight / obesity data ----
  ## Source: https://data.oecd.org/healthrisk/overweight-or-obese-population.htm
  # Reading data
  obesity_data_raw <- read_csv(here("raw_data/", "DP_LIVE_02112023125750641.csv"))
  
  # Preparing the obesity data
  obesity_data_tidy <- obesity_data_raw %>%
    filter(TIME == YEAR) %>%
    select(LOCATION, Value) %>%
    rename(Country = LOCATION) %>%
    group_by(Country) %>%
    summarise(Value = mean(Value, na.rm = TRUE)) %>%
    mutate(Country = Country %>%
           factor())
  
  ## Processing median age data
  ## Source: https://www.cia.gov/the-world-factbook/field/median-age/country-comparison/
  # Reading data
  age_data_raw <- read_csv(here("raw_data/", "cia_factbook_median_age.csv"))
  
  # Preparing the obesity data
  age_data_tidy <- age_data_raw %>%
    select(name, value) %>%
    rename(Country = name, `Median age` = value) %>%
    mutate(Country = Country %>%
             factor())
  
  # Remove unused data / values from Global Environment
  rm(list = c("air_pollution_data_raw", "obesity_data_raw", "oecd_data_raw", "age_data_raw"))
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
  
  ## Convert country tibbles to chr
  # Create empty vector
  countries <- character()
  # OECD data,
  countries <- append(countries, list(levels(countries_oecd$Country)))
  # air pollution data,
  countries <- append(countries, list(levels(countries_airpol$Country)))
  # and OECD obesity data.
  countries <- append(countries, list(levels(countries_obesity$Country)))
  
  ## Request AI completion to standardize the country names
  # Loop through OpenAI ChatGPT completion
  countries_cleaned <- vector()
  check <- character()
  country <- character()
  continent_list <- ""
  for (i in 1:3) {
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
    countries_cleaned[i]
    check <- append(check, list(strsplit(as.character(countries[i]), split = ", ")[[1]]))
    country <- append(country, list(strsplit(countries_cleaned[i], split = ", ")[[1]]))
    
    # Continent list
    if (i == 3) {
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
      country_list <- intersect(intersect(unlist(country[1]), unlist(country[2])), unlist(country[3]))
      
      # Save lists
      saveRDS(country_list, here("processed_data/presentation/", "country_list.rds"))
      saveRDS(continent_list, here("processed_data/presentation/", "continent_list.rds"))
      saveRDS(check, here("processed_data/presentation/", "check.rds"))
      saveRDS(country, here("processed_data/presentation/", "country.rds"))
    }
  }
}   else {
  message("API key is invalid, loading data from disk ...")
  
  # Load lists, if API key is missing
  country_list <- readRDS(here("processed_data/presentation/", "country_list.rds"))
  continent_list <- readRDS(here("processed_data/presentation/", "continent_list.rds"))
  check <- readRDS(here("processed_data/presentation/", "check.rds"))
  country <- readRDS(here("processed_data/presentation/", "country.rds"))
}

# Show common countries
{
  print("############################")
  print(paste0("### Common countries: ", length(country_list), " ###"))
  print("############################")
}

# Remove unused data / values from Global Environment
rm(list = c("i", "request", "countries" ,"countries_cleaned", "countries_airpol", "countries_obesity", "countries_oecd"))



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
  
  ## Create dataset `age_data_plot`
  # Filter for common countries in datasets
  age_plot <- age_data_tidy %>%
    filter(Country %in% country_list)
  
  ## Create dataset `obesity_plot`
  # Standardize country names
  obesity_plot <- obesity_data_tidy
  for (i in 1:length(unlist(country[3]))) {
    obesity_plot <- obesity_plot %>%
      mutate(Country = if_else(Country == unlist(check[3])[i], unlist(country[3])[i], Country))
  }
  # Create dataset `obesity_plot2` for table
  data_plot2 <- obesity_plot %>%
    mutate(Continent = continent_list %>%
             factor()) %>%
    rename("Obesity" = Value)
  # ... and filter for common countries in datasets
  obesity_plot <- obesity_plot %>%
    filter(Country %in% country_list)
  
  # Remove unused data / values from Global Environment
  rm(list = c("air_pollution_data_tidy", "obesity_data_tidy", "oecd_data_tidy", "age_data_tidy", "i", "check", "country", "country_list", "continent_list"))
}



### Preparation of the plotting datasets ----
{
  ## Combining the datasets
  # Joining OECD data and air pollution data
  data <- left_join(oecd_plot, air_pollution_plot, by = "Country")
  
  # Joining obesity data
  data <- left_join(data, obesity_plot, by = "Country")
  
  # Joining age data
  data <- left_join(data, age_plot, by = "Country")
  
  ## Prepare the dataset for plotting
  # Calculate the diseases per 100.000
  data_plot <- data %>%
    mutate("Diseases of the respiratory system" = `Diseases of the respiratory system` / Pop * 100000) %>%
    rename("Resp. diseases per 100.000" = "Diseases of the respiratory system") %>%
    rename("Obesity" = Value)
  
  # Remove unused data / values from Global Environment
  rm(list = c("data", "oecd_plot", "air_pollution_plot", "obesity_plot", "age_plot"))
}



### Visualisation ----
## Figure A: Point figure with a linear regression line
{
  # Calculate dependence of respiratory diseases in correlation to air pollution
  model = lm(`Resp. diseases per 100.000` ~ PM2.5, data = data_plot)
  p = round(summary(model)$coefficients[,4][2], digits = 3)
  
  # Show correlation between respiratory diseases and air pollution
  p1 <- data_plot %>%
    ggplot(aes(x = PM2.5, y = `Resp. diseases per 100.000`, color = Country)) +
    geom_point(aes(color = Country, size = `Resp. diseases per 100.000`)) +
    geom_smooth(method = "lm", se = FALSE, color = "gray") +
    geom_text(aes(label = paste0(Country, " (", `Median age`, ")"), color = `Resp. diseases per 100.000` > 2000), hjust = -0.1, vjust = 1.5) +
    labs(title = paste0("Figure A: Respiratory diseases in ", length(data_plot$Country)," european countries"),
         subtitle = paste0("in correlation to air pollution, median age in brackets (year ", YEAR, ")"),
         x = "Air pollution (particulate matter as PM2.5)", y = "Respiratory diseases (per 100.000)", 
         caption = "Data source: OECD health data & https://github.com/dw-data/edjnet-pm2p5") +
    scale_y_continuous(labels = scales::label_comma()) +
    expand_limits(x = c(1, 18), y = c(660, 2400)) +
    coord_cartesian(expand = FALSE) +
    annotate("text", x = 15, y = 1580, label = paste0("p-value = ", p)) +
    annotate("text", x = 12, y = 2200, label = "** 2 Outliers **") +
    annotate("rect", xmin = 8, xmax = 17, ymin = 2050, ymax = 2450, alpha = 0.3) +
    theme_bw() +
    theme(legend.position = "none", 
          plot.caption = element_text(face = "italic", hjust = 0),
          panel.grid.major = element_line(colour = "lightgrey", linetype = "dashed"),
          plot.title = element_text(size = 16, colour = "#2b2828", hjust = 0),
          axis.text.y = element_text(size = 10, colour = "#2b2828"),
          axis.text.x = element_text(size = 10, colour = "#2b2828", angle = 90, vjust = 0.5),
          axis.title = element_text(size = 14, hjust = 0.5, colour = "#2b2828"))
  ggsave(p1, file = here("images/presentation/", "Plot_A_Respiratory_Diseases_PM2.5.png"), width = 7.5, height = 5)
}

# Show figure
p1

# Remove unused data / values from Global Environment
rm(list = c("p1", "p", "model"))

## Figure B: Point figure with a linear regression line
{
  # Calculate dependence of respiratory diseases in correlation to overweight & obesity
  model = lm(`Resp. diseases per 100.000` ~ Obesity, data = data_plot)
  p = round(summary(model)$coefficients[,4][2], digits = 3)
  # Correlation between respiratory diseases and overweight & obesity
  p2 <- data_plot %>%
    ggplot(aes(x = Obesity, y = `Resp. diseases per 100.000`, color = Country)) +
    geom_point(aes(color = Country, size = `Resp. diseases per 100.000`)) +
    geom_text(aes(label = paste0(Country, " (", `Median age`, ")"), color = `Resp. diseases per 100.000` > 2000), hjust = -0.1, vjust = 1.5) +
    geom_smooth(method = "lm", se = FALSE, color = "gray") +
    labs(title = paste0("Figure B: Respiratory diseases in ", length(data_plot$Country), " european countries"),
         subtitle = paste0("in correlation to overweight & obesity, median age in brackets (year ", YEAR, ")"),
         x = "Overweight & obesity (in percentage)", y = "Respiratory diseases (per 100.000)", 
         caption = "Data source: OECD health data") +
    scale_x_continuous(labels = function(x) paste0(x, '%')) +
    scale_y_continuous(labels = scales::label_comma()) +
    expand_limits(x = c(45, 72), y = c(660, 2400)) +
    coord_cartesian(expand = FALSE) +
    annotate("text", x = 67, y = 1750, label = paste0("p-value = ", p)) +
    annotate("text", x = 63, y = 2220, label = "** 2 Outliers! **") +
    annotate("rect", xmin = 54, xmax = 72, ymin = 2050, ymax = 2400, alpha = 0.3) +
    theme_bw() +
    theme(legend.position = "none", 
          plot.caption = element_text(face = "italic",hjust = 0),
          panel.grid.major = element_line(colour = "lightgrey", linetype = "dashed"),
          plot.title = element_text(size = 16, colour = "#2b2828", hjust = 0),
          axis.text.y = element_text(size = 10, colour = "#2b2828"),
          axis.text.x = element_text(size = 10, colour = "#2b2828", angle = 90, vjust = 0.5),
          axis.title = element_text(size = 14, colour = "#2b2828"))
  ggsave(p2, file = here("images/presentation/", "Plot_B_Respiratory_Diseases_Obesity.png"), width = 7, height = 5)
}

# Show figure
p2

# Remove unused data / values from Global Environment
rm(list= c("p2", "data_plot", "p", "model"))


## Figure C1: Bar plot, horizontal, grouped by continents
{
  # Show obesity in worldwide, data mostly from European countries
  p3a <- data_plot2 %>%
    # order by obesity
    ggplot(aes(reorder(Country, Obesity), y = Obesity, fill = Obesity)) +
    geom_bar(stat = "identity", color = "white") +
    labs(title = "Figure C1: Overweight & obesity",
         subtitle = paste0("for ", length(data_plot2$Country), " countries, grouped by continents (year ", YEAR, ")"),
         x = "", y = "Overweight & obesity (in percentage)",  
         caption = "Data source: OECD health data") +
    scale_fill_gradient(low = "lightgreen", high = "#9f3b3d") +
    scale_y_continuous(labels = function(x) paste0(x, '%')) +
    coord_flip() +
    facet_wrap(~Continent) +
    theme_bw() +
    theme(legend.position = "none", 
          plot.caption = element_text(face = "italic", hjust = 0),
          plot.background = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), panel.border = element_blank(),
          plot.title = element_text(size = 16, colour = "#2b2828", hjust = 0),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          axis.title = element_text(size = 14, hjust = 0.5, colour = "#2b2828"))
  ggsave(p3a, file = here("images/presentation/", "Plot_C1_Obesity_Country_Continental.png"), width = 7, height = 5)
  }

# Show figure
p3a

# Remove unused data / values from Global Environment
rm(list = c("p3a", "data_plot2"))

## More data from countries on other continents needed!
# Source: https://ourworldindata.org/obesity
{
  obesity_world_data_raw <- read_csv(here("raw_data/", "share-of-deaths-obesity.csv"))
  obesity_world_data_tidy <- obesity_world_data_raw %>%
    filter(Year == YEAR) %>%
    # Drop NA rows
    drop_na(Code) %>%
    # Drop `World`
    filter(str_length(Code) <= 3) %>%
    rename("Country" = Code, "Obesity" = starts_with("Share")) %>%
    select(Country, Obesity)
}

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
  saveRDS(continents_world, here("processed_data/presentation/", "continents.rds"))
  
  # Convert country codes to country names
  countries_world <- convert_codes("Country", data = obesity_world_data_tidy$Country)
  saveRDS(countries_world, here("processed_data/presentation/", "countries.rds"))
} else {
  # Load `processed_data/continents.rds`, if API key is missing (ChatGPT requests are charged!)
  continents_world <- readRDS(here("processed_data/presentation/", "continents.rds"))
  
  # Load `processed_data/countries.rds`, if API key is missing  (ChatGPT requests are charged!)
  countries_world <- readRDS(here("processed_data/presentation/", "countries.rds"))
}

## Creating dataset for plotting
{
  obesity_world_plot <- obesity_world_data_tidy %>%
    mutate(Country = countries_world %>%
      factor()) %>%
    mutate(Continent = continents_world %>%
      factor()) %>%
    filter(Country != "Unknown") %>%
    filter(Continent != "Unknown")
  
  # Remove unused data / values from Global Environment
  rm(list = c("obesity_world_data_raw", "obesity_world_data_tidy", "continents_world", "countries_world", "OPENAI_API_KEY", "convert_codes"))
}

## Figure C2: Point figure, grouped by continents
{
  # Show deaths attributed to obesity worldwide
  p3b <- obesity_world_plot %>%
    ggplot(aes(x = Obesity, y = Country, colour = Obesity)) +
    geom_point(aes(color = Obesity)) +
    scale_color_continuous(trans = 'reverse') +
    scale_x_continuous(labels = function(x) paste0(x, '%')) +
    guides(color = guide_colorbar(reverse = TRUE), size = guide_legend(reverse = TRUE)) +
    labs(title = "Figure C2: Percentage of deaths attributed to obesity",
         subtitle = paste0("for ", length(obesity_world_plot$Country), " countries grouped by continents (year ", YEAR, ")"),
         x = "", y = "",  
         caption = "Data source: https://ourworldindata.org/obesity") +
    facet_wrap(~Continent) +
    theme_bw() +
    theme(plot.caption = element_text(face = "italic", hjust = 0),
          plot.background = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), panel.border = element_blank(),
          plot.title = element_text(size = 16, colour = "#2b2828", hjust = 0),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.title = element_text(size = 14, hjust = 0.5, colour = "#2b2828"))
  ggsave(p3b, file = here("images/presentation/", "Plot_C2_Deaths_Obesity_Country_Continent.png"), width = 7, height = 5)
}

# Show figure
p3b

# Remove unused data / values from Global Environment
rm("p3b", "obesity_world_plot", "YEAR")
