library(dplyr)
library(tidyverse)
library(glmnet)
## Load data
data_2016 <- read.csv("2016_brooklyn.csv")
colnames(data_2016) <- data_2016[4, ]
data_2016 <- data_2016[-c(1:4), ]
colnames(data_2016) <- gsub("\n", "", colnames(data_2016))

data_2017 <- read.csv("2017_brooklyn.csv")
colnames(data_2017) <- data_2017[4, ]
data_2017 <- data_2017[-c(1:4), ]
colnames(data_2017) <- gsub("\n", "", colnames(data_2017))
#colnames(data_2017)[colnames(data_2017) == "BUILDING CLASS AS OF FINAL ROLL 17/18"] <- "BUILDING CLASS AT PRESENT"
#colnames(data_2017)[colnames(data_2017) == "TAX CLASS AS OF FINAL ROLL 17/18"] <- "TAX CLASS AT PRESENT"


data_2018 <- read.csv("2018_brooklyn.csv")
colnames(data_2018) <- data_2018[4, ]
data_2018 <- data_2018[-c(1:4), ]
colnames(data_2018) <- gsub("\n", "", colnames(data_2018))
#colnames(data_2018)[colnames(data_2018) == "BUILDING CLASS AS OF FINAL ROLL 18/19"] <- "BUILDING CLASS AT PRESENT"
#colnames(data_2018)[colnames(data_2018) == "TAX CLASS AS OF FINAL ROLL 18/19"] <- "TAX CLASS AT PRESENT"

data_2019 <- read.csv("2019_brooklyn.csv")
colnames(data_2019) <- data_2019[4, ]
data_2019 <- data_2019[-c(1:4), ]
colnames(data_2019) <- gsub("\n", "", colnames(data_2019))

data_2020 <- read.csv("2020_brooklyn.csv")
colnames(data_2020) <- data_2020[6, ]
data_2020 <- data_2020[-c(1:7), ]
colnames(data_2020) <- gsub("\n", "", colnames(data_2020))
colnames(data_2020)[colnames(data_2020) == "BUILDING CLASSAT TIME OF SALE"] <- "BUILDING CLASS AT TIME OF SALE"
colnames(data_2020)[colnames(data_2020) == "COMMERCIALUNITS"] <- "COMMERCIAL UNITS"
colnames(data_2020)[colnames(data_2020) == "RESIDENTIALUNITS"] <- "RESIDENTIAL UNITS"

combined <- bind_rows(data_2016, data_2017, data_2018, data_2019, data_2020)

## Clean data
combined <- combined %>%
  mutate(`COMMERCIAL UNITS` = na_if(trimws(`COMMERCIAL UNITS`), "-"))

combined[] <- lapply(combined, function(x) if(is.character(x)) trimws(x) else x)
combined$`TOTAL UNITS` <- gsub("[^0-9.]", "", combined$`TOTAL UNITS`)
combined$`TOTAL UNITS` <- as.numeric(combined$`TOTAL UNITS`)
combined$`RESIDENTIAL UNITS` <- gsub("[^0-9.]", "", combined$`RESIDENTIAL UNITS`)
combined$`RESIDENTIAL UNITS` <- as.numeric(combined$`RESIDENTIAL UNITS`)
combined$`GROSS SQUARE FEET` <- gsub("[^0-9.]", "", combined$`GROSS SQUARE FEET`)
combined$`GROSS SQUARE FEET` <- as.numeric(gsub(",", "", combined$`GROSS SQUARE FEET`))
combined$`SALE PRICE` <- gsub(",", "", combined$`SALE PRICE`)  # Remove commas if needed
combined$`SALE PRICE` <- gsub("[^0-9.]", "", combined$`SALE PRICE`)
combined$`SALE PRICE` <- as.numeric(gsub(",", "", combined$`SALE PRICE`))

combined <- combined %>%
  filter(
    substr(`BUILDING CLASS AT TIME OF SALE`, 1, 1) %in% c("A", "R"),
    `TOTAL UNITS` == 1,
    `RESIDENTIAL UNITS` == 1,
    `GROSS SQUARE FEET` > 0,
    !is.na(`SALE PRICE`)
  )

  combined$`BOROUGH` <- as.factor(combined$`BOROUGH`)
  combined$`NEIGHBORHOOD` <- as.factor(combined$`NEIGHBORHOOD`)
  combined$`BUILDING CLASS CATEGORY` <- as.factor(combined$`BUILDING CLASS CATEGORY`)
  combined$`TAX CLASS AT PRESENT` <- as.factor(combined$`TAX CLASS AT PRESENT`)
  combined$`BLOCK` <- as.factor(combined$`BLOCK`)
  combined$`BUILDING CLASS AT PRESENT` <- as.factor(combined$`BUILDING CLASS AT PRESENT`)
  combined$`ZIP CODE` <- as.factor(combined$`ZIP CODE`)
  combined$`LOT` <- as.numeric(combined$`LOT`)
  combined$`COMMERCIAL UNITS` <- as.factor(combined$`COMMERCIAL UNITS`)
  combined$`LAND SQUARE FEET` <- gsub(",", "", combined$`LAND SQUARE FEET`)  # Remove commas if needed
  combined$`LAND SQUARE FEET` <- gsub("[^0-9.]", "", combined$`LAND SQUARE FEET`)
  combined$`LAND SQUARE FEET` <- as.numeric(gsub(",", "", combined$`LAND SQUARE FEET`))
  combined$`YEAR BUILT` <- gsub("[^0-9.]", "", combined$`YEAR BUILT`)
  combined$`YEAR BUILT` <- as.numeric(gsub(",", "", combined$`YEAR BUILT`))
  combined$`TAX CLASS AT TIME OF SALE` <- as.factor(combined$`TAX CLASS AT TIME OF SALE`)
  combined$`BUILDING CLASS AT TIME OF SALE` <- as.factor(combined$`BUILDING CLASS AT TIME OF SALE`)
  combined$`SALE DATE` <- as.Date(combined$`SALE DATE`, format = "%m/%d/%y")

  ggplot(combined, aes(x = `SALE PRICE`)) +
    geom_histogram(binwidth = 200000, 
                   fill = "skyblue", 
                   color = "black", 
                   alpha = 0.7) +
    labs(title = "Histogram of Sale Prices", 
         x = "Sale Price", 
         y = "Frequency") +
    scale_x_continuous(labels = label_dollar()) +  # Apply dollar formatting
    theme_minimal()
  
combined <- combined %>% 
    filter(`SALE PRICE` > 100000 & `SALE PRICE` < 7000000)

combined <- combined %>%
  group_by('NEIGHBORHOOD') %>%  # Group by the category column
  mutate(
    median_year = median(`YEAR BUILT`[`YEAR BUILT` > 0], na.rm = TRUE),  # Calculate median excluding 0s
    `YEAR BUILT` = ifelse(`YEAR BUILT` == 0, median_year, `YEAR BUILT`)  # Replace 0s with median
  ) %>%
  select(-median_year)

med_year <- combined %>%
  group_by('NEIGHBORHOOD') %>%
  summarise(median_year = median(`YEAR BUILT`))

#median_year <- median(combined$`YEAR BUILT`[combined$`YEAR BUILT` > 0], na.rm = TRUE)
combined$`YEAR BUILT`[combined$`YEAR BUILT` == 0] <- as.numeric(med_year)

combined <- combined %>%
  mutate(
    log_gross_square_feet = log(`GROSS SQUARE FEET` + 1),
    log_land_square_feet = log(`LAND SQUARE FEET` + 1),
    gross_square_feet_squared = `GROSS SQUARE FEET`^2,
    land_gross_square_interaction = `LAND SQUARE FEET` * `GROSS SQUARE FEET`)

combined <- combined %>%
  mutate(
    gross_square_feet_cubed = `GROSS SQUARE FEET`**3,
    land_square_feet_squared = `LAND SQUARE FEET`**2
  )

combined <- combined %>%
  mutate(
    sale_year = as.numeric(format(as.Date(`SALE DATE`, format = "%m/%d/%y"), "%Y")),
    sale_month = as.numeric(format(as.Date(`SALE DATE`, format = "%m/%d/%y"), "%m"))
  )
combined$zipcode_prefix <- as.factor(substr(combined$`ZIP CODE`, 2, 4))
combined$block_prefix <- as.factor(substr(combined$`BLOCK`, 1, 1))

combined$age_of_property <- combined$sale_year - combined$`YEAR BUILT`
combined$gross_square_feet_five <- combined$`GROSS SQUARE FEET`^5
combined$diff <- combined$`GROSS SQUARE FEET` - combined$`LAND SQUARE FEET`

combined <- combined %>%
  mutate(nbhood_cluster = case_when(
    NEIGHBORHOOD %in% c('GREENPOINT', 'WILLIAMSBURG-SOUTH', 'WILLIAMSBURG-CENTRAL', 'WILLIAMSBURG-EAST', 'WILLIAMSBURG-NORTH') ~ "Cluster_1", 
    NEIGHBORHOOD %in% c('BROOKLYN HEIGHTS', 'DOWNTOWN-FULTON MALL', 'DOWNTOWN-FULTON FERRY', 'DOWNTOWN-METROTECH', 'CLINTON HILL', 'NAVY YARD', 'FORT GREENE', 'BOERUM HILL') ~ "Cluster_2",  
    NEIGHBORHOOD %in% c('BEDFORD STUYVESANT') ~ "Cluster_3",  # Group neighborhoods G, H, I into Cluster_3
    NEIGHBORHOOD %in% c('BUSHWICK') ~ "Cluster_4", 
    NEIGHBORHOOD %in% c('CYPRESS HILLS', 'EAST NEW YORK', 'SPRING CREEK', 'WYCKOFF HEIGHTS') ~ "Cluster_5", 
    NEIGHBORHOOD %in% c('COBBLE HILL', 'CARROLL GARDENS', 'RED HOOK', 'GOWANUS', 'PARK SLOPE', 'PARK SLOPE SOUTH', 'COBBLE HILL-WEST') ~ "Cluster_6",  
    NEIGHBORHOOD %in% c('BUSH TERMINAL', 'SUNSET PARK', 'WINDSOR TERRACE') ~ "Cluster_7", 
    NEIGHBORHOOD %in% c('PROSPECT HEIGHTS', 'CROWN HEIGHTS') ~ "Cluster_8", 
    NEIGHBORHOOD %in% c('BAY RIDGE', 'DYKER HEIGHTS') ~ "Cluster_10", 
    NEIGHBORHOOD %in% c('BENSONHURST', 'BATH BEACH') ~ "Cluster_11", 
    NEIGHBORHOOD %in% c('KENSINGTON', 'BOROUGH PARK', 'OCEAN PARKWAY-NORTH', 'OCEAN PARKWAY-SOUTH') ~ "Cluster_12", 
    NEIGHBORHOOD %in% c('SEAGATE', 'CONEY ISLAND', 'BRIGHTON BEACH') ~ "Cluster_13", 
    NEIGHBORHOOD %in% c("FLATBUSH-CENTRAL", "FLATBUSH-LEFFERTS GARDEN", "FLATBUSH-NORTH", "MIDWOOD") ~ "Cluster_14", 
    NEIGHBORHOOD %in% c('GRAVESEND', 'SHEEPSHEAD BAY', 'MADISON', 'MANHATTAN BEACH', 'GERRITSEN BEACH') ~ "Cluster_15", 
    NEIGHBORHOOD %in% c('OCEAN HILL', 'BROWNSVILLE') ~ "Cluster_16", 
    NEIGHBORHOOD %in% c('FLATBUSH-EAST') ~ "Cluster_17", 
    NEIGHBORHOOD %in% c('FLATLANDS', 'CANARSIE', 'MILL BASIN', 'MARINE PARK', 'BERGEN BEACH', 'OLD MILL BASIN') ~ "Cluster_18", 
    TRUE ~ "Other"  # Default case if there are any neighborhoods not listed
  ))

combined$nbhood_cluster <- as.factor(combined$nbhood_cluster)

lm4_model <- lm(log(`SALE PRICE` ~ 
                  log(`GROSS SQUARE FEET`) +
                  `LOG AGE AT SALE` +
                  `NEIGHBORHOOD CLUSTER` +
                  `OVERALL BUILDING CODE` +
                  `TAX CLASS AT TIME OF SALE` +
                  `SALE QUARTER OF YEAR` + 
                  `SALE YEAR`,
                data = housing_data))

model <- lm(`SALE PRICE` ~ `LAND SQUARE FEET` + 
              log_gross_square_feet * log_land_square_feet + 
              `YEAR BUILT` + I(`YEAR BUILT`^2) + `BUILDING CLASS CATEGORY` + 
              gross_square_feet_squared + land_square_feet_squared + 
              gross_square_feet_cubed + land_gross_square_interaction +
              sale_year + sale_month  + 
              zipcode_prefix + `YEAR BUILT` * `GROSS SQUARE FEET` +
              `LOT` * zipcode_prefix + `LOT` * log_gross_square_feet 
           + block_prefix + I(gross_square_feet_squared**2) + I(land_square_feet_squared**2) +
             gross_square_feet_five,
            data = combined)

combined$property_age <- 2024 - combined$`YEAR BUILT`
combined <- combined %>%
    group_by(nbhood_cluster, sale_month) %>%
  mutate(avg_sale_price_nbhood = mean(`SALE PRICE`, na.rm = TRUE)) %>%
  ungroup()

combined <- combined %>%
  group_by(`ZIP CODE`) %>%
  mutate(avg_sale_price_zip = mean(`SALE PRICE`, na.rm = TRUE)) %>%
  ungroup()

combined <- combined %>%
  group_by(`BUILDING CLASS AT TIME OF SALE`) %>%
  mutate(avg_sale_price_class = mean(`SALE PRICE`, na.rm = TRUE)) %>%
  ungroup()

combined <- combined %>%
  group_by(age_factor) %>%
  mutate(avg_sale_price_age = mean(`SALE PRICE`, na.rm = TRUE)) %>%
  ungroup()


age_breakpoints <- c(0,1,1930,1978,2000, Inf)
age_labels <- c("Unknown", "Old","Lead-Paint","Used", "New")
combined$age_factor <- cut(combined$`YEAR BUILT`, breaks = age_breakpoints, age_labels = labels, right = FALSE)

model2 <- lm(sqrt(`SALE PRICE`) ~ poly(`GROSS SQUARE FEET`, 2) + 
               log_gross_square_feet:log_land_square_feet + `BUILDING CLASS AT TIME OF SALE`:`GROSS SQUARE FEET` +
               `YEAR BUILT` + land_gross_square_interaction +
              `YEAR BUILT`:`LAND SQUARE FEET`,
             data = combined)


predictions <- predict(model2, newdata = combined)
back_transformed_predictions <- predictions^2
residuals <- combined$`SALE PRICE` - back_transformed_predictions
squared_residuals <- residuals^2
mean_squared_residuals <- mean(squared_residuals, na.rm = TRUE)
rmse <- sqrt(mean_squared_residuals)





