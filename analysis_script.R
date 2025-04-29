library(tidytext)
library(tidyverse)
library(wordcloud2)
library(tm)
library(topicmodels)
library(knitr)
library(httr)
library(jsonlite)
library(tidyr)
library(dplyr)
library(leaflet)
library(readr)
library(kableExtra)
library(sf)
library(opendatatoronto)
library(plotly)
library(randomForest)
library(mgcv)


#sources:
# https://open.toronto.ca/dataset/traffic-volumes-midblock-vehicle-speed-volume-and-classification-counts/ 
# https://open.toronto.ca/dataset/neighbourhood-profiles/ 
# https://open.toronto.ca/dataset/bicycle-thefts/ 
# https://open.toronto.ca/dataset/bicycle-shops/ 
# additional data (potential):
# https://open.toronto.ca/dataset/neighbourhoods/ boundaries to overlap 
# https://open.toronto.ca/dataset/cycling-network/ 


base_url <- "https://ckan0.cf.opendata.inter.prod-toronto.ca"
package_url <- paste0(base_url, "/api/3/action/package_show")

fetch_data2 <- function(dataset_id) {
  response <- GET(package_url, query = list(id = dataset_id))
  metadata <- fromJSON(content(response, "text", encoding = "UTF-8"))
  resources <- metadata$result$resources
  datastore_resource <- resources %>%
    filter(datastore_active == TRUE) %>%
    select(id) %>%
    slice(1) %>%
    pull()
  
  csv_url <- paste0(base_url, "/datastore/dump/", datastore_resource)
  
  read_csv(csv_url)
}
fetch_data <- function(dataset_id) {
  response <- GET(package_url, query = list(id = dataset_id))
  metadata <- fromJSON(content(response, "text", encoding = "UTF-8"))
  resources <- metadata$result$resources
  csv_resource <- resources[resources$format == "CSV" & resources$url_type == "datastore", ]
  csv_url <- csv_resource$url[1]
  read_csv(csv_url)
}


neighbourhood_data <- fetch_data("neighbourhood-profiles")
traffic_data <- fetch_data("traffic-volumes-midblock-vehicle-speed-volume-and-classification-counts")
thefts_data <- fetch_data2("bicycle-thefts")
shops_data <- fetch_data2("bicycle-shops")

package <- show_package("neighbourhoods")
resources <- list_package_resources("neighbourhoods")
boundaries_data = get_resource(resources$id[1])

library(opendatatoronto)
resources <- list_package_resources("945444df-c9cf-4a9d-97a8-ee8b0feeb419")

data <- get_resource(resources$id[1])

# cannot do text wrangling - feedback is multiple 



## #check for sus values
## head(neighbourhood_data)
## head(traffic_data)
## head(thefts_data)
## head(shops_data)
## 
## 
## summary(neighbourhood_data)
## summary(thefts_data)
## 
## # some bikes cost too little
## # first date is 1975?
## 
## summary(shops_data)
## 
## unique(neighbourhood_data$Topic)
## colnames(neighbourhood_data)
## unique(neighbourhood_data$Topic)

# clean data
thefts <- thefts_data %>%
  drop_na() %>%
  rename(
    offense = PRIMARY_OFFENCE,
    date = OCC_DATE,
    year = OCC_YEAR,
    make = BIKE_MAKE,
    model = BIKE_MODEL,
    type = BIKE_TYPE,
    speed = BIKE_SPEED,
    color = BIKE_COLOUR,
    cost = BIKE_COST,
    longitude = LONG_WGS84,
    latitude = LAT_WGS84
  ) %>%
  filter(cost >= 50,
         year > 2013) %>%  
  mutate(entity = "Theft")


extract_coordinates <- function(geometry) {
  coords <- fromJSON(geometry)$coordinates
  return(data.frame(longitude = coords[1], latitude = coords[2]))
}

shops <- shops_data %>%
  rename(
    shop_id = `_id`,
    shop_name = NAME,
    address = ADDRESS_FULL,
    postal_code = POSTAL_CODE,
    city = CITY
  ) %>%
  mutate(entity = "Shop") %>% 
  mutate(coords = map(geometry, ~ fromJSON(.x)$coordinates)) %>% 
  mutate(
    longitude = map_dbl(coords, 1),  
    latitude = map_dbl(coords, 2)   
  ) %>%
  select(-geometry, -coords)

#convert to spatial dataframe  


thefts_sf <- st_as_sf(thefts, coords = c("longitude", "latitude"), crs = 4326)
shops_sf <- st_as_sf(shops, coords = c("longitude", "latitude"), crs = 4326)




# Clean and transpose neighborhood data
neighborhood_clean <- neighbourhood_data %>%
  select(-c(1:4)) %>%
  pivot_longer(cols = -Characteristic, names_to = "Neighborhood", values_to = "Value") %>%
  pivot_wider(names_from = "Characteristic", values_from = Value) %>%
  mutate(across(
    .cols = -c("Neighborhood", "TSNS2020 Designation"),
    .fns = ~ readr::parse_number(as.character(.x))
  )) %>%
  rename("Population Change % 2011-2016" = "Population Change 2011-2016",
         "Neighborhood Number" = "Neighbourhood Number",
         "Pop2016" = "Population, 2016")
  
# Get neighborhood boundaries and join with profile data
neighborhoods_sf <- st_as_sf(boundaries_data) %>%
  st_transform(4326)

neighborhood_full <- neighborhoods_sf %>%
  mutate(across(
    .cols = c("AREA_LONG_CODE"),
    .fns = ~ readr::parse_number(as.character(.x))
  )) %>%
  left_join(neighborhood_clean, by = c("AREA_LONG_CODE" = "Neighborhood Number")) %>%
  select(-c(AREA_ID, AREA_ATTR_ID, PARENT_AREA_ID, AREA_SHORT_CODE, 
            AREA_NAME, AREA_DESC, CLASSIFICATION, 
            OBJECTID, "TSNS2020 Designation")) %>%
  filter(AREA_LONG_CODE <= 140) %>%
  janitor::clean_names()


## Summary statistics in tables


num_neighbourhoods <- ncol(neighbourhood_data) - 4
overall_avg_population <- neighbourhood_data %>%
  filter(Category == "Population",
         Topic == "Population and dwellings",
         Characteristic == "Population, 2016") %>%
  select(-c(1:6)) %>%  
    mutate(across(everything(), ~ as.numeric(gsub("[^0-9]", "", .)))) %>%
  unlist() %>%
  mean(na.rm = TRUE)

neighbourhood_summary <- tibble(
  `Number of Neighbourhoods` = num_neighbourhoods,
  `Overall Avg Population` = round(overall_avg_population, 0)
)


thefts_summary <- thefts_sf %>%
  summarise(
    Total_Thefts = n(),
    Avg_Bike_Cost = mean(cost, na.rm = TRUE),
    Min_Bike_Cost = min(cost, na.rm = TRUE),
    Max_Bike_Cost = max(cost, na.rm = TRUE),
    Median_Bike_Cost = median(cost, na.rm = TRUE),
    Variance_Bike_Cost = var(cost, na.rm = TRUE),
    Q1_Bike_Cost = quantile(cost, 0.25, na.rm = TRUE),
    Q3_Bike_Cost = quantile(cost, 0.75, na.rm = TRUE)
  )

shops_summary <- shops_sf %>%
  summarise(
    Total_Shops = n(),
    Unique_Postal_Codes = n_distinct(postal_code),
    Most_Common_Postal_Code = names(sort(table(postal_code), decreasing = TRUE)[1])
  )



# Thefts Data Summary
kable(thefts_summary, caption = "Table 1: Summary of Bicycle Thefts") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive"), full_width = FALSE) %>%
  footnote(general = "This table summarizes the cost distribution of stolen bikes.")

# Shops Data Summary
kable(shops_summary, caption = "Table 2: Summary of Bicycle Shops in Toronto") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive"), full_width = FALSE) %>%
  footnote(general = "This table displays the number of bike shops and unique postal codes covered.")

# Neighbourhood Summary
kable(neighbourhood_summary, caption = "Table 3: Neighbourhood Statistics (Count & Average Population)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive"), full_width = FALSE) %>%
  footnote(general = "This table summarizes the number of neighbourhoods and their average population.")



## ## Basic maps
## 
## 
## leaflet(thefts_sf) %>%
##   addTiles() %>%
##   addCircleMarkers(
##     radius = 4,
##     color = "red",
##     fillOpacity = 0.7,
##     popup = ~paste("Bike Model:", model, "<br>Bike Cost: $", cost)
##   ) %>%
##   addLegend(position = "bottomright", colors = "red", labels = "Bike Thefts", title = "Bike Thefts Data")
## 
## 
## 
## leaflet(shops_sf) %>%
##   addTiles() %>%
##   addCircleMarkers(
##     radius = 4,
##     color = "green",
##     fillOpacity = 0.7,
##     popup = ~paste("Shop Name:", shop_name, "<br>Address:", address)
##   ) %>%
##   addLegend(position = "bottomright", colors = "green", labels = "Bike Shops", title = "Bike Shops Data")
## 
## 


# Bike Theft Costs Histogram (Interactive)
thefts_sf_log <- thefts_sf %>%
  mutate(log_cost = log1p(cost))  # log(1 + cost) to avoid log(0) issues

p2 <- ggplot(thefts_sf_log, aes(x = log_cost)) +
  geom_histogram(bins = 30, fill = "red", color = "black") +
  theme_minimal() +
  labs(
    title = "Distribution of Bike Theft Costs (Log Scale)",
    x = "Log(Bike Cost + 1)",
    y = "Frequency"
  )

ggplotly(p2, tooltip = "text") %>%
  layout(hoverlabel = list(bgcolor = "white"))


# Bike Thefts Over Years Bar Plot (Interactive)
p3 <- ggplot(thefts_sf, aes(x = year)) +
  geom_bar(fill = "red", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Bike Thefts Over the Years",
    x = "Year",
    y = "Number of Thefts"
  )

ggplotly(p3, tooltip = "count") %>% 
  layout(hoverlabel = list(bgcolor = "white"))



# Merge datasets using spatial join (nearest feature)

thefts_clean <- thefts_sf %>%
  mutate(entity = "Theft")

shops_clean <- shops_sf %>%
  mutate(entity = "Shop")

##### full join
merged_sf <- bind_rows(thefts_clean, shops_clean)




leaflet(merged_sf) %>%
  addTiles() %>%
  addCircleMarkers(
    radius = 4,
    color = ~ case_when(
      entity == "Theft" ~ "red",
      entity == "Shop" ~ "green"
    ),
    popup = ~entity
  ) %>%
  addLegend(position = "bottomright",
            colors = c("red", "green"),
            labels = c("Theft", "Shop"),
            title = "Entity Type")



thefts_with_neighborhood <- thefts_sf %>%
  st_join(neighborhood_full, join = st_intersects, left = FALSE) %>%
  janitor::clean_names()
  

# Calculate theft density per neighborhood
theft_density <- thefts_with_neighborhood %>%
  st_drop_geometry() %>%
  count(neighborhood, name = "theft_count") %>%
  left_join(
    neighborhood_full,
    by = "neighborhood"
  ) %>%
  mutate(
    theft_rate = theft_count / pop2016,
    theft_rate_per_1000 = theft_rate * 1000  
  ) 

top_20_theft <- theft_density %>%
  arrange(desc(theft_rate)) %>% 
  slice(1:20)                   

p_bar <- ggplot(top_20_theft, aes(x = reorder(neighborhood, theft_rate_per_1000), y = theft_rate_per_1000)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Neighborhoods by Bike Theft Rate",
       x = "Neighborhood",
       y = "Thefts per 1000 people") +
  theme_minimal()

ggplotly(p_bar)

bottom_20_theft <- theft_density %>%
  arrange(theft_rate) %>%  
  slice(1:20)           


ggplot(bottom_20_theft, aes(x = reorder(neighborhood, -theft_rate_per_1000), y = theft_rate_per_1000)) +
  geom_col(fill = "darkgreen") + 
  coord_flip() +
  labs(title = "20 Safest Neighborhoods for Bike Theft",
       x = "Neighborhood",
       y = "Thefts per 1000 people") +
  theme_minimal()

## also do shops/neighborhood and create a map



shops_with_neighborhood <- shops_sf %>%
  st_join(neighborhood_full, join = st_intersects, left = FALSE) %>%
  janitor::clean_names()

# Calculate number of shops per neighborhood
shop_density <- shops_with_neighborhood %>%
  st_drop_geometry() %>%
  count(neighborhood, name = "shop_count") %>%
  left_join(
    neighborhood_full,
    by = "neighborhood"
  ) %>%
  mutate(
    shops_per_1000 = shop_count / pop2016 * 1000
  )


# Merge shop density into spatial neighborhood data
neighborhood_with_shops <- neighborhood_full %>%
  left_join(shop_density %>% select(neighborhood, shops_per_1000), by = "neighborhood")




# Plot shops per neighborhood per 1000ppl
ggplot(neighborhood_with_shops) +
  geom_sf(aes(fill = shops_per_1000)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
  labs(title = "Bike Shops per 1000 Residents by Neighborhood",
       fill = "Shops per 1000") +
  theme_minimal()

# Plot thefts per neighborhood per 1000ppl
neighborhood_with_thefts <- neighborhood_full %>%
  left_join(theft_density %>% select(neighborhood, theft_rate_per_1000), by = "neighborhood")


p1 <- ggplot(neighborhood_with_thefts) +
  geom_sf(aes(fill = theft_rate_per_1000)) +
  scale_fill_viridis_c(option = "magma", na.value = "grey80") +
  labs(title = "Bike Thefts per 1000 Residents by Neighborhood",
       fill = "Thefts per 1000") +
  theme_minimal()

ggplotly(p1)

## plot joined data
combined_data <- neighborhood_full %>%
  left_join(theft_density %>% select(neighborhood, theft_rate_per_1000), by = "neighborhood") %>%
  left_join(shop_density %>% select(neighborhood, shops_per_1000), by = "neighborhood") %>%
  mutate(pop2016 = as.numeric(pop2016))

#bike shops vs. thefts, size = population
p <- ggplot(combined_data, aes(
    x = shops_per_1000,
    y = theft_rate_per_1000,
    size = pop2016,
    text = paste0(
      "<b>Neighborhood:</b> ", neighborhood, "<br>",
      "<b>Bike Shops per 1000:</b> ", round(shops_per_1000, 2), "<br>",
      "<b>Thefts per 1000:</b> ", round(theft_rate_per_1000, 2), "<br>",
      "<b>Population:</b> ", pop2016
    )
  )) +
  geom_point(alpha = 0.7, color = "steelblue") +
  scale_size_continuous(range = c(1, 10), name = "Population (2016)") +
  labs(
    title = "Relationship Between Bike Shops, Bike Thefts, and Population by Neighborhood",
    x = "Bike Shops per 1000 Residents",
    y = "Bike Thefts per 1000 Residents"
  ) +
  theme_minimal()

# Turn it into an interactive plotly plot
ggplotly(p, tooltip = "text") %>%
  layout(
    hoverlabel = list(bgcolor = "white"),
    title = list(
      text = "<b>Relationship Between Bike Shops, Bike Thefts, and Population by Neighborhood</b>"
    )
  )



#find out what features best predict bike theft by neighbourhood

#Correlation analysis for numeric variables
numeric_theft_density <- theft_density %>%
  st_drop_geometry() %>%
  select(where(is.numeric))%>%
  mutate(across(everything(), ~ {
    x <- as.numeric(.)
    ifelse(is.infinite(x) | is.na(x), mean(x, na.rm = TRUE), x)
  })) %>%
  select_if(~ !any(is.na(.)))

cor_matrix <- as.matrix(cor(numeric_theft_density))
target_cor <- cor_matrix[,"theft_rate"]
strong_vars <- names(which(abs(target_cor) > 0.6))

heatmap(cor_matrix[strong_vars, strong_vars], Colv = NA, Rowv = NA)


#select additional potential variables
demo_features <- c("pop2016", "population_density_per_square_kilometre",
                   "youth_15_24_years", "working_age_25_54_years","average_household_size",
                   "persons_living_alone_per_cent")
econ_features <- c("median_total_income_in_2015_among_recipients",
                   "in_low_income_based_on_the_low_income_measure_after_tax_lim_at","employment_rate")
housing_features <- c("apartment_in_a_building_that_has_five_or_more_storeys","row_house", "condominium",
                      "average_weeks_worked_in_reference_year")
transport_features <- c("bicycle", "public_transit", "no_fixed_workplace_address",
                        "commute_within_census_subdivision_csd_of_residence")
time_features <- c("worked_full_year_full_time", "between_5_a_m_and_5_59_a_m", "between_12_p_m_and_4_59_a_m")

selected_data <- theft_density %>%
  select(
    neighborhood, all_of(demo_features), all_of(econ_features),
    all_of(housing_features), all_of(transport_features),
    all_of(time_features),
    all_of(strong_vars)
  ) %>%
  # Convert percentages to numeric
  mutate(across(ends_with("_percent"), ~ as.numeric(gsub("%", "", .))/100))



set.seed(5858)

# add binary classification (high vs low theft rate)
selected_data <- selected_data %>%
  mutate(high_theft = as.numeric(theft_rate > median(theft_rate))) %>%
  select(-theft_rate) %>%
  st_drop_geometry()

#split
train <- sample(1:nrow(selected_data), round(0.7 * nrow(selected_data)))

train_data <- selected_data[train, ]
test_data <- selected_data[-train, ]

#preprocess
preprocess_data <- function(df, train_mean = NULL) {
  df_processed <- df %>%
    select(-neighborhood) %>%
    mutate(across(where(is.character), as.factor)) %>%
    mutate(across(where(is.factor), as.numeric)) %>%
    mutate(across(where(is.numeric) & !high_theft, ~ {
      x <- .x
      if(is.null(train_mean)) {
        x_mean <- mean(x, na.rm = TRUE)
      } else {
        x_mean <- train_mean
      }
      x[is.na(x)] <- x_mean
      scale(x, center = mean(x, na.rm = TRUE), scale = sd(x, na.rm = TRUE))
    }))
  
  # Remove columns with all NA values
  df_processed <- df_processed[, colSums(is.na(df_processed)) < nrow(df_processed)]
  
  return(df_processed)
}


train_processed <- preprocess_data(train_data)
test_processed <- preprocess_data(test_data)


# Binary outcome: high-value thefts (above median)


logit_model <- glm(high_theft ~ ., 
                  data = train_processed, 
                  family = binomial(link = "logit"),
                  control = list(maxit = 100))




rf_model <- randomForest(x = train_processed[, -which(names(train_processed) == "high_theft")],
                        y = as.factor(train_processed$high_theft),
                        ntree = 500, 
                        importance = TRUE)




train_processed$income

gam_model <- gam(high_theft ~ 
                 s(population_density_per_square_kilometre, k = 3) + 
                 s(public_transit, k = 3),
               data = train_processed,
               family = binomial(),
               method = "REML")


# Model Evaluation 
evaluate_model <- function(model, test_data) {
  # Get predicted probabilities
  preds_prob <- predict(model, newdata = test_data, type = "response")
  
  # Convert to class predictions (0/1)
  preds_class <- ifelse(preds_prob > 0.5, 1, 0)
  
  # Calculate accuracy
  accuracy <- mean(preds_class == test_data$high_theft)
  
  # Calculate RMSE and MAE
  rmse <- sqrt(mean((preds_prob - test_data$high_theft)^2))
  mae <- mean(abs(preds_prob - test_data$high_theft))
  
  # Create confusion matrix manually
  confusion <- table(Predicted = preds_class, Actual = test_data$high_theft)
  
  return(list(
    accuracy = accuracy,
    rmse = rmse,
    mae = mae,
    confusion = confusion
  ))
}



# Compare performance
logit_perf <- evaluate_model(logit_model, test_processed)
gam_perf <- evaluate_model(gam_model, test_processed)

model_results <- data.frame(
  Model = c("Logistic Regression", "GAM"),
  Accuracy = c(logit_perf$accuracy, gam_perf$accuracy),
  RMSE = c(logit_perf$rmse, gam_perf$rmse),
  MAE = c(logit_perf$mae, gam_perf$mae)
)

# Display the results in a kable
kable(model_results, 
      col.names = c("Model", "Accuracy", "RMSE", "MAE"), 
      caption = "Table 4: Model Evaluation Results")


#Random Forest Accuracy
rf_preds <- predict(rf_model, newdata = test_processed, type = "response")
rf_preds_class <- ifelse(rf_preds > 0.5, 1, 0)

rf_accuracy <- mean(rf_preds_class == test_processed$high_theft)
rf_confusion <- table(Predicted = rf_preds_class, Actual = test_processed$high_theft)

cat("Random Forest Accuracy:", rf_accuracy, "\n")


# Variable importance
importance_df <- as.data.frame(importance(rf_model))
importance_df$Variable <- rownames(importance_df)

importance_df <- importance_df %>%
  arrange(desc(MeanDecreaseGini)) %>%
  slice(3:n())

ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +  # Flip axes for easier reading
  theme_minimal(base_size = 14) +
  labs(
    title = "Random Forest Feature Importance",
    x = "Feature",
    y = "Mean Decrease Gini"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.text.y = element_text(size = 10)
  )


summary(gam_model)  
