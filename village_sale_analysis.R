# analyze manhattan townhouse sales
# data from https://data.cityofnewyork.us/Housing-Development/NYC-Calendar-Sales-Archive-/uzf5-f8n2

# consider this for determining if renovation occured:
# https://data.cityofnewyork.us/City-Government/DOF-Property-Assessment-Change/a5nd-6mit


library(tidyverse)
library(tidymodels)
library(performance)

fix_col_strings = function(str_vec){
  str_vec |> 
    as.character() |> 
    tolower() |> 
    str_replace_all(" ","_") |> 
    str_replace("as_of_final_roll","at_present")
  
}

file_names <- paste0("raw_data/",list.files("raw_data","*manhattan*"))

archive_data <- file_names |> 
  map_dfr(readxl::read_xls,skip = 3,
          col_types = "text",
          col_names = FALSE) |> 
  rename_with(.fn=~fix_col_strings(archive_data[1,])) |> 
  mutate(across(contains("date"),openxlsx::convertToDate))


recent_data <- read_delim("raw_data/NYC_Citywide_Annualized_Calendar_Sales_Update.csv",
                          delim = ";",
                          col_names = TRUE,
                          col_types = cols(.default = "c"))

recent_data <- recent_data |> 
  rename_with(.fn=~fix_col_strings(names(recent_data))) |> 
  mutate(across(contains("date"),\(x) as.Date(x,format  = "%m/%d/%Y"))) |> 
  # keep only columns in all sets
  select(1:21)


latest_data <- read_csv("raw_data/NYC_Citywide_Rolling_Calendar_Sales.csv",
                         col_names = TRUE,
                        col_types = cols(.default = "c"))

latest_data <- latest_data |> 
  rename_with(.fn=~fix_col_strings(names(latest_data))) |> 
  mutate(across(contains("date"),as.Date,format  = "%m/%d/%Y"))

combined_raw_data<- archive_data |> 
  bind_rows(latest_data) |> 
  bind_rows(recent_data) |> 
  unique()

save(combined_raw_data,file="data/combined_raw_data.rdata")

# since skip rows varies by sheet we have to manually fix column types
prop_data <- combined_raw_data |> 
  filter(!(borough == "BOROUGH")) |> 
  mutate(across(is.character,\(x) str_remove_all(x,","))) |> 
  # NOTE still have to convert borough names to numbers or vice versa.  Inconsistent now.
  # mutate(borough = as.numeric(borough)) |>
  # filter(!is.na(borough)) |>
  # mutate(borough = as.factor(borough)) |>
  mutate(across(contains("tax"),as_factor)) |>
  mutate(across(contains("building"),as_factor)) |>
  mutate(across(contains("neighborhood"),as_factor)) |>
  mutate(across(contains("price"),as.integer)) |>
  mutate(across(contains("feet"),as.integer)) |>
  mutate(across(contains("units"),as.integer)) |>
  separate(building_class_category,into = c("building_category","building_category_name"),extra = "merge") |> 
  mutate(building_category = as.numeric(building_category)) |>
  mutate(is_single_family = (building_category == 1)) |> 
  mutate(has_commercial = (commercial_units > 0)) |> 
  identity()

save(prop_data,file="data/prop_data.rdata")

village_zips <- c("10011","10014")
max_unit_size <-  6
min_sale_price <- 1000000
min_sq_feet <- 1000
max_sq_feet <- 15000
valid_categories <- c(1:8)


# isolate zips buildings of interest
village_data <- prop_data |> 
  filter(land_square_feet < 10000) |> 
  filter(zip_code %in% village_zips) |> 
  filter(borough %in% c("1","MANHATTAN")) |> 
  filter(building_category %in% valid_categories) |>
  filter(sale_price >= min_sale_price) |> 
  filter(gross_square_feet >= min_sq_feet) |> 
  filter(gross_square_feet <= max_sq_feet) |> 
  filter(residential_units > 0) |> 
  filter(residential_units <= max_unit_size) |> 
  filter(is.na(apartment_number)) |> # whole building
  identity()

village_data |> 
  # filter(year(sale_date)> 2011) |> 
  ggplot(aes(gross_square_feet,sale_price,color = is_single_family)) + geom_point() +
  geom_smooth() +
  scale_y_continuous(labels = scales::dollar) + 
  scale_x_continuous(labels = scales::comma) + 
  labs(title = "Sale Price Vs. Size",
       subtitle = "Townhouses in GV and Chelsea Sold from 2012-2022")

village_data |> 
  filter(gross_square_feet > 4500) |> 
  filter(gross_square_feet < 5500) |> 
  ggplot(aes(sale_date,sale_price,color = is_single_family)) + geom_point() + 
  scale_y_continuous(labels = scales::dollar) + 
  geom_smooth() + 
  labs(title = "Sale Price Over Time",
       subtitle = "Townhouses between 4,500 and 5,000 Square Feet in GV and Chelsea")

featured_data <- village_data |>
  select(
    sale_price,
    sale_date,
    gross_square_feet,
#    land_square_feet,
    neighborhood,
    is_single_family,
    has_commercial
  )


model1 <- lm(sale_price ~ .,data = featured_data)

summary(model1)
glance(model1)
broom::tidy(model1)
check_model(model1)


# TEST single variable
featured_data |> 
  ggplot(aes(land_square_feet,sale_price)) + geom_point() + geom_smooth()

one_house_before <- village_data |> filter(str_detect(address,"133")) |> 
  select(
  sale_price,
  sale_date,
  gross_square_feet,
  land_square_feet,
  neighborhood,
  is_single_family,
  has_commercial
) 

one_house_after <- one_house_before |> 
  mutate(sale_date = Sys.Date()) |> 
  mutate(is_single_family = TRUE) |> 
  mutate(gross_square_feet = 5188)

predict(model1,newdata = one_house_before)
predict(model1,newdata = one_house_after)

goodness <- tibble(predicted = predict(model1,data = featured_data),
                   actual = featured_data$sale_price)

goodness |> 
  ggplot(aes(actual,predicted)) + geom_point() +
  geom_smooth(method = "lm") + 
  geom_abline(slope=1,intercept = 0) +
  scale_y_continuous(labels = scales::dollar) + 
  scale_x_continuous(labels = scales::dollar)


model1_rmse <- sqrt(sum((goodness$predicted - goodness$actual)^2)/
                      nrow(goodness))
model1_rmse


model2 <- ranger::ranger(sale_price ~.,
                         data = featured_data,
                         importance = "impurity")



vip::vip(model2)
goodness <- tibble(predicted = predict(model2,data = featured_data)$prediction,
                   actual = featured_data$sale_price)
model2_rmse <- sqrt(sum((goodness$predicted - goodness$actual)^2)/
  nrow(goodness))
model2_rmse

goodness |> 
  ggplot(aes(actual,predicted)) + geom_point() +
  geom_smooth(method = "lm") + 
  geom_abline(slope=1,intercept = 0) +
  scale_y_continuous(labels = scales::dollar) + 
  scale_x_continuous(labels = scales::dollar)
  

predict(model2,data = one_house_before)$prediction
predict(model2,data = one_house_after)$prediction

# ---------------
# tidy models

ranger_spec <- rand_forest() |> 
  set_engine("ranger",importance = "impurity") |> 
  set_mode("regression") |> 
  step_log(gross_square_feet) |> 
  step_dummy(all_nominal_predictors())

rec <- recipe(sale_price ~.,data = featured_data)

wf <- workflow() |> 
add_model(ranger_spec) |> 
add_recipe(rec) 

prop_fit <- wf |> fit(featured_data)

goodness <- tibble(predicted = predict(prop_fit,featured_data)$.pred,
                   actual = featured_data$sale_price)
goodness

model3_rmse <- sqrt(sum((goodness$predicted - goodness$actual)^2)/
                      nrow(goodness))
model3_rmse

goodness |> 
  ggplot(aes(actual,predicted)) + geom_point() +
  geom_smooth(method = "loess") + 
  geom_abline(slope=1,intercept = 0) +
  scale_y_continuous(labels = scales::dollar) + 
  scale_x_continuous(labels = scales::dollar)


