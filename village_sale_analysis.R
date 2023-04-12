# analyze manhattan townhouse sales
# data from https://data.cityofnewyork.us/Housing-Development/NYC-Calendar-Sales-Archive-/uzf5-f8n2

# consider this for determining if renovation occured:
# https://data.cityofnewyork.us/City-Government/DOF-Property-Assessment-Change/a5nd-6mit


library(tidyverse)

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
  mutate(has_commericial = (commercial_units > 0)) |> 
  identity()

save(prop_data,file="data/prop_data.rdata")

village_zips <- c("10011","10014")
max_unit_size <-  6
min_sale_price <- 1000000
min_sq_feet <- 1000
max_sq_feet <- 15000
valid_categories <- c(1:8,14)


# isolate zips buildings of interest
village_data <- prop_data |> 
  filter(zip_code %in% village_zips) |> 
  filter(sale_price >= min_sale_price) |> 
  filter(gross_square_feet >= min_sq_feet) |> 
  filter(gross_square_feet <= max_sq_feet) |> 
  filter(residential_units > 0) |> 
  filter(residential_units <= max_unit_size) |> 
  filter(building_category %in% valid_categories) |>
  filter(is.na(apartment_number)) |> # whole building
#  filter(year(sale_date) > 2020) |> 
  identity()

village_data |> ggplot(aes(gross_square_feet,sale_price)) + geom_point() +
  geom_smooth() +
  scale_y_continuous(labels = scales::dollar) + 
  scale_x_continuous(labels = scales::comma)

village_data |> 
  filter(gross_square_feet > 4500) |> 
  filter(gross_square_feet < 5500) |> 
  filter(building_category == 1) |> 
  ggplot(aes(sale_date,sale_price,color = neighborhood)) + geom_point() + 
  scale_y_continuous(labels = scales::dollar) + 
  geom_smooth()
