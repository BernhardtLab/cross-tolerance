#11-figure-creation
# Ijeoma Nwafor 
# Oct. 21 2024

library(readxl)
library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())
library(janitor)
library(lubridate)

files <- c(list.files("data-raw/Evolution-2024", full.names = TRUE))

# contain the full file paths of all .xlsx files in the folder
files2 <- files[grepl(".xlsx", files)]

files2 <- files2 %>% 
  gsub(pattern = ".xlsx$", replacement = "")

#extract all the rep2 sheets
date_time_list <- list()
od600_list <- list()

# Loop through each file !! # date and time not entering correctly
for (file in files2) {
  # Get all sheet names in the file
  sheet_names <- excel_sheets(file)
  
  # Find sheets that contain "Rep2"
  rep2_sheets <- sheet_names[grepl("Rep2", sheet_names)]
  
  # Loop through matching sheets
  for (sheet in rep2_sheets) {
    # Read date and time from A5:B6
    date_time <- read_excel(file, sheet = sheet, range = "B22", col_names = FALSE)
    
    # Read OD600 values from A25:M32
    od600 <- read_excel(file, sheet = sheet, range = "A25:M32", col_names = FALSE)
    
    # Store the extracted date and time
    date_time_list[[paste(file, sheet, sep = "_")]] <- date_time
    
    # Store the extracted OD600 data
    od600_list[[paste(file, sheet, sep = "_")]] <- od600
  }
}

# then pull all individual files into a merged DF by file_name 

all_plates <- map_df(od600_list, ~as.data.frame(.x), .id = "file_name") %>%
  clean_names() %>%
  filter(!grepl("<>", x1))

pivot_plates <- all_plates %>%
  pivot_longer(cols = 3:14, names_to = "well", values_to = "OD600")

#Rename some things
clean_plates <- pivot_plates %>%
  rename(row = x1) %>% 
  mutate(
    well2 = case_when(
      well == "x2" ~ 1,
      well == "x3" ~ 2,
      well == "x4" ~ 3,
      well == "x5" ~ 4,
      well == "x6" ~ 5,
      well == "x7" ~ 6,
      well == "x8" ~ 7,
      well == "x9" ~ 8,
      well == "x10" ~ 9,
      well == "x11" ~ 10,
      well == "x12" ~ 11,
      well == "x13" ~ 12,
      TRUE ~ NA_integer_  # Default value if no condition is met
    )
  )

# separate file_name by "_", remove well, join row and well, add time 
# drop brackets and .xlsx from evol_temp
clean_plates2 <- clean_plates %>%
  separate(file_name, into = c("data","raw", "evolution","2024", "generation", "evol_temp", "xlsx", "temp", "replicate"), remove = FALSE) %>%
  select(generation, evol_temp, temp, row, well2, OD600) %>%
  mutate(well = paste0(row, well2)) %>%  # Create the new `well` column
  select(-row, -well2)

## now using date_time-list add in the time

all_times <- map_df(date_time_list, ~as.data.frame(.x), .id = "file_name") %>%
  clean_names() 

head(all_times)

#remove file with "G10+" in it - will come back to this dued to the weird naming (at35C)
all_times <- all_times %>%
  filter(!grepl("G10\\+", file_name))


all_times2 <- all_times %>%
  mutate(date_time = as.POSIXct(x1, format = "%Y-%m-%d %I:%M:%S %p")) %>%
  separate(file_name, into = c("data","raw", "evolution","2024", "generation", "evol_temp", "xlsx", "temp", "replicate"), remove = FALSE) %>% 
  select(generation, evol_temp, temp, date_time)

alltimes3 <- all_times2 %>%
  mutate(time_elapsed = round(as.numeric(difftime(date_time, min(date_time), units = "hours"))))

auris_evolution <- left_join(clean_plates2, alltimes3, by = c("generation", "evol_temp","temp"))

plate_layout <- read_excel("data-raw/HYS4.xlsx", sheet = "plate_layout")
#join by well if not there = EMPTY

combined_data <- left_join(plate_layout, auris_evolution, by = "well")

# Replace rows with missing values in 'auris_evolution' columns as 'EMPTY'
combined_data <- combined_data %>%
  mutate(across(everything(), ~replace_na(.x, "EMPTY")))

## auris csv
#write_csv(auris_evolution, "data-processed/auris_evolution.csv")

## quick tings <- NOT WORKING NEED PLATE LAYOUT - BLANKS AND CULTURE
## 35 evolv or 43/44C evolved at 44C or 43/44

combined_data %>%
  ggplot(aes(x = time_elapsed, y = OD600, color = evol_temp, group = treatment)) +
  geom_point()+
  geom_smooth(method = "loess", se = FALSE) + 
  facet_wrap(~temp, scales = "free_y") +  
  theme_minimal()

### HYS4 INSTABILITY NGLABRATUS ------
files_ng <- c(list.files("data-raw/hys4-instability-nglabrata", full.names = TRUE))

# contain the full file paths of all .xlsx files in the folder
files2_ng <- files_ng[grepl(".xlsx", files_ng)]

files2_ng <- files2_ng %>% 
  gsub(pattern = ".xlsx$", replacement = "")

#extract all the rep2 sheets
date_time_list2 <- list()
od600_list2 <- list()

# Loop through each file !! # date and time not entering correctly
for (file in files2_ng) {
  # Get all sheet names in the file
  sheet_names <- excel_sheets(file)
  
  # Find sheets that contain "G"
  g_sheets <- sheet_names[grepl("G", sheet_names)]
  
  # Loop through matching sheets
  for (sheet in g_sheets) {
    # Read date and time from A5:B6
    date_time2 <- read_excel(file, sheet = sheet, range = "B22", col_names = FALSE)
    
    # Read OD600 values from A25:M32
    od6002 <- read_excel(file, sheet = sheet, range = "A25:M32", col_names = FALSE)
    
    # Store the extracted date and time
    date_time_list2[[paste(file, sheet, sep = "_")]] <- date_time2
    
    # Store the extracted OD600 data
    od600_list2[[paste(file, sheet, sep = "_")]] <- od6002
  }
}
# then pull all individual files into a merged DF by file_name 
all_plates_ng <- map_df(od600_list2, ~as.data.frame(.x), .id = "file_name") %>%
  clean_names() %>%
  filter(!grepl("<>", x1)) %>%
  filter(complete.cases(.)) 

pivot_ng <- all_plates_ng %>%
  pivot_longer(cols = 3:14, names_to = "well", values_to = "OD600")

#Rename some things
clean_ng <- pivot_ng %>%
  rename(row = x1) %>% 
  mutate(
    well2 = case_when(
      well == "x2" ~ 1,
      well == "x3" ~ 2,
      well == "x4" ~ 3,
      well == "x5" ~ 4,
      well == "x6" ~ 5,
      well == "x7" ~ 6,
      well == "x8" ~ 7,
      well == "x9" ~ 8,
      well == "x10" ~ 9,
      well == "x11" ~ 10,
      well == "x12" ~ 11,
      well == "x13" ~ 12,
      TRUE ~ NA_integer_  # Default value if no condition is met
    )
  )

# separate file_name by "_", remove well, join row and well, add time 
# drop brackets and .xlsx from evol_temp
clean_ng2 <- clean_ng %>%
  separate(file_name, into = c("data","raw", "hys4","instability", "nglabrata", "generation", "xlsx", "gen2"), remove = FALSE) %>%
  select(generation, row, well2, OD600) %>%
  mutate(well = paste0(row, well2)) %>%  # Create the new `well` column
  select(-row, -well2)

## now using date_time-list add in the time
times_ng <- map_df(date_time_list2, ~as.data.frame(.x), .id = "file_name") %>%
  clean_names() 

times_ng2 <- times_ng %>%
  mutate(date_time = as.POSIXct(x1, format = "%Y-%m-%d %I:%M:%S %p")) %>%
  separate(file_name, into = c("data","raw", "hys4","instability", "nglabrata", "generation", "xlsx", "gen2"), remove = FALSE) %>% 
  select(generation,date_time)

times_ng2 <- times_ng2 %>%
  mutate(time_elapsed = round(as.numeric(difftime(date_time, min(date_time), units = "hours"))))

## glabrata csv
glabrata_evolution <- left_join(clean_ng2, times_ng2, by = c("generation"))
#write_csv(glabrata_evolution, "data-processed/glabrata_evolution.csv")


##de evolved glabratus 