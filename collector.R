# --- 1. THE TOOLS ---
# We use jsonlite to talk to the web and dplyr to organize the data.
library(jsonlite)
library(dplyr)
library(readr) # Added for write_csv

# --- 2. THE GEOGRAPHY ---
# Setup County List with verified center coordinates.
# These coordinates match the Shapefile for map alignment.
counties_df <- data.frame(
  county = c("Baringo", "Bomet", "Bungoma", "Busia", "Elgeyo-Marakwet", "Embu", "Garissa", 
             "Homa Bay", "Isiolo", "Kajiado", "Kakamega", "Kericho", "Kiambu", "Kilifi", 
             "Kirinyaga", "Kisii", "Kisumu", "Kitui", "Kwale", "Laikipia", "Lamu", 
             "Machakos", "Makueni", "Mandera", "Marsabit", "Meru", "Migori", "Mombasa", 
             "Murang'a", "Nairobi", "Nandi", "Narok", "Nyamira", "Nyandarua", "Nyeri", 
             "Samburu", "Siaya", "Taita Taveta", "Tana River", "Tharaka-Nithi", 
             "Trans Nzoia", "Turkana", "Uasin Gishu", "Vihiga", "Wajir", "West Pokot", "Nakuru"),
  lat = c(0.72635, -0.71609, 0.78569, 0.37571, 0.74510, -0.53283, -0.52049, -0.56240, 1.00639, 
          -2.11237, 0.49719, -0.31668, -1.03438, -3.15244, -0.46735, -0.73668, -0.19438, 
          -1.56049, -4.12059, 0.28913, -2.06368, -1.27624, -2.25364, 3.23096, 2.86048, 
          0.22905, -1.01870, -3.99045, -0.82852, -1.30049, 0.22735, -1.27988, -0.64938, 
          -0.38909, -0.31437, 1.54137, -0.05767, -3.40520, -1.53492, -0.19145, 1.04592, 
          3.16249, 0.47819, 0.08486, 1.93971, 1.89337, -0.45845),
  lon = c(36.01859, 35.23846, 34.71876, 34.26511, 35.56086, 37.66463, 40.35559, 34.31453, 
          38.74765, 36.77761, 34.80093, 35.22600, 36.84322, 39.67271, 37.30190, 34.75351, 
          34.77733, 38.37181, 39.06614, 36.82525, 40.54583, 37.39460, 37.87666, 40.71135, 
          37.71488, 37.77611, 34.30917, 39.70113, 37.00417, 36.82793, 35.12388, 35.47636, 
          34.93340, 36.49743, 36.93047, 36.94154, 34.19989, 38.37176, 39.55029, 37.96042, 
          34.98078, 35.31349, 35.30380, 34.70778, 40.02451, 35.21277, 36.10038)
)

# --- 3. DATA GATHERING ---
# We ask the API for Current Weather at all 47 coordinates in one go.
api_url <- paste0("https://api.open-meteo.com/v1/forecast?latitude=", 
                  paste(counties_df$lat, collapse=","), 
                  "&longitude=", paste(counties_df$lon, collapse=","), 
                  "&current=temperature_2m,relative_humidity_2m,precipitation")

# The computer reads the response from the web.
res <- fromJSON(api_url)

# --- 4. DATA STANDARDIZATION ---
# We package the raw numbers into a clean table (Data Frame).
new_log <- data.frame(
  # TIME ZONE FIX: Sets time specifically to Nairobi (EAT).
  timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "Africa/Nairobi"),
  county = counties_df$county,
  lat = counties_df$lat,
  lon = counties_df$lon,
  temp_c = as.numeric(res$current$temperature_2m),
  humidity_pct = as.numeric(res$current$relative_humidity_2m),
  precip_mm = as.numeric(res$current$precipitation)
)

# SAFETY CHECK: If the API failed for any county, we remove that row 
# so it doesn't break the 7-day average calculation in the dashboard.
new_log <- na.omit(new_log)

# --- 5. THE DATA LIBRARY ---
# We save this hour's data to a permanent CSV record.
file_path <- "weather_history.csv"

# SAVING: We use write_csv (from the readr tool).
# It doesn't use quotes around text, which prevents the "unambiguous format" error.
if (!file.exists(file_path)) {
  # If the file doesn't exist, create it with column headers.
  write_csv(new_log, file_path)
} else {
  # If the file exists, simply glue the new rows to the bottom.
  write_csv(new_log, file_path, append = TRUE)
}