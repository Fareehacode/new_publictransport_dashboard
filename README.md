📌 OVERVIEW <br />
This project is an interactive dashboard built using R Shiny to analyse public transport demand across New South Wales (NSW).<br />
The dashboard combines Opal Tap-On/Tap-Off data with GTFS stop coordinates to visualise:<br />
 - Tap intensity across stops and stations<br />
 - Busiest locations across the network<br />
 - Time-based travel demand trends<br /> 
 - Weekday vs weekend behaviour<br />
 - Mode-specific usage patterns (Train, Bus, Ferry, Light Rail)<br />
<br />
🌐 LIVE DASHBOARD<br />
You can access the fully deployed dashboard here: https://spiti121.shinyapps.io/nsw-opal-dashboard/<br />
<br />
📁 PROJECT STRUCTURE<br />
Below is a list of all major files and folders contained in the ZIP/GitHub repository, along with short summaries of their purpose.<br />
<br />
  1. app/ (Shiny Application Folder)<br />
     This folder contains everything required to run the dashboard.<br />
          - app.R (The main Shiny application file that builds the UI, server logic, charts, maps, filters, and KPIs.)<br />
          - data/ (Contains all datasets used by the dashboard.)<br />
           &ensp; - data/processed/<br />
                &emsp;- time_trends.csv (Cleaned and aggregated tap-on/tap-off counts by date, mode, type, and day category.)<br />
                &emsp;- stop_taps.csv (Stop-level tap activity with matched stop IDs and GTFS coordinates.)<br />
            &ensp; - data/raw/<br />
                &emsp;- stop_locations.csv (Extracted GTFS stop coordinates (lat/lon) for bubble mapping.)<br />
<br />
  2. scripts/ (Data Preprocessing Scripts)<br />
     This script prepares raw Opal and GTFS data before feeding them into the Shiny app.<br />
          - 02_data_prep.R (Cleans and merges Opal data files).<br />
          - 03_match_stops_gtfs.R (Matches Opal stop identifiers/postcodes with GTFS stop names and coordinates.)<br />
          <br />
  3. README.md<br />
     The file you are reading now — describes project purpose, structure, and usage instructions.<br />
<br />
🖥️ INSTRUCTIONS FOR RUNNING LOCALLY<br />
⚠️ We could not upload data sets on github because of storage restrictions, please download datasets before running the code locally!!! <br />
    1. Install required packages:<br />
        &ensp;install.packages(c("shiny", "tidyverse", "leaflet", "plotly", "DT", "lubridate", "bslib", "scales"))<br />
    2. Set working directory to the /app folder<br />
        &ensp;setwd("path/to/app")<br />
    3. Run the application<br />
        &ensp;shiny::runApp()<br />
<br />
🧩 DATA SOURCES<br />
All raw data comes from the official NSW Open Data Portal:<br />
      - Opal Tap-On/Tap-Off Dataset (2020) - https://opendata.transport.nsw.gov.au/dataset/opal-tap-on-and-tap-off-release-3-2020<br />
      - GTFS: Timetables Complete - https://opendata.transport.nsw.gov.au/dataset/timetables-complete-gtfs<br />
<br />
👥 CONTRIBUTERS<br />
 - Fareeha Mulla<br />
 - Spiti Choudhary<br />
<br />
