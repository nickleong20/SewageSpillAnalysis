# Load required libraries
library(bs4Dash)
library(leaflet)
library(DT)
library(plotly)
library(readr)
library(dplyr)
library(tidyverse)
library(readxl)
library(tidygeocoder)
library(sf)  
library(mapview)
library(mapdata)
library(data.table)
library(shiny)
library(ggdist)
library(ggthemes)
library(ggplot2)
library(rlang)
library(PupillometryR)
library(gridExtra)
library(networkD3)
library(tidyr)
library(One4All)
library(janitor)
library(jsonlite)
library(httr)
# Import Datasets
# Update the file paths based on the actual location of your files
sewage_spills <- read_csv("data/UPDATED_sewage_spill_locations(Sheet1).csv")
sewage_spills <- clean_names(sewage_spills)

# Read the shapefile
# Update the file path to the shapefile
watersheds_without_islands <- st_read("data/Watersheds/Watersheds.shp")
islands <- st_read("data/2020_Census_County_Boundaries/2020_Census_County_Boundaries.shp")

# Perform spatial join
watersheds <- st_intersection(watersheds_without_islands, islands)

# Convert sewage spills to spatial data frame
sewage_spills_sf <- st_as_sf(sewage_spills, coords = c("longitude", "latitude"), crs = st_crs(watersheds))

# Reproject datasets to WGS84
sewage_spills_sf <- st_transform(sewage_spills_sf, crs = 4326)
watersheds <- st_transform(watersheds, crs = 4326)

# Perform spatial join to attribute sewage spills to watersheds
sewage_spills_watershed <- st_join(sewage_spills_sf, watersheds)

# Handle NA values by finding the nearest watershed
na_spills <- sewage_spills_watershed[is.na(sewage_spills_watershed$wuname), ]
non_na_spills <- sewage_spills_watershed[!is.na(sewage_spills_watershed$wuname), ]
nearest_watershed <- sf::st_nearest_feature(na_spills, non_na_spills)

# Replace NA values with the nearest watershed name
sewage_spills_watershed$wuname[is.na(sewage_spills_watershed$wuname)] <- non_na_spills$wuname[nearest_watershed]
# Convert sewage_spills_watershed to a non-spatial data frame
sewage_spills_watershed_df <- as.data.frame(sewage_spills_watershed)
sewage_spills_watershed_df <- sewage_spills_watershed_df %>%
  select(-objectid, -wuc, -huc, -swma, -area_sqmi, -area_m2, -name_hwn, -st_areasha, -st_perimet, -objectid.1, -geoid20, -name20, -namelsad20, -aland20, -awater20, -pop20, -st_areasha.1, -st_perimet.1)

sewage_spills_watershed_df <- sewage_spills_watershed_df %>%
  left_join(watersheds, by = c("wuname", "island" = "name20"))


# Convert back to a spatial data frame
sewage_spills_watershed <- st_as_sf(sewage_spills_watershed_df)

# Import dataset
#sewage_spills_watershed <- st_read("sewage_spills_watershed.shp")

################################################################################

ui <- bs4DashPage(
  bs4DashNavbar(
    title = "Hawai'i Sewage Spill Locations",
    tags$style(
      HTML(".navbar { background-color: #22767c; }")
    )
  ),
  bs4DashSidebar(
    tags$style(
      HTML(".sidebar { background-color: #22767c; }")
    ),
    sidebarMenu(
      menuItem("Interactive Map", tabName = "mapTab", icon = icon("map"))
    )
  ),
  bs4DashBody(
    fluidRow(
      box(
        title = "Hawai'i Sewage Spill Locations",
        h3(
          tags$div(
            "This data is sourced from Hawai'i Department of Health's Clean Water Branch",
            style = "font-size: 14px;"
          )
        ),
        width = 12
      )
    ),
    fluidRow(
      column(
        width = 12,
        # Date Range Selector
        fluidRow(
          column(
            width = 6,
            selectInput("startDate", "Select Start Date:", choices = NULL)
          ),
          column(
            width = 6,
            selectInput("endDate", "Select End Date:", choices = NULL)
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        leafletOutput("mapLocation")
      )
    ),
    fluidRow(
      column(
        width = 6,
        selectInput("islandSelect", "Select Island", choices = NULL, multiple = TRUE)
      ),
      column(
        width = 6,
        selectInput("watershedSelect", "Select Watershed", choices = NULL, multiple = TRUE)
      )
    ),
    fluidRow(
      column(
        width = 12,
        # Summary Statistics Box
        box(
          title = "Number of Spills",
          solidHeader = TRUE,
          style = "background-color: #f0f0f0; color: #333; font-weight: bold; font-size: 24px;",
          textOutput("numberStats"),
          width = 12
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        # Summary Statistics Box
        box(
          title = "Total Gallons Spilled",
          solidHeader = TRUE,
          style = "background-color: #f0f0f0; color: #333; font-weight: bold; font-size: 24px;",
          textOutput("spillStats"),
          width = 12
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        # Data Table Box
        box(
          title = "Plastic Data by Location",
          style = "overflow-x: auto;",
          DT::dataTableOutput("spilltableLocation"),
          width = 12
        )
      )
    )
  )
)



server <- function(input, output, session) {
  # Ensure the dates in the dataset are in Date format
  sewage_spills_watershed$issuance_date <- as.Date(sewage_spills_watershed$issuance_date, format = "%Y-%m-%d")
  
  # Get the minimum and maximum dates
  min_date <- min(sewage_spills_watershed$year, na.rm = TRUE)
  max_date <- max(sewage_spills_watershed$year, na.rm = TRUE)
  
  # Populate the start and end date choices
  observe({
    date_choices <- sort(unique(sewage_spills_watershed$issuance_date))
    updateSelectInput(session, "startDate", choices = date_choices, selected = min_date)
    updateSelectInput(session, "endDate", choices = date_choices, selected = max_date)
  })
  
  # Reactive expression for filtering based on start and end date input
  date_filtered_data <- reactive({
    req(input$startDate, input$endDate)  # Ensure that input$startDate and input$endDate are available
    sewage_spills_watershed %>%
      filter(issuance_date >= as.Date(input$startDate) & issuance_date <= as.Date(input$endDate))
  })
  
  # Observe the date-filtered data to update island choices
  observe({
    filtered_data <- date_filtered_data()
    island_choices <- unique(filtered_data$island)
    updateSelectInput(session, "islandSelect", choices = island_choices)
  })
  
  # Observe the selected islands to update watershed choices
  observe({
    filtered_data <- date_filtered_data()
    selected_islands <- input$islandSelect
    watershed_choices <- unique(filtered_data$wuname[!is.na(filtered_data$wuname) & filtered_data$island %in% selected_islands])
    watershed_choices <- sort(watershed_choices)
    updateSelectInput(session, "watershedSelect", choices = watershed_choices)
  })
  
  # Reactive expression for filtering based on island and watershed input
  filtered_data <- reactive({
    filtered <- date_filtered_data()
    selected_islands <- input$islandSelect
    selected_watersheds <- input$watershedSelect
    
    # Filter by selected islands
    if (!is.null(selected_islands) && length(selected_islands) > 0) {
      filtered <- filtered %>% 
        filter(island %in% selected_islands)
    }
    
    # Filter by selected watersheds
    if (!is.null(selected_watersheds) && length(selected_watersheds) > 0) {
      filtered <- filtered %>% 
        filter(wuname %in% selected_watersheds)
    }
    
    return(filtered)
  })
  
  
  # Location tab
  output$spilltableLocation <- DT::renderDataTable({
    data_to_display <- filtered_data() %>%
      select(title, issuance_date, cancellation_date, location_name, wuname, volume_gallons, advisement, county) %>%
      rename(
        "Title" = title,
        "Date Issued" = issuance_date,
        "Date Cancelled" = cancellation_date,
        "Location" = location_name,
        "Gallons Spilled" = volume_gallons,
        "Advisement" = advisement,
        "Watershed" = wuname,
        "County" = county
      )
    
    datatable(
      data_to_display,
      style = "bootstrap",
      class = "cell-border stripe",
      extensions = c("Buttons"),  # Add Buttons extension
      options = list(
        columnDefs = list(
          list(targets = 6, width = "300px")  # 6th column (Advisement)
        ),
        dom = "Bfrtip",
        lengthMenu = list(c(1000), c(1000)),  # Display only 1000 rows initially
        deferRender = TRUE,  # Defer rendering of rows until visible
        scrollY = 400,  # Set the height of the table body to 400 pixels
        scrollCollapse = TRUE,  # Collapse the table if the height is less than the specified scrollY
        buttons = list(
          list(
            extend = "csv",
            filename = "spill_data",
            title = "Download CSV",
            text = '<i class="fa fa-download"></i> CSV',
            className = "btn-primary",
            exportOptions = list(
              modifier = list(selected = TRUE, visible = TRUE)
            )
          )
        )
      )
    )
  })
  
  output$numberStats <- renderText({
    data <- filtered_data()
    num_spills <- nrow(data)
    
    summary_text <- paste(num_spills,
                          collapse = "<br/>")
    
    summary_text
  })
  
  output$spillStats <- renderText({
    data <- filtered_data()
    total_gallons <- sum(data$volume_gallons, na.rm = TRUE)
    
    summary_text <- paste(total_gallons,
                          collapse = "<br/>")
    
    summary_text
  })
  
  
  
}

shinyApp(ui, server)

