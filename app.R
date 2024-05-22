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
            selectInput("startDate", "Select Start Date:", choices = 2009:2024)
          ),
          column(
            width = 6,
            selectInput("endDate", "Select End Date:", choices = 2009:2024, selected = 2024)
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
  # Populate start date and end date choices
  observe({
    updateSelectInput(session, "startDate", choices = 2009:2024)
    updateSelectInput(session, "endDate", choices = 2009:2024, selected = 2024)
  })
  
  # Populate island choices for selectInput
  observe({
    island_choices <- unique(sewage_spills_watershed$island)
    updateSelectInput(session, "islandSelect", choices = island_choices)
  })
  
  # Populate watershed choices for selectInput
  observe({
    selected_islands <- input$islandSelect
    watershed_choices <- unique(sewage_spills_watershed$wuname[!is.na(sewage_spills_watershed$wuname) & sewage_spills_watershed$island %in% selected_islands])
    watershed_choices <- sort(watershed_choices)  
    updateSelectInput(session, "watershedSelect", choices = watershed_choices)
  })
  
  # Reactive expression for filtering based on date, island, and watershed input
  filtered_data <- reactive({
    # Filter by date
    start_date <- as.numeric(input$startDate)
    end_date <- as.numeric(input$endDate)
    
    filtered <- sewage_spills_watershed
    
    if (!is.null(start_date) && !is.null(end_date)) {
      filtered <- filtered %>% 
        filter(year >= start_date, year <= end_date)
    }
    
    # Filter by selected island
    selected_islands <- input$islandSelect
    if (!is.null(selected_islands) && length(selected_islands) > 0) {
      filtered <- filtered %>% 
        filter(island %in% selected_islands)
    }
    
    # Filter by selected watersheds
    selected_watersheds <- input$watershedSelect
    if (!is.null(selected_watersheds) && length(selected_watersheds) > 0) {
      filtered <- filtered %>% 
        filter(wuname %in% selected_watersheds)
    }
    
    return(filtered)
  })
  
    #Leaflet Map
    output$mapLocation <- renderLeaflet({
      leaflet() %>%
        setView(lng = -157.8583, lat = 21.3069, zoom = 7) %>%
        addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png") %>%
        addCircleMarkers(
          data = filtered_data(),
          clusterOptions = markerClusterOptions(),
          popup = paste0(
            "<div class='custom-popup'>",
            "<h4>Sewage Spill Details</h4>",
            "<p><strong>Title:</strong> ", filtered_data()$title, "</p>",
            "<p><strong>Date Issued:</strong> ", filtered_data()$issuance_date, "</p>",
            "<p><strong>Date Cancelled:</strong> ", filtered_data()$cancellation_date, "</p>",
            "<p><strong>Location:</strong> ", filtered_data()$location_name, "</p>",
            "<p><strong>Gallons Spilled:</strong> ", filtered_data()$volume_gallons, "</p>",
            "<p><strong>Watershed:</strong> ", filtered_data()$wuname, "</p>",
            "<p><strong>County:</strong> ", filtered_data()$county, "</p>",
            "</div>"
          ),
          color = ~colorFactor("magma", unique(filtered_data()$volume_gallons))(rev(volume_gallons)),
          fillOpacity = 0.8
        ) 
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

