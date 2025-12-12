
# app.R ---------------------------------------------------------------
library(shiny)
library(leaflet)
library(terra)
library(raster)

# ----------------- Load & prep raster (outside server) ---------------

# 1. Read ESRI GRID by pointing to the folder that holds the .adf files
flood_raw <- rast("data/katrina_flood")   # <- adjust to your folder

# 2. (Optional) downsample for Shiny performance
flood_shiny <- aggregate(flood_raw, fact = 4, fun = mean)

# 3. Reproject to WGS84 for Leaflet
flood_ll <- project(flood_shiny, "EPSG:4326")

# 4. Convert to RasterLayer (still in meters)
flood_m <- raster(flood_ll)

# 5. Convert meters -> feet
feet_per_meter <- 3.28084
flood_ft <- flood_m * feet_per_meter

# 6. Cap values at 40 ft for display/legend
flood_ft_capped <- flood_ft
flood_ft_capped[flood_ft_capped > 40] <- 40

# Darker blue palette
pal_depth <- colorNumeric(
  palette = c("#c6dbef", "#6baed6", "#08519c"),  # light → medium → deep blue
  domain  = values(flood_ft_capped),
  na.color = "transparent"
)

# ----------------------------- UI ------------------------------------

ui <- fluidPage(
  titlePanel("Hurricane Katrina: Flood Depth"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Use the slider to show only areas with at least the selected flood depth (in feet)."),
      sliderInput(
        "min_depth",
        "Show areas with depth of AT LEAST:",
        min = 0,
        max = 40,   # matches legend cap
        value = 1,
        step = 1
      ),
      verbatimTextOutput("click_info")
    ),
    
    mainPanel(
      leafletOutput("flood_map", height = "600px")
    )
  )
)

# ---------------------------- SERVER ---------------------------------

server <- function(input, output, session) {
  
  output$flood_map <- renderLeaflet({
    req(input$min_depth)
    
    # Mask out areas shallower than slider value and cap at 40
    flood_masked <- flood_ft_capped
    flood_masked[flood_masked < input$min_depth] <- NA
    
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addRasterImage(
        flood_masked,
        colors  = pal_depth,
        opacity = 0.8,
        project = FALSE
      ) |>
      addLegend(
        "bottomright",
        pal     = pal_depth,
        values  = c(0, 40),            # legend fixed from 0 to 40 ft
        title   = "Flood depth (feet)"
      )
  })
  
  # Click to get depth in feet
  output$click_info <- renderPrint({
    click <- input$flood_map_click
    if (is.null(click)) {
      cat("Click the map to see flood depth (feet) at that location.")
    } else {
      xy <- cbind(click$lng, click$lat)
      depth_val_ft <- raster::extract(flood_ft, xy)
      
      if (is.na(depth_val_ft)) {
        cat("No flood depth value at this location.\n",
            "Lon:", round(click$lng, 5),
            "Lat:", round(click$lat, 5))
      } else {
        cat("Flood depth at clicked point:\n",
            round(depth_val_ft, 2), "feet\n\n",
            "Lon:", round(click$lng, 5),
            "Lat:", round(click$lat, 5))
      }
    }
  })
}

shinyApp(ui, server)



# try and deploy the app
library(rsconnect)


