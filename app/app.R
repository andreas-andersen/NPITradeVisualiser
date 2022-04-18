####---------------------  NPI/Global Trade Visualiser  --------------------####
##                                                                            ##
##     For visualizing stringency of non-pharmaceutical interventions and     ##
##                                global trade                                ##
##                                                                            ##
####------------------------------------------------------------------------####


#### DEPENDENCIES

library(leaflet)
library(plotly)
library(shiny)
library(shinybusy)



#### IMPORT DATA

main_map <- readRDS("main_map.RDS")
trade_map <- readRDS("trade_map.RDS")
unemployment <- readRDS("npi.RDS")
income <- readRDS("trade.RDS")
labels <- readRDS("labels.RDS")
trade_labels <- readRDS("trade_labels.RDS")
country_names <- readRDS("country_names.RDS")



#### SUPPORT FUNCTIONS

quantile_colors <- function(palette, data) {
  
  colorQuantile(palette, data, n = 6, na.color = "#FFFFFF")
  
}

bin_colors <- function(palette) {
  
  bins <- c(-1, -0.5, -0.25, 0, 0.25, 0.5, Inf)
  colorBin(palette, bins = bins, na.color = "#FFFFFF")
  
}



#### SHINY (UI)

ui <- tags$html(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/styles.css")
  ),
  
  tags$div(
    id = "title", class = "sidemargin",
    tags$div(
      class = "content",
      tags$h1(
        "NPI/Global Trade Visualiser"
      )
    )
  ),
  
  tags$div(
    id = "top", class = "sidemargin",
    tags$div(
      class = "content",
      leafletOutput("map", height = "50em", width = "100%")
    )
  ),
  
  tags$div(
    id = "middle", class = "sidemargin",
    tags$div(
      class = "content",
      tags$h2(
        textOutput("currentCountry")
      ),
      tags$div(
        class = "panel",
        tags$div(
          class = "left",
          "Top partners"
        ),
        tags$div(
          class = "right",
          "Trade flows relative to 2019",
          leafletOutput("tradeMap", height = "25em", width = "100%"),
          tags$div(
            class = "selectors",
            selectInput(
              "year_selector", label = NULL, width = "50%",
              choices = c(2020, 2021), selected = 2020),
            selectInput(
              "flow_selector", label = NULL, width = "50%",
              choices = c("Imports" = "i", "Exports" = "e"), selected = "i")
          )
        )
      )
    )
  ),
  
  tags$div(
    id = "bottom", class = "sidemargin",
    tags$div(
      class = "content",
      tags$h2(
        "Stringency Index"
      ),
      tags$div(
        class = "center",
        tags$div(
          id = "npi", class = "frame",
          "Graph"
        )
      )
    )
  ),
  
  tags$div(
    id = "footer", class = "sidemargin",
    tags$div(
      class = "content",
      tags$div(
        class = "panel",
        tags$div(
          class = "infos left",
          tags$h2(
            "NPI/Global Trade Visualiser"
          ),
          tags$div(
            class = "item",
            "A Shiny app to visualise the stringency of non-pharmaceutical 
            interventions and changes in global trade during the pandemic."
          )
        ),
        tags$div(
          class = "infos center",
          tags$h3(
            "Contact"
          ),
          tags$div(
            class = "item",
            tags$div(
              class = "links",
              tags$a(
                href = "https://github.com/andreas-andersen/NPITradeVisualiser",
                target = "_blank", id = "github",
                tags$img(
                  src = "img/icons8-github.svg", alt = "github", 
                  width = "50", height = "50"
                )
              ),
              tags$a(
                href = "https://www.linkedin.com/in/
                        andreas-makoto-fukuda-andersen/",
                target = "_blank", id = "linkedin",
                tags$img(
                  src = "img/icons8-linkedin.svg", alt = "linkedin", 
                  width = "50", height = "50"
                )
              )
            )
          )
        ),
        tags$div(
          class = "infos right",
          "Placeholder"
        )
      )
    )
  )
)



#### SHINY (SERVER)

server <- function(input, output, session) {
  
  ## Suppress warnings  
  
  storeWarn<- getOption("warn")
  options(warn = -1)
  
  
  ### REACTIVE VALUES
  
  ## Trade map data
  
  filtered_map <- reactiveValues(
    data = trade_map,
    labels = trade_labels[["repcode_i_2020_USA"]]
  )
  
  observeEvent(
    c(
      input$map_shape_click, 
      input$year_selector, 
      input$flow_selector
    ), ignoreInit = TRUE, {
      
      selector <- paste(
        "repcode", 
        selected_flow(),
        selected_year(),
        selected_country()
        , sep = "_"
      )
      filtered_map$data["data"] <- {
        if (selector %in% colnames(trade_map)) {
          trade_map[[selector]]
        } else {
          NA
        }
      }
      filtered_map$labels <- {
        if (selector %in% colnames(trade_map)) {
          trade_labels[[selector]]
        } else {
          NULL
        }
      }
      
    }
  )
  
  
  ### INPUTS
  
  ## Selected country
  
  selected_country <- reactive({
    
    if(is.null(input$map_shape_click) == TRUE) {
      "USA"
    } else {
      input$map_shape_click$id
    }
    
  })
  
  ## Selected year
  
  selected_year <- reactive({
    
    as.numeric(input$year_selector)
    
  })
  
  ## Selected flow
  
  selected_flow <- reactive({
    
    input$flow_selector
    
  })
  

  ### OUPUTS
  
  ## Leaflet NPI output
  
  output$map <- renderLeaflet({
    
    leaflet(
      main_map,
      options = leafletOptions(
        zoomControl = FALSE,
        minZoom = 2,
        maxZoom = 4)
     ) %>%
       setView(0, 30, 2) %>% 
       setMaxBounds(-180, -60, 180, 80) %>% 
      addPolygons(
        color = "#666666", weight = 0.5, opacity = 1,
        fillColor = ~quantile_colors("Reds", mean_si_2020)(mean_si_2020),
        highlightOptions = highlightOptions(
          color = "#000000", weight = 1, bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "400", padding = "2em", width = "20em"),
          textsize = "0.8rem", direction = "auto"),
        layerId = ~ISO3
      )
    
  })
  
  ## Leaflet trade output
  
  output$tradeMap <- renderLeaflet({
    
    leaflet(
      trade_map,
      options = leafletOptions(
        zoomControl = FALSE,
        minZoom = 1,
        maxZoom = 1)
    ) %>%
      setView(0, 30, 1) %>% 
      setMaxBounds(-180, -60, 180, 80)
    
  })
  
  observe({
    
    leafletProxy("tradeMap", data = filtered_map$data) %>%
      clearShapes() %>%
      addPolygons(
        stroke = FALSE,
        fillColor = ~bin_colors("RdBu")(data),
        label = filtered_map$labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "400", padding = "1em"),
          textsize = "0.8rem", direction = "auto"),
      )
    
  })
  
  ## Current country text
  
  output$currentCountry <- renderText({
    
    country_names[selected_country()]
    
  })

}


shinyApp(ui = ui, server)
