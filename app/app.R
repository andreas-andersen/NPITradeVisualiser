####---------------------  NPI/Global Trade Visualiser  --------------------####
##                                                                            ##
##     For visualising stringency of non-pharmaceutical interventions and     ##
##                                global trade                                ##
##                                                                            ##
####------------------------------------------------------------------------####


#### DEPENDENCIES

library(leaflet)
library(plotly)
library(shiny)
library(shinybusy)
library(tidyr)



#### IMPORT DATA

npi_map <- readRDS("npi_map.RDS")
trade_map <- readRDS("trade_map.RDS")
npi <- readRDS("npi.RDS")
trade <- readRDS("trade.RDS")
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

top_plot <- function(df, repcode, flow, year) {
  
  temp <- df[(
    df$repcode == repcode & df$flow == flow & df$year %in% c(2019, year)
  ),]
  
  top <- unique(temp$parcode)[1:7]
  top_temp <- temp[temp$parcode %in% top,] %>% 
    pivot_wider(names_from = year, values_from = value, names_prefix = "year_")
  
  if (flow == "Import") {
    color <-"252, 118, 106"
    xtitle <- "Importers"
  } else {
    color <- "91, 132, 177"
    xtitle <- "Exporters"
  }
  
  year_var <- paste0("year_", year)
  
  fig <- plot_ly(
    top_temp, x = ~parcode, y = top_temp[[year_var]], type = "bar", 
    name = year, text = ~paste0(format(diff * 100, digits = 2), "%"), 
    textposition = "inside",
    marker = list(
      color = paste0("rgba(", color, ", 0.6)"),
      line = list(color = paste0("rgba(", color, ", 1)"), width = 0)),
    hovertemplate = paste0(
      "<b>", top_temp$partner, "</b> (", top_temp$flow, ")<br>",
      "%{yaxis.title.text}: %{y}"
    )
  )
  fig <- fig %>% add_trace(
    y = ~year_2019, name = "2019", textposition = "none",
    marker = list(
      color = paste0("rgba(", color, ", 0)"),
      line = list(color = paste0("rgba(", color, ", 1)"), width = 3))
  )
  fig <- fig %>% layout(
    xaxis = list(
      categoryorder = "array",
      categoryarray = top, title = xtitle),
    yaxis = list(title = "Trade Value (US$)"),
    barmode = "overlay",
    legend = list(
      xanchor = "right", x = 0.95, y = 0.95, 
      bordercolor = "#F9F9F9", borderwidth = 2),
    margin = list(t = 0, b = 35)
  )
  
  return(fig)
  
}

si_plot <- function(df, repcode, ...) {
  
  quad_palette <- c("#FC766A", "#5B84B180", "#B0B8B480", "#184A4580")
  
  temp <- df[(df$country_code %in% c("WLD", repcode, ...)),] %>% 
    pivot_wider(names_from = country_code, values_from = si)
  
  fig <- plot_ly(
    temp, x = ~date, y = ~WLD, type = "scatter", mode = "lines", name = "WLD",
    line = list(color = "rgba(91, 132, 177, 0.5)", width = 2, dash = "dot"),
    hovertemplate = paste0(
      "<b>%{x|%d %B %Y}</b><br>",
      "%{yaxis.title.text}: %{y:.2f}"
    )
  )
  if (length(c(...)) > 0) {
    other_countries <- c(...)
    for (i in 1:length(other_countries)) {
      fig <- fig %>% add_trace(
        y = temp[[other_countries[i]]], name = other_countries[i],
        line = list(color = quad_palette[i+1], width = 2, dash = "solid")
      )
    }
  }
  fig <- fig %>% add_trace(
    y = temp[[repcode]], name = repcode, 
    line = list(color = quad_palette[1], width = 2, dash = "solid")
  )
  
  fig <- fig %>% layout(
    xaxis = list(
      title = "Date", 
      showspikes = TRUE, spikedash = "solid", spikethickness = 1),
    yaxis = list(title = "Stringency Index", range = c(0, 105)),
    legend = list(
      xanchor = "right", x = 0.95, y = 0.95,
      bordercolor = "#F9F9F9", borderwidth = 2),
    margin = list(t = 0, b = 35)
  )
  
  return(fig)
  
}


#### SHINY (UI)

ui <- tags$html(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/styles.css")
  ),
  add_busy_spinner("scaling-squares", color = "#2C3E50",
                   timeout = 1000, position = "full-page", onstart = TRUE),
  
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
      radioButtons(
        "aggSelector", label = NULL, 
        choices = list("Mean SI (2020)" = "mean", "Peak SI (2020)" = "peak"),
        selected = "mean", width = "10em"),
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
          tags$div(
            class = "description",
            "Top trading partners"
          ),
          tags$div(
            class = "partner",
            plotlyOutput(
              "importPartners", height = "23em", width = "100%", inline = TRUE)
          ),
          tags$div(
            class = "partner",
            plotlyOutput(
              "exportPartners", height = "23em", width = "100%", inline = TRUE)
          )
        ),
        tags$div(
          class = "right",
          tags$div(
            class = "description",
            "Trade flows relative to 2019"
          ),
          leafletOutput("tradeMap", height = "25em", width = "100%"),
          tags$div(
            class = "selectors",
            selectInput(
              "yearSelector", label = NULL, width = "50%",
              choices = c(2020, 2021), selected = 2020),
            selectInput(
              "flowSelector", label = NULL, width = "50%",
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
      tags$h3(
        "Stringency Index"
      ),
      tags$div(
        class = "panel",
        tags$div(
          class = "left",
          "Compare SI with:",
          selectizeInput(
            "comparisonSelector", 
            choices = NULL, label = NULL,
            options = list(maxItems = 3, maxOptions = 5))
        ),
        tags$div(
          class = "center",
          tags$div(
            id = "npi",
            plotlyOutput(
              "si", height = "100%", width = "100%", inline = TRUE)
          )
        ),
        tags$div(
          class = "right"
        ),
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
  
  ## NPI map data
  
  filtered_npi_map <- reactiveValues(
    map = npi_map,
  )
  
  observeEvent(
    c(
      input$aggSelector  
    ), ignoreInit = TRUE, {
      
      selector <- paste(
        selected_agg(),
        "si",
        "2020",
        sep = "_"
      )
      filtered_trade_map$npi["npi"] <- npi_map[[selector]]
      
    }
  )
  
  ## Trade map data
  
  filtered_trade_map <- reactiveValues(
    map = trade_map,
    labels = trade_labels[["repcode_i_2020_USA"]]
  )
  
  observeEvent(
    c(
      input$map_shape_click, 
      input$yearSelector, 
      input$flowSelector
    ), ignoreInit = TRUE, {
      
      selector <- paste(
        "repcode", 
        selected_flow(),
        selected_year(),
        selected_country(), 
        sep = "_"
      )
      
      filtered_trade_map$map["data"] <- {
        if (selector %in% colnames(trade_map)) {
          trade_map[[selector]]
        } else {
          NA
        }
      }
      
      filtered_trade_map$labels <- {
        if (selector %in% colnames(trade_map)) {
          trade_labels[[selector]]
        } else {
          NULL
        }
      }
      
    }
  )
  
  ## Possible comparison countries
  
  comparison_countries <- reactiveValues(
    data = country_names[names(country_names) != "USA"]
  )
  
  observeEvent(
    c(
      input$map_shape_click 
    ), ignoreInit = FALSE, ignoreNULL = FALSE, {
      
      comparison_countries$data <- {
        country_names[names(country_names) != selected_country()]
      }
      
      updateSelectizeInput(
        session, "comparisonSelector", 
        choices = as.list(setNames(
          names(comparison_countries$data), comparison_countries$data)), 
        server = TRUE
      )
      
    }
  )
  
  
  ### INPUTS
  
  ## Selected aggregation
  
  selected_agg <- reactive({
    
    input$aggSelector
    
  })
  
  ## Selected country
  
  selected_country <- reactive({
    
    if(is.null(input$map_shape_click) == TRUE) {
      "USA"
    } else {
      input$map_shape_click$id
    }
    
  })
  
  other_countries <- reactive({
    
    country_names[names(country_names) != input$map_shape_click$id]
    
  })
  
  ## Selected year
  
  selected_year <- reactive({
    
    as.numeric(input$yearSelector)
    
  })
  
  ## Selected flow
  
  selected_flow <- reactive({
    
    input$flowSelector
    
  })
  
  ## Selected comparison countries
  
  selected_comparison <- reactive({
    
    input$comparisonSelector
    
  })
  

  ### OUPUTS
  
  ## Leaflet NPI map output
  
  output$map <- renderLeaflet({
    
    leaflet(
      npi_map,
      options = leafletOptions(
        zoomControl = FALSE,
        minZoom = 2,
        maxZoom = 4)
     ) %>%
       setView(0, 30, 2) %>% 
       setMaxBounds(-180, -60, 180, 80)
    
  })
  
  observe({
    
    leafletProxy("map", data = filtered_npi_map$map) %>% 
      addPolygons(
        color = "#666666", weight = 0.5, fillOpacity = 0.5,
        fillColor = ~quantile_colors("Reds", npi)(npi),
        highlightOptions = highlightOptions(
          color = "#000000", weight = 1, bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "400", padding = "2em", width = "20em"),
          textsize = "0.8rem", direction = "auto", sticky = TRUE),
        layerId = ~ISO3
      ) 
    
  })
  
  ## Leaflet trade map output
  
  output$tradeMap <- renderLeaflet({
    
    leaflet(
      trade_map,
      options = leafletOptions(
        zoomControl = FALSE,
        minZoom = 1,
        maxZoom = 4)
    ) %>%
      setView(0, 30, 1) %>% 
      setMaxBounds(-180, -60, 180, 80)
    
  })
  
  observe({
    
    leafletProxy("tradeMap", data = filtered_trade_map$map) %>%
      clearShapes() %>%
      addPolygons(
        stroke = FALSE, fillOpacity = 0.8,
        fillColor = ~bin_colors("RdBu")(trade),
        label = filtered_trade_map$labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "400", padding = "1em"),
          textsize = "0.8rem", direction = "auto")
      )
    
  })
  
  ## Plotly trade partners plots
  
  output$importPartners <- renderPlotly({
    
    top_plot(trade, selected_country(), "Import", selected_year())
    
  })
  
  output$exportPartners <- renderPlotly({
    
    top_plot(trade, selected_country(), "Export", selected_year())
    
  })
  
  ## Plotly Stringency Index plot
  
  output$si <- renderPlotly({
    
    si_plot(npi, selected_country(), selected_comparison())
    
  })
  
  ## Current country text
  
  output$currentCountry <- renderText({
    
    country_names[selected_country()]
    
  })

}


shinyApp(ui = ui, server)
