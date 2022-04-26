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
library(tidyr)
library(sf)



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
  
  temp <- df[(df$flow == flow & df$year %in% c(2019, year)),]
  
  if (nrow(temp) == 0) {
    return(NULL)
  }
  
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
      "<b>", top_temp$partner, "</b> (", top_temp$flow, "s)<br>",
      "%{y} US$"
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
    margin = list(t = 0, b = 45)
  )
  
  return(fig)
  
}



#### SHINY (UI)

ui <- tags$html(
  tags$head(
    tags$link(
      rel = "stylesheet", type = "text/css", href = "styles/styles.css"),
    tags$title("NPI Global Trade Visualiser")
  ),
  shinybusy::add_busy_spinner("scaling-squares", color = "#2C3E50",
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
      tags$div(
        class = "title",
        tags$h3(
          textOutput("currentTitle") 
        )
      ),
      leafletOutput("map", height = "50em", width = "100%"),
      tags$hr(),
      tags$div(
        class = "note",
        tags$span(class = "italic", "Note: "),
        tags$span(
          textOutput("currentAgg", inline = TRUE), 
          " Stringency Index in 2020, reported by ",
          tags$a(
            href = paste0(
              "https://www.bsg.ox.ac.uk/research/research-projects",
              "/covid-19-government-response-tracker"),
            target = "_blank",
            "Oxford's COVID-19 Government Response Tracker."
          )
        ),
        tags$span(
          "Map data from ",
          tags$a(
            href = paste0(
              "https://public.opendatasoft.com/explore/dataset",
              "/world-administrative-boundaries/api/"),
            target = "_blank",
            "opendatasoft."
          )
        )
      )
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
            textOutput("currentPartners")
          ),
          tags$div(
            class = "partner",
            plotlyOutput(
              "importPartners", height = "22em", width = "100%", inline = TRUE)
          ),
          tags$div(
            class = "partner",
            plotlyOutput(
              "exportPartners", height = "22em", width = "100%", inline = TRUE)
          )
        ),
        tags$div(
          class = "right",
          tags$div(
            class = "description",
            textOutput("currentPartnersFlows")
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
      ),
      tags$div(
        class = "note",
        tags$span(class = "italic", "Note: "),
        tags$span(
          "Annual trade data (2019 - 2021) from the ",
          tags$a(
            href = "https://comtrade.un.org/",
            target = "_blank",
            "Comtrade database"
          ),
          " downloaded using ",
          tags$a(
            href = (
              "https://github.com/andreas-andersen/ComtradeDatabaseDownloader"),
            target = "_blank",
            "ComtradeDatabaseDownloader"
          ),
          "."
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
          tags$div(
            class = "description",
            "Compare ",
            textOutput("currentCountryCode", inline = TRUE),
            " SI with:"
          ),
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
              "si", height = "100%", width = "100%", inline = TRUE),
            tags$div(
              class = "note",
              tags$span(class = "italic", "Note: "),
              tags$span(
                "Time series of Stringency Index (2020 - 2021) from ",
                tags$a(
                  href = paste0(
                    "https://www.bsg.ox.ac.uk/research/research-projects",
                    "/covid-19-government-response-tracker"),
                  target = "_blank",
                  "Oxford's COVID-19 Government Response Tracker."
                )
              )
            )
          ),
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
          tags$h3(
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
      filtered_npi_map$map["npi"] <- npi_map[[selector]]
      
    }
  )
  
  ## Trade map data
  
  filtered_trade <- reactiveValues(
    map = trade_map,
    na_indicator = FALSE,
    labels = trade_labels[["i_2020_USA"]],
    top = trade[trade$repcode == "USA",]
  )
  
  observeEvent(
    c(
      input$map_shape_click, 
      input$yearSelector, 
      input$flowSelector
    ), ignoreInit = TRUE, {
      
      selector <- paste(
        selected_flow(),
        selected_year(),
        selected_country(), 
        sep = "_"
      )
      
      filtered_trade$map["trade"] <- {
        if (selector %in% colnames(trade_map)) {
          trade_map[[selector]]
        } else {
          NA
        }
      }
      
      filtered_trade$na_indicator <- {
        if (selector %in% colnames(trade_map)) {
          FALSE
        } else {
          TRUE
        }
      }
      
      filtered_trade$labels <- {
        if (selector %in% colnames(trade_map)) {
          trade_labels[[selector]]
        } else {
          NULL
        }
      }
      
      filtered_trade$top <- {
        trade[trade$repcode == selected_country(),]
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
          color = "#222222", weight = 2, bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "400", padding = "2em", width = "20em"),
          textsize = "0.8rem", direction = "auto", sticky = TRUE),
        layerId = ~iso3) 
    
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
    
    leafletProxy("tradeMap", data = filtered_trade$map) %>%
      clearShapes() %>%
      addPolygons(
        stroke = FALSE, fillOpacity = 0.8,
        fillColor = ~bin_colors("RdBu")(trade),
        label = filtered_trade$labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "400", padding = "1em"),
          textsize = "0.8rem", direction = "auto"))
    
  })
  
  ## Plotly trade partners plots
  
  output$importPartners <- renderPlotly({
    
    top_plot(filtered_trade$top, selected_country(), "Import", selected_year())
    
  })
  
  output$exportPartners <- renderPlotly({
    
    top_plot(filtered_trade$top, selected_country(), "Export", selected_year())
    
  })
  
  ## Plotly Stringency Index plot
  
  output$si <- renderPlotly({
    
    si_plot(npi, selected_country(), selected_comparison())
    
  })
  
  ## Current title text
  
  output$currentTitle <- renderText({
    
    paste0(
      tools::toTitleCase(selected_agg()),
      " Stringency Index (2020)"
    )
    
  })
  
  ## Current country text
  
  output$currentCountry <- renderText({
    
    country_names[selected_country()]
    
  })
  
  ## Current country code text
  
  output$currentCountryCode <- renderText({
    
    selected_country()
    
  })
  
  ## Current aggregation text
  
  output$currentAgg <- renderText({
    
    tools::toTitleCase(selected_agg())
    
  })
  
  ## Current flow text
  
  output$currentFlow <- renderText({
    

    
  })
  
  ## Current year text
  
  output$currentYear <- renderText({
    
    selected_year()
    
  })
  
  ## Current trading partners text
  
  output$currentPartners <- renderText({
    
    if (nrow(filtered_trade$top) == 0) {
      paste0(
        "No trade data found for ",
        selected_country()
      )
    } else if (filtered_trade$na_indicator) {
      paste0(
        "No trade data found for ",
        selected_country(),
        " in ",
        selected_year()
      )
    } else {
      paste0(
        "Change in trade flows of top trading partners of ",
        selected_country(),
        " relative to 2019"
      )
    }
    
  })
  
  ## Current trading partners flow text
  
  output$currentPartnersFlows <- renderText({
    
    if (is.na(filtered_trade$map$trade)) {
      ""
    } else {
      paste0(
        if (selected_flow() == "i") {
          "Import flows to "
        } else {
          "Export flows from "
        },
        selected_country(),
        " in ",
        selected_year(),
        " relative to 2019"
      )
    }
    
  })

}



shinyApp(ui = ui, server)
