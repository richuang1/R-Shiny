library(shiny)
library(leaflet)
library(DBI) # for SQL database connection and queries
library(odbc)
library(RColorBrewer) # for map color pallettes
library(rgdal) # for reading in the shapefile
library(dplyr) # for data summary/transformation
library(ggplot2)
library(plotly)
library(DT)

# ---------------------------------------------------------------
# ---------------------------------------------------------------
# user interface set up

ui <- navbarPage(title = "Future Water Indiana", id = "nav", 
                 
           tabPanel("Interactive map", 
                    fluidRow(
                      column(3, 
                             # input: select variable to map
                             selectInput("map.var", "Variable:", 
                                         c("Precipitation" = "_precip",
                                           "Evapotranspiration" = "_et",
                                           "Soil water content" = "_sw",
                                           "Groundwater Recharge" = "_perc", 
                                           "Baseflow" = "_gw_q",
                                           "Streamflow" = "_flow_out",
                                           "Water Yield" = "_wyld"))
                      ),
                      column(3, 
                             # input: select time period
                             selectInput("map.stype", "Summary Period:", 
                                         c("Annual" = "ann",
                                           "January" = 1,
                                           "February" = 2,
                                           "March" = 3, 
                                           "April" = 4,
                                           "May" = 5,
                                           "June" = 6,
                                           "July" = 7,
                                           "August" = 8,
                                           "September" = 9,
                                           "October" = 10,
                                           "November" = 11,
                                           "December" = 12))
                      ),
                      column(3, 
                             # input: select time period
                             selectInput("map.period", "Time Period:", 
                                         c("2020s" = "2020", 
                                           "2050s" = "2050",
                                           "2080s" = "2080"))
                      ),

                      column(3, 
                             selectInput("map.rcp", "Emissions Scenario:", 
                                                          c("Medium" = "45",
                                                            "High" = "85"))
                             )
                      ),
                    hr(),
                    leafletOutput("map", height = 500),
                    plotlyOutput("map.plot")
                    ), # end of interactive map panel      
                 
           tabPanel("Interactive Plot", 
                    fluidRow(
                      column(3, 
                             # input: select variable to map
                             selectInput("plot.var", "Variable:", 
                                         c("Precipitation" = "_precip",
                                           "Evapotranspiration" = "_et",
                                           "Soil water content" = "_sw",
                                           "Groundwater Recharge" = "_perc", 
                                           "Baseflow" = "_gw_q",
                                           "Streamflow" = "_flow_out",
                                           "Water Yield" = "_wyld"))
                      ),
                      column(3, 
                             # input: select variable to map
                             selectInput("plot.type", "Plot Type:", 
                                         c("Annual Change (%)" = "annual",
                                           "Monthly Change (%)" = "monthly"))
                      )
                      ), 
                    hr(),
                    
                    plotlyOutput("plot")
                      
                    ), # end of interactive plot panel
           
           tabPanel("Data Download", 
                    fluidRow(
                      column(3, 
                           # input: select variable to map
                           selectInput("table.var", "Variable:", 
                                       c("Precipitation" = "_precip",
                                         "Evapotranspiration" = "_et",
                                         "Soil water content" = "_sw",
                                         "Groundwater Recharge" = "_perc", 
                                         "Baseflow" = "_gw_q",
                                         "Streamflow" = "_flow_out",
                                         "Water Yield" = "_wyld"))
                             ),
                      column(3, 
                             # input: select time period
                             selectInput("table.period", "Time Period:", 
                                         c("Historical" = "1980", 
                                           "2020s" = "2020", 
                                           "2050s" = "2050",
                                           "2080s" = "2080"))
                      ),
                      column(3, 
                             # input: select time period
                             selectInput("table.stype", "Summary Period:", 
                                         c("Annual" = "ann", 
                                           "Monthly" = "month"))
                      ),
                      column(3, 
                             conditionalPanel("input.table.period != '1980'", 
                                              # only prompt for rcp if a future period (not historical)
                                              # input: select rcp
                                              selectInput("table.rcp", "Emissions Scenario:", 
                                                          c("Medium" = "45",
                                                            "High" = "85")),
                                              downloadButton('downloadData', 'Download')
                                              )
                             
                      )

                    ),
                    hr(),
                    DT::dataTableOutput("querytable")
                    
                    ) # end of data download user interface setup
           
        ) # end of user-inferface setup


# load the data - shapefile for mapping, SQL database connection, annual and monthly plot .r codes

# shapefile for subbasin boundaries
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
basins <- readOGR(dsn = "C:/Users/lomei/Downloads/simpleshp/simpleshp_03.shp", stringsAsFactors = F) # !!!! change this file path
# ---------------------------------------------------------------------------------------------------------------------------------------
basins@data$id <- as.numeric(basins@data$Subbasin)

# connect to MySQL database
db <- dbConnect(odbc(), 
                Driver = "MySQL ODBC 8.0 Unicode Driver",
                Server = "sasrdsmp01.uits.iu.edu", 
                Database = "pfechyd_swat", 
                user = "pfechyd_read", 
                password = "WabashBasin123!#", 
                port = 3306)

# driver = MySQL ODBC 8.0 Unicode Driver
# driver and R version must be 32-bit


# get column names for monthly and annual tables
columns.month <- dbGetQuery(db, "SELECT column_name FROM information_schema.columns WHERE table_name = 'hydro_month'")
columns.annual <- dbGetQuery(db, "SELECT column_name FROM information_schema.columns WHERE table_name = 'hydro_ann'")

# change from factor to character
columns.annual <- as.character(columns.annual$column_name)
columns.month <- as.character(columns.month$column_name)

# gcm lookup table for plot pop-ups
gcm.lu <- dbGetQuery(db, "SELECT gcm_id, gcm_name FROM gcm")


# lookup table for input$var - used to grab labels for map legend and pop-ups
labels <- data.frame(input.var = c("_precip",
                                   "_et",
                                   "_sw",
                                   "_perc", 
                                   "_gw_q",
                                   "_flow_out",
                                   "_wyld"), 
                     label = c("% Change Precip",
                               "% Change ET",
                               "% Change Soil water",
                               "% Change GW Recharge",
                               "% Change Baseflow",
                               "% Change Streamflow",
                               "% Change Water Yield"))


server <- function(input, output) {

  
  # generate the map
  activeSubbasin <- reactiveVal()
  
  output$map <- renderLeaflet({
    
    # build the SQL query from the user selections
    if (input$map.stype == "ann") {
      col.name <- columns.annual[grep(input$map.var, columns.annual)]
      proj.query <- paste0("SELECT ", col.name, ", subbasin FROM hydro_ann WHERE (period = ", input$map.period,
                           ") AND (rcp = ", input$map.rcp, ")")
      hist.query <- paste0("SELECT ", col.name, ", subbasin FROM hydro_ann WHERE (period = 1980)")
    } else {
      col.name <- columns.month[grep(input$map.var, columns.month)]
      proj.query <- paste0("SELECT ", col.name, ", subbasin FROM hydro_month WHERE (period = ", input$map.period,
                           ") AND (rcp = ", input$map.rcp, ") AND (calendar_month = ", 
                           input$map.stype, ")")
      hist.query <- paste0("SELECT ", col.name, ", subbasin FROM hydro_month WHERE (period = 1980) AND (calendar_month = ", 
                           input$map.stype, ")")
    }
    
    # query the database
    dat.proj <- dbGetQuery(db, proj.query)
    dat.hist <- dbGetQuery(db, hist.query)
    
    # rename the columns for use with different variables - enables the following code to be generic (for any variable)
    colnames(dat.proj) <- c("value", "subbasin")
    colnames(dat.hist) <- c("value", "subbasin")
    
    # calculate the mean value by subbasin for the 10-member gcm ensemble
    dat.proj.mean <- tapply(dat.proj$value, dat.proj$subbasin, mean)
    
    # calculate the percent change relative to historical
    pct.change <- ((dat.proj.mean - dat.hist$value) / dat.hist$value) * 100
    
    # generate a color pallette from reactive expression output
    mbreaks <- c(0, quantile(abs(pct.change), c(0.20, 0.4, 0.6, 0.8), na.rm = T), max(abs(pct.change), na.rm = T))
    mbreaks <- ceiling(mbreaks)
    mbreaks <- unique(c(rev(-1 * mbreaks), mbreaks))
    pal <- colorBin(palette = "RdBu", domain = pct.change, bins = mbreaks)
    
    id <- as.vector(basins$id)
    
    leaflet() %>% 
      addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>% 
      addPolygons(data = basins, layerId = id, stroke = T, color = "black", 
                  smoothFactor = 0.2, weight = 1, 
                  fillColor = ~pal(pct.change), fillOpacity = 0.75, 
                  popup = paste0(labels$label[grep(input$map.var, labels$input.var)], ": ", round(pct.change, 1)), 
                  highlight = highlightOptions(weight = 2.5, fillOpacity = 1, bringToFront = T)) %>% 
      addLegend("bottomleft", pal = pal, values = pct.change,
                title = labels$label[grep(input$map.var, labels$input.var)],
                opacity = 0.75)
    
  })
  

  
  observeEvent(input$map_shape_click, { # update the location selectInput on map clicks
    p <- input$map_shape_click
    activeSubbasin(p$id)
    print(activeSubbasin)
    str(activeSubbasin)
    })
  
  output$map.plot <- renderPlotly({
    
    if (is.integer(activeSubbasin())) {
      # build query for annual values
      col.name.ann <- columns.annual[grep(input$map.var, columns.annual)]
      query.annual <- paste0("SELECT ", col.name.ann, ", rcp, period, gcm_id, subbasin FROM hydro_ann WHERE subbasin = ", activeSubbasin())
      
      # query the database
      annual <- dbGetQuery(db, query.annual)
      
      # change column name to be consistent between different variable choices - same as with map - allows following code to be generic
      colnames(annual)[colnames(annual) == col.name.ann] <- "value"
      
      # further summary of the annual data - mean annual value across all subbasins
      annual <- annual %>% group_by(rcp, period, gcm_id) %>% 
        summarize(mean = mean(value)) %>% 
        arrange(gcm_id) # sort so that the historical value (gcm_id 11) is last
      
      periods <- annual$period[c(1:60)]
      periods <- paste0(periods, "s")
      rcps <- annual$rcp[c(1:60)]
      rcps[rcps == "45"] <- "4.5"
      rcps[rcps == "85"] <- "8.5"
      period.rcp <- paste(periods, rcps, sep = "-")
      
      # create an ordered factor so things plot in desired order
      period.rcp <- factor(period.rcp, levels = unique(period.rcp[order(periods, rcps)]), ordered = T)
      
      # calculate percent change from historical
      dat.pct <- annual$mean[c(1:60)]
      dat.pct <- ((dat.pct - annual$mean[61]) / annual$mean[61]) * 100
      
      dat <- data.frame(pct.change = round(dat.pct, 1), period.rcp = period.rcp, gcm_id = annual$gcm_id[c(1:60)], 
                        period = periods, rcp = rcps)
      
      # calculate mean values to add as another point layer
      dat.mean <- dat %>% group_by(period.rcp) %>% summarize(pct.change = round(mean(pct.change), 1), 
                                                             period = unique(period), 
                                                             rcp = unique(rcp))
      
      dat <- inner_join(dat, gcm.lu, by = 'gcm_id')
      dat$GCM <- as.factor(dat$gcm_name)
      
      # set size based on current map selection
      selected.period.rcp <- paste0(input$map.period, "s-", ifelse(input$map.rcp == "45", "4.5", "8.5"))
      dat$size <- 1.75
      dat$size[dat$period.rcp == selected.period.rcp] <- 2
      dat$alpha <- 0.4
      dat$alpha[dat$period.rcp == selected.period.rcp] <- 1
      dat.mean$size <- 3
      dat.mean$size[dat.mean$period.rcp == selected.period.rcp] <- 3.5
      dat.mean$alpha <- 0.4
      dat.mean$alpha[dat.mean$period.rcp == selected.period.rcp] <- 1
      
      
      p2 <- ggplot(dat, aes(period.rcp, pct.change)) + 
        geom_point(aes(color = dat$GCM, 
                       text = paste("% Change: ", dat$pct.change, 
                                    "<br>GCM: ", dat$GCM, 
                                    "<br>Period: ", dat$period, 
                                    "<br>RCP: ", dat$rcp)), 
                   size = dat$size, alpha = dat$alpha) +
        geom_point(data = dat.mean, aes(text = paste("% Change (Ensemble Mean): ", dat.mean$pct.change,  
                                                     "<br>Period: ", dat.mean$period, 
                                                     "<br>RCP: ", dat.mean$rcp)), 
                   shape = 22, size = dat.mean$size, color = "black", fill = NA, alpha = dat.mean$alpha) +
        geom_hline(yintercept = 0, linetype = "dashed") + 
        theme_classic() + 
        labs(color = "GCM:") + 
        ylab("Percent change - annual") + 
        xlab("") 
      
      ggplotly(p2, tooltip = "text")
    }
    
  })
  

  # generate a plot of the data
  output$plot <- renderPlotly({
    
    
    # build query for annual values
    col.name.ann <- columns.annual[grep(input$plot.var, columns.annual)]
    query.annual <- paste0("SELECT ", col.name.ann, ", rcp, period, gcm_id, subbasin FROM hydro_ann")
    
    # build query for monthly values
    # col.name.monthly <- columns.month[grep(input$plot.var, columns.month)]
    # query.monthly <- paste0("SELECT ", col.name.monthly, 
    #                        ", rcp, period, calendar_month, gcm_id, subbasin FROM hydro_month")
    
    # query the database
    annual <- dbGetQuery(db, query.annual)
    # monthly <- dbGetQuery(db, query.monthly)
    
    # change column name to be consistent between different variable choices - same as with map - allows following code to be generic
    colnames(annual)[colnames(annual) == col.name.ann] <- "value"
    # colnames(monthly)[colnames(monthly) == col.name.monthly] <- "value"
    
    # further summary of the annual data - mean annual value across all subbasins
    # the plot_annual_gateway function needs a vector of length 61 - mean annual value for each gcm-rcp combination
    annual <- annual %>% group_by(rcp, period, gcm_id) %>% 
      summarize(mean = mean(value)) %>% 
      arrange(gcm_id) # sort so that the historical value (gcm_id 11) is last
    
   
    periods <- annual$period[c(1:60)]
    periods <- paste0(periods, "s")
    rcps <- annual$rcp[c(1:60)]
    rcps[rcps == "45"] <- "4.5"
    rcps[rcps == "85"] <- "8.5"
    period.rcp <- paste(periods, rcps, sep = "-")
    
    # create an ordered factor so things plot in desired order
    period.rcp <- factor(period.rcp, levels = unique(period.rcp[order(periods, rcps)]), ordered = T)
    
    
    # calculate percent change from historical
    dat.pct <- annual$mean[c(1:60)]
    dat.pct <- ((dat.pct - annual$mean[61]) / annual$mean[61]) * 100
    
    dat <- data.frame(pct.change = round(dat.pct, 1), period.rcp = period.rcp, gcm_id = annual$gcm_id[c(1:60)], 
                      period = periods, rcp = rcps)
    # calculate mean values to add as another point layer
    dat.mean <- dat %>% group_by(period.rcp) %>% summarize(pct.change = round(mean(pct.change), 1), 
                                                           period = unique(period), 
                                                           rcp = unique(rcp))
    
    dat <- inner_join(dat, gcm.lu, by = 'gcm_id')
    dat$GCM <- as.factor(dat$gcm_name)
    p1 <- ggplot(dat, aes(period.rcp, pct.change)) + 
      geom_point(aes(color = dat$GCM, 
                     text = paste("% Change: ", dat$pct.change, 
                                  "<br>GCM: ", dat$GCM, 
                                  "<br>Period: ", dat$period, 
                                  "<br>RCP: ", dat$rcp))) +
      geom_point(data = dat.mean, aes(text = paste("% Change (Ensemble Mean): ", dat.mean$pct.change,  
                                                   "<br>Period: ", dat.mean$period, 
                                                   "<br>RCP: ", dat.mean$rcp)), 
                 shape = 22, color = "black", fill = NA) +
      geom_hline(yintercept = 0, linetype = "dashed") + 
      theme_classic() + 
      labs(color = "GCM:") + 
      ylab("Percent change - annual") + 
      xlab("") 
    
    ggplotly(p1, tooltip = "text")
    
  })
  
  # data download and table ------------------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------------------------------------------
  # download capability needs to be added ----------------------------------------------------------------------------
  output$querytable <- renderDataTable({
    
    # build query based on user-selections
    if (input$table.stype == "ann") {
      col.name <- columns.annual[grep(input$table.var, columns.annual)]
      if (input$table.period == "1980") {
        query <- paste0("SELECT ", col.name, ", subbasin, gcm_id FROM hydro_ann WHERE (period = ", input$table.period,
                        ")")
      } else {
        query <- paste0("SELECT ", col.name, ", subbasin, gcm_id FROM hydro_ann WHERE (period = ", input$table.period,
                        ") AND (rcp = ", input$table.rcp, ")")
      }
    } else {
      col.name <- columns.month[grep(input$table.var, columns.month)]
      if (input$table.period == "1980") {
        query <- paste0("SELECT ", col.name, ", subbasin, gcm_id, calendar_month FROM hydro_month WHERE (period = ", input$table.period,
                        ")")
      } else {
        query <- paste0("SELECT ", col.name, ", subbasin, gcm_id, calendar_month FROM hydro_month WHERE (period = ", input$table.period,
                        ") AND (rcp = ", input$table.rcp, ")")
      }
    }

    
    df <- dbGetQuery(db, query)
    
    DT::datatable(df)

    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("QueriedData", "csv", sep = ".")
    },
    content = function(file) {
      write.csv(output$querytable, file, sep = sep,
                row.names = FALSE)
    }
  )
  }
  




# create shiny app
shinyApp(ui, server)