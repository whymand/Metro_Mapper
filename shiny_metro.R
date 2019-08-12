# README ------------------------------------------------------------------

# KEY FUNCTIONS to be developed

#https://whyman.shinyapps.io/metro_mapper/

# https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/



# 1. (/) File upload: https://shiny.rstudio.com/gallery/file-upload.html

# 2. (/) Choose the variable to map for state color: https://github.com/daattali/colourpicker

# 3. (/) Choose from the color template: https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2

# 4. (/) Save the plot to local

# 5. ( ) MSA bubble maps

# 6. ( ) County maps

# 7. (/) Select donwload option (pdf/png)



# Author: David Whyman

# Date: Thu Jul 26 09:13:31 2018

# --------------

# pkgs <- c('dplyr','maps','mapproj','ggplot2','scales','ggthemes','RColorBrewer','plotly','fiftystater',

# 'shiny','colourpicker','plyr')



# check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)

# if(any(!check)){

#     pkgs.missing <- pkgs[!check]

#     install.packages(pkgs.missing)

#     check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)

#   }



# sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)

# shapefile source: 
#https://evergreen.data.socrata.com/dataset/Cartographic-Boundary-Shapefiles-Metropolitan-And-/w6x3-m3ia
# Read Data ---------------------------------------------------------------


library('RColorBrewer')

library('plotly')

library('shiny')

library('colourpicker')

library("sf")

library("readr")

library("leaflet")

library("tmap")

library("rmapshaper")

library("tidyverse")

library("tigris")

library("grid")

options(shiny.sanitize.errors = TRUE)

#Hawaii, Alaska cbsa codes
HI_AK <- c("46520","25900","28180","27980","11260","21820","27940","28540","28980") 


cbsa50<-read_sf("msas_xsimp") %>%
  dplyr::select(geoid = GEOID,
         name = NAME,
         geometry = geometry) 

cbsa50$geoid<-as.character(cbsa50$geoid)

#load cbsa shapefile, exclude Hawaii & Alaska 
cbsa<-read_sf("cbsa") %>%
  filter(!(geoid %in% HI_AK)) %>%
  dplyr::select(name, everything()) %>%
  ms_simplify()

natbo<-read_sf("states") %>%
  filter(STATE_FIPS != "02") %>%
  filter(STATE_FIPS != "15") %>%  
  ms_simplify()

akbo<-read_sf("states") %>%
  filter(STATE_FIPS == "02") %>%
  ms_simplify()

hibo<-read_sf("states") %>%
  filter(STATE_FIPS == "15") 

usborders<-tm_shape(natbo, projection = 2163) + tm_borders() + tm_layout(frame = FALSE)

borders50<-tm_shape(cbsa50, projection = 2163)+tm_borders() + tm_layout(frame = FALSE)

# Shiny R -----------------------------------------------------------------





ui <- fluidPage(
  
  titlePanel("Metro Mapper"),
  
  
  
  sidebarLayout(
    
    
    
    sidebarPanel(
      
      
      
      helpText("Contact: dwhyman@brookings.edu"),
      
      
      
      fileInput('file1',"Choose CSV File",
                
                accept = c(".csv")),
      
      
      
      actionButton("choice", "Show Data"),
      
      
      
      tags$hr(),
      
      
      
      selectInput("var", "Choose a variable to map",
                  
                  choices = NULL),
      
      conditionalPanel(condition = "input.bubbs == 'low'",
                       
                       selectInput("var2", "Choose a variable to map (bubble size)",
                                   
                                   choices = NULL)
      ),
      
      
      
      tags$hr(),
      
      
      
      radioButtons("bubbs", "map resolution (decrease for faster load)", choices = c("high", "medium","low")),
      
      
      
      checkboxInput("scale", "Custom scale breaks", FALSE),
      
      conditionalPanel(condition = "input.scale == true",
                       
                       textInput("breaks","Enter scale breaks (separate by commas)", NULL)
      ),
      
      conditionalPanel("input.scale == false",
                       
                       radioButtons("style", "Scaling", choices = c("Continuous" = "cont", "Categorical" = "pretty"))
      ),
      
      
      checkboxInput("filt", "1 Row is year X metro area", FALSE),
      
      checkboxInput("hiak", "Include Hawaii & Alaska", FALSE),
      
      conditionalPanel("input.filt == true",
                       
                       textInput("year","Year")
      ),      
      
      colourInput("low", "Choose a color for low value","#deebf7"),
      
      colourInput("high", "Choose a color for high value", "#08519c"),
      
      
      plotOutput("histo",width = "75%", height = "200px"),
      
      
      tags$hr(),
      
      
      downloadButton("plot", label = "Download the map"),
      
      radioButtons("filetype", "File type:", choices = c("png", "pdf", "html")),      
      
      textInput("legen", label = "Legend title"), 
      
      downloadButton("report", label = "Generate pdf report"),
      
      textInput("title", label = "Report title"), 
      
      textInput("subtitle", label = "Report subtitle"), 
      
      textInput("source", label = "Report source"), 
      
      textInput("notes", label = "Report notes"),
      
      downloadButton("code", label = "Download the code")      
      
    ),
    
    
    
    mainPanel(
      
      tableOutput("contents"),
      
      leafletOutput("map"))
    
    
    
  )
  
)



# Server logic ----

server <- function(input, output,session) {
  
  
  
  info <- eventReactive(input$choice,{
    
    req(input$file1)
    
    
    
    df <- read_csv(input$file1$datapath, col_types = cols(cbsa_code = col_character()))
    
    df$cbsa_code <-str_pad(df$cbsa_code, width=5, side="left", pad="0")  
    
    vars <- names(df[-c(1,2)])
    updateSelectInput(session,"var",'Choose a variable to map', choices = vars)
    updateSelectInput(session,"var2",'Choose a variable to map (bubble size)', choices = c(vars,as.numeric(0.1)))
    
    
    df
    
  })
  
  
  info2 <- reactive({
    info1<-info()
    
    info1 %>% dplyr::select(cbsa_code, dplyr::everything())
    
    
    
    
  })
  
  input_data1 <- reactive({
    
    input_data <- info2()
    
    
    #join shapefile with input_data to plug into map
    if (input$hiak == FALSE)
    {inner_join(cbsa,input_data, by = c("geoid" = "cbsa_code"))}
    else
    {inner_join(cbsa50,input_data, by = c("geoid" = "cbsa_code"))}
    
  })
  

  
  
  
  lower48 <- reactive({
    usborders + tm_shape(input_data1(), projection = 2163) + tmapper() + tm_layout(
      legend.position = c("LEFT","BOTTOM"),
      legend.outside = FALSE,
      legend.title.size = .0001,
      title.position = c("LEFT", "BOTTOM"),
      title = input$title,
      title.size = 1.5,
      fontfamily = "serif") 
  })
  
  
  reactive({
    
    req(input$filt == TRUE)
    
    req(input$year)
    
    input_data<-filter(input_data, year == input$year)   
    
  })
  
  
  
  output$contents <- renderTable({
    
    
    
    display_data <- info()
    
    
    head(display_data, 4L)
    
    
    
  })
  
  
  output$histo = renderPlot({
    
    input_data <- info()
    
    if(is.numeric(input_data[[input$var]])){
      hist(input_data[[input$var]],
           main = input$vars,
           xlab = "",
           freq = TRUE,
           breaks = 100)
    }
    
  })
  
  output$map = renderLeaflet({
    
    req(input$var)    
    
    lower_48<-lower48()
    
  
    tmap_leaflet(lower_48)
  
    
    
  })
  
  
  tmapper <- function(...){
    
    req(input$var, cancelOutput = TRUE)
    req(input$bubbs, cancelOutput = TRUE)
    
    input_data2<-input_data1()
    
    if (input$bubbs == "low") 
    {
      req(input$var2)
      
      if(input$scale == TRUE)
      { req(input$breaks, cancelOutput = TRUE)
        
        
        tm_bubbles(col = input$var, 
                   size = input$var2, 
                   palette = c(input$low, input$high),
                   breaks = as.numeric(unlist(strsplit(input$breaks,","))),
                   popup.vars=c(input$var, input$var2, "name"),
                   popup.format = list(text.align = "left", format = "f", digits = 3)
                   
        )
      } else {
        
        tm_bubbles(col = input$var, 
                   size = input$var2, 
                   palette = c(input$low, input$high),
                   style = input$style,
                   popup.vars=c(input$var, input$var2, "name"),
                   popup.format = list(text.align = "left", format = "f", digits = 3)
                   
        )     
        
        
      }
      
      
    } else {
      
      
      if(input$scale == TRUE)
      { req(input$breaks, cancelOutput = TRUE)
        
        
        tm_polygons(input$var, 
                    palette = c(input$low, input$high),
                    breaks = as.numeric(unlist(strsplit(input$breaks,","))),
                    popup.vars=c(input$var, "name"),
                    popup.format = list(text.align = "left", format = "f", digits = 3),
                    ...
        )
      } else {
        
        tm_polygons(input$var, 
                    
                    palette = c(input$low, input$high),
                    style = input$style,
                    popup.vars=c(input$var, "name"),
                    popup.format = list(text.align = "left", format = "f", digits = 3),
                    ...
        )     
        
        
      }
    }
    
    
    
    
    
    
  }
  
  
  
  
  output$plot <- downloadHandler(
    
    
    filename = function(){
      
      paste("plot", input$filetype, sep = ".")
      
    },
    
    content = function(file){
      
      tmap_save(usborders + tm_shape(input_data1(), projection = 2163) +
                  tmapper() + 
                  tm_layout(
                    legend.position = c("LEFT","BOTTOM"),
                    legend.outside = FALSE,
                    legend.title.size = .0001,
                    title.position = c("LEFT", "BOTTOM"),
                    title = input$title,
                    title.size = 3,
                    legend.text.size = 1.5,
                    fontfamily = "serif"), 
                
                file, 
                width = 16, 
                height = 10.4)
      
    }
  )
  
  
  
  
  
  output$report <- downloadHandler(
    filename = "report.html",
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      
      rmarkdown::render('report.Rmd', output_file = file)
      
    }
  )
  
  
  output$code <- downloadHandler(
    filename = "metro_coder.html",
    
    content = function(file) {
      sroc <- normalizePath('metro_coder.Rmd')
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(sroc, 'metro_coder.Rmd', overwrite = TRUE)
      
      
      rmarkdown::render('metro_coder.Rmd', output_file = file)
      
    }
  )
  
}

# Run app ----

shinyApp(ui, server)


