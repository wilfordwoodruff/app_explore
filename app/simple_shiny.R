library(shiny)
library(tidyverse)
library(leaflet)
library(DT)
library(plotly)

#downloaded for faster testing, use
# https://github.com/wilfordwoodruff/Main-Data/raw/371f9cda2709a10c38735c5e7b5486384ebb3f65/data/derived/derived_data.csv
# C:/Users/spenc/Downloads/derived_data.csv
writings <- read_csv("https://github.com/wilfordwoodruff/Main-Data/raw/371f9cda2709a10c38735c5e7b5486384ebb3f65/data/derived/derived_data.csv") %>%
  mutate(`First Date` = ymd(ifelse(is.na(`First Date`),substr(Dates,0,10),`First Date`)),
         `Text Only Transcript` = str_to_lower(`Text Only Transcript`))

floor_decade <- function(value){ return(value - value %% 10) }

#C:/Users/spenc/Documents/GitHub/app_explore/Clara_for_rshiny.csv
clara_compiled <- read_csv("https://raw.githubusercontent.com/wilfordwoodruff/app_explore/main/Clara_for_rshiny.csv")



# Define UI for application that draws a histogram
ui <- function(request) {
  fluidPage(
    tags$head(
      tags$script(
        HTML('
        document.title = "Wilford Woodruff Exploration";
      ')
      )
    ),
    
  #theme = bslib::bs_theme(bootswatch = "united"),
  # Application title
  titlePanel(img(src="https://wilfordwoodruffpapers.org/img/image-logo.png",
                 width="600px",height="auto")),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      htmlOutput("Welcome"),
      
      dateRangeInput(inputId = "startEndDate",
                     label="Writing Period (type manually or use calendar)",
                     start=min(writings$`First Date`,na.rm=TRUE),
                     end=max(writings$`First Date`,na.rm=TRUE),
                     separator='to',
                     min=min(writings$`First Date`,na.rm=TRUE),
                     max=max(writings$`First Date`,na.rm=TRUE),
                     startview = 'decade',
                     format='M dd, yyyy (D)'
      ),
      checkboxGroupInput(inputId = 'journal_type',
                         label = 'Types of Writings',
                         choices = unique(writings$`Document Type`),
                         selected = unique(writings$`Document Type`)
                         
      ),
      textInput(inputId='word_search',
                label='Search for a Specific Word',
                placeholder= "e.g. Utah"),
      htmlOutput("results")
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Getting Started",
                 htmlOutput("start_tab")
        ),
        tabPanel("Map of Locations",
                 htmlOutput("map_tab_describe"),
                 leafletOutput("claramap"),
                 bookmarkButton()
                 #Add download button for this
        ),
        
        tabPanel("Frequency & Usage",
                 htmlOutput("time_tab_describe"),
                 fluidRow(
                   splitLayout(cellWidths=c("50%","50%"),
                               plotlyOutput("entry_frequency_map"),plotlyOutput("word_frequency_map"))
                 ),
                 bookmarkButton()
        ),
        tabPanel("See Originals",
                 htmlOutput("table_tab_describe"),
                 DTOutput("fiverows"),
                 bookmarkButton()
                 #Add download for this
        )
        
        
      )
    )
  )
)}

server <- function(input, output) {
  
  #Collect the values that will update over time
    selections <- reactiveValues()
    observe({
    selections$filtered_writing <- writings %>%
      mutate(`Word Count`=str_count(`Text Only Transcript`,str_to_lower(input$word_search))) %>%
      filter(`Word Count` > 0 & `Document Type` %in% input$journal_type &
               `First Date` > input$startEndDate[1] & `First Date` < input$startEndDate[2])

#COUNT OF WORDS & APPEARANCES----------
    selections$all_decades <- year(selections$filtered_writing$`First Date`) %/% 10 * 10
    
    #The X-Axis for the graphs
    selections$decade_span <- ifelse(length(selections$all_decades)>0,
                                     seq(min(selections$all_decades), max(selections$all_decades), by = 10),
                                     seq(1800,1890,by=10))
    selections$freq_data_decade <- selections$filtered_writing %>%
      mutate(Decade = selections$all_decades) %>% 
      group_by(Decade) %>%
      summarise(Count_Words = sum(`Word Count`),
                Count_Entries = n()) %>%
      #If a decade has 0 entries, make sure it still gets put in the chart
      complete(Decade = selections$decade_span,
               fill=list(Count_Words=0,Count_Entries=0))
    
#MAP STUFF-------------
    selections$map_data <- clara_compiled %>%
      filter(short_url %in% selections$filtered_writing$`Short URL`) %>%
      group_by(lat,lng) %>%
      summarise(url=first(search_url),
                count= n(),
                city=first(city),
                day=first(day),
                state=first(state_name))
    #add date filter
    selections$max_map_count <- 0
    selections$max_map_count <- max(selections$map_data$count)
    
    #Avoid an overlap in map
    selections$map_bins <- case_when(selections$max_map_count < 24 ~ c(1,2,3,4,8,23),
                                     selections$max_map_count <= 50 ~ c(1,2,5,10,20,50),
                                     selections$max_map_count <= 100 ~ c(1,2,5,10,20,100),
                                     TRUE ~ c(1,2,5,10,50,selections$max_map_count)
                                     ) 
    selections$map_palette <- colorBin(palette="Greens", domain=selections$map_data$count,
                                         na.color="transparent", bins=selections$map_bins)
    selections$map_palette <- colorBin(palette=colorRamp(c("#B4A578", "#792310"), interpolate = "spline"), domain=selections$map_data$count,
                                       na.color="transparent", bins=selections$map_bins)
# DATA-TABLE --------------------
    selections$table_set <- selections$filtered_writing %>% 
      select(`Short URL`,`First Date`,`Document Type`,`Word Count`) %>%
      arrange(`First Date`)
    })
    
#OUTPUTS------------------
  output$fiverows <- renderDT({
    datatable(selections$table_set, colnames=c("URL","Date","Document Type","Word Appearances"),
              rownames=FALSE, selection="none", style = 'bootstrap', options = list(
      searching=FALSE,
      columnDefs = list(
        list(
          targets = 0:0,
          orderable=FALSE,
          render = JS(
            "function(data, type, row, meta) {",
            "  if (type === 'display') {",
            "    return '<a href=\"' + data + '\" target=\"_blank\">' + data + '</a>';",
            "  }",
            "  return data;",
            "}"
          )
        )
      )
    )
    )
  })
  output$claramap <- renderLeaflet({
    # creating leaflet graph
    leaflet() %>%
      addProviderTiles('CartoDB.Positron') %>%
      setView(-98.5795, 39.8283, zoom = 4) %>% #zoom in?
      
      addCircles(data=selections$map_data, 
                 lng = selections$map_data$lng, 
                 lat = selections$map_data$lat, 
                 color = ~selections$map_palette(selections$map_data$count), 
                 radius= 20,
                 popup = ~paste("<b>", "<a href=", selections$map_data$url, ">", selections$map_data$city, 
                                "</a>", ",", "</b>", selections$map_data$state, "<br>Number of Mentions:",
                                selections$map_data$count)) %>%
      addLegend(data=selections$map_data, pal=selections$map_palette, values=selections$map_data$count, opacity=0.9, title = "Mentions", 
                position = "bottomleft") 
    
  })
  output$entry_frequency_map <- renderPlotly({
    (ggplot(selections$freq_data_decade, aes(x=Decade,y=Count_Entries)) +
      geom_path() + theme_gray() + geom_point() +
      labs(y=paste("Number of Entries that say \"",input$word_search, "\"",sep=''),
           title=paste("When Does Pres. Woodruff say \"",input$word_search,"\"?",sep='')) +
      scale_x_continuous(breaks=selections$decade_span,
                         labels=selections$decade_span) +
      theme(panel.grid.minor.x = element_blank(),
            axis.title.y=element_text(size=14))) %>%
      ggplotly() %>%
      config(displayModeBar=FALSE)
  })
  output$word_frequency_map <- renderPlotly({
    (ggplot(selections$freq_data_decade, aes(x=Decade,y=Count_Words)) +
      geom_path()  + theme_gray() + geom_point() +
      labs(y=paste("Total Occurrences of \"",input$word_search, "\"",sep=''),
           title=paste("How Much Does Pres. Woodruff Talk About \"",input$word_search,"\"?",sep='')) +
      scale_x_continuous(breaks=selections$decade_span,
                         labels=selections$decade_span) +
      theme(panel.grid.minor.x = element_blank(),
            axis.title.y=element_text(size=14))) %>%
      ggplotly()
  })
  output$start_tab <- renderUI({
    HTML("<b>Welcome to the Wilford Woodruff Papers Exploration!</b><br><br>This is where you can find answers to questions like:<br>\"How much did Pres. Woodruff talk about Nauvoo after he left in 1846?\"<br>\"Where did Pres. Woodruff travel in his term as Prophet?\"<br>\"When did Pres. Woodruff ever talk about ducks?\"<br><br>Feel free to explore through the various filters on the left and the different visualization tabs above. You can also click \"See Originals\" to view the original documents back at the original website.<br>If you find something interesting, go ahead and bookmark it for later!")
  })
  output$map_tab_describe <- renderUI({
    HTML("What locations does Woodruff reference?<br>This is any place he mentions, whether he writes to them, about them, or from them.<br>Currently displays JOURNAL entries only.")
    })
  output$time_tab_describe <- renderUI({
    HTML("Within the time period, this shows how much he wrote about this topic.<br>The left side shows the number of entries within each decade, while the right side shows the number of individual references.<br> ")
  })
  output$table_tab_describe <- renderUI({
    HTML("This tab can redirect you back to the Wilford Woodruff Papers website.<br>This shows every page that matches your search on the left.<br>You can also sort by any of these columns if you want something specific.<br> ")
  })
  output$results <- renderUI({
    HTML(paste("Showing results for<b>",length(selections$filtered_writing$`Short URL`),"</b>entries",sep=" "))
  })
  output$Welcome <- renderUI({
    HTML("<b><font size='12'>Explore His Journals!</font></b>")
  })
  #4 download buttons, maybe 5 for dataset
}



# Run the application 
shinyApp(ui = ui, server = server,enableBookmarking = "url")

