library(shiny)
library(tidyverse)
library(leaflet)


writings <- read_csv('https://github.com/wilfordwoodruff/Main-Data/raw/371f9cda2709a10c38735c5e7b5486384ebb3f65/data/derived/derived_data.csv') %>%
  mutate(`First Date` = ymd(ifelse(is.na(`First Date`),substr(Dates,0,10),`First Date`)))

clara_compiled <- read_csv('https://raw.githubusercontent.com/wilfordwoodruff/Main-Data/main/_docs/Clara_for_rshiny.csv')



# Define UI for application that draws a histogram
ui <- function(request) {
  fluidPage(
  theme = bslib::bs_theme(bootswatch = "united"),
  # Application title
  titlePanel("Explore President Woodruff's Diaries"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(inputId = "startEndDate",
                     label="Writing Period",
                     start='1801-03-01',
                     end='1898-09-08',
                     separator='Journals between',
                     min='1801-03-01',
                     max='1898-09-08'
      ),
      checkboxGroupInput(inputId = 'journal_type',
                         label = 'Types of Writings',
                         choices = unique(writings$`Document Type`),
                         selected = unique(writings$`Document Type`)
                         
      ),
      textInput(inputId='word_search',
                label='Search for a Word',
                placeholder= "e.g. Utah"),
      bookmarkButton()
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("See Graphs",
                 leafletOutput("claramap")
                 #Add download button for this
        ),
        tabPanel("See Dataset",
                 tableOutput("fiverows")
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
      mutate(`Word Count`=str_count(`Text Only Transcript`,input$word_search)) %>%
      filter(`Word Count` > 0 & `Document Type` %in% input$journal_type &
               `First Date` > input$startEndDate[1] & `First Date` < input$startEndDate[2])
    
    selections$map_data <- clara_compiled %>%
      filter(short_url %in% selections$filtered_writing$`Short URL`) %>%
      group_by(lat,lng) %>%
      summarise(url=first(search_url),
                count= n(),
                city=first(city),
                day=first(day),
                state=first(state_name))
    #add date filter
    selections$max_map_count <- max(selections$map_data$count)
    
    #Avoid a 
    if(selections$max_map_count < 24) {
      selections$map_bins <- c(1,2,4,8,23)
    }
    else {
      #if the max is < 99, this gets funky
      selections$map_bins <- c(1,2,5,10,50,99,selections$max_map_count)
    }
    selections$map_palette <- colorBin(palette="YlGnBu", domain=selections$map_data$count,
                                         na.color="transparent", bins=selections$map_bins)
    
    })
  output$fiverows <- renderTable({
    head(selections$filtered_writing)
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
                 popup = ~paste("<b>", "<a href=", selections$map_data$url, ">", selections$map_data$city, 
                                "</a>", ",", "</b>", selections$map_data$state, "<br>Number of Mentions:",
                                selections$map_data$count)) %>%
      addLegend(data=selections$map_data, pal=selections$map_palette, values=selections$map_data$count, opacity=0.9, title = "Mentions", 
                position = "bottomleft")
    
  })
  #4 download buttons, maybe 5 for dataset
}



# Run the application 
shinyApp(ui = ui, server = server,enableBookmarking = "url")

