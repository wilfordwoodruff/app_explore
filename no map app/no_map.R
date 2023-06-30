library(shiny)
library(tidyverse)
library(DT)

#downloaded for faster testing, use
# https://github.com/wilfordwoodruff/Main-Data/raw/371f9cda2709a10c38735c5e7b5486384ebb3f65/data/derived/derived_data.csv
# C:/Users/spenc/Downloads/derived_data.csv
writings <- read_csv('https://github.com/wilfordwoodruff/Main-Data/raw/371f9cda2709a10c38735c5e7b5486384ebb3f65/data/derived/derived_data.csv') %>%
  mutate(`First Date` = ymd(ifelse(is.na(`First Date`),substr(Dates,0,10),`First Date`)))

floor_decade <- function(value){ return(value - value %% 10) }


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
                       separator='to',
                       min='1801-03-01',
                       max='1898-09-08',
                       startview = 'decade',
                       format='dd M, yyyy (D)'
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
          
          tabPanel("Frequency & Usage",
                   htmlOutput("time_tab_describe"),
                   fluidRow(
                     splitLayout(cellWidths=c("50%","50%"),
                                 plotOutput("entry_frequency_map"),plotOutput("word_frequency_map"))
                   )
          ),
          tabPanel("See Originals",
                   htmlOutput("table_tab_describe"),
                   DTOutput("fiverows")
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
    
    #COUNT OF WORDS & APPEARANCES----------
    selections$all_decades <- year(selections$filtered_writing$`First Date`) %/% 10 * 10
    selections$decade_span <- seq(min(selections$all_decades), max(selections$all_decades), by = 10)
    selections$freq_data_decade <- selections$filtered_writing %>%
      mutate(Decade = selections$all_decades) %>% 
      group_by(Decade) %>%
      summarise(Count_Words = sum(`Word Count`),
                Count_Entries = n()) %>%
      complete(Decade = selections$decade_span,
               fill=list(Count_Words=0,Count_Entries=0))
    
    # DATA-TABLE --------------------
    selections$table_set <- selections$filtered_writing %>% 
      select(`Short URL`,`First Date`,`Document Type`,`Word Count`) %>%
      arrange(`First Date`)
  })
  
  #OUTPUTS------------------
  output$fiverows <- renderDT({
    datatable(selections$table_set, options = list(
      columnDefs = list(
        list(
          targets = 1:1,
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
  output$entry_frequency_map <- renderPlot({
    ggplot(selections$freq_data_decade, aes(x=Decade,y=Count_Entries)) +
      geom_path() + theme_gray() + geom_point() +
      labs(y=paste("Number of Entries that say \"",input$word_search, "\"",sep=''),
           title=paste("When Does Pres. Woodruff say \"",input$word_search,"\"?",sep='')) +
      scale_x_continuous(breaks=selections$decade_span,
                         labels=selections$decade_span) +
      theme(panel.grid.minor.x = element_blank())
  })
  output$word_frequency_map <- renderPlot({
    ggplot(selections$freq_data_decade, aes(x=Decade,y=Count_Words)) +
      geom_path()  + theme_gray() + geom_point() +
      labs(y=paste("Number of Times \"",input$word_search, "\" Is Referenced by Decade",sep=''),
           title=paste("How Much Does Pres. Woodruff Talk About \"",input$word_search,"\"?",sep='')) +
      scale_x_continuous(breaks=selections$decade_span,
                         labels=selections$decade_span) +
      theme(panel.grid.minor.x = element_blank())
  })
  output$map_tab_describe <- renderPrint({
    cat(paste("What locations does Woodruff reference?",
              "This is any place he mentions, whether he writes to them, about them, or from them.",
              "Currently displays JOURNAL entries only.",sep="\n"))})
  output$time_tab_describe <- renderUI({
    HTML("Within the time period, this shows how much he wrote about this topic.<br>The left side shows the number of entries within each decade, while the right side shows the number of individual references.<br> ")
  })
  output$table_tab_describe <- renderUI({
    HTML("This tab can redirect you back to the Wilford Woodruff Papers website.<br>This shows every page that matches your search on the left.<br>You can also sort by any of these columns if you want something specific.<br> ")
  })
  #4 download buttons, maybe 5 for dataset
}



# Run the application 
shinyApp(ui = ui, server = server,enableBookmarking = "url")

