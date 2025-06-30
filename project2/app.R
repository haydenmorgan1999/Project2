library("httr")
library("jsonlite")
library("tidyverse")
library("shiny")
library("bslib")
library("ggplot2")
library("DT")

#UI
ui <- page_navbar(
  navset_card_underline(
    nav_panel("About", 
              p("The purpose of this app is to search the Hyrule Compendium and display its data in various ways."), 
              p("The data comes from the Hyrule Compendium API, which can be found",  
                a("here", href = "https://gadhagod.github.io/Hyrule-Compendium-API/#/", target = "_blank"), "."), 
              p("The current tab is the 'About' tab, which explains the app."), 
              p("The 'Data Download' tab allows you to query data and save your query."), 
              p("The 'Data Exploration' tab allows you to customize and display numerical/graphical summaries of the data."),
              img(src = "picture.jpg", width = "100%")
    ),
    
    nav_panel("Data Download",
              layout_columns(
                card(tags$h1("Display Returned Data"),
                     radioButtons(
                       "game1",
                       label = "Which compendium?",
                       choices = 
                         c("BOTW",
                           "TOTK"),
                       selected = "BOTW"
                     ),
                     downloadButton("download_all", "Download Compendium Data"),
                     actionButton(
                       "return_data",
                       label = "Show Data"
                     ),
                     DT::dataTableOutput(
                       "returned_data")
                     ),
                card(tags$h1("Function 1: Subset data by category and/or hearts recovered"),
                     selectInput(
                       "category",
                       label = "Choose a category in the Hyrule compendium:",
                       choices = 
                         c("Creatures",
                           "Equipment",
                           "Materials",
                           "Monsters",
                           "Treasure"),
                       selected = "Creatures"
                     ),
                     sliderInput(
                       "hearts",
                       label = "Hearts recovered:",
                       min = 0,
                       max = 5,
                       value = 2.5,
                       step = 0.25
                     ),
                     downloadButton("download_data1", "Download Function 1 Data")),
                card(tags$h1("Function 2: Look at specific compendium entries from either BOTW or TOTK"),
                     numericInput(
                       "entry",
                       label = "Which entry (enter a whole number)?",
                       value = 1
                     ),
                     radioButtons(
                       "game", label = "Which game?",
                       choices = 
                         c("BOTW",
                           "TOTK")
                     ),
                     downloadButton("download_data2", "Download Function 2 Data")),
                col_widths = c(6, 3, 3)
              )
            ),
    
    nav_panel("Data Exploration")
  )
)



#Server
server <- function(input, output){
  output$download_all <- downloadHandler(
    filename = function(){
      paste0(input$game1, "_compendium.csv")
    },
    
    content = function(file){
      game <- tolower(input$game1)
      
      GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/all?game=", game))
      parsed <- fromJSON(rawToChar(GET_result$content))
      info <- parsed$data
      
      info_tibble <- tibble(
        id = info$id,
        name = info$name,
        category = info$category,
        description = info$description,
        common_locations = list(info$common_locations),
        drops = list(info$drops),
        image = info$image
      ) 
      
      write_csv(info_tibble, file)
      
    }
  )
  
  returned_data <- eventReactive(input$return_data, {
    game <- tolower(input$game1)
    GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/all?game=", game))
    parsed <- fromJSON(rawToChar(GET_result$content))
    info <- as_tibble(parsed$data)
  })
  
  output$returned_data <- DT::renderDataTable({
    returned_data()
  })
  
  output$download_data1 <- downloadHandler(
    filename = function(){
      paste0(input$category, "_", input$hearts, "heart.csv")
      },
    
    content = function(file){
      category <- tolower(input$category)
      hearts <- input$hearts
      
      GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/category/", category))
      parsed <- fromJSON(rawToChar(GET_result$content))
      info <- as_tibble(parsed$data)
      
      if(category %in% c("materials", "creatures")){
        info2 <- filter(info, hearts_recovered == hearts)
        if(nrow(info2) == 0){
          write_csv(tibble(message = "No data matches the search criteria."), file)
        } else {
          write_csv(info2, file)
        }
      } else {
        write_csv(info, file)
      }
    }
  )
  
  output$download_data2 <- downloadHandler(
    filename = function(){
      paste0("entry", input$entry, "_", input$game, ".csv")
    },
    
    content = function(file){
      entry_number <- input$entry
      botw_or_totk <- tolower(input$game)

        GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/entry/", entry_number, "?game=", botw_or_totk))
        parsed <- fromJSON(rawToChar(GET_result$content))
        info <- parsed$data
        
        info_tibble <- tibble(
          id = info$id,
          name = info$name,
          category = info$category,
          description = info$description,
          common_locations = list(info$common_locations),
          drops = list(info$drops),
          image = info$image
        ) 
        
        write_csv(info_tibble, file)
      
    }
  )
}

#Run app 
shinyApp(ui = ui, server = server)