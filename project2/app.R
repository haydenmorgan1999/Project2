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
              tags$h1("Project 2 - Hayden Morgan"),
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
                       "game", 
                       label = "Which game?",
                       choices = 
                         c("BOTW",
                           "TOTK")
                     ),
                     downloadButton("download_data2", "Download Function 2 Data")),
                col_widths = c(6, 3, 3)
              )
            ),
    
    nav_panel("Data Exploration",
              card(tags$h1("What type of data do you want to see?"),
                   selectInput(
                     "data_type",
                     label = "Choose a data display:",
                     choices = 
                       c("Contingency tables",
                         "Numerical summaries",
                         "Plots"),
                     selected = "Contingency tables"
                   ),
                   conditionalPanel(
                     condition = "input.data_type == 'Contingency tables'",
                     radioButtons(
                       "game2",
                       label = "Which game?",
                       choices = 
                         c("BOTW",
                           "TOTK")
                     ),
                     actionButton(
                       "show_data",
                       label = "Show Data"
                     ),
                     tableOutput("contingency1"),
                     tableOutput("contingency2"),
                     tableOutput("contingency3")
                   ),
                   conditionalPanel(
                     condition = "input.data_type == 'Numerical summaries'",
                     radioButtons(
                       "game3",
                       label = "Which game?",
                       choices = 
                         c("BOTW",
                           "TOTK")
                     ),
                     actionButton(
                       "center",
                       label = "Measures of Center"
                     ),
                     tableOutput("center_data1"),
                     tableOutput("center_data2"),
                     actionButton(
                       "spread",
                       label = "Measures of Spread"
                     ),
                     tableOutput("spread_data1"),
                     tableOutput("spread_data2")
                   ),
                   conditionalPanel(
                     condition = "input.data_type == 'Plots'",
                     radioButtons(
                       "game4",
                       label = "Which game?",
                       choices = 
                         c("BOTW",
                           "TOTK")
                     ),
                   )
                   ))
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
  
  shown_data1 <- eventReactive(input$show_data, {
    game <- tolower(input$game2)
    GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/all?game=", game))
    parsed <- fromJSON(rawToChar(GET_result$content))
    info <- as_tibble(parsed$data)
    tbl <- table(info$category)
    df <- as.data.frame(tbl)
    colnames(df) <- c("Category", "Frequency")
    df
  })
  
  output$contingency1 <- renderTable({
    shown_data1()
  })
  
  shown_data2 <- eventReactive(input$show_data, {
    game <- tolower(input$game2)
    GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/all?game=", game))
    parsed <- fromJSON(rawToChar(GET_result$content))
    info <- as_tibble(parsed$data)
    tbl <- table(info$category, info$edible, useNA = "no")
    df <- as.data.frame(tbl)
    colnames(df) <- c("Category", "Edible?", "Frequency")
    df
  })
  
  output$contingency2 <- renderTable({
    shown_data2()
  })
  
  shown_data3 <- eventReactive(input$show_data, {
    game <- tolower(input$game2)
    GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/all?game=", game))
    parsed <- fromJSON(rawToChar(GET_result$content))
    info <- as_tibble(parsed$data)
    tbl <- table(unlist(info$common_locations), useNA = "no")
    df <- as.data.frame(tbl)
    colnames(df) <- c("Location of Compendium Entries", "Frequency")
    df
  })
  
  output$contingency3 <- renderTable({
    shown_data3()
  })
  
  center1 <- eventReactive(input$center, {
    game <- tolower(input$game3)
    GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/all?game=", game))
    parsed <- fromJSON(rawToChar(GET_result$content))
    info <- as_tibble(parsed$data)
    info <- info |> 
      select(-id)
    
    summary <- info |>
      group_by(category) |>
      drop_na(category) |>
      summarize(across(where(is.numeric),
                       list("mean" = ~ mean(.x, na.rm = TRUE), "median" = ~ median(.x, na.rm = TRUE)),
                       .names = "{.fn}_{.col}"
                       ))
    if(game =="botw"){
      summary <- summary |>
        rename(
          "Mean Hearts Recovered" = mean_hearts_recovered,
          "Median Hearts Recovered" = median_hearts_recovered
        ) 
      } else if(game == "totk"){
          summary <- summary |>
            rename(
              "Mean Hearts Recovered" = mean_hearts_recovered,
              "Median Hearts Recovered" = median_hearts_recovered,
              "Mean Fuse Attack Power" = mean_fuse_attack_power,
              "Median Fuse Attack Power" = median_fuse_attack_power
            )
      }
    
  })
  
  output$center_data1 <- renderTable({
    center1()
  })
  
  center2 <- eventReactive(input$center, {
    game <- tolower(input$game3)
    GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/category/equipment?game=", game))
    parsed <- fromJSON(rawToChar(GET_result$content), flatten = T)
    info <- as_tibble(parsed$data)
    
    info <- info |>
      mutate(
        attack = map_dbl(properties.attack, ~ ifelse(is.null(.x), NA_real_, .x)),
        defense = map_dbl(properties.defense, ~ ifelse(is.null(.x), NA_real_, .x))
      )
    
    info <- info |>
      filter(attack < 1000000000) #outlier removed 
  
    info |>
      summarize(
        "Mean Equipment Attack" = mean(attack, na.rm = T),
        "Median Equipment Attack" = median(attack, na.rm = T),
        "Mean Equipment Defense" = mean(defense, na.rm = T),
        "Median Equipment Defense" = median(defense, na.rm = T)
      )
    
  })
  
  output$center_data2 <- renderTable({
    center2()
  })
  
  spread1 <- eventReactive(input$spread, {
    game <- tolower(input$game3)
    GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/all?game=", game))
    parsed <- fromJSON(rawToChar(GET_result$content), flatten = T)
    info <- as_tibble(parsed$data)
    
    if(game == "botw"){
      info |>
        group_by(category) |>
        drop_na(category) |>
        summarize("SD of Hearts Recovered" = sd(hearts_recovered, na.rm = T),
                  "IQR of Hearts Recovered" = IQR(hearts_recovered, na.rm = T))
    } else if(game == "totk"){
      info |>
        group_by(category) |>
        drop_na(category) |>
        summarize("SD of Hearts Recovered" = sd(hearts_recovered, na.rm = T),
                  "IQR of Hearts Recovered" = IQR(hearts_recovered, na.rm = T),
                  "SD of Fuse Attack Power" = sd(fuse_attack_power, na.rm = T),
                  "IQR of Fuse Attack Power" = IQR(fuse_attack_power, na.rm = T))
    }
    
    
    
  })
  
  output$spread_data1 <- renderTable({
    spread1()
  })
  
  spread2 <- eventReactive(input$spread, {
    game <- tolower(input$game3)
    GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/all?game=", game))
    parsed <- fromJSON(rawToChar(GET_result$content), flatten = T)
    info <- as_tibble(parsed$data)
    
    info <- info |>
      mutate(
        attack = map_dbl(properties.attack, ~ ifelse(is.null(.x), NA_real_, .x)),
        defense = map_dbl(properties.defense, ~ ifelse(is.null(.x), NA_real_, .x))
      )
    
    info <- info |>
      filter(attack < 1000000000) #outlier removed 
    
    info |>
      summarize(
        "SD of Equipment Attack" = sd(attack, na.rm = T),
        "IQR of Equipment Attack" = IQR(attack, na.rm = T),
        "SD of Equipment Defense" = sd(defense, na.rm = T),
        "IQR of Equipment Defense" = IQR(defense, na.rm = T)
      )
    
  })
  
  output$spread_data2 <- renderTable({
    spread2()
  })

}

#Run app 
shinyApp(ui = ui, server = server)