###HEADER
#Author: Hayden Morgan
#Date: 7/8/25

# Load packages ------------------------------------------------------------
library("httr")
library("jsonlite")
library("tidyverse")
library("shiny")
library("bslib")
library("ggplot2")
library("DT")

# Define UI ----------------------------------------------------------------
ui <- page_navbar(
    nav_panel("About",
              tags$h1("Project 2 - Hayden Morgan"),
              p("The purpose of this app is to search the Hyrule Compendium and display its data in various ways."),
              p("It utilizes data from both Breath of the Wild (BOTW) and Tears of the Kingdom (TOTK)."),
              p("The data comes from the Hyrule Compendium API, which can be found",  
                a("here", href = "https://gadhagod.github.io/Hyrule-Compendium-API/#/", target = "_blank"), "."), 
              p("The current tab is the 'About' tab, which explains the app."), 
              p("The 'Data Download' tab allows you to query data and download your query."), 
              p("The 'Data Exploration' tab allows you to customize and display numerical/graphical summaries of the data."),
              img(src = "picture.jpg", width = "100%")
    ),
    
    nav_panel("Data Download",
              layout_columns(
                card(tags$h1("Display Returned Data"),
                     radioButtons(
                       "choosegame_downloadall",
                       label = "Which compendium?",
                       choices = 
                         c("BOTW",
                           "TOTK"),
                       selected = "BOTW"
                     ),
                     downloadButton("download_all", "Download Compendium Data"),
                     actionButton(
                       "show_data_all",
                       label = "Show Data"
                     ),
                     DT::dataTableOutput(
                       "returned_data")
                     ),
                card(tags$h1("Function 1: Subset data by category and/or hearts recovered"),
                     selectInput(
                       "category",
                       label = "Choose a category in the Hyrule compendium (Note: if 'hearts_recovered' doesn't exist for a particular category, the entire category will be returned):",
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
                     downloadButton("download_fxn1", "Download Function 1 Data")),
                card(tags$h1("Function 2: Look at specific compendium entries from either BOTW or TOTK"),
                     numericInput(
                       "entry_number",
                       label = "Which entry (enter a whole number)?",
                       value = 1
                     ),
                     radioButtons(
                       "choosegame_downloadfxn2", 
                       label = "Which game?",
                       choices = 
                         c("BOTW",
                           "TOTK")
                     ),
                     downloadButton("download_fxn2", "Download Function 2 Data")),
                col_widths = c(6, 3, 3)
              )
            ),
    
    nav_panel("Data Exploration",
              card(tags$h1("What type of summary do you want to see?"),
                   selectInput(
                     "summary_type",
                     label = "Choose a data display:",
                     choices = 
                       c("Contingency tables",
                         "Numerical summaries",
                         "Plots"),
                     selected = "Contingency tables"
                   ),
                   conditionalPanel(
                     condition = "input.summary_type == 'Contingency tables'",
                     sidebarLayout(
                       sidebarPanel(
                     radioButtons(
                       "choosegame_contingency",
                       label = "Which game?",
                       choices = 
                         c("BOTW",
                           "TOTK")
                     ),
                     radioButtons(
                       "contingency_var",
                       label = "Which variable(s)?",
                       choices = 
                         c("Category",
                           "Category x Edible Status",
                           "Location")
                     ),
                     actionButton(
                       "show_data_contingency",
                       label = "Show Data"
                     )),
                     mainPanel(
                     tableOutput("contingency"))
                   )),
                   conditionalPanel(
                     condition = "input.summary_type == 'Numerical summaries'",
                     sidebarLayout(
                       sidebarPanel(
                     radioButtons(
                       "choosegame_numerical",
                       label = "Which game?",
                       choices = 
                         c("BOTW",
                           "TOTK")
                     ),
                     uiOutput("numeric_var_ui"),
                     radioButtons(
                       "center_or_spread",
                       label = "Do you want to measure center or spread?",
                       choices = 
                         c("Center",
                           "Spread",
                           "Both")
                     ),
                     actionButton(
                       "center_spread",
                       label = "Show Data"
                     )),
                     mainPanel(
                     tableOutput("center_spread_data"))
                   )),
                   conditionalPanel(
                     condition = "input.summary_type == 'Plots'",
                     sidebarLayout(
                       sidebarPanel(
                         selectInput(
                           "select_x",
                           label = "X:",
                           choices = 
                             c("Category",
                               "Compendium ID Number",
                               "Game (BOTW or TOTK)"),
                         ),
                         selectInput(
                           "select_y",
                           label = "Y:",
                           choices = 
                             c("Number of Entries",
                               "Equipment Attack Power",
                               "Equipment Defense Power",
                               "Hearts Recovered",
                               "Location in Hyrule")
                         ),
                         uiOutput("plot_ui")
                       ),
                       mainPanel(
                         plotOutput("plot_data")
                       )
                     )
                     )
                   )
                   )
  )

# Define Server ------------------------------------------------------------
server <- function(input, output){
  output$download_all <- downloadHandler(
    filename = function(){
      paste0(input$choosegame_downloadall, "_compendium.csv")
    },
    
    content = function(file){
      game <- tolower(input$choosegame_downloadall)
      
      GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/all?game=", game))
      parsed <- fromJSON(rawToChar(GET_result$content))
      data <- parsed$data
      
      data_tibble <- tibble(
        id = data$id,
        name = data$name,
        category = data$category,
        description = data$description,
        common_locations = list(data$common_locations),
        drops = list(data$drops),
        image = data$image
      ) #had to manually make a tibble because it was doing something weird with
      
      write_csv(data_tibble, file)
      
    }
  )
  
  all_data <- eventReactive(input$show_data_all, {
    game <- tolower(input$choosegame_downloadall)
    GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/all?game=", game))
    parsed <- fromJSON(rawToChar(GET_result$content))
    data <- as_tibble(parsed$data)
  })
  
  output$returned_data <- DT::renderDataTable({
    all_data()
  })
  
  output$download_fxn1 <- downloadHandler(
    filename = function(){
      if(input$category %in% c("Materials", "Creatures")){
        paste0(input$category, "_", input$hearts, "heart.csv")
      } else {
        paste0(input$category, ".csv")
      }
      },
    
    content = function(file){
      category <- tolower(input$category)
      hearts <- input$hearts
      
      GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/category/", category))
      parsed <- fromJSON(rawToChar(GET_result$content), flatten = T)
      data <- as_tibble(parsed$data)
      
      if(input$category %in% c("Materials", "Creatures")){ #https://stackoverflow.com/questions/74111575/difference-between-in-and-in-operator-in-r
        data <- filter(data, hearts_recovered == hearts)
        if(nrow(data) == 0){
          write_csv(tibble(message = "No data matches the search criteria."), file)
        } else {
          write_csv(data, file)
        }
      } else {
        write_csv(data, file)
      }
    }
  )
  
  output$download_fxn2 <- downloadHandler(
    filename = function(){
      paste0("entry", input$entry_number, "_", input$choosegame_downloadfxn2, ".csv")
    },
    
    content = function(file){
      entry_number <- input$entry_number
      game <- tolower(input$choosegame_downloadfxn2)

        GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/entry/", entry_number, "?game=", game))
        parsed <- fromJSON(rawToChar(GET_result$content))
        data <- parsed$data
        
        data_tibble <- tibble(
          id = data$id,
          name = data$name,
          category = data$category,
          description = data$description,
          common_locations = list(data$common_locations),
          drops = list(data$drops),
          image = data$image
        ) 
        
        write_csv(data_tibble, file)
      
    }
  )
  
  contingency_data <- eventReactive(input$show_data_contingency, {
    game <- tolower(input$choosegame_contingency)
    GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/all?game=", game))
    parsed <- fromJSON(rawToChar(GET_result$content))
    data <- as_tibble(parsed$data)
    
    if(input$contingency_var == "Category"){
      table <- table(data$category)
      df <- as.data.frame(table)
      colnames(df) <- c("Category", "Frequency")
    } else if(input$contingency_var == "Category x Edible Status"){
      table <- table(data$category, data$edible, useNA = "no")
      df <- as.data.frame(table)
      colnames(df) <- c("Category", "Edible?", "Frequency")
    } else {
      table <- table(unlist(data$common_locations), useNA = "no")
      df <- as.data.frame(table)
      colnames(df) <- c("Location of Compendium Entries", "Frequency")
    }
    
    df
    
  })
  
  output$contingency <- renderTable({
    contingency_data()
  })
  
  output$numeric_var_ui <- renderUI ({
    game <- tolower(input$choosegame_numerical)
    if(game == "totk"){
      radioButtons("num_var", "Which variable(s)?", choices = c("Hearts Recovered", "Fuse Attack Power", "Equipment Attack", "Equipment Defense"))
    } else {
      radioButtons("num_var", "Which variable(s)?", choices = c("Hearts Recovered", "Equipment Attack", "Equipment Defense"))
    }
  })
  
  num_sum <- eventReactive(input$center_spread, {
    game <- tolower(input$choosegame_numerical)
    GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/all?game=", game))
    parsed <- fromJSON(rawToChar(GET_result$content), flatten = T)
    data <- as_tibble(parsed$data)
    
    if(input$num_var == "Hearts Recovered"){
      if(input$center_or_spread == "Center"){
        data <- data |>
          group_by(category) |>
          drop_na(category) |>
          summarize(
            "Mean Hearts Recovered" = mean(hearts_recovered, na.rm = T),
            "Median Hearts Recovered" = median(hearts_recovered, na.rm = T)
          )
      } else if(input$center_or_spread == "Spread"){
        data <- data |>
          group_by(category) |>
          drop_na(category) |>
          summarize(
            "SD of Hearts Recovered" = sd(hearts_recovered, na.rm = T),
            "IQR of Hearts Recovered" = IQR(hearts_recovered, na.rm = T)
          )
      } else {
        center <- data |>
          group_by(category) |>
          drop_na(category) |>
          summarize(
            "Mean Hearts Recovered" = mean(hearts_recovered, na.rm = T),
            "Median Hearts Recovered" = median(hearts_recovered, na.rm = T)
          )
        
        spread <- data |>
          group_by(category) |>
          drop_na(category) |>
          summarize(
            "SD of Hearts Recovered" = sd(hearts_recovered, na.rm = T),
            "IQR of Hearts Recovered" = IQR(hearts_recovered, na.rm = T)
          )
        
        combined <- inner_join(center, spread, by = "category")
        
        combined
        
      }
      
    } else if(input$num_var == "Fuse Attack Power"){
      
      if(input$center_or_spread == "Center"){
        data <- data |>
          group_by(category) |>
          drop_na(category) |>
          summarize(
            "Mean Fuse Attack Power" = mean(fuse_attack_power, na.rm = T),
            "Median Fuse Attack Power" = median(fuse_attack_power, na.rm = T)
          )
      } else if(input$center_or_spread == "Spread"){
        data <- data |>
          group_by(category) |>
          drop_na(category) |>
          summarize(
            "SD of Fuse Attack Power" = sd(fuse_attack_power, na.rm = T),
            "IQR of Fuse Attack Power" = IQR(fuse_attack_power, na.rm = T)
          )
      } else {
        center <- data |>
          group_by(category) |>
          drop_na(category) |>
          summarize(
            "Mean Fuse Attack Power" = mean(fuse_attack_power, na.rm = T),
            "Median Fuse Attack Power" = median(fuse_attack_power, na.rm = T)
          )
        
        spread <- data |>
          group_by(category) |>
          drop_na(category) |>
          summarize(
            "SD of Fuse Attack Power" = sd(fuse_attack_power, na.rm = T),
            "IQR of Fuse Attack Power" = IQR(fuse_attack_power, na.rm = T)
          )
        
        combined <- inner_join(center, spread, by = "category")
        
        combined
        
      }
      
    } else if(input$num_var == "Equipment Attack"){
      
      data <- data |>
        mutate(
          attack = map_dbl(properties.attack, ~ ifelse(is.null(.x), NA_real_, .x))
        ) |>
        filter(attack < 1000000000) #outlier removed 
      
      if(input$center_or_spread == "Center"){
        data <- data |>
          group_by(category) |>
          drop_na(category) |>
          summarize(
            "Mean Equipment Attack" = mean(attack, na.rm = T),
            "Median Equipment Attack" = median(attack, na.rm = T)
          )
      } else if(input$center_or_spread == "Spread"){
        data <- data |>
          group_by(category) |>
          drop_na(category) |>
          summarize(
            "SD of Equipment Attack" = sd(attack, na.rm = T),
            "IQR of Equipment Attack" = IQR(attack, na.rm = T)
          )
      } else {
        center <- data |>
          group_by(category) |>
          drop_na(category) |>
          summarize(
            "Mean Equipment Attack" = mean(attack, na.rm = T),
            "Median Equipment Attack" = median(attack, na.rm = T)
          )
        
        spread <- data |>
          group_by(category) |>
          drop_na(category) |>
          summarize(
            "SD of Equipment Attack" = sd(attack, na.rm = T),
            "IQR of Equipment Attack" = IQR(attack, na.rm = T)
          )
        
        combined <- inner_join(center, spread, by = "category")
        
        combined
        
      }
      
    } else if(input$num_var == "Equipment Defense"){
      
      data <- data |>
        mutate(
          defense = map_dbl(properties.defense, ~ ifelse(is.null(.x), NA_real_, .x))
        )
      
      if(input$center_or_spread == "Center"){
        
        data <- data |>
          group_by(category) |>
          drop_na(category) |>
          summarize(
            "Mean Equipment Defense" = mean(defense, na.rm = T),
            "Median Equipment Defense" = median(defense, na.rm = T)
          )
      } else if(input$center_or_spread == "Spread"){
        data <- data |>
          group_by(category) |>
          drop_na(category) |>
          summarize(
            "SD of Equipment Defense" = sd(defense, na.rm = T),
            "IQR of Equipment Defense" = IQR(defense, na.rm = T)
          )
      } else {
        center <- data |>
          group_by(category) |>
          drop_na(category) |>
          summarize(
            "Mean Equipment Defense" = mean(defense, na.rm = T),
            "Median Equipment Defense" = median(defense, na.rm = T)
          )
        
        spread <- data |>
          group_by(category) |>
          drop_na(category) |>
          summarize(
            "SD of Equipment Defense" = sd(defense, na.rm = T),
            "IQR of Equipment Defense" = IQR(defense, na.rm = T)
          )
        
        combined <- inner_join(center, spread, by = "category")
        
        combined
        
      }
    }
    
  })
  
  output$center_spread_data <- renderTable({
    num_sum()
  })
  
  output$plot_ui <- renderUI ({
    if(input$select_y == "Number of Entries" & input$select_x != "Game (BOTW or TOTK)"){
      checkboxInput("facet_games", "Facet by game (BOTW/TOTK)?", value = FALSE)
    } 
  })
  
  output$plot_data <- renderPlot({
    
    if(input$select_y == "Number of Entries"){
      GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/all?game=botw"))
      parsed <- fromJSON(rawToChar(GET_result$content))
      botw <- as_tibble(parsed$data)
      botw <- mutate(botw, game = "BOTW")
      
      GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/all?game=totk"))
      parsed <- fromJSON(rawToChar(GET_result$content), flatten = T)
      totk <- as_tibble(parsed$data)
      totk <- mutate(totk, game = "TOTK")
      
      combined <- bind_rows(botw, totk)
      
      if(input$facet_games == FALSE){
        if(input$select_x == "Category"){
          
          ggplot(combined, aes(x = category)) +
            geom_bar(fill="lightblue", stat = "count") + 
            labs(x = "Compendium Category", y = "# of Entries", title = "Count of Compendium Item Entries in Each Category")
          
        } else if(input$select_x == "Compendium ID Number"){
          
          combined <- combined |>
            filter(id %in% c(385:394)) #I chose this subset because BOTW only has 389 entries and TOTK has 509, so you can see a change in the graph here.
          
          ggplot(combined, aes(x = factor(id))) +
            geom_bar(fill="gray", stat = "count") + 
            labs(x = "Compendium ID Number (Subset)", y = "# of Entries", title = "Count of Compendium Item Entries for a Subset of ID Numbers")
          
        } else {
          
          ggplot(combined, aes(x = game, fill = game)) +
            geom_bar(stat = "count") + 
            labs(x = "Game (BOTW or TOTK)", y = "# of Entries", title = "Count of Compendium Item Entries in Each Game (BOTW or TOTK)", fill = "Game")+
            scale_fill_manual(values = c("BOTW" = "lightgreen", "TOTK" = "steelblue"))
        }
        
      } else {
        if(input$select_x == "Category"){
          
          ggplot(combined, aes(x = category, fill = game)) +
            geom_bar(stat = "count") + 
            scale_fill_manual(values = c("BOTW" = "pink", "TOTK" = "yellow")) +
          labs(x = "Compendium Category", y = "# of Entries", title = "Count of Compendium Item Entries in Each Category", fill = "Game")
          
        } else if(input$select_x == "Compendium ID Number"){
          
          combined <- combined |>
            filter(id %in% c(385:394))
          
          ggplot(combined, aes(x = factor(id), fill = game)) +
            geom_bar(stat = "count") + 
            scale_fill_manual(values = c("BOTW" = "pink", "TOTK" = "yellow"))+
            labs(x = "Compendium ID Number (Subset)", y = "# of Entries", title = "Count of Compendium Item Entries for a Subset of ID Numbers", fill = "Game")
          
        } 
        
      } 
      
    } else if(input$select_y == "Equipment Attack Power"){
      
      GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/category/equipment?game=botw"))
      parsed <- fromJSON(rawToChar(GET_result$content), flatten = T)
      botw <- as_tibble(parsed$data)
      botw <- mutate(botw, game = "BOTW")
      
      GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/category/equipment?game=totk"))
      parsed <- fromJSON(rawToChar(GET_result$content), flatten = T)
      totk <- as_tibble(parsed$data)
      totk <- mutate(totk, game = "TOTK")
      
      combined <- bind_rows(botw, totk)
      
      combined <- combined |>
        mutate(attack = map_dbl(properties.attack, ~ ifelse(is.null(.x), NA_real_, .x))) |>
        filter(attack < 1000000000) #outlier removed
      
      if(input$select_x == "Category"){
        
        ggplot(combined, aes(x = category, y = attack, color = game))+
          geom_point() +
          labs(x = "Category", y = "Attack Power", title = "Attack Power by Compendium Category (Only the 'Equipment' Category Has Attack Power)", color = "Game")
        
      } else if(input$select_x == "Compendium ID Number"){
        
        ggplot(combined, aes(x = id, y = attack, color = game))+
          geom_point() +
          labs(x = "Entry ID Number", y = "Equipment Attack Power", title = "Equipment Attack Power by ID Number of Compendium Entry", color = "Game")
        
      } else {
        
        ggplot(combined, aes(x = game, y = attack, color = game))+
          geom_point() +
          scale_color_manual(values = c("BOTW" = "lightgreen", "TOTK" = "steelblue"))+
          labs(x = "Game (BOTW or TOTK)", y = "Equipment Attack Power", title = "Equipment Attack Power by Game (BOTW or TOTK)", color = "Game")
        
      }
      
    } else if(input$select_y == "Hearts Recovered"){
      GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/all?game=botw"))
      parsed <- fromJSON(rawToChar(GET_result$content))
      botw <- as_tibble(parsed$data)
      botw <- mutate(botw, game = "BOTW")
      
      GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/all?game=totk"))
      parsed <- fromJSON(rawToChar(GET_result$content), flatten = T)
      totk <- as_tibble(parsed$data)
      totk <- mutate(totk, game = "TOTK")
      
      combined <- bind_rows(botw, totk)
      
      combined <- combined |>
        filter(!is.na(hearts_recovered))
      
      if(input$select_x == "Category"){
        ggplot(combined, aes(x = category, y = hearts_recovered, fill = category))+
          geom_boxplot()+
          scale_fill_manual(values = c("creatures" = "gray", "materials" = "red"))+
          theme(legend.position = "none")+
          labs(x = "Category", y = "Hearts Recovered", title = "Hearts Recovered Across Categories (For Categories That Include 'Hearts Recovered' Variable Only)")
        
      } else if(input$select_x == "Compendium ID Number"){
        combined <- combined |>
          mutate(id_group = case_when(
            id %in% 1:250 ~ "ids_first_half",
            id %in% 251:509 ~ "ids_second_half"
          ))
        
        ggplot(combined, aes(x = factor(id_group), y = hearts_recovered, fill = id_group))+
          geom_boxplot()+
          scale_fill_manual(values = c("ids_first_half" = "lavender", "ids_second_half" = "white"))+
          theme(legend.position = "none")+
          labs(x = "Entry ID Number Group (First or Second Half of Compendium)", y = "Hearts Recovered", title = "Hearts Recovered for Each Entry ID Group (First or Second Half of the Compendium)")
        
      } else {
        ggplot(combined, aes(x = game, y = hearts_recovered, fill = game))+
          geom_boxplot()+
          scale_fill_manual(values = c("BOTW" = "lightgreen", "TOTK" = "lightblue"))+
          theme(legend.position = "none")+
          labs(x = "Game (BOTW or TOTK)", y = "Hearts Recovered", title = "Hearts Recovered for Each Game (BOTW or TOTK)")
      }
      
    } else if(input$select_y == "Location in Hyrule") {
      GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/all?game=botw"))
      parsed <- fromJSON(rawToChar(GET_result$content))
      botw <- as_tibble(parsed$data)
      botw <- mutate(botw, game = "BOTW")
      
      GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/all?game=totk"))
      parsed <- fromJSON(rawToChar(GET_result$content), flatten = T)
      totk <- as_tibble(parsed$data)
      totk <- mutate(totk, game = "TOTK")
      
      combined <- bind_rows(botw, totk)
      
      no_listcols <- combined |>
        select(name, category, common_locations, id, game) |>
        unnest_longer(common_locations, values_to = "location") #https://tidyr.tidyverse.org/reference/unnest_longer.html
      
      top_locs <- no_listcols |>
        count(location, sort = T) |>
        slice_head(n = 10) |>
        pull(location)
      
      no_listcols <- no_listcols |>
        filter(location %in% top_locs) #https://stackoverflow.com/questions/74111575/difference-between-in-and-in-operator-in-r
      
      if(input$select_x == "Category"){
        no_listcols <- no_listcols |>
          count(location, category)
        
        ggplot(no_listcols, aes(x = category, y = location, fill = n)) +
          geom_tile()+
          scale_fill_viridis_c(option = "inferno")+
          labs(x = "Category of Compendium Entry", y = "Location in Hyrule", title = "Where Compendium Entries are Found in Hyrule (Top 10 Most Common Locations)", fill = "# Entries")
        
      } else if(input$select_x == "Compendium ID Number"){
        no_listcols <- no_listcols |>
          count(location, id) |>
          filter(id %in% c(385:404))
        
        ggplot(no_listcols, aes(x = id, y = location, fill = n)) +
          geom_tile()+
          scale_fill_viridis_c(option = "magma")+
          labs(x = "Entry ID Number", y = "Location in Hyrule", title = "Where a Subset of Compendium Entries Are Found in Hyrule (Each Entry Only Shows Once in a Single Location)", fill = "# of Entry Appearances")
        
      } else {
        no_listcols <- no_listcols |>
          count(location, game)
        
        ggplot(no_listcols, aes(x = game, y = location, fill = n)) +
          geom_tile()+
          scale_fill_viridis_c(option = "turbo")+
          labs(x = "Game (BOTW or TOTK)", y = "Location in Hyrule", title = "Where Compendium Entries are Found in Hyrule (Top 10 Most Common Locations) for Each Game (BOTW or TOTK)", fill = "# Entries")
        
      }
      
    } else if(input$select_y == "Equipment Defense Power"){
      
      GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/category/equipment?game=botw"))
      parsed <- fromJSON(rawToChar(GET_result$content), flatten = T)
      botw <- as_tibble(parsed$data)
      botw <- mutate(botw, game = "BOTW")
      
      GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/category/equipment?game=totk"))
      parsed <- fromJSON(rawToChar(GET_result$content), flatten = T)
      totk <- as_tibble(parsed$data)
      totk <- mutate(totk, game = "TOTK")
      
      combined <- bind_rows(botw, totk)
      
      combined <- combined |>
        mutate(defense = map_dbl(properties.defense, ~ ifelse(is.null(.x), NA_real_, .x)))
      
      if(input$select_x == "Category"){
        
        ggplot(combined, aes(x = category, y = defense, color = game))+
          geom_point() +
          labs(x = "Category", y = "Defense Power", title = "Defense Power by Compendium Category (Only the 'Equipment' Category Has Defense Power)", color = "Game")
        
      } else if(input$select_x == "Compendium ID Number"){
        
        ggplot(combined, aes(x = id, y = defense, color = game))+
          geom_point() +
          labs(x = "Entry ID Number", y = "Equipment Defense Power", title = "Equipment Defense Power by ID Number of Compendium Entry", color = "Game")
        
      } else {
        
        ggplot(combined, aes(x = game, y = defense, color = game))+
          geom_point() +
          scale_color_manual(values = c("BOTW" = "lightgreen", "TOTK" = "steelblue"))+
          labs(x = "Game (BOTW or TOTK)", y = "Equipment Defense Power", title = "Equipment Defense Power by Game (BOTW or TOTK)", color = "Game")
        
      } 
    }
    
  })
  
}

# Run App --------------------------------------------------------------------
shinyApp(ui = ui, server = server)