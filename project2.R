library("httr")
library("jsonlite")
library("tidyverse")
library("shiny")
library("bslib")
library("ggplot2")

query_category_andor_hearts <- function(category, hearts = -0.5){
  GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/category/", category))
                    parsed <- fromJSON(rawToChar(GET_result$content))
                    info <- as_tibble(parsed$data)
                    
                    if(hearts == -0.5){
                      return(info)
                    } else if(category %in% c("materials", "creatures")){
                      info2 <- filter(info, hearts_recovered == hearts)
                      return(info2)
                    } else {
                      print("No heart recovery information included. Here's the input category, isolated, as is:")
                      return(info)
                    }
}

#examples
query_category_andor_hearts("equipment", 1)
query_category_andor_hearts("materials", 0.5)
query_category_andor_hearts("creatures", 1)

query_entry_andor_game <- function(entry_number, botw_or_totk = "botw"){
  if(entry_number == 0){
    GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/all", "?game=", botw_or_totk))
    parsed <- fromJSON(rawToChar(GET_result$content))
    info <- as_tibble(parsed$data)
    return(info)
  } else {
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
    
    return(info_tibble)
  }
  
}

#examples
query_entry_andor_game(1, "totk")
query_entry_andor_game(1, "botw")
query_entry_andor_game(0, "totk")

#Contingency tables
botw <- query_entry_andor_game(0, "botw") #the "default" compendium is botw 
totk <- query_entry_andor_game(0, "totk")

table(botw$category)
table(botw$category, botw$edible, useNA = "always")
table(unlist(botw$common_locations), useNA = "no")

#Numerical summaries


#Four plots, one of which being a plot we didn't cover in class 
