library("httr")
library("jsonlite")
library("tidyverse")
library("shiny")
library("bslib")

query_hearts <- function(category, hearts){
  GET_result <- GET(paste0("https://botw-compendium.herokuapp.com/api/v3/compendium/category/", category))
                    parsed <- fromJSON(rawToChar(GET_result$content))
                    info <- as_tibble(parsed$data)
                    
                    if(category %in% c("materials", "creatures")){
                      info2 <- filter(info, hearts_recovered == hearts)
                      return(info2)
                    } else {
                      print("No heart recovery information included. Here's the input category, isolated, as is:")
                      return(info)
                    }
}

#examples
query_hearts("equipment", 1)
query_hearts("materials", 0.5)

query_entry_game <- function(entry_number, botw_or_totk = "botw"){
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
  ) #had to build a tibble because the row lengths were not cooperative at first
  
  return(info_tibble)
}

#examples
query_entry_game(18, "totk")
query_entry_game(18, "botw")
query_entry_game(110)
