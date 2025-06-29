library("httr")
library("jsonlite")
library("tidyverse")
library("shiny")
library("bslib")

#UI
ui <- page_fluid(
  navset_card_underline(
    nav_panel("About", 
              p("The purpose of this app is to search the Hyrule Compendium and display its data in various ways."), 
              p("The data comes from the Hyrule Compendium API, which can be found", 
                a("here", href = "https://gadhagod.github.io/Hyrule-Compendium-API/#/", target = "_blank")), 
              p("The current tab is the 'About' tab, which explains the app. The 'Data Download' tab allows you to query data and save your query. The 'Data Exploration' tab allows you to customize and display numerical/graphical summaries of the data."),
              img(src = "picture.jpg", height = "300px")
    ),
    
    nav_panel("Data Download",),
    
    nav_panel("Data Exploration",)
  )
)

#Server
server <- 
  
  #Run app 
  shinyApp(ui = ui, server = server)