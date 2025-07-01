# Project2

**Description of App + App Purpose**
This app allows users to search the Hyrule Compendium (Legend of Zelda) in a few different ways:

1) By category (equipment, monsters, materials, etc.) and/or by how many hearts Link recovers when he ingests items from said category
2) By the individual entry numbers assigned to each compendium item as relevant to either of the open world LoZ games (Breath of the Wild (BOTW) and Tears of the Kingdom (TOTK))

It also displays the following summaries based on input data:
- Contingency tables
- Numerical summaries across categorical variables
- Plots 

**Packages Needed to Run App**
- httr
- jsonlite
- tidyverse
- shiny
- bslib
- ggplot2
- DT

**Code to Install All Packages**

install.packages(c("httr", "jsonlite", "tidyverse", "shiny", "bslib", "ggplot2", "DT"))

**Copy and Paste This into RStudio to Run App**

shiny::runGitHub(repo = "Project2", username = "haydenmorgan1999")