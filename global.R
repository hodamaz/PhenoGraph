library(dplyr)
library(magrittr)
library(shiny)
library(shinythemes)
library(tidyverse)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
library(shinybusy)
library(kableExtra)
library(shinyBS)
library(rsconnect)
library(auth0)
library(DT)
library(readxl)
library(kableExtra)
library(knitr)
library(ggbeeswarm)
library(patchwork)
library(performance)
library(easystats)

user = "admin"
pass = "admin"

indep_names <- c("ACC", 
                 "Environment", 
                 "Population",
                 "SEL", 
                 "SOILvsSALT",
                 "CONvsANC", 
                 "AvsC",
                 "PSvsSS")
