#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(shinyjs)
library(plotly)
library(DT)
###########  UI ELEMENTS ##################
source("ui/homeUI.R",local=T)
source("ui/dataUI.R",local=T)
source("ui/MBUI.R",local=T)


###########################################


shinyUI(
  navbarPage(title=div(img(src="rankrate_logo.png",
                           height = 25,
                           width = 25,
                           style = "margin:1px 1px"),
                       "Peer Review"),
             homeUI,
             dataUI,
             MBUI
  )
)
