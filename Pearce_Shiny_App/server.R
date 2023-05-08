#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
devtools::install_github("pearce790/rankrate")
library(rankrate)
library(ggplot2)
library(reshape2)
library(toOrdinal)
library(highcharter)
library(shinycssloaders)
library(plotly)
library(DT)
library(ggrepel)
library(latexpdf)
library(knitr)

# Define server logic required to draw a histogram
function(input, output, session) {
    source("server/dataServer.R",local=T)
    source("server/MB.R",local=T)
    source("server/dataUpload.R", local=T)
}
