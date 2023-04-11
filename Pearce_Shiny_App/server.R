#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rankrate)
library(ggplot2)
library(reshape2)
library(toOrdinal)
library(highcharter)
library(shinycssloaders)
library(plotly)
# Define server logic required to draw a histogram
function(input, output, session) {
    source("server/dataServer.R",local=T)
    source("server/MB.R",local=T)
    source("server/dataUpload.R", local=T)
}
