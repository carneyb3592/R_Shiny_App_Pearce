getParams <- reactive({
  
  plots <- input$reportPlots
  return(list(plot1=ratings_plot_input(),
               plot2=rankings_plot_input(),
               plot3=inconsistencies_plot_input(),
               plot4=mb_quality_plot_input(),
               plot5=mb_rank_plot_input(),
               plot6=mb_mean_plot_input(),
               plotlist=plots
  ))
})


output$downloadReport <- downloadHandler(
  filename = function() {
    paste('MBReport.pdf', sep='')
  },
  content = function(file) {
    showModal(modalDialog("Processing Report, please wait...", footer=NULL))
    tempReport <- file.path(tempdir(),"report.Rmd")
    file.copy("report.Rmd",tempReport,overwrite = TRUE)
    params <- getParams()
    
    rmarkdown::render(tempReport,output_file = file,
                      output_format = "pdf_document",
                      params = params,
                      envir = new.env(parent=globalenv()),
                      
    )
    on.exit(removeModal())
  }
)

