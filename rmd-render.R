library(rmarkdown)
rmarkdown::render('report.Rmd',
                  output_file = paste('report.', Sys.Date(),
                                      '.pdf', sep=''))
# render("report.Rmd")
