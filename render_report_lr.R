library(rmarkdown)
rmarkdown::render('report_lr.Rmd',
                  output_file = paste('report.', Sys.Date(),
                                      '.pdf', sep=''))
# render("report.Rmd")
