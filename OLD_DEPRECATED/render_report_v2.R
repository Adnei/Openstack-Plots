library(rmarkdown)
rmarkdown::render('report_v2.Rmd',
                  output_file = paste('report.', Sys.Date(),
                                      '.pdf', sep=''))
# render("report.Rmd")
