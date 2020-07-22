library(rmarkdown)
rmarkdown::render('report_lr.Rmd',
                  output_file = paste('linear_regression_report.', Sys.Date(),
                                      '.pdf', sep=''))
# render("report.Rmd")
