library(rmarkdown)
rmarkdown::render('boxplot/report_boxplot_final.Rmd',
                  output_file = paste('boxplot_report.', Sys.Date(),
                                      '.pdf', sep=''))
