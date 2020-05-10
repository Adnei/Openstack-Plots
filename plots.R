source('objects.R')
source('db_interact.R')
source('data_handler.R')

plots.plot_schema <- function(schema, label_pos, col_arr, is_pdf=FALSE){
  if(is_pdf){
      pdf(schema$info$pdf)
  }
  par(mar = c(6, 6, 3.5, 3.5), mgp = c(5, 1, 0))
  plot(
    seq(0,2), seq(0,2),
    xlim = c(0,max(x_max_array)),
    ylim = c(0,max(ceiling(y_max_array))),
    lwd = 1,
    type = 'n',
    xlab = 'Time (seconds)',
    ylab = 'Traffic Volume (MB)',
    cex.lab = 1.2,
    font.lab = 2,
    xaxt = 'n',
    xaxs="i",
    yaxs="i",
    main = schema$info$title
  )

  op_idx <- 1
  for(operation in schema$operations){
    axis_values <- c(min(schema$timeline[[operation]]$second),
                      max(schema$timeline[[operation]]$second))
    lines(schema$timeline[[operation]]$second,
      schema$timeline[[operation]]$traffic,
      col=col_arr[op_idx], lwd=3, type='l')
    axis(1,axis_values,
      col=col_arr[op_idx], lwd=2, padj = label_pos[op_idx], cex.axis=0.9)
    op_idx <- op_idx + 1

  }

  legend('top',
     legend = c('CREATE() Operation', 'SUSPEND() Operation','RESUME() Operation','STOP() Operation','SHELVE() Operation'),
     col = col_arr,
     lty=1, lwd=2, cex=0.8, text.font=2, box.lty=0, bg="transparent")

  if(is_pdf){
    dev.off()
  }
}
