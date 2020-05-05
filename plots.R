library(RSQLite)
source('objects.R')
source('db_interact.R')
source('data_handler.R')

plots_schema <- objects.build_schema()
label_pos <- c(0,1,2,3,0)
col_arr <- c('#000080', '#800000', '#267326', '#e68a00', '#cc0000')
schema_counter <- 1
y_max <- 0
x_max <- 0


for(schema in plots_schema){
  start_x <- 0
  plots_schema[[schema_counter]]$timeline <- list()
  exec_id <- sample(schema$executions_id_list, 1)

  pdf(schema$info$pdf)
  par(mar = c(6, 6, 3.5, 3.5), mgp = c(5, 1, 0))
  plot.new()
  # plot(
  #   seq(0,2), seq(0,2),
  #   lwd = 1,
  #   type = 'n',
  #   xlab = 'Time (seconds)',
  #   ylab = 'Traffic Volume (MB)',
  #   cex.lab = 1.2,
  #   font.lab = 2,
  #   xaxt = 'n',
  #   xaxs="i",
  #   yaxs="i",
  #   main = schema$info$title
  # )

  op_idx <- 1
  for(operation in schema$operations){
      plots_schema[[schema_counter]]$timeline[[operation]] <- list()
      traffic_info <- db_interact.get_traffic_info(schema$image_name, operation, exec_id)
      traffic_by_second <- data_handler.groupPackets(traffic_info)
      plots_schema[[schema_counter]]$timeline[[operation]]$second <- traffic_by_second$x + start_x
      plots_schema[[schema_counter]]$timeline[[operation]]$traffic <- traffic_by_second$y
      y_max <- max(c(y_max, plots_schema[[schema_counter]]$timeline[[operation]]$traffic))
      x_max <- max(c(x_max, plots_schema[[schema_counter]]$timeline[[operation]]$second))

      lines(plots_schema[[schema_counter]]$timeline[[operation]]$second,
          plots_schema[[schema_counter]]$timeline[[operation]]$traffic,
          col=col_arr[op_idx], lwd=3, type='l')
      # axis(1, seq(0,x_max))
      # axis(1, c(min(plots_schema[[schema_counter]]$timeline[[operation]]$second),
      #     max(plots_schema[[schema_counter]]$timeline[[operation]]$second)),
      #   col=col_arr[op_idx], lwd=2, padj = label_pos[op_idx], cex.axis=0.9)



      start_x <- max(plots_schema[[schema_counter]]$timeline[[operation]]$second) + 1
      op_idx <- op_idx + 1
  }


  plot.window(ylim = c(0,ceiling(y_max)), xlim = c(0,x_max))

  # plot.window(xlim = c(0,x_max), ylim = c(0,ceiling(y_max)) )

  plot(
    seq(0,2),
    xlim = c(0,x_max),
    ylim = c(0,ceiling(y_max)),
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

  # pdf(schema$info$pdf)
  # par(mar = c(6, 6, 3.5, 3.5), mgp = c(5, 1, 0))
  # plot.window(
  #   ylim = c(0,ceiling(y_max)),
  #   xlim = c(0,x_max),
  #   lwd = 1,
  #   # type = 'n',
  #   xlab = 'Time (seconds)',
  #   ylab = 'Traffic Volume (MB)',
  #   cex.lab = 1.2,
  #   font.lab = 2,
  #   xaxt = 'n',
  #   main = schema$info$title
  # )

  dev.off()
  schema_counter <- schema_counter + 1
}
