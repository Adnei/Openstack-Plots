library(RSQLite)
source('objects.R')
source('db_interact.R')
source('data_handler.R')

blank_schema_list <- objects.build_schema()
traffic_by_second_schemas <- vector('list', length(blank_schema_list))
x_max_array <- c()
y_max_array <- c()

schema_counter <- 1
## First fill plots schema with data and then plot
for(schema in blank_schema_list) {
  #blank_schema_list is preserved so it can be used again to create other plots with other kind of data (packets by second, etc)
  traffic_by_second_schemas[[schema_counter]] <- data_handler.fill_final_schema_traffic_by_second(schema)
  x_max_array[schema_counter] <- traffic_by_second_schemas[[schema_counter]]$timeline$x_max
  y_max_array[schema_counter] <- traffic_by_second_schemas[[schema_counter]]$timeline$y_max
  schema_counter <- schema_counter + 1
}


label_pos <- c(0,1,2,3,0)
col_arr <- c('#000080', '#663300', '#267326', '#e68a00', '#cc0000')


for(plot_schema in traffic_by_second_schemas){
  pdf(plot_schema$info$pdf)
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
    main = plot_schema$info$title
  )

  op_idx <- 1
  for(operation in plot_schema$operations){
    axis_values <- c(min(plot_schema$timeline[[operation]]$second),
                      max(plot_schema$timeline[[operation]]$second))
    lines(plot_schema$timeline[[operation]]$second,
      plot_schema$timeline[[operation]]$traffic,
      col=col_arr[op_idx], lwd=3, type='l')
    axis(1,axis_values,
      col=col_arr[op_idx], lwd=2, padj = label_pos[op_idx], cex.axis=0.9)
    op_idx <- op_idx + 1

  }

  legend('top',
     legend = c('CREATE() Operation', 'SUSPEND() Operation','RESUME() Operation','STOP() Operation','SHELVE() Operation'),
     col = col_arr,
     lty=1, lwd=2, cex=0.8, text.font=2, box.lty=0, bg="transparent")

  dev.off()
}
