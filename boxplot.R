source('objects.R')
source('db_interact.R')
source('data_handler.R')




boxplot.blank_schema_list <- objects.build_schema()
boxplot.boxplot_list <- vector('list', length(boxplot.blank_schema_list))
schema_counter <- 1

for(schema in boxplot.blank_schema_list){
  boxplot_schema <- schema
  for(operation in schema$operations){
    boxplot_schema[[operation]] = list(all_traffic = c(), execution = list(), operation_sd = 0, operation_mean = 0, execution_time_list=c() )
    operation_traffic_by_second <- c()
    execution_sd_list <- c()
    execution_mean_list <- c()
    execution_times <- c()
    for(execution_id in schema$executions_id_list){
      traffic_info <- db_interact.get_traffic_info(boxplot_schema$image_name, operation, execution_id)
      traffic_by_second <- data_handler.groupPackets(traffic_info)
      execution_time <- max(traffic_by_second$x) - min(traffic_by_second$x)
      operation_traffic_by_second <- c(operation_traffic_by_second, traffic_by_second$y)
      boxplot_schema[[operation]]$execution[[as.character(execution_id)]] = traffic_by_second$y
      execution_sd_list <- c(execution_sd_list, sd(traffic_by_second$y, na.rm=TRUE))
      execution_mean_list <- c(execution_mean_list, mean(traffic_by_second$y))
      execution_times <- c(execution_times, execution_time)
    }
    boxplot_schema[[operation]]$operation_sd <- sd(execution_sd_list, na.rm=TRUE)
    boxplot_schema[[operation]]$operation_mean <- mean(execution_mean_list)
    boxplot_schema[[operation]]$all_traffic <- operation_traffic_by_second
    boxplot_schema[[operation]]$execution_time_list <- execution_times
  }
  boxplot.boxplot_list[[schema_counter]] <- boxplot_schema
  schema_counter <- schema_counter + 1
}


#
# cat("\n")
# plot(x = seq(1,1), y = seq(1,1),
#   xlim = c(0,max_time),
#   ylim = c(0,1),
#   lwd = 1,
#   type = 'n',
#   lty = 1,
#   xlab = 'Time (seconds)',
#   ylab = 'Cumulative Proportion',
#   main = boxplot_obj$info$title
# )
# op_counter <- 1
# for(operation in experiment$operations){
#   lines(ecdf(boxplot_obj[[operation]]$execution_time_list), col=color_arr[op_counter])
#   op_counter <- op_counter + 1
# }
# legend( legend=experiment$operations, col=color_arr)
# cat("\n")




#
# for( boxplot_obj in boxplot.boxplot_list){
#   boxplot(
#     boxplot_obj$CREATE$all_traffic,
#     boxplot_obj$SUSPEND$all_traffic,
#     boxplot_obj$RESUME$all_traffic,
#     boxplot_obj$STOP$all_traffic,
#     boxplot_obj$SHELVE$all_traffic,
#     main = boxplot_obj$info$title,
#     names = boxplot_obj$operations,
#     col = c('red', 'orange', 'grey', 'blue', 'green'),
#     border = 'brown',
#     horizontal = TRUE, notch=FALSE, outline = FALSE)
# }
#


# boxplot(boxplot_obj$CREATE$all_traffic,
#   main = boxplot_obj$info$title,
#   names = c('CREATE'),
#   labels = c('CREATE'),
#   col = c('red'),
#   border = 'brown')

  # boxplot(log(boxplot_obj$CREATE$all_traffic), main = boxplot_obj$info$title, names = c('CREATE'), labels = c('CREATE'), col = c('red'), border = 'brown')


# plot(x = seq(1,1), y = seq(1,1),
#   xlim = c(3,max_time),
#   ylim = c(0,1),
#   lwd = 1,
#   type = 'n',
#   lty = 1,
#   xlab = 'Time (seconds)',
#   ylab = 'Cumulative Proportion',
#   main = boxplot_obj$info$title
# )
# op_counter <- 1
# color_arr <- c('red', 'orange', 'grey', 'blue', 'green')
# for(operation in op_arr){
#   # lines(ecdf(boxplot_obj[[operation]]$execution_time_list), col=color_arr[op_counter])
#   print(boxplot_obj[[operation]]$execution_time_list)
#   # op_counter <- op_counter + 1
# }
# legend('bottomright', legend=op_arr, col=color_arr, pch=15)
#
# plot(boxplot_obj$CREATE$execution_time_list, ylab="Fn(x)", verticals = FALSE, col.01line = "gray70", pch = 19)

# filter_fn <- function(element, match_title){
#   element[['info']][['title']] == match_title
# }
#
# #usage
# boxplot.boxplot_list[unlist(lapply(boxplot.boxplot_list,filter_fn,'Timeline: GNU/Linux Debian 10 (2000 MB)'))][[1]]
# boxplot.boxplot_list[unlist(lapply(boxplot.boxplot_list,filter_fn,'Timeline: GNU/Linux Debian 10 (550 MB)'))][[1]]
