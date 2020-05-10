data_handler.groupPackets <- function(data, step=1, from=0, to=ceiling(max(data$ts) - min(data$ts))){
  axis.x <- seq(from,to, by=step)
  sum_by_sec <- c()
  reference_time <- min(data$ts)

  # This is not so pretty... should try and not use for...
  # Appending to a vector may be slow
  #https://stackoverflow.com/questions/22235809/append-value-to-empty-vector-in-r
  idx <- 1
  for( second in axis.x ){
  # interval <- data$ts[( (data$ts - reference_time) >= second) & ( (data$ts - reference_time < second +1 ) )]
  # sum_by_sec[idx] = sum(data$MB[ match(interval, data$ts)])
  #The following code line is the same as the comment above
    sum_by_sec[idx] = sum(data$MB[( (data$ts - reference_time) >= second) & ( (data$ts - reference_time < second + 1) )])
  #Finds an interval, and sums up each packet size (data$MB) in the interval
  #less readable, but faster :)
    idx <- idx +1
  }

  return (data.frame(x=axis.x, y=sum_by_sec))
}



##################################################################################
# ATTENTION! This function is slow and causes a lot of huge copies from memory   #
##################################################################################
data_handler.fill_final_schema_traffic_by_second <- function(schema){
  final_schema <- schema
  start_x <- 0
  y_max <- 0
  x_max <- 0
  op_idx <- 1
  final_schema$timeline <- list()
  ## It will get data from a random execution from the execution list
  final_schema$sample_exec_id <- sample(final_schema$executions_id_list, 1)
  for(operation in final_schema$operations){
      final_schema$timeline[[operation]] <- list()
      traffic_info <- db_interact.get_traffic_info(final_schema$image_name, operation, final_schema$sample_exec_id)
      traffic_by_second <- data_handler.groupPackets(traffic_info)
      final_schema$timeline[[operation]]$second <- traffic_by_second$x + start_x
      final_schema$timeline[[operation]]$traffic <- traffic_by_second$y
      y_max <- max(c(y_max, final_schema$timeline[[operation]]$traffic))
      x_max <- max(c(x_max, final_schema$timeline[[operation]]$second))
      start_x <- max(final_schema$timeline[[operation]]$second) + 1
      op_idx <- op_idx + 1
  }

  final_schema$timeline$y_max <- y_max
  final_schema$timeline$x_max <- x_max

  return(final_schema)

}
