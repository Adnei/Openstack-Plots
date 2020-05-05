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
