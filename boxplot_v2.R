library(dplyr)
source('objects.R')
source('db_interact.R')
source('data_handler.R')


#@ATTENTION -> rbind causes a lot of copies of memory. It gets very, very slow for really large dfs
#@FIXME -> Should try and pre-allocate the whole df. This will prevent R from allocating it a ton times

build_traffic_info <- function(operation, exec_list, image){
  traffic_info.df <- data.frame()
  for(exec in exec_list){
    traffic_by_packet <- db_interact.get_traffic_info(image, operation, exec)
    amount_by_sec.df <- data_handler.groupPackets(traffic_by_packet)
    time <- amount_by_sec.df$x       # second
    traffic <- amount_by_sec.df$y    # MB

    exec.df <- data.frame(
      image = image,
      operation = operation,
      exec_id = exec,
      traffic = traffic,
      time = time,
      exec_time = max(time) - min(time),
      exec_traffic_sd = sd(traffic),
      exec_traffic_mean = mean(traffic),
      op_traffic_sd = NA,
      op_traffic_mean = NA,
      op_exec_time_sd = NA,
      op_exec_time_mean = NA
    )
    traffic_info.df <- rbind(traffic_info.df, exec.df)
  }

  return(traffic_info.df)
}

data.df <- data.frame()
for(operation in objects.operations){
  for(image in objects.images){
      executions <- db_interact.get_exec_list_by_image(image)
      traffic_info.df <- build_traffic_info(operation, executions, image)
      current.df <- traffic_info.df[traffic_info.df$image == image & traffic_info.df$operation == operation,]
      exec_traffic.df <- unique(select(current.df, exec_id, exec_traffic_sd, exec_time))
      op_traffic_sd <- sd(exec_traffic.df$exec_traffic_sd)
      op_traffic_mean <- mean(exec_traffic.df$exec_traffic_sd)
      op_exec_time_sd <- sd(exec_traffic.df$exec_time)
      op_exec_time_mean <- mean(exec_traffic.df$exec_time)
      traffic_info.df[traffic_info.df$image == image & traffic_info.df$operation == operation,]$op_traffic_sd = op_traffic_sd
      traffic_info.df[traffic_info.df$image == image & traffic_info.df$operation == operation,]$op_exec_time_sd = op_exec_time_sd
      traffic_info.df[traffic_info.df$image == image & traffic_info.df$operation == operation,]$op_traffic_mean = op_traffic_mean
      traffic_info.df[traffic_info.df$image == image & traffic_info.df$operation == operation,]$op_exec_time_mean = op_exec_time_mean
      data.df <- rbind(data.df, traffic_info.df)
  }
}
