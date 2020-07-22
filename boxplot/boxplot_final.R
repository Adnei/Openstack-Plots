library(dplyr)
source('objects.R')
source('db_interact.R')
source('data_handler.R')

CONST_API_SERVICES <- c('nova', 'keystone', 'glance','cinder','neutron','heat')
db_list <- c(
  'fedora_bionic_30/network_metering_experiment.db',
  'exp_windows/30_exec/network_metering_experiment.db'
)

db.df <- db_interact.filter_images_by_db(db_list)
# exclude_images.df <- db_interact.get_excluded_images(db.df, db_list)

columns_len <- length(unique(db.df$image)) * length(objects.operations)
data_info.df <- data.frame(
  image = character(columns_len),
  operation = character(columns_len),
  exec_time_mean = numeric(columns_len),
  exec_time_sd = numeric(columns_len),
  total_traffic_mean = numeric(columns_len),
  total_traffic_sd = numeric(columns_len),
  total_api_calls_mean  = numeric(columns_len),
  total_api_calls_sd = numeric(columns_len),
  stringsAsFactors=FALSE
 )
boxplot_data.df <- data.frame()
counter <- 0
#FIXME --> Should try and use plyr summarize if possible
for(database_path in unique(db.df$database)){
  traffic_info.df <- db_interact.get_traffic_info(database=database_path)
  db_images <- unique(db.df[db.df$database == database_path,]$image)
  total_traffic.df <- db_interact.get_service_traffic(total=TRUE, database=database_path)
  total_api_calls.df <- db_interact.get_service_calls(total=TRUE, database=database_path)

  for(image in db_images){
    for(operation in objects.operations){
      counter <- counter + 1
      traffic_arr <- c()
      exec_time_arr <- c()
      total_traffic_arr <- c()
      total_api_calls_arr <- c()
      for(exec_id in db_interact.get_exec_list_by_image(image, database=database_path)){
        this.traffic_info.df <- traffic_info.df[traffic_info.df$operation == operation &
        traffic_info.df$exec_id == exec_id &
        traffic_info.df$image == image, ]

        this.total_traffic.df <- total_traffic.df[total_traffic.df$operation == operation &
        total_traffic.df$exec_id == exec_id &
        total_traffic.df$image == image, ]

        this.total_calls.df <- total_api_calls.df[total_api_calls.df$operation == operation &
        total_api_calls.df$exec_id == exec_id &
        total_api_calls.df$image == image, ]

        amount_by_sec.df <- data_handler.groupPackets(this.traffic_info.df)
        exec_time <- max(amount_by_sec.df$x) - min(amount_by_sec.df$x)

        traffic_arr <- c(traffic_arr, amount_by_sec.df$y) #amount_by_sec.df$y == traffic in MB
        exec_time_arr <- c(exec_time_arr, exec_time)
        total_traffic_arr <- c(total_traffic_arr, this.total_traffic.df$traffic)
        total_api_calls_arr <- c(total_api_calls_arr, this.total_calls.df$calls)
      }
      data_info.df[counter,] <- c(image,
        operation,
        mean(exec_time_arr),
        sd(exec_time_arr),
        mean(total_traffic_arr),
        sd(total_traffic_arr),
        mean(total_api_calls_arr),
        sd(total_api_calls_arr)
      )
      aux_boxplot.df <- data.frame(
        image = image,
        operation = operation,
        traffic = traffic_arr
      )
      boxplot_data.df <- rbind(boxplot_data.df, aux_boxplot.df)
    }
  }

  rm(traffic_info.df)
  gc()
}
