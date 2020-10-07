library(dplyr)
source('objects.R')
source('db_interact.R')
source('data_handler.R')

CONST_API_SERVICES <- c('nova', 'keystone', 'glance','cinder','neutron','heat')

db.df <- db_interact.filter_images_by_db(params.db_list)
# db.df <- db.df[db.df$image != 'debian10raw',]
db.df <- db.df[db.df$image %in% c('fedora31'), ]
columns_len <- length(db.df$image) * length(objects.operations)
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
exec_time.df <- data.frame()
total_traffic.df <- data.frame()
total_api.df <- data.frame()
counter <- 0

for(image in db.df$image){
  database_path <- db.df[db.df$image == image, ]$database
  traffic_info.df <- db_interact.get_traffic_info(database=database_path)


  for(operation in objects.operations){
    traffic_arr <- c()
    exec_time_arr <- c()
    for(exec_id in db_interact.get_exec_list_by_image(image, database=database_path)){
      this.traffic_info.df <- traffic_info.df[traffic_info.df$operation == operation &
        traffic_info.df$exec_id == exec_id &
        traffic_info.df$image == image, ]
      amount_by_sec.df <- data_handler.groupPackets(this.traffic_info.df)
      exec_time <- max(amount_by_sec.df$x) - min(amount_by_sec.df$x)
      traffic_arr <- c(traffic_arr, amount_by_sec.df$y) #amount_by_sec.df$y == traffic in MB
      exec_time_arr <- c(exec_time_arr, exec_time)
    }
    aux_boxplot.df <- data.frame(
      image = image,
      operation = operation,
      traffic = traffic_arr
    )
    aux_exec_time.df <- data.frame(
      image = image,
      operation = operation,
      exec_time = exec_time_arr
    )
    boxplot_data.df <- rbind(boxplot_data.df, aux_boxplot.df)
    exec_time.df <- rbind(exec_time.df, aux_exec_time.df)
  }

  # traffic_info may use a lot of memory. Once boxplot_data.df is done, traffic_info.df is no longer needed
  rm(traffic_info.df)
  rm(this.traffic_info.df)
  gc()

  db_total_traffic.df <- db_interact.get_service_traffic(total=TRUE, database=database_path)
  db_total_api_calls.df <- db_interact.get_service_calls(total=TRUE, database=database_path)
  total_traffic.df <- rbind(total_traffic.df, db_total_traffic.df)
  total_api.df <- rbind(total_api.df, db_total_api_calls.df)

}
