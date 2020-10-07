library(dplyr)
source('objects.R')
source('db_interact.R')
source('data_handler.R')

CONST_API_SERVICES <- c('nova', 'keystone', 'glance','cinder','neutron','heat')

#@ATTENTION -> rbind causes a lot of copies of memory. It gets very, very slow for really large dfs
#@FIXME -> Should try and pre-allocate the whole df. This will prevent R from allocating it a ton times

build_traffic_info <- function(operation, exec, image, params.DB_PATH){
  traffic_by_packet <- db_interact.get_traffic_info(image, operation, exec, database=params.DB_PATH)
  total_traffic <- db_interact.get_total_traffic(image, operation, exec, service='TOTAL', database = params.DB_PATH)
  total_api_calls <- db_interact.get_api_calls_counter(image, operation, exec, service='TOTAL', database=params.DB_PATH)
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
    op_exec_time_mean = NA,
    tt_mean = NA,
    tt_sd = NA,
    tt = total_traffic,
    ta = total_api_calls,
    ta_sd = NA,
    ta_mean = NA
  )
  return(exec.df)
}


db_list <- c(
  'fedora_bionic_30/network_metering_experiment.db',
  'exp_windows/30_exec/network_metering_experiment.db'
)
data.df <- data.frame()
traffic_info.df <- data.frame()
api_calls.df <- data.frame()
total_traffic.df <- data.frame()

for (db in db_list){
  params.DB_PATH <- paste0(params.COMMON_PATH, db)
  #FIXME It assumes there are no repeated images inside the databases
  db_images <- db_interact.get_images(database=params.DB_PATH)
  services <- c(db_interact.get_services(database=params.DB_PATH),'MISC')
  api_services <- intersect(services, CONST_API_SERVICES)
  for(operation in objects.operations){
    for(image in db_images){
      executions <- db_interact.get_exec_list_by_image(image, database=params.DB_PATH)
      for(exec in executions){
        exec_traffic_info.df <- build_traffic_info(operation, exec, image, params.DB_PATH)
        traffic_info.df <- rbind(traffic_info.df, exec_traffic_info.df)
        #Service Traffic
        exec_service_traffic.df <- db_interact.get_service_traffic(image, operation, database=params.DB_PATH)
        no_traffic <- services[!(services %in% exec_service_traffic.df$service)]
        if (length(no_traffic) > 0){
          no_traffic_services.df <- data.frame(service=no_traffic, traffic=0)
          exec_service_traffic.df <- rbind(exec_service_traffic.df, no_traffic_services.df)
        }
        aux_traffic.df <- data.frame(image = image,
          operation = operation,
          exec_id = exec,
          service = exec_service_traffic.df$service,
          service_traffic = exec_service_traffic.df$traffic,
          s_traffic_mean = NA,
          s_traffic_sd = NA)
        total_traffic.df <- rbind(total_traffic.df, aux_traffic.df)
        #API Calls
        exec_service_calls.df <- db_interact.get_service_calls(image, operation, exec, database=params.DB_PATH)
        not_called <- api_services[!(api_services %in% exec_service_calls.df$service)]
        if (length(not_called) > 0){
          not_called_services.df <- data.frame(service=not_called, calls=0)
          exec_service_calls.df <- rbind(exec_service_calls.df, not_called_services.df)
        }

        aux_api.df <- data.frame(image = image,
          operation = operation,
          exec_id = exec,
          service = exec_service_calls.df$service,
          service_calls = exec_service_calls.df$calls,
          s_calls_mean = NA,
          s_calls_sd = NA)
        api_calls.df <- rbind(api_calls.df, aux_api.df)
      }
      #calculate traffic_info stats
      current.df <- traffic_info.df[traffic_info.df$image == image & traffic_info.df$operation == operation,]
      exec_traffic.df <- unique(select(current.df, exec_id, exec_traffic_sd, exec_time, tt, ta))
      op_traffic_sd <- sd(exec_traffic.df$exec_traffic_sd)
      op_traffic_mean <- mean(exec_traffic.df$exec_traffic_sd)
      op_exec_time_sd <- sd(exec_traffic.df$exec_time)
      op_exec_time_mean <- mean(exec_traffic.df$exec_time)
      tt_mean <- mean(exec_traffic.df$tt)
      tt_sd <- sd(exec_traffic.df$tt)
      ta_mean <- ceiling(mean(exec_traffic.df$ta))
      ta_sd <- sd(exec_traffic.df$ta)

      #fill traffic_info
      traffic_info.df[traffic_info.df$image == image & traffic_info.df$operation == operation,]$op_traffic_sd = op_traffic_sd
      traffic_info.df[traffic_info.df$image == image & traffic_info.df$operation == operation,]$op_exec_time_sd = op_exec_time_sd
      traffic_info.df[traffic_info.df$image == image & traffic_info.df$operation == operation,]$tt_sd = tt_sd
      traffic_info.df[traffic_info.df$image == image & traffic_info.df$operation == operation,]$ta_sd = ta_sd
      traffic_info.df[traffic_info.df$image == image & traffic_info.df$operation == operation,]$op_traffic_mean = op_traffic_mean
      traffic_info.df[traffic_info.df$image == image & traffic_info.df$operation == operation,]$op_exec_time_mean = op_exec_time_mean
      traffic_info.df[traffic_info.df$image == image & traffic_info.df$operation == operation,]$tt_mean = tt_mean
      traffic_info.df[traffic_info.df$image == image & traffic_info.df$operation == operation,]$ta_mean = ta_mean

      data.df <- rbind(data.df, traffic_info.df)

      #calculate service traffic stats
      for(service in services){
        service_traffic.df <- total_traffic.df[total_traffic.df$image == image & total_traffic.df$operation == operation & total_traffic.df$service == service,]
        total_traffic.df[total_traffic.df$image == image & total_traffic.df$operation == operation & total_traffic.df$service == service,]$s_traffic_mean = mean(service_traffic.df$service_traffic)
        total_traffic.df[total_traffic.df$image == image & total_traffic.df$operation == operation & total_traffic.df$service == service,]$s_traffic_sd = sd(service_traffic.df$service_traffic)

        #calculate service calls stats
        if(service %in% api_services){
          service_calls.df <- api_calls.df[api_calls.df$image == image & api_calls.df$operation == operation & api_calls.df$service == service,]
          api_calls.df[api_calls.df$image == image & api_calls.df$operation == operation & api_calls.df$service == service,]$s_calls_mean = ceiling(mean(service_calls.df$service_calls))
          api_calls.df[api_calls.df$image == image & api_calls.df$operation == operation & api_calls.df$service == service,]$s_calls_sd = sd(service_calls.df$service_calls)
        }
      }
    }
  }
}
