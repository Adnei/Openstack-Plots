source('params.R')
source('db_interact.R')
library(janitor)
library(reshape2)

objects.images <- db_interact.get_images()
objects.services <- db_interact.get_services()
objects.operations <- c('CREATE', 'SUSPEND', 'RESUME', 'STOP', 'SHELVE')
objects.nics <- db_interact.get_nics()

#@DEPRECATED
objects.get_plot_infos <- function(image){
  switch(image,
    fedora31={
      return (
        list(
          title = 'Timeline: GNU/Linux Fedora Cloud 31-1.9 (319 MB)',
          pdf = 'fedoraTimeLine.pdf'
        )
      )
    },focal_ubuntu={
      return (
        list(
          title = 'Timeline: GNU/Linux Ubuntu Server 20.04 (Focal Fossa, 508 MB)',
          pdf = 'ubuntuFocalTimeLine.pdf'
        )
      )
    },bionic_ubuntu={
      return (
        list(
          title = 'Timeline: GNU/Linux Ubuntu Server 18.04 (Bionic Beaver, 329 MB)',
          pdf = 'ubuntuBionicTimeLine.pdf'
        )
      )
    },centos7_light={
      return (
        list(
          title = 'Timeline: Centos7 (898 MB)',
          pdf = 'centosLightTimeLine.pdf'
        )
      )
    },centos7={
      return (
        list(
          title = 'Timeline: Centos7 (1300 MB)',
          pdf = 'centosTimeLine.pdf'
        )
      )
    },cirros={
      return (
        list(
          title = 'Timeline: CirrOS 0.4.0 (15 MB)',
          pdf = 'cirrosTimeLine.pdf'
        )
      )
    },debian10raw={
      return (
        list(
          title = 'Timeline: GNU/Linux Debian 10 (2000 MB)',
          pdf = 'debian10_raw.pdf'
        )
      )
    },debian10qcow2={
      return (
        list(
          title = 'Timeline: GNU/Linux Debian 10 (550 MB)',
          pdf = 'debian10_qcow2.pdf'
        )
      )
    },windows_server={
      return (
        list(
          title = 'Timeline: MS Windows Server2012 R2 (6600 MB)',
          pdf = 'windows_server.pdf'
        )
      )
    },{
      return (
        list(
          title = 'DEFAULT PLOT',
          pdf = 'default_plot.pdf'
        )
      )
    }
  )
}

#@DEPRECATED
objects.build_schema <- function(){
  #static list is the best way of adding to a list inside a loop
  #yet so, adding items to a list inside a loop is not encouraged
  schema <- vector('list', length(objects.images))
  count <- 1

  for(image in objects.images){
    executions <- db_interact.get_exec_list_by_image(image)
    info <- objects.get_plot_infos(image)
    image_obj <- list(image_name = image,
      info = info,
      executions_id_list = executions,
      operations = objects.operations,
      nics = objects.nics
    )
    schema[[count]] <- image_obj

    count <- count +1
  }
  return(schema)
}

objects.build_api_calls_df <- function(os_image, operation_list, service_list, exec_id, database=params.DEFAULT_DATABASE){
  elements_counter <- 1
  redundant_data <- data.frame(
                    service=vector(length=length(service_list) * length(operation_list)),
                    operation=vector(length=length(service_list) * length(operation_list)),
                    calls=vector(length=length(service_list) * length(operation_list))
                  )
  for(service in service_list){
    for(operation in operation_list){
      redundant_data$service[elements_counter] <- service
      redundant_data$operation[elements_counter] <- operation
      redundant_data$calls[elements_counter] <- db_interact.get_api_calls_counter(os_image, operation, exec_id, service=service, database=database)
      elements_counter <- elements_counter + 1
    }
  }

  result_set <- dcast(redundant_data, operation~service, fun.aggregate=sum)
  #ordering by operation (custom order)
  result_set$operation <- ordered(result_set$operation, c('CREATE', 'SUSPEND', 'RESUME', 'STOP', 'SHELVE') )
  #FIXME Should use service_list to order and not do it by hand
  ordered_set <- result_set[with(result_set, order(operation, cinder, glance, heat, keystone, neutron, nova)),]
  return(ordered_set)
}

objects.build_total_traffic <- function(os_image, operation_list, service_list, exec_id, database=params.DEFAULT_DATABASE){

  service_list <- c(service_list, 'MISC')
  elements_counter <- 1
  redundant_data <- data.frame(
                    service=vector(length=length(service_list) * length(operation_list)),
                    operation=vector(length=length(service_list) * length(operation_list)),
                    traffic_mb=vector(length=length(service_list) * length(operation_list))
                  )

  for(service in service_list){
    for(operation in operation_list){
      redundant_data$service[elements_counter] <- service
      redundant_data$operation[elements_counter] <- operation
      redundant_data$traffic_mb[elements_counter] <- db_interact.get_total_traffic(os_image, operation, exec_id, service=service, database=database)
      elements_counter <- elements_counter + 1
    }
  }

  result_set <- dcast(redundant_data, operation~service, fun.aggregate=sum)
  result_set[is.na(result_set)] <- 0
  result_set <- adorn_totals(result_set, "col")
  #ordering by operation (custom order)
  result_set$operation <- ordered(result_set$operation, c('CREATE', 'SUSPEND', 'RESUME', 'STOP', 'SHELVE') )
  ordered_set <- result_set[with(result_set, order(operation, cinder, glance, heat, keystone, MISC, neutron, nova, swift, Total)),]
  return(ordered_set)
}
