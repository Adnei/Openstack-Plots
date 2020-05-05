source('db_interact.R')

#STATIC CONST
objects.images <- db_interact.get_images()
objects.services <- db_interact.get_services()
objects.operations <- c('CREATE', 'SUSPEND', 'RESUME', 'STOP', 'SHELVE')
objects.nics <- db_interact.get_nics()

objects.get_plot_infos <- function(image){
  switch(image,
    fedora31={
      return (
        list(
          title = 'Timeline: GNU/Linux Fedora Cloud 31-1.9',
          pdf = 'fedoraTimeLine.pdf'
        )
      )
    },focal_ubuntu={
      return (
        list(
          title = 'Timeline: GNU/Linux Ubuntu Server 20.04 (Focal Fossa)',
          pdf = 'ubuntuFocalTimeLine.pdf'
        )
      )
    },bionic_ubuntu={
      return (
        list(
          title = 'Timeline: GNU/Linux Ubuntu Server 18.04 (Bionic Beaver)',
          pdf = 'ubuntuBionicTimeLine.pdf'
        )
      )
    },centos7_light={
      return (
        list(
          title = 'Timeline: Centos7',
          pdf = 'centosLightTimeLine.pdf'
        )
      )
    },cirros={
      return (
        list(
          title = 'Timeline: CirrOS 0.4.0',
          pdf = 'cirrosTimeLine.pdf'
        )
      )
    },{
      return (
        list(
          title = 'DEFAULT PLOT',
          pdf = 'default_plot.pdf',
        )
      )
    }
  )
}

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
