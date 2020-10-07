library(dplyr)
source('objects.R')
source('db_interact.R')
source('data_handler.R')

cv_fn <- function(mean, sd){
    (sd/mean)*100
}

get_image_size <- function(image){
  switch(image,
    fedora31={return(319)},
    fedora32={return(289)},
    focal_ubuntu={return(519)},
    bionic_ubuntu={return(329)},
    centos7_light={return(898)},
    centos7={return(1300)},
    cirros={return(15)},
    debian10raw={return(2000)},
    debian10qcow2={return(550)},
    windows_server={return(6600)},
    freebsd12={return(454)},
    {return(NA)}) #DEFAULT
}

db.df <- db_interact.filter_images_by_db(params.db_list)
db.df <- db.df[db.df$image != 'debian10raw',]
# lr == 'linear regression
lr.df <- data.frame(
  image = character(length(objects.operations)*length(db.df$image)),
  image_size = numeric(length(objects.operations)*length(db.df$image)),
  mean_total_traffic = numeric(length(objects.operations)*length(db.df$image)),
  median_total_traffic = numeric(length(objects.operations)*length(db.df$image)),
  sd_total_traffic = numeric(length(objects.operations)*length(db.df$image)),
  cv_total_traffic = numeric(length(objects.operations)*length(db.df$image)),
  operation = character(length(objects.operations)*length(db.df$image)),
  stringsAsFactors = FALSE
)

# op_traffic.df <- data.frame()

idx_counter <- 0
for(operation in objects.operations){
  for(image in db.df$image){
    idx_counter <- idx_counter + 1
    #filter_images_by_db function has to ensure that there's only one database per image.
    database <- db.df[db.df$image == image, ]$database
    exec_list <- db_interact.get_exec_list_by_image(image, database = database)
    traffic_arr <- c()
    exec_counter <- 1
    for(exec_id in exec_list){
      traffic_arr[exec_counter] = db_interact.get_total_traffic(image, operation, exec_id, service='TOTAL', database = database)
      exec_counter <- exec_counter + 1
    }
    mean_total_traffic <- mean(traffic_arr)
    median_total_traffic <- median(traffic_arr)
    sd_total_traffic <- sd(traffic_arr)
    cv_total_traffic <- cv_fn(mean_total_traffic, sd_total_traffic)
    image_size <- get_image_size(image)
    lr.df[idx_counter,] = c(image,
                            as.numeric(image_size),
                            as.numeric(mean_total_traffic),
                            as.numeric(median_total_traffic),
                            as.numeric(sd_total_traffic),
                            as.numeric(cv_total_traffic),
                            operation)
  }
}
