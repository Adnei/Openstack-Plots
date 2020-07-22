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
    focal_ubuntu={return(508)},
    bionic_ubuntu={return(329)},
    centos7_light={return(898)},
    centos7={return(1300)},
    cirros={return(15)},
    debian10raw={return(2000)},
    debian10qcow2={return(550)},
    windows_server={return(6600)},
    {return(NA)}) #DEFAULT
}

filter_images_by_db <- function(db_list){
  all_images <- c()
  exec_arr <- c()
  db_arr <- c()
  for(db in db_list){
    db_path <- paste0(COMMON_PATH, db)
    db_images <- db_interact.get_images(database=db_path)
    n_exec <- length(db_interact.get_exec_list_by_image(db_images[1], database=db_path))
    new_images <- db_images
    conflict_idx <- match(db_images, all_images)
    conflict_idx <- conflict_idx[!is.na(conflict_idx)]

    if(length(conflict_idx) > 0){
      old_n_exec <- exec_arr[c(conflict_idx)][1]
      if(n_exec > old_n_exec){
        db_arr[c(conflict_idx)] = db_path
        exec_arr[c(conflict_idx)] = n_exec
      }
      conflict_images <- all_images[c(conflict_idx)]
      remove_idx <- match(conflict_images, new_images)
      remove_idx <- remove_idx[!is.na(remove_idx)]
      new_images <- new_images[-remove_idx]
    }

    all_images <- c(all_images, new_images)
    exec_arr <- c(exec_arr, rep(n_exec, length(new_images)))
    db_arr <- c(db_arr, rep(db_path, length(new_images)))
  }

  if(length(db_arr) != length(exec_arr) || length(db_arr) != length(all_images)){
    print('ERROR!')
    print('Something went wrong during the database.dataframe (db.df) creation.')
    return(data.frame())
  }

  db.df <- data.frame(database = db_arr, n_exec = exec_arr, image = all_images, stringsAsFactors = FALSE)

  return(db.df)
}

db.df <- filter_images_by_db(objects.db_list)
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
