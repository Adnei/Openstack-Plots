source('plots.R')

label_pos <- c(0,1,2,3,0)
col_arr <- c('#000080', '#663300', '#267326', '#e68a00', '#cc0000')
blank_schema_list <- objects.build_schema()
traffic_by_second_schemas <- vector('list', length(blank_schema_list))
x_max_array <- c()
y_max_array <- c()

schema_counter <- 1
## First fill plots schema with data and then plot
for(schema in blank_schema_list) {
  #blank_schema_list is preserved so it can be used again to create other plots with other kind of data (packets by second, etc)
  traffic_by_second_schemas[[schema_counter]] <- data_handler.fill_final_schema_traffic_by_second(schema)
  x_max_array[schema_counter] <- traffic_by_second_schemas[[schema_counter]]$timeline$x_max
  y_max_array[schema_counter] <- traffic_by_second_schemas[[schema_counter]]$timeline$y_max
  schema_counter <- schema_counter + 1
}

for(schema in traffic_by_second_schemas){
    #Plotting Timeline
    plots.plot_schema(schema, label_pos, col_arr, is_pdf=TRUE )

    #Generating API Calls table
    # api_calls <- objects.build_api_calls_df(schema$image_name,
    #   schema$operations,
    #   objects.services,
    #   schema$sample_exec_id
    # )
    #
    # #Generating Total Traffic per service table
    # total_traffic <- objects.build_total_traffic(schema$image_name,
    #   schema$operations,
    #   objects.services,
    #   schema$sample_exec_id
    # )

}
