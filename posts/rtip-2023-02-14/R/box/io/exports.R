# Function for exporting data

#' @export
export_data <- function(df_list) {
  
  box::use(vroom)
  box::use(purrr)
  
  # Export data
  purrr$map2(.x = df_list,
             .y = names(df_list),
             ~vroom$vroom_write(x = .x,
                               file = paste0('data/output/', 
                                             .y),
                               delim = ','))
  
}