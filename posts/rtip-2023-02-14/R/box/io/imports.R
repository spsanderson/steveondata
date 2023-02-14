# Function for importing data

#' @export
load_all <- function(file_path) {
  
  box::use(purrr)
  box::use(vroom)
  
  file_path |> 
    
    # Get all csv files from folder
    list.files(full.names = TRUE) |> 
    
    # Set list names
    purrr$set_names(\(file) basename(file)) |> 
    
    # Load all csvs into list
    purrr$map(\(file) vroom$vroom(file))

}