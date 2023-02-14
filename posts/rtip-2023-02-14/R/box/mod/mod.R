# Function for modifying data

#' @export
modify_data <- function(df_list) {
  
  box::use(dplyr)
  box::use(purrr)
  
  map_fun <- function(df) {
    
    df |> 
      dplyr$select(name:mass) |> 
      dplyr$mutate(lol = height * mass) |> 
      dplyr$filter(lol > 1500)
  }
  
  # Apply mapping function to list
  purrr$map(df_list, map_fun)
  
}