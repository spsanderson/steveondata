# Main script

# Script setup -----------------------------------------------------------------

# Load box modules
box::use(. / box / global_options / global_options)
box::use(. / box / io / imports)
box::use(. / box / io / exports)
box::use(. / box / mod / mod)

# Load global options
global_options$set_global_options() 


# Main script ------------------------------------------------------------------

# Load data, process it, and export results
all_data <- getOption('data_dir') |> 
  
  # Load all data
  imports$load_all() |> 
  
  # Modify dataset
  mod$modify_data() |> 
  
  # Export data
  exports$export_data()



