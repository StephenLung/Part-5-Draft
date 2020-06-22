read_user_base <-
function(){
  # READ - Function saves the tibble into the global environment as opposed to within the function
  user_base_tbl <<- read_rds(path = "00_data_local/port_user_base_tbl.rds")
}
update_and_write_user_base <-
function(user_name, column_name, assign_input){
  # Combining the UPDATE and WRITE together in a function
  # UPDATE - Assigning new entries into the global environment based on user, col_name and input
  # WRITE - Save the updated tibble into an RDS file
  user_base_tbl[user_base_tbl$user == user_name,][[column_name]] <<-assign_input
  write_rds(user_base_tbl, path = "00_data_local/port_user_base_tbl.rds")
}
