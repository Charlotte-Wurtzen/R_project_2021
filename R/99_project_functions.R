# Define project functions ------------------------------------------------
groupnest <- function(data, col_name){
  col_name <- enquo(col_name)
  data %>% 
    group_by(!!col_name) %>% 
    nest() %>% 
    ungroup() 
}
