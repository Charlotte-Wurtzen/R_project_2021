# Define project functions ------------------------------------------------
longer <- function(data){
  data_long <- data %>% 
    pivot_longer(cols = -c(type, value),
                 names_to = "gene", 
                 values_to = "expr_level") %>% 
    mutate(norm_expr_level = (expr_level - mean(expr_level))/sd(expr_level)) %>% 
    select(-c(value, expr_level))
  return(data_long)
}

groupnest <- function(data, col_name){
  col_name <- enquo(col_name)
  data %>% 
    group_by(!!col_name) %>% 
    nest() %>% 
    ungroup() 
}
