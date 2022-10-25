#
# Sourced from: https://github.com/jthomasmock/gtExtras/blob/HEAD/R/gt_index.R
# gt_index is a function in the gtExtras package that is not installed on 
# PFE production version of R. 
#
#
#
# sections <- c("Documents", "Presentations")
# gt_object <- table_data %>% 
#   dplyr::filter(section %in% sections) %>% 
#   gt(groupname_col = "section", rowname_col = "name")
# column <- "Documents"

gt_index <- function(gt_object, column, as_vector = TRUE){
  
  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))
  stopifnot("'as_vector' must be a TRUE or FALSE" = is.logical(as_vector))
  
  if(length(gt_object[["_row_groups"]]) >= 1){
    
    # if the data are grouped you need to identify the group column
    # and arrange by that column. I convert to a factor so that the
    # columns don't default to arrange by other defaults
    #  (ie alphabetical or numerical)
    gt_row_grps <- gt_object[["_row_groups"]]
    
    grp_vec_ord <- gt_object[["_stub_df"]] %>%
      dplyr::mutate(group_id = factor(groupname, levels = gt_row_grps)) %>%
      dplyr::arrange(group_id) %>%
      dplyr::pull(rownum_i)
    
    df_ordered <- gt_object[["_data"]] %>%
      dplyr::slice(grp_vec_ord)
    
  } else {
    # if the data are not grouped, then it will just "work"
    df_ordered <- gt_object[["_data"]]
    
  }
  
  # return as vector or tibble in correct, gt-indexed ordered
  if(isTRUE(as_vector)){
    df_ordered %>%
      dplyr::pull({{column}})
  } else {
    df_ordered
  }
  
}