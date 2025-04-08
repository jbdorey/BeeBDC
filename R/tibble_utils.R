
# Sorry everyone, I just want to cut down on tibble as a dependency. These are simple enough
# internal functions though.
#### 1.0 column_to_rownames ####
  # replaces tibble's column_to_rownames
column_to_rownames_internal <- function(.data, var = "rowname") {
  stopifnot(is.data.frame(.data))
  
  .data <- as.data.frame(.data)
  rownames(.data) <- .data[[var]]
  .data[[var]] <- NULL
  .data
}



  #### 2.0 rownames to column ####
  # replaces tibble's rownames_to_column and remove_rownames
rownames_to_column_int <- function(.data, var = "rowname") {
  # rename, because .data has special semantics in tidy evaluation
  df <- .data
  
  stopifnot(is.data.frame(df))
  
  new_df <- df %>%
    dplyr::mutate(var_in = rownames(df))
  
  
  names(new_df)[names(new_df) == "var_in"] <- var
    
  remove_rownames_int(new_df)
}

remove_rownames_int <- function(.data) {
  stopifnot(is.data.frame(.data))
  rownames(.data) <- NULL
  .data
}





