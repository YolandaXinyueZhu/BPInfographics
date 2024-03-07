library(stringr)

is_row_valid <- function(data_row) {
  # re-cast as numeric since single digits seems to have a leading space in them
  data_row[['today_act_score']] <- as.numeric(data_row[['today_act_score']])
  data_row[['previous_bp_score']] <- as.numeric(data_row[['previous_bp_score']])
  
  if (is.na(data_row[['display_name']])) {
    return(list(bool = FALSE,
                reason = "no display name")
           )
  }
  
  if (!stringr::str_to_lower(data_row[['language']]) %in% c("english", "spanish")) {
    return(list(bool = FALSE,
                reason = "invalid language")
           )
  }
  
  if (
    (!is.na(data_row[['today_year']]) |
       !is.na(data_row[['today_month']]) |
       !is.na(data_row[['today_day']])
     ) &
      (is.na(data_row[['today_date']]))
    ) {
    return(list(bool = FALSE,
                reason = "Invalid today date"))
  }

  if (
    (!is.na(data_row[['previous_year']]) |
     !is.na(data_row[['previous_month']]) |
     !is.na(data_row[['previous_day']])
    ) &
    (is.na(data_row[['previous_date']]))
  ) {
    return(list(bool = FALSE,
                reason = "Invalid previous date"))
  }
  
  if (is.na(data_row[['today_date']])) {
    return(list(bool = FALSE,
                reason = "missing today's date")
           )
  }
  
  if (is.na(data_row[['today_act_score']])) {
    return(list(bool = FALSE,
                reason = "missing today's score")
    )
  }
  
  if (!is.na(data_row[['previous_date']]) &&
      data_row[['previous_date']] > data_row[['today_date']]) {
    return(list(bool = FALSE,
                reason = "previous date after today's date")
           )
  }
  
  if (!data_row[['today_act_score']] %in% 5:25) {
    #print(data_row)
    #print("today score out of range")
    return(list(bool = FALSE,
                reason = "today's score out of range")
    )
  }
  
  if (!is.na(data_row[['previous_bp_score']]) & !data_row[['previous_bp_score']] %in% 5:25) {
    #print(data_row)
    #print("previous score out of range")
    return(list(bool = FALSE,
                reason = "previous score out of range")
    )
  }
  
  # make sure display name contains letters
  # unicode characters
  # https://stackoverflow.com/questions/2385701/regular-expression-for-first-and-last-name
  if (!stringr::str_detect(data_row[['display_name']], "^[\\p{N}\\p{L}'-]*$")) {
    #print(data_row)
    #print("name unicode error")
    return(list(bool = FALSE,
                reason = "display name letter unicode error")
    )
  }
  
  return(list(
    bool = TRUE,
    reason = ""
  ))
}
