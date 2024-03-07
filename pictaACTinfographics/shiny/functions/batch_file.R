library(dplyr)
library(lubridate)
library(glue)

read_batch_file_excel <- function(pth) {
  batch_df <- readxl::read_excel(pth,
                                 col_types = c(
                                   "text"     # id_file
                                   ,"text"    # display_name
                                   ,"text"    # language
                                   ,"numeric" # today_year
                                   ,"numeric" # today_month
                                   ,"numeric" # today_day
                                   ,"numeric" # today_act_score
                                   ,"numeric" # previous_year
                                   ,"numeric" # previous_month
                                   ,"numeric" # previous_day
                                   ,"numeric" # previous_bp_score
                                 ))
  batch_df <- batch_df %>%
    dplyr::mutate(today_date = lubridate::make_date(today_year, today_month, today_day),
                  previous_date = lubridate::make_date(previous_year, previous_month, previous_day)
    )
  return(batch_df)
}


validate_batch_file <- function(dat) {
  valid_reason <- apply(dat, MARGIN = 1, FUN = is_row_valid)
  valid_reason_t <- purrr::transpose(valid_reason)
  
  dat$is_valid <- valid_reason_t$bool
  dat$reason <- valid_reason_t$reason
  
  #dat$is_valid[problem_rows] <- FALSE
  #dat$reason[problem_rows] <- "readr error"
  
  dat <- dat %>%
    dplyr::select(reason, display_name, language, today_date, today_act_score, previous_date, previous_bp_score, everything())
  return(dat)
}


# source("./shiny/functions/data_validator.R")
# xl_df <- read_batch_file_excel("~/../Desktop/picta_act_infographics/example_batch_file_separate_dates.xlsx")
# batch_df <- validate_batch_file(xl_df)
# batch_df
