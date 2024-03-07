gen_pt_info <- function(display_name, language,
                        today_date, today_act_score,
                        previous_date, previous_bp_score) {
  pt_values_asthma <- list(
    language = language,
    name = display_name,
    display_name = display_name, # name and display name are coming from the same input
    today_act = today_act_score,
    today_date = today_date,
    today_date_text = NA,
    previous_bp = previous_bp_score,
    previous_date = previous_date,
    previous_date_text = NA,
    asthma_interpretive_statement = NA,
    asthma_score_statement = NA,
    asthma_progress_statment = NA
  )
  
  if (is.na(pt_values_asthma$previous_bp)) {
    pt_values_asthma$previous_date_text <- ""
    pt_values_asthma$asthma_progress_statment <- ""
  } else {
    previous_date_date <- as.Date(pt_values_asthma$previous_date)
    pt_values_asthma$previous_date_text <- strftime(previous_date_date, "%m/%d/%y")
    pt_values_asthma$asthma_progress_statment <- gen_asthma_progress_statment(pt_values_asthma$today_act,
                                                                              pt_values_asthma$previous_bp,
                                                                              pt_values_asthma$language)
  }
  
  today_date_date <- as.Date(pt_values_asthma$today_date)
  pt_values_asthma$today_date_text <- gen_asthma_date_blob(today_date_date, pt_values_asthma$language)
  
  pt_values_asthma$asthma_interpretive_statement <- gen_asthma_interpretive_statement_blob(pt_values_asthma$today_act,
                                                                                           pt_values_asthma$language)
  
  if (pt_values_asthma$language == "spanish") {
    pt_values_asthma$asthma_score_statement <- sprintf("Su puntaje es %s", pt_values_asthma$today_act)
    pt_values_asthma$png_url <- png_url_spanish
    pt_values_asthma$act_rnw_f <- "act-pamphlet_interrior-spanish.Rnw"
  } else {
    pt_values_asthma$asthma_score_statement <- sprintf("Your score is %s", pt_values_asthma$today_act)
    pt_values_asthma$png_url <- png_url_english
    pt_values_asthma$act_rnw_f <- "act-pamphlet_interrior-english.Rnw"
  }
  return(pt_values_asthma)
}
