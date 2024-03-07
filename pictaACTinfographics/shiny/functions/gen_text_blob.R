gen_asthma_date_blob <- function(date, language) {
  if (language == "spanish") {
    translated_mo <- translate_eng_mo_to_sp(strftime(date, "%B"))
    format_eng <- strftime(date,
                           glue::glue("Fecha:  %d de {translated_mo}, %Y"))
  } else {
    return(strftime(date, "Date:  %B %d, %Y"))
  }
}

gen_asthma_interpretive_statement_blob <- function(today_act_score, language) {
  print(glue::glue("generating interpretive statement: {today_act_score}, {language}"))
  if (today_act_score %in% 5:15) {
    if (language == "spanish") {
      return("Su asma est\u00e1 muy mal controlada")
    } else {
      return("Your asthma is very poorly controlled")
    }
  } else if (today_act_score %in% 16:19) {
    if (language == "spanish") {
      return("Su asma est\u00e1 mal controlada")
    } else {
      return("Your asthma is not well controlled")
    }
  } else if (today_act_score %in% 20:25) {
    if (language == "spanish") {
      txt <- "Su asma est\u00e1 bien controlada"
      print(txt)
      return(txt)
    } else {
      return("Your asthma is well controlled")
    }
  } else {
    stop(sprintf("Invalid act score given. Expected 5-25, got %s", today_act_score))
  }
}

gen_asthma_progress_statment <- function(today_act_score, previous_bp_score, language) {
  if (previous_bp_score - today_act_score >= 3) {
    if (language == "spanish") {
      return("Ha empeorado desde su \u00faltima visita.")
    } else {
      return("It has gotten worse since your last visit.")
    }
  } else if (today_act_score <= 19 && abs(previous_bp_score - today_act_score) <= 2) {
    if (language == "spanish") {
      return("Sigue igual que desde su \u00faltima visita.")
    } else {
      return("It is about the same as at your last visit.")
    }
  } else if (today_act_score >= 20 && previous_bp_score <= 19) {
    if (language == "spanish") {
      return("\u00a1Muy bien!")
    } else {
      return("Great job!")
    }
  } else if (today_act_score >= 20 &&
             (today_act_score - previous_bp_score >= 0 || previous_bp_score - today_act_score >= 2)) {
    return("")
  } else if (today_act_score <= 19 && today_act_score - previous_bp_score >= 3) {
    if (language == "spanish") {
      return("\u00a1Ha habido mejora!")
    } else {
      return("You made good progress!")
    }
  } else {
    stop("Unknown progress statement condition")
  }
}
