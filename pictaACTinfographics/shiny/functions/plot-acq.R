gen_x_coords_acq <- function(language) {
  # arrow positions are the same for both languages
  arrow_x_all <- (4.70 + 0:60 * 1.4775)
  return(arrow_x_all)
}

gen_exterior_png_pth_acq <- function(language) {
  eng_or_spa <- stringr::str_to_upper(ifelse(language == "english", "eng", "spa"))
  return(glue::glue(
    "./www/graphical_elements_act/PICTA new ACT exterior {eng_or_spa}.png"
  ))
}

geom_score_arrows_acq <- function(
    base_g,
    today_acq,
    previous_acq,
    previous_date,
    language,
    x_breaks,
    today_arrow_ystart = acq_arrow_values$score_arrow_y1,
    today_arrow_yend = acq_arrow_values$score_arrow_y2,
    today_value_y = acq_arrow_values$score_today_numb_label_y,
    today_today_y = acq_arrow_values$score_today_text_label_y,
    previous_arrow_ystart = acq_arrow_values$previous_score_arrow_y1,
    previous_arrow_yend = acq_arrow_values$previous_score_arrow_y2,
    previous_arrow_yend_same = acq_arrow_values$previous_score_arrow_y2_same,
    previous_value_y = acq_arrow_values$previous_score_today_numb_label_y,
    diff_arrow_spacing_x = acq_arrow_values$diff_arrow_buffer_x,
    diff_arrow_spacing_y = acq_arrow_values$diff_arrow_buffer_y) {
  

  today_acq <- as.numeric(today_acq)
  previous_acq <- as.numeric(previous_acq)
  today_acq_string <- sprintf(today_acq, fmt = '%#.1f')
  print(today_acq)
  x_breaks_i <- floor((round(today_acq, 1) * 10) + 1)
  print(x_breaks_i)
  print(x_breaks[x_breaks_i])

  today_arrow <- base_g +

    geom_today_score_arrow(aes(x = x_breaks[x_breaks_i],
                               y = today_arrow_ystart,
                               xend = x_breaks[x_breaks_i],
                               yend = today_arrow_yend)) +
    
    geom_today_score_value(x = x_breaks[x_breaks_i],
                           y = today_value_y,
                           label = glue::glue("{today_acq_string}")) +

    geom_today_score_today(x = x_breaks[x_breaks_i], y = today_today_y, language = language)

    # adding previous value, if applicable
    if (!is.na(previous_acq)) {
      previous_acq_i <- floor((round(previous_acq, 1) * 10) + 1)
      previous_acq_string <- sprintf(previous_acq, fmt = '%#.1f')
      # values can be same as previous or different

      # save values require overlapping arrows
      # if the values are the same truncate the previous arrow
      if (previous_acq == today_acq) {
      previous_today_arrow <- today_arrow +
        geom_previous_score_arrow(aes(x = x_breaks[previous_acq_i],
                                      y = previous_arrow_ystart,
                                      xend = x_breaks[previous_acq_i],
                                      yend = previous_arrow_yend_same),
                                  arrow_head = FALSE) +
        geom_previous_score_value(x = x_breaks[previous_acq_i],
                                  y = previous_value_y,
                                  # the act text here is correct, it's legacy param name
                                  previous_bp_score = previous_acq_string) +
        geom_previous_score_date(x = x_breaks[previous_acq_i],
                                 y = acq_arrow_values$previous_score_today_text_label_y,
                                 language = language,
                                 text = ifelse(previous_acq == today_acq, FALSE, TRUE),
                                 previous_date = previous_date)
      return(previous_today_arrow)
      } else {
        # the the previous value is not the same as the current value
        previous_today_arrow <- today_arrow +
        geom_previous_score_arrow(aes(x = x_breaks[previous_acq_i],
                                      y = previous_arrow_ystart,
                                      xend = x_breaks[previous_acq_i],
                                      yend = previous_arrow_yend)) +
        geom_previous_score_value(x = x_breaks[previous_acq_i],
                                  y = previous_value_y,
                                  previous_bp_score = previous_acq) +
        geom_previous_score_date(x = x_breaks[previous_acq_i],
                                 y = acq_arrow_values$previous_score_today_text_label_y,
                                 language = language,
                                 text = ifelse(previous_acq == today_acq, FALSE, TRUE),
                                 previous_date = previous_date)
        
        # draw the connecting arrow from previous to current
        if (today_acq - previous_acq > 0) {
          #print("adding right arrow")
          # previous greater than now, arrow point right
          previous_today_arrow <- previous_today_arrow +
            geom_diff_arrow_pos_right(
              aes(x = x_breaks[previous_acq_i] + diff_arrow_spacing_x,
                  y = previous_arrow_yend - diff_arrow_spacing_y,
                  xend = x_breaks[x_breaks_i] - diff_arrow_spacing_x,
                  yend = previous_arrow_yend - diff_arrow_spacing_y
                )
              )
          
        } else if (today_acq - previous_acq < 0) {
          #print("adding left arrow")
          # previous less than now, arrow point left
          previous_today_arrow <- previous_today_arrow +
            geom_diff_arrow_neg_left(
              aes(x = x_breaks[previous_acq_i] - diff_arrow_spacing_x,
                  y = previous_arrow_yend - diff_arrow_spacing_y,
                  xend = x_breaks[x_breaks_i] + diff_arrow_buffer_x,
                  yend = acq_arrow_values$previous_score_arrow_y2 - diff_arrow_buffer_y
              )
            )
        } else {
          stop("Something wrong. I should be returning the overlapping arrow")
        }
        return(previous_today_arrow)
      }

    }

    return(today_arrow)

}
