gen_exterior_png_pth <- function(language) {
  eng_or_spa <- stringr::str_to_upper(ifelse(language == "english", "eng", "spa"))
  return(glue::glue(
    "./www/graphical_elements_act/PICTA new ACT exterior {eng_or_spa}.png"
  ))
}

geom_base_image <- function(base_image_grob, data = dummy, mapping = aes(x = x, y = y)) {
  ggplot(data = data, mapping = mapping) +
    geom_point(alpha = 1/1000000000) +
    annotation_custom(base_image_grob, xmin = -Inf, xmax = Inf, ymin = 20, ymax = Inf) +
    theme_nothing_text() +
    scale_x_continuous(breaks = seq(1, 100, by = 5), 1) +
    theme(legend.position = "none") +
    theme(axis.title = element_blank(), axis.text = element_blank()) +
    theme(plot.title = element_text(size = 30, colour = "black")) +
    theme(text = element_text(family = "sans"))
}

theme_nothing_text <- function(base_size = 12, base_family = "Arial") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      rect              = element_blank(),
      line              = element_blank(),
      axis.ticks        = element_blank()
    )
}

geom_today_score_value <- function(x, y, label, size = 10, fontface = "bold") {
  layer(
    geom = 'text',
    stat = "identity",
    position = "identity",
    params = list(
      x = x,
      y = y,
      label = label,
      size = size,
      fontface = fontface
    )
  )
}

geom_today_score_today <- function(x, y, language, size = 4.5, fontface = "bold") {
  layer(
    geom = 'text',
    stat = "identity",
    position = "identity",
    params = list(
      x = x,
      y = y,
      label = ifelse(language == "spanish", "Hoy", "Today"),
      size = size,
      fontface = fontface
    )
  )
}

geom_previous_score_value <- function(x, y, previous_bp_score, color = "#939598", size = 10, fontface = "bold") {
  layer(
    geom = 'text',
    stat = "identity",
    position = "identity",
    params = list(
      x = x,
      y = y,
      label = previous_bp_score,
      color = color,
      size = size,
      fontface = fontface
    )
  )
}

geom_previous_score_date <- function(x, y, language, previous_date, text, size = 4.5, color = "#939598") {
  if (text) {
    display_text <- glue::glue("{ifelse(language == 'spanish', '\u00daltima Visita','Last visit')}\n{previous_date}")
  } else {
    display_text <- glue::glue("{previous_date}")
  }
  print(glue::glue("Y from geom_previous_score_date: {y}"))
  layer(
    geom = 'text',
    stat = "identity",
    position = "identity",
    params = list(
      x = x,
      y = y,
      label = display_text,
      color = color,
      size = size
    )
  )
}

geom_today_score_arrow <- function(mapping = NULL, arrow_length = score_arrow_length_unit, arrow_size = score_arrow_size) {
  layer(data = NULL,
        mapping = mapping,
        stat = "identity",
        geom = GeomSegment, 
        position = "identity",
        params = list(arrow = arrow(length = unit(arrow_length, "cm")),
                      size = arrow_size,
                      arrow.fill = NULL, 
                      lineend = "butt",
                      linejoin = "round"))
}

geom_previous_score_arrow <- function(mapping = NULL, arrow_length = score_arrow_length_unit, arrow_size = previous_score_arrow_size, arrow_color = "#939598", arrow_head = TRUE) {
  if (!arrow_head) {
    # when previous score and today score are the same, there is no arrow head
    layer(data = NULL,
          mapping = mapping,
          stat = "identity",
          geom = GeomSegment, 
          position = "identity",
          params = list(size = arrow_size,
                        color = arrow_color,
                        arrow.fill = NULL, 
                        lineend = "butt",
                        linejoin = "round"))
  } else {
    layer(data = NULL,
          mapping = mapping,
          stat = "identity",
          geom = GeomSegment, 
          position = "identity",
          params = list(arrow = arrow(length = unit(arrow_length, "cm")),
                        size = arrow_size,
                        color = arrow_color,
                        arrow.fill = NULL, 
                        lineend = "butt",
                        linejoin = "round"))
  }
}

geom_diff_arrow_pos_right <- function(mapping = NULL, arrow_length = score_arrow_length_unit, arrow_size = previous_score_arrow_size, arrow_color = "#939598") {
  layer(data = NULL,
        mapping = mapping,
        stat = "identity",
        geom = GeomSegment, 
        position = "identity",
        params = list(arrow = arrow(length = unit(arrow_length, "cm")),
                      size = arrow_size,
                      color = arrow_color,
                      arrow.fill = NULL, 
                      lineend = "butt",
                      linejoin = "round"))
}

geom_diff_arrow_neg_left <- function(mapping = NULL, arrow_length = score_arrow_length_unit, arrow_size = previous_score_arrow_size, arrow_color = "#939598") {
  layer(data = NULL,
        mapping = mapping,
        stat = "identity",
        geom = GeomSegment, 
        position = "identity",
        params = list(arrow = arrow(length = unit(arrow_length, "cm")),
                      size = arrow_size,
                      color = arrow_color,
                      arrow.fill = NULL, 
                      lineend = "butt",
                      linejoin = "round"))
}

geom_score_arrows <- function(base_g,
                              today_act,
                              previous_bp,
                              previous_date,
                              language,
                              x_breaks,
                              today_arrow_ystart = score_arrow_y1,
                              today_arrow_yend = score_arrow_y2,
                              today_value_y = score_today_numb_label_y,
                              today_today_y = score_today_text_label_y,
                              previous_arrow_ystart = previous_score_arrow_y1,
                              previous_arrow_yend = previous_score_arrow_y2,
                              previous_arrow_yend_same = previous_score_arrow_y2_same,
                              previous_value_y = previous_score_today_numb_label_y,
                              diff_arrow_spacing_x = diff_arrow_buffer_x,
                              diff_arrow_spacing_y = diff_arrow_buffer_y) {
  
  today_arrow <- base_g +
    
    geom_today_score_arrow(aes(x = x_breaks[today_act],
                               y = today_arrow_ystart,
                               xend = x_breaks[today_act],
                               yend = today_arrow_yend)) +
    
    geom_today_score_value(x = x_breaks[today_act],
                           y = today_value_y,
                           label = glue::glue("{today_act}")) +
    
    geom_today_score_today(x = x_breaks[today_act], y = today_today_y, language = language)
  
  if (!is.na(previous_bp)) {
    # if there is a previous act value
    #print("adding previous act marker")
    
    
    if (previous_bp == today_act) {
      # if the values are the same truncate the previous arrow
      previous_today_arrow <- today_arrow +
        geom_previous_score_arrow(aes(x = x_breaks[previous_bp],
                                      y = previous_arrow_ystart,
                                      xend = x_breaks[previous_bp],
                                      yend = previous_arrow_yend_same),
                                  arrow_head = FALSE) +
        geom_previous_score_value(x = x_breaks[previous_bp],
                                  y = previous_value_y,
                                  previous_bp_score = previous_bp) +
        geom_previous_score_date(x = x_breaks[previous_bp],
                                 y = previous_score_today_text_label_y,
                                 language = language,
                                 text = ifelse(previous_bp == today_act, FALSE, TRUE),
                                 previous_date = previous_date)
      return(previous_today_arrow)
      
    } else {
      # if values are not the same previous goes to the number line
      previous_today_arrow <- today_arrow +
        geom_previous_score_arrow(aes(x = x_breaks[previous_bp],
                                      y = previous_arrow_ystart,
                                      xend = x_breaks[previous_bp],
                                      yend = previous_arrow_yend)) +
        geom_previous_score_value(x = x_breaks[previous_bp],
                                  y = previous_value_y,
                                  previous_bp_score = previous_bp) +
        geom_previous_score_date(x = x_breaks[previous_bp],
                                 y = previous_score_today_text_label_y,
                                 language = language,
                                 text = ifelse(previous_bp == today_act, FALSE, TRUE),
                                 previous_date = previous_date)
    }

    if (today_act - previous_bp > 0) {
      #print("adding right arrow")
      # previous greater than now, arrow point right
      previous_today_arrow <- previous_today_arrow + geom_diff_arrow_pos_right(
        aes(x = x_breaks[previous_bp] + diff_arrow_spacing_x,
            y = previous_arrow_yend - diff_arrow_spacing_y,
            xend = x_breaks[today_act] - diff_arrow_spacing_x,
            yend = previous_arrow_yend - diff_arrow_spacing_y))
      
    } else if (today_act - previous_bp < 0) {
      #print("adding left arrow")
      # previous less than now, arrow point left
      previous_today_arrow <- previous_today_arrow + geom_diff_arrow_neg_left(
        aes(x = x_breaks[previous_bp] - diff_arrow_spacing_x,
            y = previous_arrow_yend - diff_arrow_spacing_y,
            xend = x_breaks[today_act] + diff_arrow_buffer_x,
            yend = previous_score_arrow_y2 - diff_arrow_buffer_y))
    } else {
      print("no arrow")
      # when previous act is the same as today's act
      previous_today_arrow <- today_arrow
    }
    return(previous_today_arrow)
    
  } else {
    # no previous value provided (NA)
    return(today_arrow)
  }
}

gen_x_coords <- function(language) {
  if (language == "spanish") {
    arrow_x_poor <- c( 8.7, 11.8, 14.9, 18.0, 20.7, # 5
                       23.8, 26.9, 30.0, 33.1, 36.5, # 10
                       40.4, 44.1, 47.8, 51.5, 55.2  # 15
    )
    
    arrow_x_16 <- 59.5
    arrow_x_notwell <- c(63.6, 67.4, 71.1)
    arrow_x_20 <- 75
    arrow_x_well <- c(79.4, 83.2, 86.9, 90.6)
    arrow_x_25 <- 95.0
  } else {
    arrow_x_poor <- c( 8.7, 11.8, 14.9, 18.0, 21.1, # 5
                       24.2, 27.3, 30.4, 33.5, 36.8, # 10
                       40.5, 44.2, 47.9, 51.6, 55.3  # 15
    )
    
    arrow_x_16 <- 59.5
    arrow_x_notwell <- c(63.5, 67.2, 70.9)
    arrow_x_20 <- 75
    arrow_x_well <- c(79, 82.7, 86.4, 90.1)
    arrow_x_25 <- 94.2
  }
  arrow_x_all <- c(arrow_x_poor, arrow_x_16, arrow_x_notwell, arrow_x_20, arrow_x_well, arrow_x_25)
  return(arrow_x_all)
}