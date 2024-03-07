library(here)
library(RCurl)
library(ggplot2)
library(shiny)
library(readr)
library(DT)
library(zip)
library(stringr)
library(purrr)
library(knitr)
library(glue)
library(tinytex)
library(png)

purrr::walk(fs::dir_ls('functions'), function(x){print(x); source(x)})

dummy <- data.frame(x = c(0,100), y = c(0, 100)) # need this to actually show plot

score_arrow_y1 = 38
score_arrow_y2 = 41.5
score_arrow_size = 1.0
score_arrow_length_unit = 0.25
score_today_numb_label_y = 35
score_today_text_label_y = 31.5

previous_score_arrow_y1 = 25
previous_score_arrow_y2 = 41.5
previous_score_arrow_y2_same = 29
previous_score_arrow_size = 1.0
previous_score_arrow_length_unit = 0.25
previous_score_today_numb_label_y = 22
previous_score_today_text_label_y = 16.5

diff_arrow_buffer_x = 1
diff_arrow_buffer_y = .5

acq_arrow_values <- list(
  score_arrow_y1 = 35.25,
  score_arrow_y2 = 38.75,
  score_arrow_size = 1.0,
  score_arrow_length_unit = 0.25,
  score_today_numb_label_y = 32.25,
  score_today_text_label_y = 28.75,

  previous_score_arrow_y1 = 22.25,
  previous_score_arrow_y2 = 38.75,
  previous_score_arrow_y2_same = 26.25,
  previous_score_arrow_size = 1.0,
  previous_score_arrow_length_unit = 0.25,
  previous_score_today_numb_label_y = 19.25,
  previous_score_today_text_label_y = 13.75,

  diff_arrow_buffer_x = 1,
  diff_arrow_buffer_y = .5
)

png_url_english <- "./www/graphical_elements_act/ACT number line ENG - cropped.png"
png_url_spanish <- "./www/graphical_elements_act/ACT number line SPA - cropped.png"

png_url_english_acq <- "./www/graphical_elements_acq/ACQ for BITSI_ACQ image only ENG - cropped.png"
png_url_spanish_acq <- "./www/graphical_elements_acq/ACQ for BITSI_ACQ image only SPA - cropped.png"

empty_batch_df <- data.frame(id_file = NA,
                             display_name = NA,
                             language = NA,
                             today_date = NA,
                             today_act_score = NA,
                             previous_date = NA,
                             previous_bp_score = NA)
