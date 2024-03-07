# example CURL call:
# curl -X POST "http://127.0.0.1:8000/gen_image?language=english&today_act=25&today_date=2022-08-21&previous_bp=5&previous_date=2021-07-14" -H "accept: image/png" -H "Content-Type: application/json" -d "{\"dummy_data\":[{\"x\":0,\"y\":0},{\"x\":100,\"y\":100}]}"
#
# example Request URL:
# http://127.0.0.1:8000/gen_image?language=english&today_act=25&today_date=2022-08-21&previous_bp=5&previous_date=2021-07-14

source("global.R")

#* Asthma Control Questionnaire Image

#* Return the Asthma Control Questionnaire (ACQ) image
#* @param language
#* @param display_name
#* @param today_acq
#* @param previous_acq
#* @param previous_date
#* 
#* @serializer contentType list(type='image/png')
#* @post /gen_image_acq
gen_image_acq <- function(
  language = "english",
  today_acq = 1.5,
  previous_acq = 0.3,
  previous_date = "2022-08-15"
) {
  
  pt_info_acq <- gen_pt_info_acq(
    display_name = "",
    language = language,
    today_date = "",
    today_acq_score = today_acq,
    previous_acq = previous_acq,
    previous_date = previous_date
  )
  
  plot_info <- list(
    arrow_x_all = gen_x_coords_acq(pt_info_acq$language),
    image = png::readPNG(pt_info_acq$png_url)
  )
  plot_info$base_image_g <- grid::rasterGrob(
    plot_info$image, interpolate = TRUE
  )
  plot_info$plot_pth_norm <- fs::path_norm(tempfile(fileext = '.png'))
  plot_info$plot_pth_unix <- gsub("\\\\", "/", plot_info$plot_pth_norm)

  print(glue::glue(
    "Generating base plot."
  ))
  base_g <- geom_base_image(plot_info$base_image_g)
  
  arrow_g <- geom_score_arrows_acq(
    base_g = base_g,
    today_acq = pt_info_acq$today_acq,
    previous_acq = pt_info_acq$previous_acq,
    previous_date = pt_info_acq$previous_date_text,
    language = pt_info_acq$language,
    x_breaks = plot_info$arrow_x_all
  )
  print(glue::glue(
    "Plumber: Returning plot."
  ))
  # https://www.r-bloggers.com/2021/01/how-to-make-rest-apis-with-r-a-beginners-guide-to-plumber/
  file <- "plot.png"
  ggsave(
    filename = file,
    arrow_g,
    scale = 1,
    width = 11,
    height = 8.5,
    units = "in",
    dpi = 300)
  readBin(file, "raw", n = file.info(file)$size)
}

#* Asthma Control Test Image
#*
#* Returns the Asthma Control Test (ACT) image
#*
#* This was the first image originally created for PICTA
#* 
#* @param language Language, one of: "english", or "spanish"
#* @param display_name Name of patient, defaults to ""
#* @param today_act Current ACT score
#* @param today_date Current date for ACT score, YYYY-MM-DD
#* @param previous_bp Previous ACT score
#* @param previous_date Previous date for ACT score, YYYY-MM-DD
#* @param dummy_data Empty R dataframe to draw the ggplot2 canvas
#* 
#* @serializer contentType list(type='image/png')
#* @post /gen_image_act
gen_image_act <- function(
  language = "english",
  display_name = "",
  today_act = 25,
  today_date = "2022-08-21",
  previous_bp = 5,
  previous_date = "2021-07-14",
  dummy_data = dummy
) {

  today_act <- as.integer(today_act)
  previous_bp <- as.integer(previous_bp)

  # patient info -----
  print(glue::glue(
    "Plumber: Generating patient info."
  ))
  pt_info <- gen_pt_info(
    display_name = display_name,
    language = language,
    today_date = today_date,
    today_act_score = today_act,
    previous_date = previous_date,
    previous_bp_score = previous_bp
  )

  # plot info -----
  print(glue::glue(
    "Plumber: Generating plot info."
  ))
  plot_info <- list(
    arrow_x_all = gen_x_coords(pt_info$language),
    image = png::readPNG(pt_info$png_url)
  )
  plot_info$base_image_g <- grid::rasterGrob(
    plot_info$image, interpolate = TRUE
  )

  plot_info$plot_pth_norm <- fs::path_norm(tempfile(fileext = '.png'))
  plot_info$plot_pth_unix <- gsub("\\\\", "/", plot_info$plot_pth_norm)
  plot_info$fig_pth_act_exterior <- gen_exterior_png_pth(
    pt_info$language
  )

  print(glue::glue(
    "Plumber: Generating base plot."
  ))
  base_g <- geom_base_image(plot_info$base_image_g)

  print(glue::glue(
    "Plumber: Generating plot from base."
  ))
  arrow_g <- geom_score_arrows(
    base_g = base_g,
    today_act = pt_info$today_act,
    previous_bp = pt_info$previous_bp,
    previous_date = pt_info$previous_date_text,
    language = pt_info$language,
    x_breaks = plot_info$arrow_x_all
  )

  print(glue::glue(
    "Plumber: Returning plot."
  ))
  # https://www.r-bloggers.com/2021/01/how-to-make-rest-apis-with-r-a-beginners-guide-to-plumber/
  file <- "plot.png"
  ggsave(
    filename = file,
    arrow_g,
    scale = 1,
    width = 11,
    height = 8.5,
    units = "in",
    dpi = 300)
  readBin(file, "raw", n = file.info(file)$size)
}
