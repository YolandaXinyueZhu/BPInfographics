gen_single_params <- function(row_dat) {
  print(row_dat)
  print(row_dat$is_valid)
  current_pt_info <- list(
    language = row_dat$language,
    display_name = row_dat$display_name,
    today_act = row_dat$today_act_score,
    today_date = row_dat$today_date,
    previous_bp = row_dat$previous_bp_score,
    previous_date = row_dat$previous_date
  )
  #print(current_pt_info)
  
  pt_info <- gen_pt_info(
    display_name = current_pt_info$display_name,
    language = current_pt_info$language,
    today_date = current_pt_info$today_date,
    today_act_score = current_pt_info$today_act,
    previous_date = current_pt_info$previous_date,
    previous_bp_score = current_pt_info$previous_bp
  )
  
  pt_info$pdf_name <- glue::glue("act-{pt_info$language}-{pt_info$display_name}-{row_dat$id_file}-{pt_info$today_date}.pdf")
  pt_info$pdf_name <- stringr::str_replace_all(pt_info$pdf_name, " ", "_")
  
  #print(class(pt_info))
  print(pt_info)
  return(pt_info)
}

gen_single_env <- function(single_params) {
  e <- new.env()
  
  e$PT_INFO <- function(){
    return(
      list(
        display_name = single_params$display_name,
        asthma_interpretive_statement = single_params$asthma_interpretive_statement,
        asthma_score_statement = single_params$asthma_score_statement,
        asthma_progress_statment = single_params$asthma_progress_statment,
        today_date_text = single_params$today_date_text
      )
    )
  }
  
  e$plot_pth_norm_call <- function() {
    plot_pth_norm_batch <- fs::path_norm(tempfile(fileext = '.png'))
    if (fs::file_exists(plot_pth_norm_batch)) {fs::file_delete(plot_pth_norm_batch)}
    return(plot_pth_norm_batch)
  }
  
  e$plot_pth_norm <- e$plot_pth_norm_call()
  
  e$plot_pth_unix <- function() {
    plot_pth_unix_batch <- gsub("\\\\", "/", e$plot_pth_norm)
    return(plot_pth_unix_batch)
  }
  
  e$fig_pth_act_exterior <- function() {
    fig_pth_act_exterior_batch <- gen_exterior_png_pth(single_params$language)
    return(fig_pth_act_exterior_batch)
  }
  
  print("generated environment")
  print(e$PT_INFO())
  print(e$plot_pth_norm)
  print(e$plot_pth_unix())
  print(e$fig_pth_act_exterior())
  
  return(e)
}

gen_figure <- function(language, png_url, today_act, previous_bp, previous_date_text) {
  
  arrow_x_all <- gen_x_coords(language)
  
  image <- png::readPNG(png_url)
  base_image_g <-grid::rasterGrob(image, interpolate = TRUE)
  
  base_g <- geom_base_image(base_image_g)
  
  arrow_g <- geom_score_arrows(base_g = base_g,
                               today_act = today_act,
                               previous_bp = previous_bp,
                               previous_date = previous_date_text,
                               language = language,
                               x_breaks = arrow_x_all)
  return(arrow_g)
  
}

# gen_pdf_filename <- function(name, language, today_date) {
#   name_underscore <- gsub(" ", "_", input$name)
#   return(glue::glue("act-{language}-{name_underscore}-{today_date}.pdf"))
# }

gen_pdf_from_single_row <- function(row_dat, pdf_dir) {
  row_pt_info <- gen_single_params(row_dat)
  knit_env <- gen_single_env(row_pt_info)
  arrow_g <- gen_figure(language = row_pt_info$language,
                        png_url = row_pt_info$png_url,
                        today_act = row_pt_info$today_act,
                        previous_bp = row_pt_info$previous_bp,
                        previous_date_text = row_pt_info$previous_date_text)
  print(glue::glue("Saving figure to: {knit_env$plot_pth_norm}"))
  ggplot2::ggsave(filename = knit_env$plot_pth_norm,
                  plot = arrow_g,
                  width = 11,
                  height = 8.5)
  
  pdf_path <- fs::path_join(c(pdf_dir, row_pt_info$pdf_name))
  tex_path <- stringr::str_replace(pdf_path, "\\.pdf$", "\\.tex")
  
  print(glue::glue("tex saved to: {tex_path}"))
  print(glue::glue("PDF saved to: {pdf_path}"))
  
  #print(row_pt_info)
  
  #browser()
  
  knitr::knit2pdf(input = row_pt_info$act_rnw_f,
                  output = tex_path,
                  clean = TRUE,
                  #quiet = TRUE,
                  envir = knit_env,
                  compiler = "xelatex",
                  engine_args = "--shell-escape --enable-write18")
  
}

gen_pdf_from_df <- function(dat) {
  print("in gen_pdf_from_df")
  print(dat)
  if (identical(dat, empty_batch_df)) {
    print("Need to upload data.")
  } else {
    pdf_dir <- fs::path_temp('batch_pdfs')
    if (fs::dir_exists(pdf_dir)) {
      print(glue::glue("Removing: {pdf_dir}"))
      fs::dir_delete(pdf_dir)
    }
    fs::dir_create(pdf_dir, recurse = TRUE)
    print(glue::glue("Created: {pdf_dir}"))
    fs::dir_copy("www",
                 fs::path_join(c(pdf_dir, "www")),
                 overwrite = TRUE)
    
    for (row_i in 1:nrow(dat)) {
      row_dat <- dat[row_i, , drop = FALSE]
      if (row_dat$is_valid == TRUE) {
        print("************************* BATCH *************************")
        gen_pdf_from_single_row(row_dat, pdf_dir)
      } else {
        next
      }
      #break
    }
    print("BATCH PDF CREATION DONE.")
  }
  return(pdf_dir)
}