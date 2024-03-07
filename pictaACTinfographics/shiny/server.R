options(tinytex.verbose = TRUE)

server <- function(input, output, session) {

  default_inputs <- reactive({
    list(
      language = input$language,
      name = input$name,
      display_name = input$name, # name and display name are coming from the same input
      today_act = input$today_act,
      today_date = input$today_date,
      today_date_text = NA,
      previous_bp = input$previous_bp,
      previous_date = input$previous_date,
      previous_date_text = NA,
      asthma_interpretive_statement = NA,
      asthma_score_statement = NA,
      asthma_progress_statment = NA
    )
  })
  
  # PT_INFO -----
  PT_INFO <- reactive({
    PT_VALUES_ASTHMA <- default_inputs()
    return(
      gen_pt_info(
        display_name = PT_VALUES_ASTHMA$display_name,
        language = PT_VALUES_ASTHMA$language,
        today_date = PT_VALUES_ASTHMA$today_date,
        today_act_score = PT_VALUES_ASTHMA$today_act,
        previous_date = PT_VALUES_ASTHMA$previous_date,
        previous_bp_score = PT_VALUES_ASTHMA$previous_bp 
      )
    )
  })
  
  # x coord for arrows -----
  arrow_x_all <- reactive({
    gen_x_coords(PT_INFO()$language)
  })

  # base image -----
  image <- reactive({png::readPNG(PT_INFO()$png_url)})
  base_image_g <- reactive({grid::rasterGrob(image(), interpolate = TRUE)})
  
  
  
  plot_pth_norm <- reactive({fs::path_norm(tempfile(fileext = '.png'))})
  plot_pth_unix <- reactive({gsub("\\\\", "/", plot_pth_norm())})
  fig_pth_act_exterior <- reactive({gen_exterior_png_pth(PT_INFO()$language)})
  
  arrow_g <- reactive({
    base_g <- geom_base_image(base_image_g())
    
    arrow_g <- geom_score_arrows(base_g = base_g,
                                 today_act = PT_INFO()$today_act,
                                 previous_bp = PT_INFO()$previous_bp,
                                 previous_date = PT_INFO()$previous_date_text,
                                 language = PT_INFO()$language,
                                 x_breaks = arrow_x_all())
    return(arrow_g)
  })

  # image -----
  output$plot <- renderImage({
    outfile <- plot_pth_norm()

    ggplot2::ggsave(filename =  outfile,
                    plot = arrow_g(),
                    scale = 1,
                    width = 11, height = 8.5, units = "in", dpi = 300)

    list(src = outfile,
         contentType = 'image/png',
         width = "100%",
         #height = "100%",
         alt = "Alternative text")
  }, deleteFile = FALSE)

  # pdf file name -----
  pdf_single_filename <- reactive({
    name <- gsub(" ", "_", input$name)
    print(name)
    return(glue::glue("act-{PT_INFO()$language}-{name}-{PT_INFO()$today_date}.pdf"))
  })
  
  # single download button -----
  output$download_single <- downloadHandler(
    filename = function() {
      pdf_single_filename()
    },
    content = function(file) {
      withProgress(message = "Generating PDF", {
        out = knitr::knit2pdf(input = PT_INFO()$act_rnw_f,
                              #output = glue::glue("{input$name}-{input$today_date}.tex"),
                              clean = TRUE,
                              #quiet = TRUE,
                              compiler = "xelatex")
        #if (fs::file_exists("act-pamphlet_interrior-english.log")) {fs::file_delete("act-pamphlet_interrior-english.log")}
        #if (fs::file_exists("act-pamphlet_interrior-english.tex")) {fs::file_delete("act-pamphlet_interrior-english.tex")}
        #if (fs::file_exists("act-pamphlet_interrior-spanish.log")) {fs::file_delete("act-pamphlet_interrior-spanish.log")}
        #if (fs::file_exists("act-pamphlet_interrior-spanish.tex")) {fs::file_delete("act-pamphlet_interrior-spanish.tex")}
      })
      #file.rename(out, file) # move pdf to file for downloading
      fs::file_move(out, file)
    },
    contentType = 'application/pdf'
  )


  # batch file ----
  input_file <- reactive({input$file})
 
  input_file_df <- reactive({
    if (!is.null(input_file())) {
      batch_df <- read_batch_file_excel(input_file()$datapath)
      
      #batch_df <- readr::read_csv()
      #problem_rows <- readr::problems(batch_df)$row
      
      # valid_reason <- apply(batch_df, MARGIN = 1, FUN = is_row_valid)
      # valid_reason_t <- purrr::transpose(valid_reason)
      # 
      # batch_df$is_valid[problem_rows] <- FALSE
      # batch_df$reason[problem_rows] <- "readr error"
      
      batch_df <- validate_batch_file(batch_df)

      print(batch_df)
      return(batch_df)
    } else {
      empty_df <- empty_batch_df
      print(empty_df)
      print("Need to upload data.")
      showNotification("Need to upload data.", type = "error")
      return(empty_df)
    }
  })

  output$table <- DT::renderDT(input_file_df())
  
  output$table_errors <- DT::renderDT({
    if (is.null(input_file())) {
      return(empty_batch_df)
    } else {
      return(input_file_df() %>% dplyr::filter(is_valid == FALSE))
    }
  })
  
  output$table_good <- DT::renderDT({
    if (is.null(input_file())) {
      return(empty_batch_df)
    } else {
      return(input_file_df() %>% dplyr::filter(is_valid == TRUE))
    }
  })

  new_pt_info_batch <- reactive({})
  new_pt_info_batch_i <- reactive({})
  
  output$download_batch <- downloadHandler(
    filename = function() {glue::glue("act-batch.zip")},
    content = function(file) {
        saved_pdfs <- gen_pdf_from_df(input_file_df())
        
        print(glue::glue("Compressing for download: {saved_pdfs}"))
        print(glue::glue("Creating for download: {file}"))
        zip_files = fs::dir_ls(path = saved_pdfs, recurse = TRUE, glob = "*.pdf")
        print(glue::glue("Zip contents:"))
        print(zip_files)
        zip::zip(zipfile = file,
                 files = zip_files,
                 mode = "cherry-pick")
        
        print("All done. You should've gotten a save request.")
    },
    contentType = 'application/zip'
  )
  
  # debug print output -----
  output$plot_pth_debug <- renderPrint({
    print(plot_pth_norm());
    print(plot_pth_unix());
    print(fig_pth_act_exterior())
  })
  
  output$asthma_statements <- renderPrint({
    print(glue::glue("Name: {PT_INFO()$display_name}"))
    print(glue::glue("Date text: {PT_INFO()$today_date_text}"))
    print(glue::glue("Score: {PT_INFO()$asthma_score_statement}"))
    print(glue::glue("Interpretive: {PT_INFO()$asthma_interpretive_statement}"))
    print(glue::glue("Progress: {PT_INFO()$asthma_progress_statment}"))
  })
  
  output$pt_list <- renderPrint({PT_INFO()})
  
  output$cwd <- renderPrint({getwd()})
  
  output$tinytex_info <- renderPrint({
    print(tinytex:::is_tinytex())
    print(tinytex::tinytex_root())
  })
  
  output$pdf_single_fn <- renderPrint({print(pdf_single_filename())})
  
  output$batch_file_pth <- renderPrint({str(input_file())})
}
