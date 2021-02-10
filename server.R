

server <- function(input, output, session) {
  
  list_name_all <- read_rds("list_name_all.rds") %>%
    dplyr::mutate(name = trimws(name)) %>%
    dplyr::filter(!name %in% c("", " ")) %>% 
    dplyr::mutate(
      start_letter = stringr::str_sub(name, 1, 1),
      name = stringr::str_replace(name, "\\.", ""),
      link = NULL
    ) %>%
    dplyr::distinct()

  observeEvent(input$button_names, {
    
    req(input$gender)
    req(input$num_names)
    req(input$lastletter)
    
    output$names_table <- renderText(

      # tutte ultime lettere - tutti sesso
      if(input$lastletter %in% "ALL" & input$gender %in% "ALL") {
        list_name_all %>%
          dplyr::mutate(name = toupper(name),
                        gender = toupper(gender),
                        gender = trimws(gender)) %>%
          dplyr::distinct() %>%
          dplyr::mutate(link = NULL) %>%
          # dplyr::filter(nchar(name) <= 15) %>%
          dplyr::select(name) %>%
          dplyr::sample_n(., min(5, input$num_names), replace = T) %>%
          knitr::kable("html") %>%
          kable_styling("striped", full_width = F) 
        # A ! ultime lettere - tutti sesso
      } else if (! input$lastletter %in% "ALL" & input$gender %in% "ALL") {
        list_name_all %>%
          dplyr::mutate(name = toupper(name),
                        gender = toupper(gender),
                        gender = trimws(gender)) %>%
          dplyr::filter(!str_sub(name, start= -1) %in% "A") %>%
          dplyr::distinct() %>%
          dplyr::mutate(link = NULL) %>%
          # dplyr::filter(nchar(name) <= 15) %>%
          dplyr::select(name) %>%
          dplyr::sample_n(., min(5, input$num_names), replace = T) %>%
          knitr::kable("html") %>%
          kable_styling("striped", full_width = F) 
        # tutte ultime lettere - sesso
      } else if (input$lastletter %in% "ALL" & input$gender %in% "ALL") {
        list_name_all %>%
          dplyr::mutate(name = toupper(name),
                        gender = toupper(gender),
                        gender = trimws(gender)) %>%
          dplyr::distinct() %>%
          dplyr::mutate(link = NULL) %>%
          dplyr::filter(
            # nchar(name) <= 15, 
            gender %in% input$gender) %>%
          dplyr::select(name) %>%
          dplyr::sample_n(., as.numeric(input$num_names), replace = T) %>%
          knitr::kable("html") %>%
          kable_styling("striped", full_width = F) 
      } else {
        list_name_all %>%
          dplyr::mutate(name = toupper(name),
                        gender = toupper(gender),
                        gender = trimws(gender)) %>%
          dplyr::filter(!str_sub(name, start= -1) %in% "A") %>%
          dplyr::distinct() %>%
          dplyr::mutate(link = NULL) %>%
          dplyr::filter(
            # nchar(name) <= 15, 
            gender %in% input$gender) %>%
          dplyr::select(name) %>%
          dplyr::sample_n(., as.numeric(input$num_names), replace = T) %>%
          knitr::kable("html") %>%
          kable_styling("striped", full_width = F) 
      }
    )
  })
  
  all_names_reactive <- reactive({
    req(input$start_letter)
      datatable(list_name_all %>%
                  dplyr::mutate(name = toupper(name),
                                gender = toupper(gender),
                                gender = trimws(gender)) %>%
                  dplyr::filter(str_sub(name, start= 1, end = 1) %in% input$start_letter,
                                gender %in% input$gender_start_letter) %>%
                  dplyr::distinct() %>%
                  dplyr::select(name) %>%
                  dplyr::arrange(name),
                rownames = F)
  })
  
  output$all_names_table <- renderDT(
    all_names_reactive()
    )
  
  observeEvent(input$go, {
    screenshot()
  })
  
  # output$row_selected = renderPrint(input$all_names_table_rows_selected)
  names_selected <- reactive({
    names_sel <- unlist(all_names_reactive())
    names_sel <- names_sel[1+input$all_names_table_rows_selected]
    names_sel %>% unlist %>% table() %>% names
  })
  output$row_selected = renderPrint(names_selected())
  output$names_selected_table <- renderDT(
    datatable(tibble(name = names_selected()),
              rownames = F)
  )

}