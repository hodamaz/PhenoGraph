# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Define Server logic
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

shinyServer(function(input, output, session) {
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Custom log in 
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  login_pass <- eventReactive(input$ab_login_button_basic,{
    req(!is.na(input$ti_user_name_basic))
    req(!is.na(input$ti_password_basic))
    pass_user <- FALSE
    if(input$ti_user_name_basic %in% user && input$ti_password_basic %in% pass){
      pass_user <- TRUE
    }
    return(pass_user)
  })
  
  observe({
    req(!is.na(input$ti_user_name_basic))
    req(!is.na(input$ti_password_basic))
    if(login_pass() == FALSE){
      output$pass_text <- renderText({
        "Wrong User Name or Password, please enter the correct values!"
      })
    }
  })

  # Log in 
  observeEvent(input$ab_login_button_basic, {
    
    req(!is.na(input$ti_user_name_basic))
    req(!is.na(input$ti_password_basic))
    req(login_pass() == TRUE)
    
    hideTab(inputId = "inTabset", target = "Login")
    # hideTab(inputId = "inTabset", target = "Registration", session = session)
    
    # show welocome landing page ...
    shinyjs::show(id = "lodiv")
    shinyjs::show(id = "didiv")
    shinyjs::show(id = "msdiv")
    shinyjs::show(id = "dsdiv")
    shinyjs::show(id = "adiv")
    
    updateTabsetPanel(session,
                      "inTabset",
                      selected = "Data Import")
    
  })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  # Buttons 
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # landing page close and open
  observeEvent(input$closePanel,{
    shinyjs::hide("landingpage")
  })
  
  observeEvent(input$openLand,{
    shinyjs::show("landingpage")
  })
  
  # button to new tab
  observeEvent(input$go_to_data_selection, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Data Selection")
  })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  # Data upload
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  filesName <- reactiveValues(file_name = NA)
  
  output$file_upload <- renderUI({
    input$reset_file_upload # Create a dependency with the reset button
    filesName$file_name <- NA
    fileInput(inputId = "input_xlsx", label = NULL, multiple = FALSE, accept = c(".xlsx"), width = "100%", buttonLabel = "Browse: ", placeholder = "Please select .xlsx file")
  })
  
  observeEvent(input$upload_input_data, {
    req(input$input_xlsx)
    filesName$file_name <- input$input_xlsx$name
    
    print(filesName$file_name)
    print(here::here())
    
    if(!is.na(filesName$file_name)){
      show_alert(
        title = "Data successfully uploaded!",
        text = tags$div(
          a(target="_blank", img(src = "PhenoGraph_logo1.jpg", style = "align: center; height:150px;"))
        ),
        html = TRUE,
        width = "40%",
        type = "success"
      )
    } else{
      return()
    }
  })
  
  observeEvent(input$reset_file_upload,{
    filesName$file_name <- NA
  })
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  # Read xlsx with data
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Raw data table
  data_xlsx <- eventReactive(input$upload_input_data,{
    req(input$input_xlsx)
    inFile <- input$input_xlsx
    if (is.null(inFile))
      return(NULL)
    
    xlsx_data <- readxl::read_excel(inFile$datapath, sheet = "G11&G12_V3") %>% 
      as.data.frame()
    
    xlsx_data$Block=factor(xlsx_data$Block)
    xlsx_data$ACC=factor(xlsx_data$ACC)
    xlsx_data$Ind=factor(xlsx_data$Ind)
    xlsx_data$Environment=factor(xlsx_data$Environment)
    xlsx_data$SEL=factor(as.numeric(xlsx_data$SelectionLevel>1),labels=c("ancestor or control","selected"))
    xlsx_data$SOILvsSALT=factor(as.numeric(xlsx_data$SelectionLevel>2),labels=c("not soil","soil"))
    xlsx_data$CONvsANC=factor(as.numeric(xlsx_data$SelectionLevel>0),labels=c("ancestor","not ancestor"))
    xlsx_data$Population=factor(xlsx_data$Population)
    xlsx_data$AvsC=factor(xlsx_data$AvsC,labels=c("apomictic","crossed"))
    xlsx_data$psvsss=as.numeric(xlsx_data$PSDvsSSD>1)*0+as.numeric(xlsx_data$PSDvsSSD==1)*1+as.numeric(xlsx_data$PSDvsSSD==0)*2
    xlsx_data$PSvsSS=factor(xlsx_data$psvsss,labels=c("SSD","PSD","EHP"))
    
    xlsx_data %<>% dplyr::mutate(Block_num = extract_numeric(Block))
    
    xlsx_data
    
  })
  
  # Description sheet
  data_preview <- eventReactive(input$upload_input_data,{
    req(input$input_xlsx)
    inFile <- input$input_xlsx
    if (is.null(inFile))
      return(NULL)
    
    xlsx_data <- readxl::read_excel(inFile$datapath, sheet = "Description") %>% 
      as.data.frame()
    
    xlsx_data
    
  })
  
  
  # Preview Tab
  # --------------------------
  
  
  # output$table_preview <- function() {
  #   req(data_xlsx())
  #   kbl(data_xlsx()[1:10, ]) %>%
  #     # knitr::kable("html") %>%
  #     kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F)
  #     # kable_styling("striped", full_width = F) %>%
  #     # add_header_above(c(" ", "Group 1" = 5, "Group 2" = 6))
  # }
  # 
  
  output$table_desc <- function() {
    req(data_preview())
    
    kbl(data_preview()) %>%
      # knitr::kable("html") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) # , full_width = F
  }
  
  # Data Tab
  # --------------------------
  
  output$table_data <- DT::renderDataTable({
    DT::datatable(
      data_xlsx(),
      escape=F,
      extensions = list('Buttons'),
      options = list("pageLength" = 10, 
                     dom = 'Bfrtip', 
                     buttons = I('colvis'),
                     lengthMenu = c(5, 10, 15, 20),
                     deferRender = TRUE,
                     scrollX = TRUE,
                     scrollY = "500px"),
      selection = list(mode = 'multiple')
    ) %>% DT::formatStyle(0, target= 'row', lineHeight='70%') 
    
  })
  
  # Update selectizeInput on server side
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$acc_factor.dynamicui <- renderUI({
    req(input$ch_acc_factor == TRUE)
    req(data_xlsx())
    
    selectizeInput(
      inputId = 'acc_factor',
      label = "Select Accession: ",
      choices = unique(data_xlsx()$ACC),
      selected = "",
      multiple = T,
      options = list(
        placeholder = 'Please select Accession',
        onInitialize = I('function() { this.setValue(""); }')
      ))
  })
  
  # output$block_factor.dynamicui <- renderUI({
  #   req(input$ch_block_factor == TRUE)
  #   req(data_xlsx())
  #   
  #   noUiSliderInput(
  #     inputId = "block_factor", 
  #     label = "Block / tray: ",
  #     min = 1, max = 20, value = 3, step = 1,
  #     width = "90%",
  #     color = "#bc2929", 
  #     tooltips = TRUE, 
  #     format = wNumbFormat(decimals = 0)
  #   )
  # })
  
  output$env_factor.dynamicui <- renderUI({
    req(input$ch_env_factor == TRUE)
    req(data_xlsx())
    
    selectizeInput(
      inputId = 'env_factor',
      label = "Select Environment: ",
      choices = unique(data_xlsx()$Environment),
      selected = "",
      multiple = T,
      options = list(
        placeholder = 'Please select Environment',
        onInitialize = I('function() { this.setValue(""); }')
      ))
    
  })
  
  output$pol_factor.dynamicui <- renderUI({
    req(input$ch_pol_factor == TRUE)
    req(data_xlsx())
    
    selectizeInput(
      inputId = 'pol_factor',
      label = "Select Population: ",
      choices = unique(data_xlsx()$Population),
      selected = "",
      multiple = T,
      options = list(
        placeholder = 'Please select Population',
        onInitialize = I('function() { this.setValue(""); }')
      )) 
    
  })
  
  observeEvent(data_xlsx(), {
    
    # 1
    updateSelectizeInput(session,
                         inputId = 'dep_factor',
                         choices = names(data_xlsx())[9:15],
                         selected = "",
                         options = list(
                           placeholder = 'Please select Measurement',
                           onInitialize = I('function() { this.setValue(""); }')),
                         server = TRUE)
    
    # updateSelectizeInput(session,
    #                      inputId = 'indep_factor',
    #                      choices = indep_names,
    #                      options = list(
    #                        placeholder = 'Please select Independent Measurement',
    #                        onInitialize = I('function() { this.setValue(""); }')),
    #                      server = TRUE)
    
    
    # 2
    # updateSelectizeInput(session,
    #                      inputId = 'acc_factor',
    #                      choices = unique(data_xlsx()$ACC),
    #                      selected = "",
    #                      options = list(
    #                        placeholder = 'Please select Accession',
    #                        onInitialize = I('function() { this.setValue(""); }')),
    #                      server = TRUE)
    
    # updateSelectizeInput(session,
    #                      inputId = 'env_factor',
    #                      choices = unique(data_xlsx()$Environment),
    #                      selected = "",
    #                      options = list(
    #                        placeholder = 'Please select Environment',
    #                        onInitialize = I('function() { this.setValue(""); }')),
    #                      server = TRUE)
    
    # updateSelectizeInput(session,
    #                      inputId = 'pol_factor',
    #                      choices = unique(data_xlsx()$Population),
    #                      selected = "",
    #                      options = list(
    #                        placeholder = 'Please select Population',
    #                        onInitialize = I('function() { this.setValue(""); }')),
    #                      server = TRUE)
    
    # 3
    
    # updateSelectizeInput(session,
    #                      inputId = 'sel_factor',
    #                      choices = unique(data_xlsx()$SEL),
    #                      options = list(
    #                        placeholder = 'Please select SEL',
    #                        onInitialize = I('function() { this.setValue(""); }')),
    #                      server = TRUE)
    # 
    # updateSelectizeInput(session,
    #                      inputId = 'svs_factor',
    #                      choices = unique(data_xlsx()$SOILvsSALT),
    #                      options = list(
    #                        placeholder = 'Please select SOILvsSALT',
    #                        onInitialize = I('function() { this.setValue(""); }')),
    #                      server = TRUE)
    # 
    # updateSelectizeInput(session,
    #                      inputId = 'cva_factor',
    #                      choices = unique(data_xlsx()$CONvsANC),
    #                      options = list(
    #                        placeholder = 'Please select CONvsANC',
    #                        onInitialize = I('function() { this.setValue(""); }')),
    #                      server = TRUE)
    # 
    # updateSelectizeInput(session,
    #                      inputId = 'avc_factor',
    #                      choices = unique(data_xlsx()$AvsC),
    #                      options = list(
    #                        placeholder = 'Please select AvsC',
    #                        onInitialize = I('function() { this.setValue(""); }')),
    #                      server = TRUE)
    # 
    # updateSelectizeInput(session,
    #                      inputId = 'pvs_factor',
    #                      choices = unique(data_xlsx()$PSvsSS),
    #                      options = list(
    #                        placeholder = 'Please select PSvsSS',
    #                        onInitialize = I('function() { this.setValue(""); }')),
    #                      server = TRUE)
    
    
  })
  
  
  # observeEvent(input$ch_acc_factor, {
  #   
  #   if(input$ch_acc_factor == FALSE){
  #   
  #   # selectizeInput(
  #   #   inputId = 'acc_factor',
  #   #   label = "Select Accession: ",
  #   #   choices = NULL,
  #   #   multiple = T,
  #   #   options = list(
  #   #     placeholder = 'Please select Accession',
  #   #     onInitialize = I('function() { this.setValue(""); }')
  #   #   ))
  #   # 
  #   # 
  # 
  #   
  #   }
  # })
  # 
  # observeEvent(input$ch_env_factor == FALSE, {
  #   # selectizeInput(
  #   #   inputId = 'env_factor',
  #   #   label = "Select Environment: ",
  #   #   choices = NULL,
  #   #   multiple = T,
  #   #   options = list(
  #   #     placeholder = 'Please select Environment',
  #   #     onInitialize = I('function() { this.setValue(""); }')
  #   #   ))
  # 
  # })
  # 
  # observeEvent(input$ch_pol_factor == FALSE, {
  #   # selectizeInput(
  #   #   inputId = 'pol_factor',
  #   #   label = "Select Population: ",
  #   #   choices = NULL,
  #   #   multiple = T,
  #   #   options = list(
  #   #     placeholder = 'Please select Population',
  #   #     onInitialize = I('function() { this.setValue(""); }')
  #   #   ))
  #   
  # })
  
  # Histograms - based on specified dependent variable
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # output$hist_select_var <- renderUI({
  #   cols_n <- input$dep_factor
  #   
  #   selectizeInput(inputId = "select_hist_var", 
  #                  label = "Dependent variable: ", 
  #                  multiple = F,
  #                  choices = cols_n,  
  #                  selected = "",
  #                  options = list(
  #                    placeholder = 'Please select one of Measurments',
  #                    onInitialize = I('function() { this.setValue(""); }')))# cols_n[1])
  # })
  
  
  
  dataInput <- reactiveValues(data = NULL)
  
  observeEvent(c(input$dep_factor, # select_hist_var
                 input$acc_factor, 
                 #input$svs_factor,
                 #input$sel_factor, 
                 #input$cva_factor, 
                 #input$pvs_factor,
                 
                 input$pol_factor,
                 input$env_factor,
                 #input$avc_factor,
                 input$block_factor,
                 input$ch_acc_factor,
                 input$ch_pol_factor,
                 input$ch_env_factor,
                 input$ch_block_factor),{
    
    req(data_xlsx())
  
    data <- data_xlsx()
    
    if(input$ch_block_factor == TRUE){
      data %<>% dplyr::filter(Block_num == as.numeric(input$block_factor))
    }
    
    # is.null(input$svs_factor) & is.null(input$cva_factor) & is.null(input$pvs_factor) & is.null(input$avc_factor)
    
    # if(is.null(input$acc_factor) &  is.null(input$dep_factor) & is.null(input$pol_factor) & is.null(input$env_factor) ){
    #   data <- data
    #   
    # } else{
    #   
    #   data %<>% dplyr::filter(ACC %in% input$acc_factor & Environment %in% input$env_factor & Population %in% input$pol_factor) #%>% # | SEL %in% input$sel_factor | SOILvsSALT %in% input$svs_factor | CONvsANC %in% input$cva_factor | AvsC %in% input$avc_factor | PSvsSS %in% input$pvs_factor
    #     #dplyr::filter(Block_num == as.numeric(input$block_factor))
    #   
    # }
    
    if(input$ch_acc_factor == FALSE){
      
      acc_factor <- NULL 
    } else{
      
      acc_factor <- input$acc_factor
    }
    
    if(input$ch_pol_factor == FALSE){
      
      pol_factor <- NULL 
    } else{
      
      pol_factor <- input$pol_factor
    }
    
    if(input$ch_env_factor == FALSE){
      
      env_factor <- NULL 
    } else{
      
      env_factor <- input$env_factor
    }
    
    
    
    if(is.null(acc_factor) & is.null(input$dep_factor) & is.null(pol_factor) & is.null(env_factor)){
      
      data <- data_xlsx()
      print(1)
      
    } else if(is.null(acc_factor) & is.null(pol_factor) & is.null(env_factor)){
      
      data <- data
      print(2)
    }
      else if(is.null(acc_factor) & !is.null(pol_factor) & !is.null(env_factor)){
      
      data %<>% dplyr::filter(Environment %in% env_factor & Population %in% pol_factor) 
      print(3)
    } else if(!is.null(acc_factor) & !is.null(pol_factor) & is.null(env_factor)){
      
      data %<>% dplyr::filter(ACC %in% acc_factor & Population %in% pol_factor) 
      print(4)
    } else if(!is.null(acc_factor) & is.null(pol_factor) & !is.null(env_factor)){
      
      data %<>% dplyr::filter(ACC %in% acc_factor & Environment %in% env_factor) 
      print(5)
    } else if(is.null(acc_factor) & is.null(pol_factor) & !is.null(env_factor)){
      
      data %<>% dplyr::filter(Environment %in% env_factor)
      print(6)
    } else if(is.null(acc_factor) & !is.null(pol_factor) & is.null(env_factor)){
      
      data %<>% dplyr::filter(Population %in% pol_factor) 
      print(7)
    } else if(!is.null(acc_factor) & is.null(pol_factor) & is.null(env_factor)){
      
      data %<>% dplyr::filter(ACC %in% acc_factor) 
      print(8)
    } else { #if(!is.null(acc_factor) & !is.null(pol_factor) & !is.null(env_factor)){
      
      data %<>% dplyr::filter(ACC %in% acc_factor & Environment %in% env_factor & Population %in% pol_factor) 
      print(9)
    }
    
    if(dim(data)[1] == 0){
      data <- data_xlsx() #%>% dplyr::filter(Block_num == as.numeric(input$block_factor))
    }
     
    dataInput$data <- data
  })
  
  # Histogram
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$hist_var <- renderPlot({
    req(input$dep_factor)#select_hist_var)
    req(dataInput$data)
    
    data <- dataInput$data
    
    cc <- input$dep_factor# select_hist_var
    
    print(cc)
    
    hist <- ggplot(data = data, aes_string(cc)) + 
      geom_histogram(binwidth = 0.05,
                     col="red", 
                     aes(y = ..density..,
                         fill = ..count..)) +
      scale_fill_gradient("Count", low = "blue", high = "red")+
      stat_function(fun = dnorm,
                    color = "orange",
                    size = 1.5,
                    args = list(mean = mean(as.numeric(data[cc][,]), na.rm = TRUE), sd = sd(as.numeric(data[cc][,]), na.rm = TRUE)))+
      # geom_text(x = min(as.numeric(data[cc][,])) + sd(as.numeric(data[cc][,]), na.rm = TRUE), y =  sd(as.numeric(data[cc][,]), na.rm = TRUE) * 3, label = paste0("Mean: ", mean(as.numeric(data[cc][,]), na.rm = TRUE), "\nSD: ", sd(as.numeric(data[cc][,]), na.rm = TRUE))) +
      labs(title=paste("Variable Histogram: ","Dependent - ", cc, sep = ""), 
           subtitle = paste0("Mean: ", round(mean(as.numeric(data[cc][,]), na.rm = TRUE), 2), "\nSD: ", round(sd(as.numeric(data[cc][,]), na.rm = TRUE), 2))) +
      labs(x="Sample values", y="Density") +
      theme_bw()
    
    return(hist)
  },
  width = "auto",
  height = "auto")
  
  
  # Gig-lot
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$gig_var <- renderPlot({
    req(input$dep_factor)
    req(dataInput$data)
    
    data <- dataInput$data
    
    cc <- input$dep_factor# select_hist_var
    
    pp1 <- data %>%
      ggplot(
        aes(x = Population, y = !!sym(cc), color = Population)) +
      geom_quasirandom(alpha = .5, show.legend = FALSE) +
      geom_boxplot(fill = NA, color = 'black',
                   varwidth = FALSE, outlier.shape = NA) +
      stat_summary(fun = mean, color = 'darkred') +
      scale_color_brewer(palette = "Dark2") +
      labs(
        x = 'Populations',
        y = 'Measurement',
        title = paste0('Distribution of the raw data: ', cc)) +
      facet_grid(vars(Environment), vars(ACC), scales = 'free', space = 'free') +
      theme_classic(base_size = 10) +
      theme(
        axis.text.x = element_text(size = 10, angle = 45, vjust = .5),
        plot.title.position = 'plot',
        plot.subtitle = element_text(color = 'gray40'),
        plot.margin = unit(rep(.25, 4), 'cm'))
    
    return(pp1)
  },
  width = "auto",
  height = "auto")
  
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
    
}) # shinyServer