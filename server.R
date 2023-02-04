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
    
    xlsx_data
    
  })
  
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
  
  
  observeEvent(data_xlsx(), {
    
    # 1
    updateSelectizeInput(session,
                         inputId = 'dep_factor',
                         choices = names(data_xlsx())[9:15],
                         options = list(
                           placeholder = 'Please select Measurement',
                           onInitialize = I('function() { this.setValue(""); }')),
                         server = TRUE)
    
    # 2
    updateSelectizeInput(session,
                         inputId = 'acc_factor',
                         choices = unique(data_xlsx()$ACC),
                         options = list(
                           placeholder = 'Please select Accession',
                           onInitialize = I('function() { this.setValue(""); }')),
                         server = TRUE)
    
    updateSelectizeInput(session,
                         inputId = 'env_factor',
                         choices = unique(data_xlsx()$Environment),
                         options = list(
                           placeholder = 'Please select Environment',
                           onInitialize = I('function() { this.setValue(""); }')),
                         server = TRUE)
    
    updateSelectizeInput(session,
                         inputId = 'pol_factor',
                         choices = unique(data_xlsx()$Population),
                         options = list(
                           placeholder = 'Please select Population',
                           onInitialize = I('function() { this.setValue(""); }')),
                         server = TRUE)
    
    # 3
    
    updateSelectizeInput(session,
                         inputId = 'sel_factor',
                         choices = unique(data_xlsx()$SEL),
                         options = list(
                           placeholder = 'Please select SEL',
                           onInitialize = I('function() { this.setValue(""); }')),
                         server = TRUE)
    
    updateSelectizeInput(session,
                         inputId = 'svs_factor',
                         choices = unique(data_xlsx()$SOILvsSALT),
                         options = list(
                           placeholder = 'Please select SOILvsSALT',
                           onInitialize = I('function() { this.setValue(""); }')),
                         server = TRUE)
    
    updateSelectizeInput(session,
                         inputId = 'cva_factor',
                         choices = unique(data_xlsx()$CONvsANC),
                         options = list(
                           placeholder = 'Please select CONvsANC',
                           onInitialize = I('function() { this.setValue(""); }')),
                         server = TRUE)
    
    updateSelectizeInput(session,
                         inputId = 'avc_factor',
                         choices = unique(data_xlsx()$AvsC),
                         options = list(
                           placeholder = 'Please select AvsC',
                           onInitialize = I('function() { this.setValue(""); }')),
                         server = TRUE)
    
    updateSelectizeInput(session,
                         inputId = 'pvs_factor',
                         choices = unique(data_xlsx()$PSvsSS),
                         options = list(
                           placeholder = 'Please select PSvsSS',
                           onInitialize = I('function() { this.setValue(""); }')),
                         server = TRUE)
    
    
  })
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  # # Example
  # output$mytable2 <- DT::renderDataTable({
  #   DT::datatable(mtcars, options = list(orderClasses = TRUE))
  # })
  # 
  # output$mytable3 <- DT::renderDataTable({
  #   DT::datatable(iris, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  # })
  # 
  
    
}) # shinyServer