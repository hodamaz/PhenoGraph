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
    shinyjs::show(id = "didiv")
    shinyjs::show(id = "msdiv")
    shinyjs::show(id = "dsdiv")
    shinyjs::show(id = "adiv")
    
    updateTabsetPanel(session,
                      "inTabset",
                      selected = "Data Import")
    
  })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # landing page close and open
  observeEvent(input$closePanel,{
    shinyjs::hide("landingpage")
  })
  
  observeEvent(input$openLand,{
    shinyjs::show("landingpage")
  })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  # Example
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(mtcars, options = list(orderClasses = TRUE))
  })
  
  output$mytable3 <- DT::renderDataTable({
    DT::datatable(iris, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
  
    
}) # shinyServer