# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Define user interface
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

shinyUI(
  tagList(
    tags$style("@import url(https://use.fontawesome.com/releases/v5.10.0/css/all.css);"),
    tags$head(
      includeCSS("styles.css") # Include custom CSS
    ),
    
    shinybusy::busy_start_up(
      loader = spin_kit(spin = c("cube-grid"), color = "white", style = "width:100px; height:100px;"),
      text = div(
        strong(h2("Loading app...")),
        img(src = "PhenoGraph_logo1.jpg", style = "align: center; height: 250px; width: 250px;"),
        strong(h2("PhenoGraph Shiny aplication"))
      ),
      mode = "timeout",
      timeout = 3000, 
      color = "white",
      background = "#bc2929"
    ),
    
    tags$script(htmlwidgets::JS("setTimeout(function(){history.pushState({}, 'Page Title', window.location.pathname);},2000);")),
    
    tags$script(HTML(
      "document.body.style.fontFamily = 'Recoleta';"
    )),
    
    tags$head(tags$link(rel="shortcut icon", href="PhenoGraph_logo1.jpg")),
    tags$head(HTML("<title>PhenoGraph app</title>")),
    tags$head(tags$script(type="text/javascript", src = "code.js")),
    useShinyjs(),
    
    
    tags$footer(title="",  align = "center", style = "
                      position:fixed;
                      bottom:0;
                      width:100%;
                      height:20px; /* Height of the footer */
                      color: black;
                      padding: 0px;
                      background-color: rgba(255, 255, 255, 0.5);
                      z-index: 1000;
                      display: inline-block;
                      font-size: 14px !important;
                    ",
                tags$a(div(
                  a("PhenoGraph Shiny App | 2023", target="_blank", style = "text-align: center; align: center; height:20px; text-decoration: none; color: #bc2929; font-size: 14px !important;"),
                  style="height:20px;  text-align: center;  float:center;  align: center; padding: 0px; bottom:0; display: inline-block;"))
                
    ), # tags$footer
    
    # tags$style(type="text/css",
    #            ".shiny-output-error { visibility: hidden; }",
    #            ".shiny-output-error:before { visibility: hidden; }"
    # ),
    
    navbarPage(fluid = TRUE,
               id = "inTabset",
               title = a(href = "", span(img(src = "PhenoGraph_logo1.jpg", style = "align: center; height:30px;"), title ="Reload the app", "PhenoGraph"), style = "cursor: pointer; text-decoration: none; color:white;"), 
               selected ="Login",
               
               tabPanel(title = span("Login", title = "Login and authentication"), value = "Login", icon = icon("user-circle"),
                        div(
                          id = "login-basic",
                          style = "width: 500px; max-width: 100%; margin: 0 auto;",
                          
                          div(
                            class = "well",
                            h4(class = "text-center", "Welcome to PhenoGraph Shiny Application!", style = "color: #bc2929;"),
                            div(style = "text-align: center;",
                                a(img(src = "databases.png", style = "align: center; height:50px; ")),
                            ),
                            p(""),
                            h4(class = "text-center", "Please login: ", style = "color: #bc2929;"),
                            
                            
                            textInput(
                              inputId     = "ti_user_name_basic",
                              label       = tagList(icon("user"),
                                                    "User Name"), # or E-mail address
                              placeholder = "Enter user name" # or e-mail address
                            ),
                            
                            passwordInput(
                              inputId     = "ti_password_basic",
                              label       = tagList(icon("unlock-alt"),
                                                    "Password"),
                              placeholder = "Enter password"
                            ),
                            p(""),
                            div(
                              class = "text-center-left",
                              textOutput(outputId = "pass_text"),
                              tags$head(tags$style("#pass_text{color: red;
                                 font-size: 12px;
                                 }"
                              )
                              )
                              
                            ),
                            p(""),
                            div(
                              class = "text-center",
                              actionButton(
                                inputId = "ab_login_button_basic",
                                label = "Log in",
                                class = "btn-danger"
                              )
                            ),
                            p(""),
                            div(a(href = 'mailto: adohmk@gmail.com/', target="_blank", img(src = "PhenoGraph_logo1.jpg", style = "align: center; height:200px;")), style="text-align: center;")
                          )
                        )
                        
                        
                        
               ), # tabPanel Login
               
               
               tabPanel(title = span("Data Import", title = "Import the data"), value = "Data Import", icon = icon("file-import"),
                        hidden(
                          div(id = "didiv", 
                            div(
                              id = "landingpage",
                              fluidRow(
                                absolutePanel(id = "controls1", 
                                              class = "panel panel-default", 
                                              fixed = TRUE,
                                              draggable = FALSE, 
                                              top = "15%", left = "50%", right = "auto", bottom = "auto",
                                              width = 700, height = "auto",# 550,
                                              p(""),
                                              actionButton(inputId = "closePanel", label= "", class = "btn-info", icon = shiny::icon("close", verify_fa = FALSE), style = "background-color: rgba(255, 255, 255, 0.5); color:black; border:rgba(255, 255, 255, 0.5); float:right;"),
                                              p(""),
                                              br(),
                                              br(),
                                              fluidRow(
                                                p(""),
                                                div(class = "containerS",style = "align: center; height: 300px; color: white;",
                                                    div(strong(
                                                      h4("PhenoGraph Shiny App", style = "text-align: center; color: white; font-size: 30px;"),
                                                      p(""),
                                                      
                                                      p("PhenoGraph — interactive Shiny web application."),
                                                      
                                                      p("This interactive platform is used for data import, modeling and selection."),
                                                      p("The goal is, to find relations among the groups using Multiple Linear Regression (MLR)."),
                                                      
                                                      p(""),
                                                      p("Need help using PhenoGraph Shiny app or have feedback? Email to Hoda Mazaheri:", a(href = "mailto: adohmk@gmail.com", "adohmk@gmail.com")),
                                                      style = "text-align: left;"
                                                      
                                                    ))
                                                )
                                              ),
                                              br(),
                                              #div(img(src = "PhenoGraph_logo1.jpg", style = "align: center; height: 200px; width: 200px;"), style="text-align: center;"),
                                              div(a(href = 'mailto: adohmk@gmail.com/', target="_blank", img(src = "PhenoGraph_logo1.jpg", style = "align: center; height:200px;")), style="text-align: center;")
                                )  # absolutePanel
                              ) # fluidRow
                            ) # div
                          ) # div: ddiv
                          ) # hidden
                        
               ), # tabPanel: Data Import
               
               tabPanel(title = span("Model Selection", title = "Select the model"), value = "Model Selection", icon = icon("microchip"),
                        hidden(
                          div(id = "msdiv", 
                              sidebarLayout(
                                sidebarPanel(
                                  
                                  conditionalPanel(
                                    'input.dataset === "mtcars"',
                                    helpText("Click the column header to sort a column.")
                                  ),
                                  conditionalPanel(
                                    'input.dataset === "iris"',
                                    helpText("Display 5 records by default.")
                                  )
                                ),
                                
                                mainPanel(
                                  tabsetPanel(
                                    id = 'dataset',
                                    tabPanel("mtcars", DT::dataTableOutput("mytable2") %>% withSpinner(color="#bc2929")),
                                    tabPanel("iris", DT::dataTableOutput("mytable3") %>% withSpinner(color="#bc2929"))
                                  )
                                )
                                
                              ) # sidebarLayout
                          )
                        )
               ), # tabPanel: Model Selection
               
               tabPanel(title = span("Data Selection", title = "Select the data"), value = "Data Selection", icon = icon("database"),
                        hidden(
                          div(id = "dsdiv", 
                              
                          )
                        )
               ), # tabPanel: Data Selection
               
               
               tabPanel(title = span("About", title = "About"), value = "About", icon = icon("address-card"),
                        
                        hidden(
                          div(id = "adiv",
                              fluidRow(
                                setBackgroundImage(src = "background_image1.jpg"),
                                absolutePanel(id = "controls2", 
                                              class = "panel panel-default", 
                                              fixed = TRUE,
                                              draggable = FALSE, 
                                              top = "20%", left = "50%", right = "auto", bottom = "auto",
                                              width = 800, height = "auto",
                                              
                                              p(""),
                                              strong("Background"),
                                              p(""),
                                              p("PhenoGraph — interactive Shiny web application."),
                                              
                                              p("This interactive platform is used for data import, modeling and selection."),
                                              p("The goal is, to find relations among the groups using Multiple Linear Regression (MLR)."),
                                              p(""),
                                              strong("Contact"),
                                              p(""),
                                              p("Need help using PhenoGraph Shiny app or have feedback? Email to Hoda Mazaheri:", a(href = "mailto: adohmk@gmail.com", "adohmk@gmail.com")),
                                              p(""),
                                              p(strong("Design and functionality by: "), a(href = 'https://github.com/BursacPetar', target="_blank", "Petar Bursac")), 
                                              p("")           
                                )
                              )
                          
                              )
                        
                          )
                        
               ) # tabPanel: About
               
               
    ) # navbarPage
  ) # tagList
) # shinyUI

