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
               
               navbarMenu("", icon = icon("user-circle"),
                        
                              tabPanel(list(
                                
                                hidden(
                                  div(id = "lodiv",
                                      a(href = "", auth0::logoutButton(label = "Logout", style="background:#db4040; color:white;"), style="text-align: center;"), style="text-align: center;"
                                      ))
                                )) # tabPanel Logout
                              
                         
               ), # tabPanel: Logout
               
               tabPanel(title = span("Data Import", title = "Import the data"), value = "Data Import", icon = icon("file-import"),
                        hidden(
                          div(id = "didiv", 
                              fluidRow(
                                
                                column(width = 3,
                                       div(id = "conditionalPanel0", class = "divClassSideBar",
                                           # p(""),
                                           # hr(),
                                           h4("Data upload: ", style = "color: #bc2929; font-size: 16px;"),
                                           splitLayout(cellWidths = c("90%", "10%"),
                                                       uiOutput(outputId = "file_upload", inline = TRUE), 
                                                       actionButton(inputId = "reset_file_upload", label= "", class = "btn-danger shiny-bound-input", icon = shiny::icon("times-circle"), style='padding:4px; font-size:80%; margin-top:5px')),
                                           
                                           actionButton(inputId = "upload_input_data", label= "Upload data now", class = "btn-danger btn-block", icon = shiny::icon("file-import")),
                                           p(""),
                                           hr(),
                                           actionButton(inputId = "go_to_data_selection", label= "Data selection", class = "btn-info btn-block", icon = shiny::icon("arrow-circle-right"))
                                           
                                       ) # divClassSideBar
                                ), # column 3
                                
                                
                                column(width = 9,
                                       tabsetPanel(
                                         tabPanel("Preview",
                                                  div(id = "pdiv", style = "background:white;",
                                                      # h4("Data preview: ", style = "color: #bc2929; font-size: 18px;"),
                                                      # tableOutput("table_preview"),
                                                      # p("10 of 960 rows shown. See Data-tab for details."),
                                                      
                                                      # hr(),
                                                      
                                                      h4("Data Description: ", style = "color: #bc2929; font-size: 18px;"),
                                                      
                                                      p("A dataset containing the phenotypic data and other attributes of a sample of certain number of individuals. The variables are as follows:"),
                                                      p(""),
                                                      tableOutput("table_desc")
                                                  
                                                  )
                                                  ),
                                         tabPanel("Data",
                                                  
                                                  DT::dataTableOutput("table_data") %>% withSpinner(color="#bc2929") # , style = "height:500px; overflow-y: scroll;"
                                                  
                                                  )
                                         )
                                       
                                       
                                ) # column 9
                                
                              ), # fluidRow
                        
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
                        ), # hidden
               ), # tabPanel: Data Import
               
               tabPanel(title = span("Data Selection", title = "Select the data"), value = "Data Selection", icon = icon("database"),
                        hidden(
                          div(id = "dsdiv", 
                              
                              fluidRow(
                                
                                column(width = 4,
                                       div(id = "conditionalPanel0", class = "divClassSideBar",
                                           # p(""),
                                           # hr(),
                                           h4("Data Selection: ", style = "color: #bc2929; font-size: 16px;"),
                                           p(""),
                                           bsCollapse(id = "collapseExample", open = "1. Dependent factor",
                                                      
                                                      bsCollapsePanel("1. Dependent factor", style = "danger",
                                                                      
                                                                      selectizeInput(
                                                                        inputId = 'dep_factor',
                                                                        label = "Dependent factor: ",
                                                                        choices = NULL,  
                                                                        multiple = F,
                                                                        options = list(
                                                                          placeholder = 'Please select Measurement',
                                                                          onInitialize = I('function() { this.setValue(""); }')
                                                                        ))# ,
                                                                      
                                                                      # selectizeInput(
                                                                      #   inputId = 'indep_factor',
                                                                      #   label = "Independent factor: ",
                                                                      #   choices = NULL,  
                                                                      #   multiple = T,
                                                                      #   options = list(
                                                                      #     placeholder = 'Please select Independent Measurement',
                                                                      #     onInitialize = I('function() { this.setValue(""); }')
                                                                      #   ))
                                                                      
                                                      ),
                                                      
                                                      bsCollapsePanel("2. Independent factor", style = "danger", # Filter with Independent Measurement
                                                                      
                                                                      prettyCheckbox(inputId = "ch_acc_factor",
                                                                                     label = "Use Accession",
                                                                                     thick = TRUE,
                                                                                     animation = "pulse",
                                                                                     status = "danger"),
                                                                      
                                                                      uiOutput("acc_factor.dynamicui"),
                                                                      
                                                                      
                                                                      prettyCheckbox(inputId = "ch_block_factor",
                                                                                     label = "Use Block / tray",
                                                                                     thick = TRUE,
                                                                                     animation = "pulse",
                                                                                     status = "danger"),
                                                                      # uiOutput("block_factor.dynamicui"),
                                                                      noUiSliderInput(
                                                                        inputId = "block_factor", 
                                                                        label = "Block / tray: ",
                                                                        min = 1, max = 20, value = 3, step = 1,
                                                                        width = "90%",
                                                                        color = "#bc2929", 
                                                                        tooltips = TRUE, 
                                                                        format = wNumbFormat(decimals = 0)
                                                                      ),
                                                                      
                                                                      prettyCheckbox(inputId = "ch_env_factor",
                                                                                     label = "Use Environment",
                                                                                     thick = TRUE,
                                                                                     animation = "pulse",
                                                                                     status = "danger"),
                                                                      uiOutput("env_factor.dynamicui"),
                                                                      
                                                                      
                                                                      prettyCheckbox(inputId = "ch_pol_factor",
                                                                                     label = "Use Population",
                                                                                     thick = TRUE,
                                                                                     animation = "pulse",
                                                                                     status = "danger"),
                                                                      uiOutput("pol_factor.dynamicui")
                                                                              
                                                                      
                                                      )#, 
                                                      # bsCollapsePanel("3. Independent", style = "danger",
                                                      #                 selectizeInput(
                                                      #                   inputId = 'sel_factor',
                                                      #                   label = "Select SEL: ",
                                                      #                   choices = NULL,  
                                                      #                   multiple = T,
                                                      #                   options = list(
                                                      #                     placeholder = 'Please select SEL',
                                                      #                     onInitialize = I('function() { this.setValue(""); }')
                                                      #                   )),
                                                      #                 selectizeInput(
                                                      #                   inputId = 'svs_factor',
                                                      #                   label = "Select SOILvsSALT: ",
                                                      #                   choices = NULL,  
                                                      #                   multiple = T,
                                                      #                   options = list(
                                                      #                     placeholder = 'Please select SOILvsSALT)',
                                                      #                     onInitialize = I('function() { this.setValue(""); }')
                                                      #                   )),
                                                      #                 selectizeInput(
                                                      #                   inputId = 'cva_factor',
                                                      #                   label = "Select CONvsANC: ",
                                                      #                   choices = NULL,  
                                                      #                   multiple = T,
                                                      #                   options = list(
                                                      #                     placeholder = 'Please select CONvsANC',
                                                      #                     onInitialize = I('function() { this.setValue(""); }')
                                                      #                   )),
                                                      #                 selectizeInput(
                                                      #                   inputId = 'avc_factor',
                                                      #                   label = "Select AvsC: ",
                                                      #                   choices = NULL,  
                                                      #                   multiple = T,
                                                      #                   options = list(
                                                      #                     placeholder = 'Please select AvsC)',
                                                      #                     onInitialize = I('function() { this.setValue(""); }')
                                                      #                   )),
                                                      #                 selectizeInput(
                                                      #                   inputId = 'pvs_factor',
                                                      #                   label = "Select PSvsSS: ",
                                                      #                   choices = NULL,  
                                                      #                   multiple = T,
                                                      #                   options = list(
                                                      #                     placeholder = 'Please select PSvsSS',
                                                      #                     onInitialize = I('function() { this.setValue(""); }')
                                                      #                   ))
                                                      #                            
                                                      # )
                                           ) # bsCollapse
                                       
                                       ) # divClassSideBar
                                           
                                       
                                ), # column 3
                                
                                
                                column(width = 8,
                                       tabsetPanel(
                                         tabPanel("Graphs",
                                                  
                                                  tabsetPanel(
                                                    
                                                    tabPanel("Histograms",
                                                             p(""),
                                                             fluidRow(
                                                               # column(width = 3,
                                                               #        uiOutput(outputId = "hist_select_var")
                                                               #        ), 
                                                               column(width = 12,
                                                                      plotOutput(outputId = "hist_var") %>% withSpinner(color="#bc2929")
                                                               )
                                                             )
                                                             ),
                                                    
                                                    tabPanel("Gig-lot",
                                                             p(""),
                                                             fluidRow(
                                                               column(width = 12,
                                                                      plotOutput(outputId = "gig_var") %>% withSpinner(color="#bc2929")
                                                                      )
                                                               
                                                             )
                                                             
                                                             )
                                                    
                                                    
                                                  )
                                                  
                                                  
                                                  )#, 
                                         # tabPanel("Data summary"
                                         #          
                                         #          )
                                         
                                       ) # tabsetPanel
                                       
                                ) # column 9
                                
                              ), # fluidRow
                              
                              
                              
                          )
                        )
               ), # tabPanel: Data Selection
               
               tabPanel(title = span("Model Selection", title = "Select the model"), value = "Model Selection", icon = icon("microchip"),
                        hidden(
                          div(id = "msdiv"
                              
                              
                              
                              
                              
                              
                          )
                        )
               ), # tabPanel: Model Selection
               
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

