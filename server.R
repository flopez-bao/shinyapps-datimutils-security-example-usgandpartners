# import libraries -----
library(shiny)
library(futile.logger)
library(shinyWidgets)
library(datimutils)
library(httr)
library(magrittr)
library(xml2)

source("./read_data.R")

### js ----
# allows for using the enter button for the log in
jscode_login <- '$(document).keyup(function(e) {
    var focusedElement = document.activeElement.id;
    console.log(focusedElement);
    if (e.key == "Enter" && focusedElement == "user_name") {
    $("#password").focus();
    } else if (e.key == "Enter" && focusedElement == "password") {
    $("#login_button").click();
    }
});'

### Initiate logging
logger <- flog.logger()
flog.appender(appender.console(), name = "datimutils")

### OAuth Client information
if (interactive()) {
  # NOTE: The line below must be ran manually to set the port
  # OR this line can be added to .Rprofile.
  # This is not an issue when using a single file version of shiny, ie app.R
  # The order by which the files execute is the reasoning behind this.
  # options(shiny.port = 3123)
  # # testing url
  APP_URL <- "http://127.0.0.1:3123/"# This will be your local host path
} else {
  # deployed URL
  APP_URL <- Sys.getenv("APP_URL") #This will be your shiny server path
}

oauth_app <- httr::oauth_app(Sys.getenv("OAUTH_APPNAME"),
                             key = Sys.getenv("OAUTH_KEYNAME"), # dhis2 = Client ID
                             secret = Sys.getenv("OAUTH_SECRET"), #dhis2 = Client Secret
                             redirect_uri = APP_URL
)

oauth_api <- httr::oauth_endpoint(base_url = paste0(Sys.getenv("BASE_URL"), "uaa/oauth"),
                                  request = NULL,
                                  authorize = "authorize",
                                  access = "token"
)

oauth_scope <- "ALL"


has_auth_code <- function(params) {
  
  return(!is.null(params$code))
}

# which users should have access to data? IN THIS CASE ONLY PARTNERS - USG FOLKS SEE EVERYTHING, PARTNERS LIMITED TO MECH ACCESS
USG_USERS = c("Agency", "Interagency", "Global Agency", "Global")
PARTNER_USERS = c("Global Partner", "Partner")

# server ----
server <- function(input, output, session) {
  
  ready <- reactiveValues(ok = FALSE)
  
  # user information
  user_input  <-  reactiveValues(authenticated = FALSE,
                                 status = "",
                                 d2_session = NULL,
                                 memo_authorized = FALSE)
  
  # logout process ----
  observeEvent(input$logout_button, {
    req(input$logout)
    # Returns to the log in screen without the authorization code at top
    updateQueryString("?", mode = "replace", session = session)
    flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged out."))
    ready$ok <- FALSE
    user_input$authenticated  <-  FALSE
    user_input$user_name <- ""
    user_input$authorized  <-  FALSE
    user_input$d2_session  <-  NULL
    d2_default_session <- NULL
    gc()
    session$reload()
  })
  
  
  # is the user authenticated?
  output$ui <- renderUI({
    if(user_input$authenticated == FALSE) {
      uiOutput("uiLogin")
    } else {
      uiOutput("authenticated")
    }
  })
  
  
  
  # login page with username and password
  output$uiLogin  <-  renderUI({
    
    fluidPage(
      wellPanel(
        fluidRow(
          h4("The following up was created as an example of how developers can access the datim security information of their shiny app users utilizing the package datimutils. Please login with your DATIM credentials:"),
          br()
        ),
        fluidRow(
          # textInput("user_name", "Username: ", width = "500px"),
          # passwordInput("password", "Password:", width = "500px"),
          # actionButton("login_button", "Log in!"),
          
          actionButton("login_button_oauth", "Log in with DATIM"),
          uiOutput("ui_hasauth"),
          uiOutput("ui_redirect")
        )
      )
    )
    
  })
  
  # once you login this page shows up
  output$authenticated <- renderUI({ 
    fluidPage(
      fluidRow(
        h4("Click the following button to see information about this user, session and data access:")
      ),
      fluidRow(
        column(
          br(),
          br(),
          actionButton("groupid_button", "Streams"),
          br(),
          br(),
          actionButton("me_button", "User Type"),
          br(),
          br(),
          actionButton("mech_cocuid_button", "Mechanisms by Category Option Combos Id"),
          br(),     
          br(),
          actionButton("mech_id_button", "Mechanisms by Mech Code"),
          br(),
          br(),
          actionButton("mech_name_button", "Mechanisms by Name"),
          br(),
          br(),
          actionButton("test_data", "Test Data"),
          br(),
          br(),
          width = 6
        ),
        column(
          # actionButton("logout_button", "Log out of Session", style="color: #fff; background-color: #FF0000; border-color: #2e6da4"),
          # width = 6,
          actionButton("logout",
                       "Return to Login Page",
                       icon = icon("sign-out")),
          width = 6
        )
      ),
      br(),
      fluidRow(
        column(12,
               wellPanel(
                 verbatimTextOutput("message")
                 ,style = "overflow-y:scroll; max-height: 400px")
        )
      ),
      br(),
      fluidRow(
        column(12,
               dataTableOutput('table')
        )
      )
      
    )  
  })
  
  
  
  
  #UI that will display when redirected to OAuth login agent
  output$ui_redirect <- renderUI({
    #print(input$login_button_oauth) useful for debugging
    if (!is.null(input$login_button_oauth)) { # nolint
      if (input$login_button_oauth > 0) { # nolint
        url <- httr::oauth2.0_authorize_url(oauth_api, oauth_app, scope = oauth_scope)
        redirect <- sprintf("location.replace(\"%s\");", url)
        tags$script(HTML(redirect))
      } else NULL
    } else NULL
  })
  
  
  
  
  
  
  
  # User and mechanisms reactive value pulled only once ----
  user <- reactiveValues(type = NULL)
  mechanisms <- reactiveValues(my_cat_ops = NULL)
  userGroups <- reactiveValues(streams = NULL)
  
  
  
  
  
  
  ### Login Button oauth Checks
  observeEvent(input$login_button_oauth > 0, {
    
    #Grabs the code from the url
    params <- parseQueryString(session$clientData$url_search)
    #Wait until the auth code actually exists
    req(has_auth_code(params))
    
    #Manually create a token
    token <- httr::oauth2.0_token(
      app = oauth_app,
      endpoint = oauth_api,
      scope = oauth_scope,
      use_basic_auth = TRUE,
      oob_value = APP_URL,
      cache = FALSE,
      credentials = httr::oauth2.0_access_token(endpoint = oauth_api,
                                                app = oauth_app,
                                                code = params$code,
                                                use_basic_auth = TRUE)
    )
    
    loginAttempt <- tryCatch({
      user_input$uuid <- uuid::UUIDgenerate()
      datimutils::loginToDATIMOAuth(base_url =  Sys.getenv("BASE_URL"),
                                    token = token,
                                    app = oauth_app,
                                    api = oauth_api,
                                    redirect_uri = APP_URL,
                                    scope = oauth_scope,
                                    d2_session_envir = parent.env(environment())
      ) },
      # This function throws an error if the login is not successful
      error = function(e) {
        flog.info(paste0("User ", input$user_name, " login failed. ", e$message), name = "datimutils")
      }
    )
    
    if (exists("d2_default_session")) {
      
      user_input$authenticated  <-  TRUE
      user_input$d2_session  <-  d2_default_session$clone()
      d2_default_session <- NULL
      
      #Need to check the user is a member of the PRIME Data Systems Group, COP Memo group, or a super user
      user_input$memo_authorized <-
        grepl("VDEqY8YeCEk|ezh8nmc4JbX", user_input$d2_session$me$userGroups) |
        grepl(
          "jtzbVV4ZmdP",
          user_input$d2_session$me$userCredentials$userRoles
        )
      flog.info(
        paste0(
          "User ",
          user_input$d2_session$me$userCredentials$username,
          " logged in."
        ),
        name = "datimutils"
      )
      
      
      flog.info(
        paste0(
          "User ",
          user_input$d2_session$me$userCredentials$username,
          " logged in."
        ),
        name = "datimutils"
      )
    }
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  # # Login process ----
  # observeEvent(input$login_button, {
  #   tryCatch({
  #     datimutils::loginToDATIM(base_url = Sys.getenv("BASE_URL"),
  #                              username = input$user_name,
  #                              password = input$password,
  #                              d2_session_envir = parent.env(environment())
  #     )
  #     
  #     # DISALLOW USER ACCESS TO THE APP-----
  #     
  #     # store data so call is made only once
  #     userGroups$streams <-  datimutils::getMyStreams()
  #     user$type <- datimutils::getMyUserType()
  #     mechanisms$my_cat_ops <- datimutils::listMechs()
  #     
  #     # if a user is not to be allowed deny them entry
  #     if (!user$type %in% c(USG_USERS, PARTNER_USERS)) {
  #       
  #       # alert the user they cannot access the app
  #       sendSweetAlert(
  #         session,
  #         title = "YOU CANNOT LOG IN",
  #         text = "You are not authorized to use this application",
  #         type = "error"
  #       )
  #       
  #       # log them out
  #       Sys.sleep(3)
  #       flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged out."))
  #       user_input$authenticated  <-  FALSE
  #       user_input$user_name <- ""
  #       user_input$authorized  <-  FALSE
  #       user_input$d2_session  <-  NULL
  #       d2_default_session <- NULL
  #       gc()
  #       session$reload()
  #       
  #     }
  #   },
  #   # This function throws an error if the login is not successful
  #   error = function(e) {
  #     flog.info(paste0("User ", input$username, " login failed."), name = "datapack")
  #   }
  #   )
  #   
  #   if (exists("d2_default_session")) {
  #     if (any(class(d2_default_session) == "d2Session")) {
  #       user_input$authenticated  <-  TRUE
  #       user_input$d2_session  <-  d2_default_session$clone()
  #       d2_default_session <- NULL
  #       
  #       
  #       # Need to check the user is a member of the PRIME Data Systems Group, COP Memo group, or a super user
  #       user_input$memo_authorized  <-
  #         grepl("VDEqY8YeCEk|ezh8nmc4JbX", user_input$d2_session$me$userGroups) |
  #         grepl(
  #           "jtzbVV4ZmdP",
  #           user_input$d2_session$me$userCredentials$userRoles
  #         )
  #       flog.info(
  #         paste0(
  #           "User ",
  #           user_input$d2_session$me$userCredentials$username,
  #           " logged in."
  #         ),
  #         name = "datapack"
  #       )
  #     }
  #   } else {
  #     sendSweetAlert(
  #       session,
  #       title = "Login failed",
  #       text = "Please check your username/password!",
  #       type = "error"
  #     )
  #   }
  # })
  
  
  # show user information ----
  observeEvent(input$me_button, {
    output$message <- renderPrint({ user$type })
  })
  
  # show streams ids data ----
  observeEvent(input$groupid_button, {
    groups_id_df <- userGroups$streams
    
    # display streams
    output$message <- renderPrint({ groups_id_df })
    
  })
  
  # show mechs by cocuid ----
  observeEvent(input$mech_cocuid_button, {
    
    # display mechanisms  
    output$table <- renderDataTable(mechanisms$my_cat_ops[,c("combo_id", "name")],
                                    options = list(
                                      pageLength = 10
                                    )
    )
    
  })
  
  # show mechs by mechs code----
  observeEvent(input$mech_id_button, {
    
    # display mechanisms  
    output$table <- renderDataTable(mechanisms$my_cat_ops[,c("mech_code", "name")],
                                    options = list(
                                      pageLength = 10
                                    )
    )
    
  })
  
  # show mechs by name ----
  observeEvent(input$mech_name_button, {
    
    # display mechanisms 
    output$table <- renderDataTable(mechanisms$my_cat_ops[,c("name"), drop=FALSE],
                                    options = list(
                                      pageLength = 10
                                    )
    )
    
    
  })
  
  # test data button ----
  observeEvent(input$test_data, {
    
    # show data test data based on user type
    if (user$type %in% USG_USERS) {
      
      # USG users can see all the data
      output$table <- renderDataTable(sample_data,
                                      options = list(
                                        pageLength = 10
                                      )
      )
      
      # PARTNERS see filtered data
    } else {
      
      sample_data_f <- merge(mechanisms$my_cat_ops, sample_data, by= "mech_code")
      output$table <- renderDataTable(sample_data_f,
                                      options = list(
                                        pageLength = 10
                                      )
      )
    }
    
  })
  
}