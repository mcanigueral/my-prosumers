auth0_ui(fluidPage(
  shinyWidgets::useShinydashboard(),
  theme = shinytheme("darkly"),
  use_waiter(),
  # # This removes the "code=XXX" of the URL after login, so avoids the error after refreshing
  tags$script(JS("setTimeout(function(){history.pushState({}, 'Page Title', '/');}, 2000);")),

  # Application title
  titlePanel(tagList(
    # img(src = "udg_logo_short.png", height = 40),
    # HTML("&nbsp;"),
    span(strong("My Smart Meter")),
    span(
      logoutButton(
        "", icon = icon('sign-out-alt'),
        style = "border-radius: 20px;"
      ),
      style = "position:absolute;right:1em;"
    )
  ), windowTitle = "My prosumption"),
  hr(),

  # Menu
  uiOutput("menu"),

  # Body
  fluidRow(
    infoBoxOutput('imported_today'),
    infoBoxOutput('power_now'),
    infoBoxOutput('exported_today')
  ),
  hr(),
  fluidRow(
    highchartOutput('plot_timeseries'),
    column(
      12,
      radioButtons(
        'columns_unit', label = NULL, inline = T,
        choices = c("Daily" = "day", "Weekly" = "week", "Monthly" = "month")
      )
    ),
    highchartOutput('plot_columns')
  ),
  # hr(),
  # uiOutput('month_demand'),
  # hr(),
  #
  # # Download data
  # downloadButton("download", "Descarrega't les dades (Excel)"),
  hr()
), info = a0_info)
