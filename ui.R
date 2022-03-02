auth0_ui(fluidPage(
  shinyWidgets::useShinydashboard(),
  theme = shinytheme("darkly"),
  use_waiter(),
  waiterOnBusy(html = waiting_screen("Loading..."), color = "#00000080"),
  # autoWaiter(),
  # # This removes the "code=XXX" of the URL after login, so avoids the error after refreshing
  tags$script(JS("setTimeout(function(){history.pushState({}, 'Page Title', '/');}, 2000);")),

  # Application title
  titlePanel(tagList(
    img(src = "https://aecl.nl/wp-content/uploads/2021/05/beeldmerk_aecl.png", height = 40),
    span(strong("My Smart Meter")),
    span(
      logoutButton(
        "", icon = icon('sign-out-alt'),
        style = "border-radius: 20px;"
      ),
      style = "position:absolute;right:1em;"
    )
  ), windowTitle = "My smart meter"),
  hr(),

  # Menu
  uiOutput("menu"),

  # Body
  fluidRow(
    # column(
    #   4, align='center',
    #   div(style="display: inline-block;", infoBoxOutput('imported_today', width = 12))
    # ),
    # column(
    #   4, align='center',
    #   div(style="display: inline-block;", infoBoxOutput('power_now', width = 12))
    # ),
    # column(
    #   4, align='center',
    #   div(style="display: inline-block;", infoBoxOutput('exported_today', width = 12))
    # ),
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
