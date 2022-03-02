auth0_server(function(input, output, session) {

  user_metadata <- reactive({
    user_data <- users_metadata %>%
      filter(mail == session$userData$auth0_info$name) %>%
      as.list()
    user_data[['filename']] <- paste0('db/', user_data$id, '.xlsx')
    return(user_data)
  })

  power_data <- reactive({
    req(user_metadata())
    if (file.exists(user_metadata()$filename)) {
      last_power_data <- readxl::read_excel(user_metadata()$filename) %>%
        mutate(datetime = with_tz(datetime, tz = config$tzone))
      last_date <- as_date(max(last_power_data$datetime))
    } else {
      last_power_data <- tibble()
      last_date <- dmy(01012022)
    }
    query_tbl <- query_timeseries_data_table_py(
      power_table, 'id', user_metadata()$id, 'timestamp', last_date, today()+days(1)
    )
    if (!is.null(query_tbl)) {
      new_power_data <- query_tbl %>%
        mutate(
          datetime = floor_date(as_datetime(timestamp/1000, tz = config$tzone), '5 minutes'),
          map_dfr(payload, parse_meters_payload)
        ) %>%
        select(datetime, exported, imported)

      power_tbl <- bind_rows(last_power_data, new_power_data) %>%
        arrange(datetime) %>%
        mutate_if(is.numeric, round, 3) %>%
        distinct()
      writexl::write_xlsx(power_tbl, user_metadata()$filename)
    } else {
      power_tbl <- tibble(datetime = today(), power = NA)
    }
    return(power_tbl)
  })


  output$power_now <- renderInfoBox({

    pdata <- power_data() %>%
      convert_historic_to_instant_power() %>%
      mutate(power = Imported - Exported)

    infoBox(
      title = 'Power now',
      value = paste(round(pdata$power[nrow(pdata)]), "W"),
      subtitle = paste("At", strftime(pdata$datetime[nrow(pdata)], format = "%H:%M")),
      icon = icon("bolt"),
      color = 'olive',
      fill = T
    )
  })

  energy_today <- reactive({
    power_data() %>%
      filter(between(date(datetime), today()-days(2), today())) %>%
      mutate(date = floor_date(datetime, unit = 'day', week_start = 1)) %>%
      group_by(date) %>%
      summarise(Imported = max(imported), Exported = max(exported)) %>%
      mutate(
        Imported = round(Imported - lag(Imported), 2),
        Exported = round(Exported - lag(Exported), 2)
      ) %>%
      drop_na() %>%
      filter(date == today())
  })

  output$imported_today <- renderInfoBox({
    infoBox(
      title = "Imported today",
      value = paste(energy_today()$Imported, "kWh"),
      subtitle = paste0(round(energy_today()$Imported/energy_today()$Exported*100), "% of exported energy"),
      icon = icon("level-down-alt"),
      color = 'light-blue',
      fill = T
    )
  })

  output$exported_today <- renderInfoBox({
    infoBox(
      title = "Exported today",
      value = paste(energy_today()$Exported, "kWh"),
      subtitle = paste0(round(energy_today()$Exported/energy_today()$Imported*100), "% of imported energy"),
      icon = icon("level-up-alt"),
      color = 'yellow',
      fill = T
    )
  })


  output$plot_timeseries <- renderHighchart({
    instant_power <- power_data() %>%
      filter(date(datetime) >= (today()-days(8))) %>% # 16 seconds
      # filter(date(datetime) >= (today()-days(15))) %>% # 22 seconds
      # filter(date(datetime) >= (today()-days(30))) %>% # 33 seconds
      convert_historic_to_instant_power()

    plot_power <- instant_power %>%
      pivot_longer(cols = c(Imported, Exported), names_to = "Flux") %>%
      mutate(datetime = datetime_to_timestamp(datetime)) %>%
      hchart(hcaes(x = datetime, y = value, group = Flux),  type = 'area',
             color = c("#edd17e", "#7cb5ec"), name = c("Exported (W)", "Imported (W)")) %>%
      hc_xAxis(type = 'datetime') %>%
      hc_navigator(enabled = T) %>%
      hc_rangeSelector(
        enabled = T,
        inputEnabled = T,
        buttons = list(
          # list(type = 'all', text = 'Total', title = 'All data'),
          # list(type = 'month', count = 1, text = '1m', title = '1 month'),
          # list(type = 'week', count = 2, text = '2w', title = '2 weeks'),
          list(type = 'week', count = 1, text = '1w', title = '1 week'),
          list(type = 'day', count = 4, text = '4d', title = '4 days'),
          list(type = 'day', count = 2, text = '2d', title = '2 days'),
          list(type = 'day', count = 1, text = '1d', title = '1 day'),
          list(type = 'hour', count = hour(now()), text = 'Today', title = 'Today'),
          list(type = 'hour', count = 6, text = '6h', title = '6 hours'),
          list(type = 'hour', count = 1, text = '1h', title = '1 hour')
        ),
        selected = 4
      ) %>%
      hc_exporting(enabled = T) %>%
      hc_legend(itemStyle = list(color = 'white', fill = 'white'))

    return(plot_power)
  })

  output$plot_columns <- renderHighchart({
    power_data() %>%
      mutate(date = floor_date(datetime, unit = input$columns_unit, week_start = 1)) %>%
      group_by(date) %>%
      summarise(Imported = max(imported), Exported = max(exported)) %>%
      mutate(
        Imported = round(Imported - lag(Imported), 2),
        Exported = round(Exported - lag(Exported), 2)
      ) %>%
      drop_na() %>%
      pivot_longer(cols = c(Imported, Exported), names_to = "Flux") %>%
      mutate(date = datetime_to_timestamp(date)) %>%
      hchart(type = "column", hcaes(x = date, y = value, group = Flux),
             color = c("#edd17e", "#7cb5ec"), name = c("Exported (kWh)", "Imported (kWh)")) %>%
      hc_xAxis(type = 'datetime') %>%
      hc_rangeSelector(enabled = F) %>%
      hc_exporting(enabled = T) %>%
      hc_legend(itemStyle = list(color = 'white', fill = 'white'))
  })

  # output$download <- downloadHandler(
  #   filename = function() {
  #     paste0("consum_", today(), ".xlsx")
  #   },
  #   content = function(file) {
  #     writexl::write_xlsx(power_data(), file)
  #   }
  # )

}, info = a0_info)
