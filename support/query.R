
source('global.R')

start_date <- today()-days(7)
end_date <- today() + days(1)

# Query for one user ------------------------------------------------------

user_demand <- query_timeseries_data_table_py(
  power_table, 'id', '001', 'timestamp',
  start_date, end_date
) %>%
  mutate(
    datetime = floor_date(as_datetime(timestamp/1000, tz = config$tzone), '5 minutes'),
    map_dfr(payload, parse_meters_payload)
  ) %>%
  select(datetime, id, exported, imported)

user_demand %>%
# readxl::read_excel('db/009.xlsx') %>%
  filter(datetime >= today()-days(7)) %>%
  convert_historic_to_instant_power() %>%
  pivot_longer(cols = c(Imported, Exported), names_to = "Flux") %>%
  mutate(datetime = datetime_to_timestamp(datetime)) %>%
  hchart(hcaes(x = datetime, y = value, group = Flux),  type = 'area', panning = T, zoomType = 'x',
         color = c("#edd17e", "#7cb5ec"), name = c("Exported (W)", "Imported (W)")) %>%
  hc_xAxis(type = 'datetime') %>%
  hc_navigator(enabled = T) %>%
  hc_rangeSelector(
    enabled = T,
    inputEnabled = T,
    buttons = list(
      list(type = 'all', text = 'Total', title = 'Totes les dades'),
      list(type = 'month', count = 1, text = '1m', title = '1 month'),
      list(type = 'week', count = 1, text = '1w', title = '1 week'),
      list(type = 'day', count = 1, text = '1d', title = '1 day'),
      list(type = 'hour', count = 6, text = '6h', title = '6 hours'),
      list(type = 'hour', count = 1, text = '1h', title = '1 hour')
    ),
    selected = 3
  ) %>%
  hc_exporting(enabled = T) %>%
  hc_legend(itemStyle = list(color = 'white', fill = 'white'))


readxl::read_excel('db/009.xlsx') %>%
  convert_historic_to_instant_power() %>%
  dyplot() %>%
  dySeries('Imported', color = 'navy', fillGraph = T) %>%
  dySeries('Exported', color = 'orange', fillGraph = T)


# Time series plot
user_demand %>%
  select(datetime, imported, exported) %>%
  convert_historic_to_instant_power() %>%
  mutate(datetime = datetime_to_timestamp(datetime)) %>%
  pivot_longer(cols = c(Imported, Exported), names_to = "Flux") %>%
  hchart(hcaes(x = datetime, y = value, group = Flux),  type = 'area',
         color = c("#edd17e", "#7cb5ec"), name = c("Exported (W)", "Imported (W)")) %>%
  hc_xAxis(type = 'datetime') %>%
  hc_navigator(enabled = T) %>%
  hc_rangeSelector(
    buttons = list(
      list(type = 'all', text = 'Total', title = 'Totes les dades'),
      list(type = 'month', count = 1, text = '1m', title = '1 month'),
      list(type = 'week', count = 1, text = '1w', title = '1 week'),
      list(type = 'day', count = 1, text = '1d', title = '1 day'),
      list(type = 'hour', count = 6, text = '6h', title = '6 hours'),
      list(type = 'hour', count = 1, text = '1h', title = '1 hour')
    ),
    selected = 3
  ) %>%
  hc_exporting(enabled = T)

# Column plot
user_demand %>%
  mutate(date = floor_date(datetime, unit = 'week', week_start = 1)) %>%
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
  hc_exporting(enabled = T)


# Indicators energy
user_demand %>%
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


# Query from all users in the metadata file -------------------------------
users_demand <- map_dfr(
  users_metadata$id %>% set_names,
  ~ query_timeseries_data_table_py(
      power_table, 'id', .x, 'timestamp',
      start_date, end_date
    ),
  .id = 'id'
) %>%
  mutate(
    datetime = floor_date(as_datetime(timestamp/1000, tz = config$tzone), '5 minutes'),
    map_dfr(payload, parse_meters_payload)
  ) %>%
  select(datetime, id, exported, imported)



