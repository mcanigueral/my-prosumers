

# Smart meters parsing -----------------------------------------------

parse_meters_payload <- function(payload) {
  if ('imported' %in% names(payload)) {
    tibble(
      imported = as.numeric(payload$imported),
      exported = as.numeric(payload$exported)
    )
  } else {
    tibble(
      imported = (payload[stringr::str_which(names(payload), 'to client')] %>% map_dbl(payload_str_to_num) %>% sum),
      exported = (payload[stringr::str_which(names(payload), 'by client')] %>% map_dbl(payload_str_to_num) %>% sum),
    )
  }
}


# From historic log to instant value --------------------------------------
# df must have columns "imported" and "exported"
convert_historic_to_instant_power <- function(df) {
  df %>%
    rename(Imported = imported, Exported = exported) %>%
    mutate(
      Imported = Imported - lag(Imported),
      Exported = Exported - lag(Exported)
    ) %>%
    drop_na() %>%
    mutate(
      Imported = Imported*1000*60/5, # E=P*t=P*5min/60min -> P=E*60/5
      Exported = Exported*1000*60/5
    ) %>%
    mutate_if(is.numeric, round, 2) %>%
    filter(
      between(Imported, 0, 100000),
      between(Exported, 0, 100000)
    ) %>%
    distinct() %>%
    ungroup()
}


