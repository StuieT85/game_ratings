sales_chart_global <- function(year) {
  `%>%` <- magrittr::`%>%`
  address = paste0("https://www.vgchartz.com/yearly/", year, "/Global/")

  chart <- rvest::read_html(address) %>%
    rvest::html_elements(".chart") %>%
    rvest::html_table() %>%
    .[[1]]

  chart <- chart[c(1, 2, 8, 9)]
  colnames(chart) <- c("pos", "game", "year_sales", "total_sales")
  chart <- chart[!is.na(chart$pos), ]

  vg_sys <- c("Wii", "WiiU", "NS", "3DS", "DS",
              "PS3", "PS4", "PS5", "PSP", "PSV",
              "X360", "XOne", "XS",
              "PC")

  sys_regex = paste0(
    "(.*) \\((",
    paste0(vg_sys, collapse = "|"),
    ")\\)(.*), (.*)"
  )

  chart %>%
    tidyr::extract(
      game,
      into = c("name", "system", "publisher", "genre"),
      regex = sys_regex
    ) %>%
    dplyr::mutate(
      year_sales = readr::parse_number(year_sales, na = "N/A"),
      total_sales = readr::parse_number(total_sales, na = "N/A"),
      name = stringr::str_replace(name, "PokÃ©mon", "Pokemon")
    )
}
