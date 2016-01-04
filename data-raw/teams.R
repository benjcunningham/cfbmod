library(rvest)
library(stringr)

raw <-
  'http://espn.go.com/college-football/teams' %>%
  html() %>%
  html_nodes('.mod-container .mod-open-list')

teams <- data.frame()

for (i in 1:length(raw)) {

  teams <-
    data.frame(
      CONF =
        raw %>%
        html_nodes('.colhead') %>%
        html_text() %>%
        .[i] %>%
        as.factor(),
      TEAM =
        raw[i] %>%
        html_nodes('.bi') %>%
        html_text(),
      ESPN.ID =
        raw[i] %>%
        html_nodes('.bi') %>%
        html_attr('href') %>%
        str_extract('(?<=team\\/_\\/id\\/)(.*)(?=\\/)') %>%
        as.integer(),
      stringsAsFactors = FALSE
    ) %>%
    rbind(teams, .)

}

devtools::use_data(teams, overwrite = TRUE)
