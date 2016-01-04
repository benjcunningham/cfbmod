library(rvest)
library(stringr)

raw <-
  'http://espn.go.com/college-football/teams' %>%
  html() %>%
  html_nodes('.span-4 .span-2')

confs <- data.frame()

for (i in 1:length(raw)) {

  confs <-
    data.frame(
      DIV =
        raw %>%
        html_nodes('.stathead h4') %>%
        html_text() %>%
        .[i] %>%
        str_extract('(.*)(?= \\()') %>%
        as.factor(),
      CONF =
        raw[i] %>%
        html_nodes('.colhead') %>%
        html_text(),
      stringsAsFactors = FALSE
    ) %>%
    rbind(confs, .)

}

devtools::use_data(confs, overwrite = TRUE)
