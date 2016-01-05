get_game <- function(ids) {

  lapply(seq_along(ids), function(i) get_game_(ids[i]))

}

get_game_ <-function(id) {

  espn <-
    paste0('http://espn.go.com/college-football/playbyplay?gameId=', id)

  raw <-
    espn %>%
    xml2::read_html()

  # Begin scraping team info
  raw.teams <-
    raw %>%
    rvest::html_nodes('.team-info')

  teams <-
    list(
      AWAY = team_info_(raw.teams[1]),
      HOME = team_info_(raw.teams[2])
    )

  # Begin scraping play-by-play
  raw.accord <-
    raw %>%
    rvest::html_nodes('.accordion-item')

  raw.play <-
    raw %>%
    rvest::html_nodes('.post-play') %>%
    rvest::html_text() %>%
    stringr::str_replace_all('(^\\s+)|(\\s+$)', '')

  drive.lens <-
    raw.accord %>%
    {lapply(., function(x)
                 x %>%
                 rvest::html_nodes('.post-play') %>%
                 length())} %>%
    unlist()

  qtr <-
    c('End of 1st Quarter', 'End of 2nd Quarter',
      'End of 3rd Quarter', 'End of 4th Quarter') %>%
      {which(raw.play %in% .)}

  raw.head <-
    raw %>%
    rvest::html_nodes('h3') %>%
    rvest::html_text() %>%
    {c(.[1:qtr[2]], .[qtr[2]:length(.)])}

  game <- data.frame(
    POSS =
      raw.accord %>%
      {lapply(seq_along(.), function(i)
                              raw.accord[i] %>%
                              logo_id_() %>%
                              rep(drive.lens[i]))} %>%
      unlist() %>%
      {c(.[1:qtr[2]], .[qtr[2]:length(.)])} %>%
      as.factor(),
    QT =
      1:length(raw.play) %>%
      {ifelse(. <= qtr[1], 1,
       ifelse(. <= qtr[2], 2,
       ifelse(. <= qtr[3], 3, 4)))},
    TIME =
      raw.play %>%
      stringr::str_extract('[0-9]{1,2}:[0-9]{1,2}') %>%
      {replace(., is.na(.), '0:00')},
    DOWN =
      raw.head %>%
      stringr::str_extract('(.*?)(?=st|nd|rd|th)') %>%
      {suppressWarnings(as.numeric(.))},
    TO.GO =
      raw.head %>%
      stringr::str_extract('(?<=and )(.*)(?= at)') %>%
      {suppressWarnings(as.numeric(.))},
    YDL =
      raw.head %>%
      stringr::str_extract('[0-9]+$') %>%
      {suppressWarnings(as.numeric(.))},
    PLAY =
      raw.play %>%
      stringr::str_replace('^\\(.*?\\)\\s*', ''),
    HOME.PTS =
      drive_score(raw.accord, 'away', qtr[2]) %>%
      parse_pts_(),
    AWAY.PTS =
      drive_score(raw.accord, 'home', qtr[2]) %>%
      parse_pts_(),
    stringsAsFactors = FALSE
  )

  list(TEAMS = teams, GAME = game)

}



drive_score <- function(accordion, side, half) {

  accordion %>%
  {lapply(., function(x)
    x %>%
      rvest::html_nodes('.post-play') %>%
      length() %>%
      {c(rep(NA, . - 1), drive_score_(x, side))})} %>%
    unlist() %>%
    {c(.[1:half], .[half:length(.)])}

}

drive_score_ <- function(accordion, side) {

  accordion %>%
    {rvest::html_nodes(., paste0('.', side, ' .team-score'))} %>%
    rvest::html_text() %>%
    as.numeric()

}

logo_id_ <- function(accordion) {

  accordion %>%
    rvest::html_nodes('.team-logo') %>%
    rvest::html_attr('src') %>%
    stringr::str_extract('(?<=ncaa\\/500\\/)(.*)(?=.png)')

}

parse_pts_ <- function(v) {

  v[!is.na(v)] %>%
    {lapply(2:length(.), function(i) .[i] - .[i - 1])} %>%
    unlist() %>%
    c(v[!is.na(v)][1], .) %>%
    replace(v, which(!is.na(v)), .) %>%
    {replace(., . == 0, NA)}

}

team_info_ <- function(side) {

  list(
    ID =
      side %>%
      rvest::html_nodes('.team-name') %>%
      rvest::html_attr('href') %>%
      stringr::str_extract('(?<=\\/id\\/)(.*)$'),
    NAME =
      side %>%
      rvest::html_nodes('.abbrev') %>%
      rvest::html_attr('title'),
    ABBR =
      side %>%
      rvest::html_nodes('.abbrev') %>%
      rvest::html_text()
  )

}
