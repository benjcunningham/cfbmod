#' Retrieve the play-by-play for a given game
#'
#' Scrapes the play-by-play for a given football game.
#'
#' @param ids Vector of \code{espn.go.com} game identifiers, as either
#'   characters or numerics.
#' @return A list of team information and play-by-play data presented in
#'   a dataframe. If \code{`teams`} is a vector, a list of lists is
#'   returned.
#' @import magrittr
#' @export
#' @examples
#' \dontrun{
#' get_game(c(400852682, 400852737))
#' }
get_game <- function(ids) {

  lapply(seq_along(ids), function(i) get_game_(ids[i]))

}

#' Non-vectorized play-by-play scraper
#'
#' @return A list of team information and play-by-play presented in a
#'   dataframe.
#' @rdname get_game
#' @keywords internal
get_game_ <-function(id) {

  espn <-
    paste0('http://espn.go.com/college-football/playbyplay?gameId=', id)

  raw <-
    espn %>%
    xml2::read_html()

  raw.teams <-
    raw %>%
    rvest::html_nodes('.team-info')

  teams <-
    list(
      AWAY = team_info_(raw.teams[1]),
      HOME = team_info_(raw.teams[2])
    )

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

#' Retrieve cumulative scores at end of each drive for a team
#'
#' @return A vector where elements represent one team's cumulative
#'   number of points at the end of each drive and all other elements
#'   are \code{NA}.
#' @keywords internal
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

#' Retrieve cumulative score at end of drive
#'
#' @param accordion HTML for a team drive "accordion" on
#'   \code{espn.go.com}.
#' @param side Team to scrape; either `home` or `away`.
#' @return A vector the length of the number of plays in a drive, where
#'   the last element is one team's cumulative number of points at the
#'   end of the drive and all other elements are \code{NA}.
#' @rdname drive_score
#' @keywords internal
drive_score_ <- function(accordion, side) {

  accordion %>%
    {rvest::html_nodes(., paste0('.', side, ' .team-score'))} %>%
    rvest::html_text() %>%
    as.numeric()

}

#' Retrieve the offensive team identifier for a drive
#'
#' @param accordion HTML for a team drive "accordion" on
#'   \code{espn.go.com}.
#' @return A team identifier as a character
#' @keywords internal
logo_id_ <- function(accordion) {

  accordion %>%
    rvest::html_nodes('.team-logo') %>%
    rvest::html_attr('src') %>%
    stringr::str_extract('(?<=ncaa\\/500\\/)(.*)(?=.png)')

}

#' Adjust a vector of cumulative points to points scored per drive
#'
#' @param v Vector of cumulative points.
#' @return A vector of points as numerics
#' @keywords internal
parse_pts_ <- function(v) {

  v[!is.na(v)] %>%
    {lapply(2:length(.), function(i) .[i] - .[i - 1])} %>%
    unlist() %>%
    c(v[!is.na(v)][1], .) %>%
    replace(v, which(!is.na(v)), .) %>%
    {replace(., . == 0, NA)}

}

#' Retrieve high-level information for a team at game time
#'
#' @param side Team to scrape; either `home` or `away`.
#' @return A list of team information
#' @keywords internal
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
