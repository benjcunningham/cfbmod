#' Retrieve schedule for a given team and season
#'
#' Scrapes a football schedule and weekly overview for a given team and
#' season.
#'
#' @param teams Vector of teams for which to extract schedules. Elements
#'   should match team names available in \code{cfbteams}.
#' @param yrs Vector of years for which to extract schedules. Elements
#'   should directly correspond with those passed to \code{`teams`}.
#' @return A list of data frames of the requested schedules.
#' @import magrittr
#' @export
#' @examples
#' \dontrun{
#' get_schedule(c('Iowa', 'Stanford'), rep(2015, 2))
#' get_schedule(rep('Alabama', 3), 2010:2012)
#' }
get_schedule <- function(teams, yrs) {

  if (length(teams) != length(yrs)) {
    stop('`teams` and `yrs` must be the same length.', call. = FALSE)
  }

  ids <- cfbteams$ESPN.ID[match(teams, cfbteams$TEAM)]

  if (any(is.na(ids))) {
    teams[which(is.na(ids))] %>%
      paste(collapse = '`\t`') %>%
      paste0('Identifiers for the following teams cannot be found:\n',
             '`', ., '`') %>%
    stop(call. = FALSE)
  }

  lapply(seq_along(teams), function(i) get_schedule_(ids[i], yrs[i]))

}

#' Non-vectorized schedule scraper
#'
#' @return A data frame of the requested schedule
#' @keywords internal
get_schedule_ <- function(id, yr) {

  espn <- paste0('http://espn.go.com/college-football/team/fpi/_/id/',
                 id, '/year/', yr)

  schedule <-
    espn %>%
    XML::readHTMLTable(as.data.frame = TRUE,
                       stringsAsFactors = FALSE) %>%
    .[[5]] %>%
    magrittr::set_names(value = .[1, ]) %>%
    .[2:nrow(.), ] %>%
    .[!is.na(.[2]), ]

  names(schedule) <- gsub('[/ ()]+', '.', names(schedule))

  schedule$OPP <-
    schedule$OPPONENT %>%
    stringr::str_replace_all('(vs |@ )|[#0-9*]+[ ]*', '')
  schedule$OPP.RK <-
    schedule$OPPONENT %>%
    stringr::str_extract('#[0-9]*') %>%
    stringr::str_replace('#', '') %>%
    as.numeric()
  schedule$VENUE <-
    schedule$OPPONENT %>%
    {ifelse(grepl('\\*', .), 'NEUTRAL',
     ifelse(grepl('@ ', .), 'AWAY', 'HOME'))} %>%
    as.factor()
  schedule$RESULT <-
    schedule$RESULT.PROJ %>%
    stringr::str_extract('W|L') %>%
    as.factor()
  schedule$PTS.FOR <-
    schedule$RESULT.PROJ %>%
    stringr::str_extract_all('[0-9]+') %>%
    {ifelse(schedule$RESULT == 'W',
            rvest::pluck(., 1),
            rvest::pluck(., 2))} %>%
    as.numeric()
  schedule$PTS.OPP <-
    schedule$RESULT.PROJ %>%
    stringr::str_extract_all('[0-9]+') %>%
    {ifelse(schedule$RESULT == 'L',
            rvest::pluck(., 1),
            rvest::pluck(., 2))} %>%
    as.numeric()
  schedule$OPP.FPI <-
    schedule$OPP.FPI.RK. %>%
    stringr::str_extract('(-*[0-9.])*') %>%
    as.numeric()
  schedule$DATE <-
    grepl('Jan|Feb', schedule$DATE) %>%
    {ifelse(., yr + 1, yr)} %>%
    paste(schedule$DATE, .) %>%
    stringr::str_replace('Sept', 'Sep') %>%
    as.Date('%a, %b %d %Y')
  schedule$GAME.ID <-
    espn %>%
    rvest::html() %>%
    rvest::html_nodes('.score a') %>%
    rvest::html_attr('href') %>%
    stringr::str_extract('(?<=id=)(.*)') %>%
    {c(., rep(NA, nrow(schedule) - length(.)))}
  schedule$GAME.SCORE <- as.numeric(schedule$GAME.SCORE)

  schedule <-
    c('DATE', 'VENUE', 'OPP', 'OPP.RK', 'RESULT', 'PTS.FOR', 'PTS.OPP',
      'OPP.FPI', 'GAME.SCORE', 'GAME.ID') %>%
    {schedule[ , .]}

  schedule

}
