#' Get season schedule
#'
#' Extracts an NCAAF schedule and overview results for a team.
#' @param team Team for which to extract schedule
#' @param yr Year of the requested schedule
#' @return A data frame of the requested schedule
#' @import magrittr
#' @export
#' @examples
#' getSchedule('iowa-hawkeyes', 2013)
getSchedule <- function(team, yr = format(Sys.Date(), '%Y')) {

  espn <- 'http://espn.go.com/college-football/team/fpi/_/id/'

  df <-
    paste0(espn, getID(team), '/year/', yr) %>%
    XML::readHTMLTable(as.data.frame = TRUE,
                       stringsAsFactors = FALSE) %>%
    .[[5]] %>%
    magrittr::set_names(value = .[1, ]) %>%
    .[2:nrow(.), ]

  names(df) <- gsub('[/ ()]+', '.', names(df))

  df <- df[!is.na(df[2]), ]

  df$OPP <- gsub('(vs |@ )|[#0-9*]+[ ]*', '', df$OPPONENT)
  df$OPP.RK <-
    stringr::str_extract(df$OPPONENT, '#[0-9]*') %>%
    gsub('#', '', .)
  df$VENUE <- ifelse(grepl('@ ', df$OPPONENT), 'AWAY', 'HOME')
  df$RESULT <- stringr::str_extract(df$RESULT.PROJ, 'W|L')

  df$PTS.FOR <-
    ifelse(df$RESULT == 'W',
           stringr::str_extract_all(df$RESULT.PROJ, '[0-9]+') %>%
           rvest::pluck(1),
           stringr::str_extract_all(df$RESULT.PROJ, '[0-9]+') %>%
           rvest::pluck(2))

  df$PTS.OPP <-
    ifelse(df$RESULT == 'L',
           stringr::str_extract_all(df$RESULT.PROJ, '[0-9]+') %>%
             rvest::pluck(1),
           stringr::str_extract_all(df$RESULT.PROJ, '[0-9]+') %>%
             rvest::pluck(2))

  df$OPP.FPI <- stringr::str_extract(df$OPP.FPI.RK., '(-*[0-9.])*')

  df$DATE <-
    paste(df$DATE, ifelse(grepl('Jan|Feb', df$DATE), yr + 1, yr)) %>%
    stringr::str_replace('Sept', 'Sep') %>%
    as.Date('%a, %b %d %Y')

  ord <- c('DATE', 'VENUE', 'OPP', 'OPP.RK', 'RESULT', 'PTS.FOR',
           'PTS.OPP', 'OPP.FPI', 'GAME.SCORE')

  df <- df[ , ord]

  for (i in c(4, 6:length(df))) {
    df[ , i] <- suppressWarnings(as.numeric(df[ , i]))
  }

  df

}

#' Scrape ID of NCAAF team
#'
#' @return Numeric ID of requested team
#' @keywords internal
getID <- function(team) {

  espn <- 'http://espn.go.com/college-football/team/fpi/_/id/2'

  fbs <-
    rvest::html(espn) %>%
    rvest::html_nodes('.select-box option') %>%
    rvest::html_attrs() %>%
    rvest::pluck(1) %>%
    sapply('[[', 1) %>%
    strsplit("/") %>%
    .[3:length(.)]

  if ( !(team %in% sapply(fbs, '[[', 10)) ) {
    stop(paste0('Cannot find "', team, '" in list of FBS schools.'),
         call. = FALSE)
  }

  sapply(fbs, '[[', 9)[sapply(fbs, '[[', 10) == team] %>%
    as.numeric()

}
