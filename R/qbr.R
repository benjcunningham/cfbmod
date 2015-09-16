#' Get NCAAF quarterback ratings
#'
#' Extracts a period of NCAAF QBR rankings from ESPN.com and returns it
#' as a data frame.
#' @param x Type of the requested rankings
#' @param qlf Whether rankings should use qualified ratings
#' @param yr Year of the requested rankings
#' @return A data frame of the requested rankings
#' @import magrittr
#' @export
#' @examples
#' # Capture QBR single game rankings for 2013
#' getQBR('player-game', yr = 2013)
getQBR <- function(x = c('player-season', 'player-week', 'player-game',
                         'team-season', 'alltime-season',
                         'alltime-game'),
                   qlf = TRUE,
                   yr = format(Sys.Date(), '%Y')) {

  x <- match.arg(x)

  espn <- 'http://espn.go.com/ncf/qbr/_/type/'

  df <-
    paste0(espn, x, '/qualified/', tolower(qlf), '/year/', yr) %>%
    XML::readHTMLTable(as.data.frame = TRUE,
                       stringsAsFactors = FALSE) %>%
    .[[1]] %>%
    .[.[1] != 'RK', ]

  s <- 4
  if (grepl('season', x)) {
    s <- 3
  }

  for (i in c(1, s:length(df))) {
    df[ , i] <- suppressWarnings(as.numeric(df[ , i]))
  }

  df$RK <- zoo::na.locf(df$RK)

  names(df) <- gsub('[ /]', '.', names(df))

  df

}
