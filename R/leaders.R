#' Get NCAAF leaderboards
#'
#' Extracts NCAAF team statistics leaderboard from ESPN.com and returns
#' it as a data frame.
#' @param x Statistics group of the requested rankings
#' @param pos Position of the requested rankings
#' @param yr Year of the requested rankings
#' @return A data frame of the requested leaderboard
#' @import magrittr
#' @export
#' @examples
#' # Capture team rushing offense stats for 2013
#' df <- getLeaders('rushing', yr = 2013)
getLeaders <- function(x = c('total', 'downs', 'passing', 'rushing',
                             'receiving', 'returning', 'kicking',
                             'punting', 'defense'),
                       pos = c('offense', 'defense'),
                       yr = format(Sys.Date(), '%Y')) {

  x <- match.arg(x)
  pos <- match.arg(pos)

  espn <- 'http://espn.go.com/college-football/statistics/team/_/stat/'

  df <-
    paste0(espn, x, '/position/', pos,'/year/', yr) %>%
    XML::readHTMLTable(as.data.frame = TRUE,
                       stringsAsFactors = FALSE) %>%
    .[[1]] %>%
    magrittr::set_names( value = .[.[1] == 'RK', ][1, ] ) %>%
    .[.[1] != 'RK', ]

  for (i in c(1, 3:length(df))) {
    df[ , i] <- suppressWarnings(as.numeric(df[ , i]))
  }

  df <- df[!is.na(df[3]), ]

  df$RK <- zoo::na.locf(df$RK)

  names(df) <- gsub('[ /]', '.', names(df))

  df

}
