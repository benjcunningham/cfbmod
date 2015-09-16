#' Get NCAAF power rankings
#'
#' Extracts a week of NCAAF team power rankings from ESPN.com and
#' returns it as a data frame.
#' @param wk Week of the requested rankings
#' @return A data frame of the requested leaderboard
#' @import magrittr
#' @export
#' @examples
#' # Capture ESPN power rankings for Week 3
#' getPowerRankings(3)
getPowerRankings <- function(wk) {

  espn <- 'http://espn.go.com/college-football/powerrankings/_/week/'

  df <-
    paste0(espn, wk) %>%
    XML::readHTMLTable(as.data.frame = TRUE,
                       stringsAsFactors = FALSE) %>%
    .[[1]] %>%
    magrittr::set_names(value = .[1, ])

  df[ , 1] <- suppressWarnings(as.numeric(df[ , 1]))

  df <- df[!is.na(df[1]), ]

  if ( 'TRENDING' %in% colnames(df) ) {

    suppressWarnings(

      df$TRENDING <-
        df$TRENDING %>%
        strsplit(split = 'Last Week: ') %>%
        sapply('[[', 2) %>%
        as.numeric()

    )

  } else {
    df$TRENDING <- NA
  }

  names(df) <- gsub('[ /]+', '.', names(df))

  df$TEAM <-
    df$TEAM.RECORD %>%
    gregexpr('[0-9]([^;]*)[0-9]', .) %>%
    regmatches(df$TEAM.RECORD, ., invert = TRUE) %>%
    sapply('[[', 1)

  rec <-
    df$TEAM.RECORD %>%
    gregexpr('[0-9]([^;]*)[0-9]', .) %>%
    regmatches(df$TEAM.RECORD, .) %>%
    unlist() %>%
    strsplit('-')

  df$WIN  <- sapply(rec, '[[', 1)
  df$LOSS <- sapply(rec, '[[', 2)

  names(df) <- plyr::mapvalues(names(df),
                               from = c('TRENDING'),
                               to = c('LAST.WEEK'))

  ord <- c('RANK', 'TEAM', 'WIN', 'LOSS', 'LAST.WEEK', 'PTS', 'COMMENT')

  df[ , ord]

}
