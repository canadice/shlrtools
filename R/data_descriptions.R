#' Table of the TPE costs
#'
#' A data set containing the costs for the different attributes values
#' using the old (pre-S61) TPE scale and the new.
#'
#' @format A data frame with 17 observations and 5 variables
#' \describe{
#'    \item{Skill.level}{The value of the attribute, 4-20}
#'    \item{TPE}{The TPE cost of having the attribute at that level, using the old scale}
#'    \item{newTPE}{The TPE cost of having the attribute at that level, using the new scale}
#'    \item{oldTPECost}{Cost of increasing value to this level, old scale}
#'    \item{newTPECost}{Cost of increasing value to this level, new scale}
#' }
"tpeCost"

#' Table of all team information for the SHL leagues
#'
#' A data set containing information about the different teams in the 4 leagues of the SHL
#'
#' @format A data frame with 90+ observations and 12 variables
#' \describe{
#'    \item{franchiseID}{ID value of the franchise}
#'    \item{teamID}{ID value of the team, based on its creation in their respective league}
#'    \item{fhmID}{The ID value that FHM uses for the team}
#'    \item{team}{Team name}
#'    \item{Inaugural.Season}{First season in their respective league}
#'    \item{leagueID}{ID value of the league the team is playing in}
#'    \item{league}{Name of the league the team is playing in}
#'    \item{abbr}{Short abbreviation for the team}
#'    \item{primary}{Primary color of the team}
#'    \item{secondary}{Secondary color of the team}
#'    \item{alt1}{Alternative color of the team}
#'    \item{alt2}{Alternative color of the team}
#' }
"teamInfo"

#' Table of attribute names
#'
#' A table containing the abbreviations, FHM, and normal attribute names.
#'
#' @format A data frame with 41 observations and 3 variables
#' \describe{
#'    \item{abb}{Three letter abbreviation of the attribute names}
#'    \item{attributeIndex}{Name of attribute from the Index (exported from FHM)}
#'    \item{attribute}{Normal name of the attribute}
#' }
"attKey"

#' Forum Scrape Data
#'
#' An .RData containing the result of a daily forum scrape. It includes information of all players.
#'
#' @format A data frame with a number of observations and 41 variables
"forumData"

#' History Skater Data
#'
#' An .RData containing the historical data of all skaters from all four leagues (WJC is incomplete)
#'
#' @format A data frame with a number of observations and variables
"historySkaterSeason"
