#' Calls the an SHL API
#'
#' @param url The url to the SHL API either the Index or the Portal
#' @param ... Additional queries for the API call.
#' @return A data frame of the data.
#' @export
#' @examples
#' readAPI(
#'    url = "https://index.simulationhockey.com/api/v1/players/ratings",
#'    query = list(league = 0, season = 63)
#' )

readAPI <- function(url, ...){
  tryCatch({
    temp <-
      url %>%
      # Gets the API information, the ... allows for specific queries with query = list()
      httr::GET(...)

    temp$content %>%
      # Extracts the data
      rawToChar() %>%
      # Converts it from JSON to a data frame
      jsonlite::fromJSON() %>%
      return()
  },
  error = function(e){
    simpleError(message = paste0(url, e, collapse = "\n"))
  })
}

#' Loading the player attributes
#'
#' @param leagueID ID of the league to get attributes from, SHL = 0, SMJHL = 1, IIHF = 2, WJC = 3
#' @param season The season to get the attributes from
#'
#' @returns
#' Returns a list of two data.frames, one for players and one for goalies
#'
#' @export
#' @examples
#' # Loads the SHL player attributes from S64
#' playerLoader(leagueID = 0, season = 64)

playerLoader <- function(leagueID, season = NULL){
  players <-
    readAPI(
      url = "https://index.simulationhockey.com/api/v1/players/ratings",
      query = list(league = leagueID, season = season)
    )
  goalies <-
    readAPI(
      url = "https://index.simulationhockey.com/api/v1/goalies/ratings",
      query = list(league = leagueID, season = season)
    )

  ## Checks if season implies scale change
  ## If is.null() is TRUE, it's a season with the new scale
  newScale <- season %>% is.null()
  if(is.numeric(season)){
    newScale <- season >= 60
  }

  ## Calculates the used TPE for each player based on their attribute values
  usedTPE <-
    ## Starts with all players
    players %>%
    ## dplyr::selects only names (for grouping) and attributes
    dplyr::select(
      name,
      team,
      screening:professionalism
    ) %>%
    ## Creates a attribute column and value column for each player
    tidyr::pivot_longer(
      cols = screening:professionalism,
      names_to = "attribute"
    ) %>%
    ## Adds the TPE cost for each respective attribute value
    dplyr::left_join(
      tpeCost,
      by = c("value" = "Skill.level")
    ) %>%
    ## Groups by name for summarizing
    dplyr::group_by(
      name,
      team
    ) %>%
    ## Summarizes the used TPE based on attribute value
    dplyr::summarize(
      ## Removes the fixed attributes to 15 and compensates for 11 starting Stamina (OLD SCALE)
      ## Removes the fixed attributes to 15 and compensates for 12 starting Stamina (NEW SCALE after season 60)
      usedTPE = dplyr::if_else(newScale, sum(newTPE) - 42*5 - 13, sum(TPE) - 62*5 - 16)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(name, usedTPE)

  players <-
    players %>%
    dplyr::left_join(
      usedTPE,
      by = c("name")
    )

  ## Calculates the used TPE for each player based on their attribute values
  usedTPE <-
    ## Starts with all goalies
    goalies %>%
    ## dplyr::selects only names (for grouping) and attributes
    dplyr::select(
      name,
      team,
      blocker:professionalism
    ) %>%
    ## Creates a attribute column and value column for each player
    tidyr::pivot_longer(
      cols = blocker:professionalism,
      names_to = "attribute"
    ) %>%
    ## Adds the TPE cost for each respective attribute value
    dplyr::left_join(
      tpeCost,
      by = c("value" = "Skill.level")
    ) %>%
    ## Groups by name for summarizing
    dplyr::group_by(
      name,
      team
    ) %>%
    ## Summarizes the used TPE based on attribute value
    dplyr::summarize(
      ## Removes the fixed attributes to 15 and compensates for 8 Aggression
      usedTPE = sum(TPE) - 62*3 - 4
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(name, usedTPE)

  goalies <-
    goalies %>%
    dplyr::left_join(
      usedTPE,
      by = c("name")
    ) %>%
    dplyr::rename(
      goaliePassing = passing,
      goaliePuckhandling = puckhandling,
      goaliePositioning = positioning
    )

  ## Return a list of the loaded data
  list(
    players = players,
    goalies = goalies
  ) %>%
    return()
}

#' Loading the player statistics
#'
#' @param leagueID ID of the league to get attributes from, SHL = 0, SMJHL = 1, IIHF = 2, WJC = 3
#' @param season The season to get the attributes from
#' @param type Selects whether pre-season, regular season, or post-season
#'
#' @returns
#' List of two data.frames, one consisting of players and one consisting of goalies
#'
#' @export
#' @examples
#' # Loads the SHL player statistics from S64
#' indStatsLoader(leagueID = 0, season = 64, type = "regular")

indStatsLoader <- function(leagueID, season = NULL, type = NULL){
  players <-
    readAPI(
      url = "https://index.simulationhockey.com/api/v1/players/stats",
      query = list(league = leagueID, season = season, type = type)
    )
  goalies <-
    readAPI(
      url = "https://index.simulationhockey.com/api/v1/goalies/stats",
      query = list(league = leagueID, season = season, type = type)
    )

  players <-
    players %>%
    dplyr::mutate(
      ## Using the format with glue grammar that allows for dynamic variable names
      dplyr::across(
        dplyr::contains("TimeOnIce"),
        ~ format(
          as.POSIXct(
            .x/gamesPlayed,
            origin = "1970-01-01"
          ),
          "%M:%S"
        )
      )
    )

  ## Return a list of the loaded data
  list(
    players = players,
    goalies = goalies
  ) %>%
    return()
}

#' Loads team statistics and information from the Index
#'
#' @param leagueID ID of the league to get attributes from, SHL = 0, SMJHL = 1, IIHF = 2, WJC = 3
#' @param season The season to get the attributes from
#'
#' @returns
#' Returns a data.frame with the information
#'
#' @export
#' @examples
#' # Loads the SHL team information from season 64
#' teamLoader(leagueID = 0, season = 64)

teamLoader <-  function(leagueID, season = NULL){
  teams <-
    readAPI(
      url = "https://index.simulationhockey.com/api/v1/teams",
      query = list(league = leagueID, season = season)
    )

  return(teams)
}

#' Loads schedule from the Index
#'
#' @param leagueID ID of the league to get attributes from, SHL = 0, SMJHL = 1, IIHF = 2, WJC = 3
#' @param season The season to get the attributes from
#' @param type Selects whether pre-season, regular season, or post-season
#'
#' @returns
#' Returns a data.frame with the scheduling
#'
#' @export
#' @examples
#' # Loads the SHL schedule from season 64
#' gamesLoader(leagueID = 0, season = 64, type = "regular")
#'
gamesLoader <- function(leagueID, season = NULL, type = "regular"){
  if(type == "playoffs"){
    schedule <-
      readAPI(
        url = "https://index.simulationhockey.com/api/v1/standings/playoffs",
        query = list(league = leagueID, season = season)
      )
  } else {
    schedule <-
      readAPI(
        url = "https://index.simulationhockey.com/api/v1/schedule",
        query = list(league = leagueID, season = season, type = type)
      )
  }

  return(schedule)
}

#' Loads team standings from the Index
#'
#' @param leagueID ID of the league to get attributes from, SHL = 0, SMJHL = 1, IIHF = 2, WJC = 3
#' @param season The season to get the attributes from
#'
#' @returns
#' Returns a data.frame with the information
#'
#' @export
#' @examples
#' # Loads the SHL team information from season 64
#' standingsLoader(leagueID = 0, season = 64)

standingsLoader <- function(leagueID, season = NULL){
  standings <-
    readAPI(
      url = "https://index.simulationhockey.com/api/v1/standings",
      query = list(league = leagueID, season = season)
    )

  return(standings)
}


#' Loads player data from the SHL Portal
#'
#' @returns
#' Returns a data.frame with the information
#'
#' @export

portalPlayers <- function(){

  url <- "https://portal.simulationhockey.com/api/v1/player"

  seasonCutoff <-
    "https://portal.simulationhockey.com/api/v1/season" %>%
    readAPI() %>%
    .$startDate

  data <- readAPI(url) %>%
    mutate(
      USERLINK = paste("https://simulationhockey.com/member.php?action=profile&uid=", uid, sep = ""),
      LINK = paste("https://portal.simulationhockey.com/player/", pid, sep = ""),
      CLASS = paste("S", draftSeason, sep = ""),
      `SHL TEAM` =
        case_when(
          currentLeague == "SHL" ~ currentTeamID,
          TRUE ~ shlRightsTeamID
        ),

    ) %>%
    ## Adding filtering to account for players that are retired
    filter(!(retirementDate < seasonCutoff) | status == "active") %>%
    left_join(
      teamInfo %>%
        filter(
          !(is.na(fhmID))
        ),
      by = c("currentLeague" = "league", "currentTeamID" = "fhmID")
    ) %>%
    return()
}


#' Reads and summarizes player earnings in a season from the SHL Portal
#'
#' @returns A data frame of earnings from the past seasons
#'
#' @export
#'
tpeEarnings <- function(uid, pid) {
  url <- "https://portal.simulationhockey.com/api/v1/tpeevents"

  result <-
    readAPI(
      url = url,
      query = list(pid = pid)
    ) %>%
    filter(submittedByID == uid) %>%
    mutate(
      activeStatus =
      if_else(
        any(
          submissionDate %>% lubridate::as_datetime() > (lubridate::now() - lubridate::days(21))
        ),
        1,
        0
      )
    ) %>%
      select(pid, activeStatus) %>%
      unique()

  if(nrow(result) == 0){
    return(data.frame(pid = pid, activeStatus = 0))
  } else {
    return(result)
  }



}


