##---------------------------------------------------------------
##          Function that reads from API based on url           -
##---------------------------------------------------------------
readAPI <- function(url, ...){
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
}

##---------------------------------------------------------------
##        Function that loads player ratings (attributes)       -
##---------------------------------------------------------------
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
      .data$name,
      .data$team,
      .data$screening:.data$professionalism
    ) %>%
    ## Creates a attribute column and value column for each player
    tidyr::pivot_longer(
      cols = .data$screening:.data$professionalism,
      names_to = "attribute"
    ) %>%
    ## Adds the TPE cost for each respective attribute value
    dplyr::left_join(
      tpeCost,
      by = c("value" = "Skill.level")
    ) %>%
    ## Groups by name for summarizing
    dplyr::group_by(
      .data$name,
      .data$team
    ) %>%
    ## Summarizes the used TPE based on attribute value
    dplyr::summarize(
      ## Removes the fixed attributes to 15 and compensates for 11 starting Stamina (OLD SCALE)
      ## Removes the fixed attributes to 15 and compensates for 12 starting Stamina (NEW SCALE after season 60)
      usedTPE = dplyr::if_else(newScale, sum(.data$newTPE) - 42*5 - 13, sum(.data$TPE) - 62*5 - 16)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$name, .data$usedTPE)

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
      .data$name,
      .data$team,
      .data$blocker:.data$professionalism
    ) %>%
    ## Creates a attribute column and value column for each player
    tidyr::pivot_longer(
      cols = .data$blocker:.data$professionalism,
      names_to = "attribute"
    ) %>%
    ## Adds the TPE cost for each respective attribute value
    dplyr::left_join(
      tpeCost,
      by = c("value" = "Skill.level")
    ) %>%
    ## Groups by name for summarizing
    dplyr::group_by(
      .data$name,
      .data$team
    ) %>%
    ## Summarizes the used TPE based on attribute value
    dplyr::summarize(
      ## Removes the fixed attributes to 15 and compensates for 8 Aggression
      usedTPE = sum(.data$TPE) - 62*3 - 4
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$name, .data$usedTPE)

  goalies <-
    goalies %>%
    dplyr::left_join(
      usedTPE,
      by = c("name")
    ) %>%
    dplyr::rename(
      goaliePassing = .data$passing,
      goaliePuckhandling = .data$puckhandling,
      goaliePositioning = .data$positioning
    )

  ## Return a list of the loaded data
  list(
    players = players,
    goalies = goalies
  ) %>%
    return()
}

##---------------------------------------------------------------
##            Function that loads player statistics             -
##---------------------------------------------------------------
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
            .x/.data$gamesPlayed,
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

##---------------------------------------------------------------
##      Function that loads team statistics and information     -
##---------------------------------------------------------------
teamLoader <-  function(leagueID, season = NULL){
  teams <-
    readAPI(
      url = "https://index.simulationhockey.com/api/v1/teams",
      query = list(league = leagueID, season = season)
    )

  return(teams)
}

##----------------------------------------------------------------
##                  Function that loads schedule                 -
##----------------------------------------------------------------
gamesLoader <- function(leagueID, season = NULL, type = NULL){
  schedule <-
    readAPI(
      url = "https://index.simulationhockey.com/api/v1/schedule",
      query = list(league = leagueID, season = season, type = type)
    )

  return(schedule)
}

playoffLoader <- function(leagueID, season = NULL){
  schedule <-
    readAPI(
      url = "https://index.simulationhockey.com/api/v1/standings/playoffs",
      query = list(league = leagueID, season = season)
    )

  return(schedule)
}

##---------------------------------------------------------------
##                Function that loads standings                 -
##---------------------------------------------------------------
standingsLoader <- function(leagueID, season = NULL){
  standings <-
    readAPI(
      url = "https://index.simulationhockey.com/api/v1/standings",
      query = list(league = leagueID, season = season)
    )

  return(standings)
}
