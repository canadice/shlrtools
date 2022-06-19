raw <- "https://raw.githubusercontent.com/canadice/shl/main/"

require(shlrtools)
require(dplyr)

##----------------------------------------------------------------
##                Team information and color codes               -
##----------------------------------------------------------------

teamInfo <-
  read.csv2(
    paste(
    raw,
    "csv/team_information.csv",
    sep = "")
  ) %>%
  mutate(
    abbr =
      case_when(
        abbr == "CZE" ~ "CZH",
        abbr == "JAP" ~ "JPN",
        abbr == "LAT" ~ "LTV",
        abbr == "NOLA" ~ "NOL",
        TRUE ~ abbr
      )
  )

usethis::use_data(teamInfo, internal = FALSE, overwrite = TRUE)


##----------------------------------------------------------------
##                        Attribute keys                         -
##----------------------------------------------------------------

attKey <- read.csv2(paste(raw, "csv/attribute_key.csv", sep = ""))

usethis::use_data(attKey, internal = FALSE, overwrite = TRUE)


##---------------------------------------------------------------
##                        Historical Data                       -
##---------------------------------------------------------------

historySkaterSeason <- read.csv2(paste(raw, "csv/history_skaters.csv", sep = ""), sep = ",") %>%
  mutate(leagueID = leagueID - 1)

# leagueId <- 1
# season <- 53

historyUpdate <- function(leagueId, season){
  load("data/historySkaterSeason.rda")

  if(historySkaterSeason %>% filter(leagueID == leagueId & Season == season) %>% nrow() != 0){
    stop("The current season for the league is already present in the data")
  }

  data <-
    indStatsLoader(leagueId, season, type = "regular") %>%
    lapply(
      X = .,
      FUN = function(x){
        do.call(what = data.frame, args = x) %>%
          ## Adds indication that stats are regular season
          mutate(
            type = 0
          ) %>%
          relocate(
            type,
            .after = season
          )
      }
    ) %>%
    append(
      indStatsLoader(leagueId, season, type = "playoffs") %>%
        lapply(
          X = .,
          FUN = function(x){
            do.call(what = data.frame, args = x) %>%
              ## Adds indication that stats are playoffs
              mutate(
                type = 1
              ) %>%
              relocate(
                type,
                .after = season
              )
          }
        )
    ) %>%
    do.call(what = plyr::rbind.fill, args = .) %>%
    # mutate(
    #   id = id
    # ) %>%
    rename(
      fhmID = id
    ) %>%
    left_join(
      teamInfo %>%
        select(
          franchiseID,
          leagueID,
          teamID = fhmID,
          abbr
        ) %>%
        mutate(
          teamID = if_else(abbr == "ANA", 5 %>% as.integer(), teamID)
        ) %>%
        filter(
          !is.na(teamID)
        ),
      by = c("team" = "abbr", "league" = "leagueID")
    ) %>%
    select(
      franchiseID,
      skaterID = fhmID,
      Name = name,
      leagueID = league,
      -team,
      teamID,
      Season = season,
      isPlayoffs = type,
      GamesPlayed = gamesPlayed,
      Goals = goals,
      Assists = assists,
      Points = points,
      PlusMinus = plusMinus,
      PenaltyMinutes = pim,
      Hits = hits,
      Shots = shotsOnGoal,
      ShotsBlocked = shotsBlocked,
      MinutesPlayed = timeOnIce,
      PPGoals = ppGoals,
      PPAssists = ppAssists,
      PPPoints = ppPoints,
      PPMinutes = ppTimeOnIce,
      PKGoals = shGoals,
      PKAssists = shAssists,
      PKPoints = shPoints,
      PKMinutes = shTimeOnIce,
      FightsWon = fightWins,
      FightsLost = fightLosses,
      Giveaways = giveaways,
      Takeaways = takeaways,
      GR = gameRating,
      OGR = offensiveGameRating,
      DGR = defensiveGameRating,
      contains("advancedStats"),
      -(minutes:savePct)
    ) %>%
    filter(
      !is.na(Goals)
    ) %>%
    rename_with(
      stringr::str_remove,
      contains("advancedStats"),
      pattern = "advancedStats."
    )

  historySkaterSeason <-
    historySkaterSeason %>%
    plyr::rbind.fill(
      data
    )
}

# fixIndex <-
#   which(
#     historySkaterSeason$leagueID == 2 &
#     historySkaterSeason$teamID %>% is.na()
#   )
#
# historySkaterSeason[fixIndex, c("teamID", "franchiseID")] <- 101

historySkaterSeason <- historyUpdate(leagueId = 2, season = 64)

usethis::use_data(historySkaterSeason, internal = FALSE, overwrite = TRUE)



