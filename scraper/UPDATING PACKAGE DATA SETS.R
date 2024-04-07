require(shlrtools)
require(dplyr)
require(dbplyr)
require(DBI)
require(RSQLite)

## Downloads a local file for the database
dbFile <- tempfile(fileext = ".db")

dbUrl <- ("https://github.com/canadice/shl/blob/main/database/SHL_Database.db?raw=true")

download.file(dbUrl, destfile = dbFile, mode = "wb")

con <-
  dbConnect(
    SQLite(),
    dbFile
  )

##----------------------------------------------------------------
##                Team information and color codes               -
##----------------------------------------------------------------

teamInfo <-
  tbl(con, "teamInfo") %>%
  collect()

usethis::use_data(teamInfo, internal = FALSE, overwrite = TRUE)


##----------------------------------------------------------------
##                        Attribute keys                         -
##----------------------------------------------------------------

attKey <- read.csv2(paste(raw, "csv/attribute_key.csv", sep = ""))

usethis::use_data(attKey, internal = FALSE, overwrite = TRUE)


##---------------------------------------------------------------
##                        Historical Data                       -
##---------------------------------------------------------------
#
# historySkaterSeason <- read.csv2(paste(raw, "csv/history_skaters.csv", sep = ""), sep = ",") %>%
#   mutate(leagueID = leagueID - 1)

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
          newTeamID = teamID,
          abbr
        ),
      by = c("team" = "abbr", "league" = "leagueID")
    ) %>%

    ### Removes duplicated rows where the team abbreviation has been used for older teams
    group_by(name, type) %>%
    filter(
      newTeamID == max(newTeamID)
    ) %>%
    select(
      franchiseID,
      skaterID = fhmID,
      Name = name,
      leagueID = league,
      -team,
      newTeamID,
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

{
# fixIndex <-
#   which(
#     !(historySkaterSeason$PKMinutes %>% str_detect(pattern = ":"))
#   )
#
# historySkaterSeason[fixIndex,"PKMinutes"] <-
#   historySkaterSeason[fixIndex,"PKMinutes"] %>%
#   as.numeric() %>%
#   as.POSIXct(
#     .,
#     origin = "1970-01-01"
#   ) %>%
#   format(
#     "%M:%S"
#   )

  # data.frame(newTeamID = .) %>%
  # left_join(
  #   teamInfo %>%
  #     select(
  #       franchiseID,
  #       leagueID,
  #       teamID,
  #       fhmID
  #     ) %>%
  #     filter(leagueID == 1),
  #   by = c("newTeamID" = "fhmID")
  # ) %>%
  # select(teamID)
#
# historySkaterSeason <-
#   historySkaterSeason %>%
#   mutate(
#     newTeamID =
#       case_when(
#         leagueID == 1 ~ # SMJHL
#           case_when(
#             teamID == 1058 ~ 68, #Carolina Kraken
#             teamID == 15 ~ 56, #Detroit Falcons
#             teamID == 17 ~ 52, #Vancouver Whalers
#             teamID == 18 ~ 65, #Colorado Raptors
#             teamID == 19 ~ 64, #Anchorage Armada
#             teamID == 21 ~ 53, #Kelowna Knights
#             teamID == 22 ~ 63, #Montreal Militia
#             teamID == 23 ~ 67, #Lethbridge Lions
#             teamID == 24 ~ 66, #Anaheim Outlaws
#             teamID == 2058 ~ 69, #Newfoundland Berserkers
#             teamID == 46 & Season > 24 ~ 62, #Prince George Firebirds
#             teamID == 46 & Season < 18 ~ 51, #Prince George Firebirds
#             teamID == 48 ~ 54, #Montreal Impact
#             teamID == 49 ~ 59, #Regina Force
#             teamID == 50 ~ 58, #St. Louis Scarecrows
#             teamID == 51 ~ 60, #Colorado Mammoths
#             teamID == 52 ~ 61, #Halifax Raiders
#             teamID == 57 ~ 61, #Halifax Raiders
#             TRUE ~ newTeamID
#           ),
#         leagueID == 2 ~
#           case_when(
#             teamID == 25 ~ 112, #Sweden
#             teamID == 26 ~ 114, #USA
#             teamID == 28 ~ 107, #Ireland
#             teamID == 29 ~ 110, #Norway
#             teamID == 30 ~ 109, #Latvia
#             teamID == 31 ~ 105, #Germany
#             teamID == 32 ~ 104, #Finland
#             teamID == 33 ~ 103, #Czechia
#             teamID == 34 ~ 102, #Canada
#             teamID == 35 ~ 111, #Russia
#             teamID == 36 ~ 101, #Austria
#             teamID == 53 ~ 108, #Japan
#             teamID == 54 ~ 113, #Switzerland
#             teamID == 56 ~ 106, #Great Britain
#             teamID == 58 ~ 108, #Japan
#             teamID == 59 ~ 103, #Czechia
#             teamID == 60 ~ 106, #Great Britain
#             TRUE ~ newTeamID
#           ),
#         TRUE ~ newTeamID
#       )
#   )
}

# Checks which data is in the history
table(historySkaterSeason$leagueID, historySkaterSeason$Season)

historySkaterSeason <- historyUpdate(leagueId = 0, season = 66)

usethis::use_data(historySkaterSeason, internal = FALSE, overwrite = TRUE)



