require(shlrtools)
require(dplyr)

### Loading data sets
## Current forum scrape data from Google Sheets that is automatically written every day
googlesheets4::gs4_deauth()

iihfTransfer <-
  googlesheets4::read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1shZphSBULx7G8hYDtoUqTW6gy445_DDA6NIHqFrayLs/edit#gid=0",
    sheet = "Transfers"
  )

draftedProspects <-
  c(
    #ATL
    "https://simulationhockey.com/forumdisplay.php?fid=706",
    #BAP
    "https://simulationhockey.com/forumdisplay.php?fid=601",
    #BUF
    "https://simulationhockey.com/forumdisplay.php?fid=595",
    #HAM
    "https://simulationhockey.com/forumdisplay.php?fid=596",
    #MAN
    "https://simulationhockey.com/forumdisplay.php?fid=597",
    #MTL
    "https://simulationhockey.com/forumdisplay.php?fid=744",
    #NEW
    "https://simulationhockey.com/forumdisplay.php?fid=599",
    #PHI
    "https://simulationhockey.com/forumdisplay.php?fid=743",
    #TBB
    "https://simulationhockey.com/forumdisplay.php?fid=607",
    #TOR
    "https://simulationhockey.com/forumdisplay.php?fid=600",
    #CGY
    "https://simulationhockey.com/forumdisplay.php?fid=603",
    #CHI
    "https://simulationhockey.com/forumdisplay.php?fid=636",
    #EDM
    "https://simulationhockey.com/forumdisplay.php?fid=604",
    #LAP
    "https://simulationhockey.com/forumdisplay.php?fid=605",
    #MIN
    "https://simulationhockey.com/forumdisplay.php?fid=598",
    #NOLA
    "https://simulationhockey.com/forumdisplay.php?fid=641",
    #SFP
    "https://simulationhockey.com/forumdisplay.php?fid=606",
    #SEA
    "https://simulationhockey.com/forumdisplay.php?fid=705",
    #TEX
    "https://simulationhockey.com/forumdisplay.php?fid=608",
    #WPG
    "https://simulationhockey.com/forumdisplay.php?fid=602"
  ) %>%
  lapply(
    X = .,
    FUN = draftedProspectScraper
  ) %>%
  do.call(
    what = rbind,
    args = .
  )

## Scrapes the forum
data <-
  scraper()

forumData <-
  data %>%
  ## Removes height, weight attributes as they are not used
  ## Removes duplicated position that is taken from the player info
  ## POSITION is taken from the post title
  # select(-Weight, -Height, -Position, -(Player.Type:last_col())) %>%
  left_join(
    iihfTransfer %>%
      mutate(
        `Transfer Season` = unlist(`Transfer Season`)
      ) %>%
      group_by(player) %>%
      dplyr::filter(
        `Transfer Season` == max(`Transfer Season`)
      ),
    by = c("NAME" = "player")
  ) %>%
  mutate(
    `IIHF NATION` =
      case_when(
        is.na(`IIHF Nation`) ~ `IIHF NATION`,
        TRUE ~ `IIHF Nation`
      )
  ) %>%
  select(
    -c(`IIHF Nation`)
  ) %>%
  left_join(
    draftedProspects,
    by = c("LINK" = "Prospect")
  ) %>%
  mutate(
    SHL.Team =
      Rights
  ) %>%
  select(-Rights) %>%
  arrange(
    CREATED
  ) %>%
  mutate(
    HANDEDNESS =
      coalesce(
        HANDEDNESS, SHOOTS
      ),
    `SHL TEAM` =
      coalesce(
        `SHL TEAM`, SHL.Team
      )
  ) %>%
  relocate(
    BLOCKER:`GOALIE STAMINA`,
    .before = `IIHF NATION`
  ) %>%
  relocate(
    Original:`Transfer Season`,
    .after = `IIHF NATION`
  ) %>%
  select(
    NAME:CLEAN_NAME
  )


save(
  forumData,
  file = "data/forumData.RData"
)

write.csv(forumData, file = "data/forumData.csv", row.names = FALSE)
#
# forumData %>%
#   dplyr::select(
#     USERLINK,
#     NAME,
#     POSITION,
#     Handedness,
#     Recruited.By,
#     Player.Render,
#     Jersey.Nr.,
#     Height,
#     Weight,
#     Birthplace,
#     CLASS,
#     league,
#     fhmID,
#     SHL.Team,
#     IIHF.Nation,
#     TPE,
#     Screening:Goalie.Stamina
#   ) %>%
#   dplyr::rename(
#     user = USERLINK,
#     name = NAME,
#     position = POSITION,
#     handedness = Handedness,
#     recruiter = Recruited.By,
#     render = Player.Render,
#     jerseyNumber = Jersey.Nr.,
#     height = Height,
#     weight = Weight,
#     birthplace = Birthplace,
#     season = CLASS,
#     currentLeague = league,
#     teamID = fhmID,
#     shlRightsTeamID = SHL.Team,
#     iihfNation = IIHF.Nation,
#     tpeTotal = TPE
#   ) %>%
#   mutate(
#     user = user %>% stringr::str_extract_all(pattern = "[0-9]+", simplify = TRUE),
#     season = season %>% stringr::str_extract_all(pattern = "[0-9]+", simplify = TRUE)
#   ) %>%
#   left_join(
#     teamInfo %>%
#       dplyr::filter(
#         league == "SHL"
#       ) %>%
#       dplyr::select(fhmID, team),
#     by = c("shlRightsTeamID" = "team")
#   ) %>%
#   mutate(
#     shlRightsTeamID = fhmID %>% as.character() %>% tidyr::replace_na(replace = ""),
#     across(
#       c(currentLeague, teamID, Screening:Goalie.Stamina),
#       ~ .x %>%
#         as.character() %>%
#         tidyr::replace_na(replace = "")
#     )
#   ) %>%
#   select(
#     -fhmID
#   ) %>%
#   write.csv(file = "data/forumDataImport.csv", row.names = FALSE)




