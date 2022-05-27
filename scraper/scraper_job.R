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

data <-
  data %>%
  do.call(
    args = .,
    what = plyr::rbind.fill
  )

forumData <-
  data %>%
  ## Removes height, weight attributes as they are not used
  ## Removes duplicated position that is taken from the player info
  ## POSITION is taken from the post title
  # select(-Weight, -Height, -Position, -(Player.Type:last_col())) %>%
  left_join(
    iihfTransfer %>%
      group_by(player) %>%
      dplyr::filter(
        `Transfer Season` == max(`Transfer Season`)
      ),
    by = c("NAME" = "player")
  ) %>%
  mutate(
    IIHF.Nation =
      case_when(
        is.na(`IIHF Nation`) ~ IIHF.Nation,
        TRUE ~ `IIHF Nation`
      )
  ) %>%
  select(
    -`IIHF Nation`
  ) %>%
  left_join(
    draftedProspects,
    by = c("LINK" = "Prospect")
  ) %>%
  mutate(
    SHL.Team =
      case_when(
        !is.na(Rights) & SHL.Team != Rights ~ Rights,
        TRUE ~ SHL.Team
      )
  ) %>%
  select(-Rights) %>%
  ## Create a "clean name" variable without special characters
  ## This can be used in connection with the index data
  mutate(
    clean_name =
      case_when(
        First.Name %>% is.na() ~
          stringi::stri_trans_general(
            NAME,
            id = "Latin-ASCII"
          ),
        TRUE ~ stringi::stri_trans_general(
          paste(First.Name,
                Last.Name, sep = " "
          ),
          id = "Latin-ASCII"
        )
      )
  ) %>%

  ## These players have too long names (or other names) in FHM6
  mutate(clean_name =
           case_when(
             clean_name == "James \"Jimmy\" Yzerman" ~ "James Yzerman",
             clean_name == "Asclepius Perseus Flitterwind" ~ "Asclepius Perseus Flitter",
             clean_name == "Hennesey-Gallchobhar O'McGuiness" ~ "Hennesey-Gallchobhar O'Mc",
             clean_name == "Terrence \"Big Terry\" Smith" ~ "Terrence Smith",
             clean_name == "Ragnar-Alexandre Ragnarsson-Tremblay" ~ "Ragnar-Alexandre Ragnarss",
             TRUE ~ clean_name
           )
  ) %>%

  ## Cleans up the transformation a bit
  mutate(
    clean_name =
      str_squish(clean_name)) %>%

  ## Uses standard names for positions
  ## Transforms some variables to numeric
  mutate(POSITION =
           case_when(
             POSITION %in% c("G", "Goaltender") ~ "Goalie",
             POSITION %in% c("C", "Centre") ~ "Center",
             POSITION %in% c("D", "Defence", "Defenseman") ~ "Defense",
             POSITION == "LW" ~ "Left Wing",
             POSITION == "W" ~ "Winger",
             POSITION %in% c("RHD", "Right Defence", "Right Defenseman", "Right Defender") ~ "Right Defense",
             POSITION %in% c("LHD", "Left Defence", "Left Defenseman", "Left Defender") ~ "Left Defense",
             POSITION %in% c("RW", "Right Winger") ~ "Right Wing",
             TRUE ~ POSITION
           ) %>%
           factor(
             levels =
               c(
                 "Goalie",
                 "Defense",
                 "Left Defense",
                 "Right Defense",
                 "Left Wing",
                 "Center",
                 "Right Wing",
                 "Winger"
               )
           ),
         NAME=str_remove(NAME, pattern = " \\*"),
         Posts = as.numeric(str_remove_all(Posts, pattern = "[^0-9]")),
         Threads = as.numeric(str_remove_all(Threads, pattern = "[^0-9]")),
         Reputation = as.numeric(str_remove_all(Reputation, pattern = "[^0-9]")),
         Jersey.Nr. = as.numeric(str_remove_all(Jersey.Nr., pattern = "[^0-9]"))
  )

saveRDS(
  forumData,
  file =
    paste(
      "data/forumData",
      lubridate::today(),
      ".rda",
      sep = ""
    )
)



