
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



