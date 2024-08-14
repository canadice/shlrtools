require(shlrtools)
require(dplyr)

## Grabs data from the Portal API
data <-
  portalPlayers()

## Scrapes data from the forum for usernames
userData <-
  data$USERLINK %>%
  unique() %>%
  lapply(
    X = .,
    FUN = function(x){
    scrape <-
      tryCatch(
        userScraper(x), error = function(e) paste(x, "produces this error: ", e)
      )

    if(
      !is.data.frame(scrape)
    ){
      print(x)
    }
    else {
      # print("OK")
      return(scrape)
    }
  }
  ) %>%
  .[which(lapply(., is.data.frame) %>% unlist())] %>%
  do.call(
    args = .,
    what = plyr::rbind.fill
  )

attributes <-
  data %>%
  select(
    pid,
    attributes
  ) %>%
  do.call(
    what = data.frame,
    args = .
  ) %>%
  rename_with(
    .cols = contains("attributes"),
    .fn = function(x) x %>% stringr::str_remove(pattern= "attributes.")
  )

forumData <-
  data %>%
  left_join(
    userData,
    by = c("USERLINK")
  ) %>%
  select(
    pid,
    NAME = name,
    CLASS,
    POSITION = position,
    TPE = totalTPE,
    CREATED = creationDate,
    LINK,
    USER = username,
    USERLINK,
    JOINED = Joined,
    `LAST VISIT` = `Last Visit`,
    `ONLINE FOR` = `Online For`,
    `BANK BALANCE` = bankBalance,
    `SHL TEAM`,
    `IIHF NATION` = iihfNation,
    REPUTATION = Reputation,
    ACTIVE = Active,
    HANDEDNESS = handedness,
    `JERSEY NUMBER` = jerseyNumber,
    HEIGHT = height,
    WEIGHT = weight,
    BIRTHPLACE = birthplace,
    `TPE AVAILABLE` = bankedTPE,
    franchiseID:alt2,
    RENDER = render,
    RECRUITER = recruiter
  ) %>%
  left_join(
    attributes,
    by = "pid"
  ) %>%
  mutate(
    CLEAN_NAME =
      stringi::stri_trans_general(
        NAME,
        id = "Latin-ASCII"
      )
  ) %>%
  arrange(
    CREATED
  ) %>%
  relocate(
    c(RENDER, RECRUITER),
    .after = CLEAN_NAME
  ) %>%
  mutate(
    ACTIVE = if_else(ACTIVE == "Active" | activeStatus == 1, "Active", "IA")
  ) %>%
  select(
    !activeStatus
  )

save(
  forumData,
  file = "data/forumData.RData"
)

write.csv(forumData, file = "data/forumData.csv", row.names = FALSE)
