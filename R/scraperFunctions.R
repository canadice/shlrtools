#' Scrapes team links from a league
#'
#' @param league The forum link to a league
#' @export
#'

teamScraper <- function(league){
  baseLink <- "https://simulationhockey.com/"

  ## Reads the information from the league page
  main <-
    xml2::read_html(league)

  ## Scrapes the links to all the team pages
  teamForums <-
    main %>%
    rvest::html_elements("strong a") %>%
    rvest::html_attr("href") %>%
    unlist()

  ## Pastes the full links to the team pages
  teamLinks <- paste(baseLink, teamForums, sep = "")

  return(teamLinks)
}

#' Scrapes player links from undrafted prospecs or FA forum page
#'
#' @param link The forum link
#' @export
#'

prospectsFAScraper <- function(link){
  base_link <- "https://simulationhockey.com/"

  players <- rvest::read_html(link)

  players %>%
    rvest::html_elements("table.tborder2 span.subject_new a") %>%
    rvest::html_attr("href") %>%
    return()
}

#' Scrapes player links from teams
#'
#' @param team The forum link to the team
#' @export
#'

playerLinkScraper <- function(team){
  baseLink <- "https://simulationhockey.com/"

  ## Reads the information on the team page
  teamPage <-
    rvest::read_html(team)

  ## Scrapes the roster pages from the team page
  teamRoster <-
    rvest::html_attr(
      rvest::html_elements(teamPage, "strong a")[
        sapply(
          rvest::html_elements(teamPage, "strong a"),
          FUN = function(x){
            x %>%
              rvest::html_text2() %>%
              stringr::str_detect(pattern = "Roster")
          }
        )
      ],
      "href"
    )

  ## As some teams have more than 20 players, the second page of the roster is added
  ## THIS MIGHT PRODUCE DUPLICATES IN CASE TEAMS HAVE LESS PLAYERS
  rosterLinks <-
    c(
      teamRoster,
      paste(
        teamRoster,
        "&page=2",
        sep = ""
      )
    )

  ## Scrapes all player links from the roster pages
  playerLinks <-
    lapply(
      rosterLinks,
      FUN = function(page){
        links <-
          paste(baseLink, page, sep = "") %>%
          rvest::read_html() %>%
          rvest::html_elements("td.forumdisplay_regular div span a") %>%
          rvest::html_attr("href")

        cleanLinks <-
          links[
            !stringr::str_detect(links, "&action=newpost") &
              !stringr::str_detect(links, "&action=lastpost") &
              !stringr::str_detect(links, "simulationhockey") &
              !stringr::str_detect(links, "&page=2")
          ] %>%
          stringi::stri_remove_empty_na() %>%
          unique()

      }
    ) %>%
    unlist() %>%
    unique()

  return(playerLinks)
}

#' Scrapes the prospects from each SHL team
#'
#' @param link Forum link to the prospect pages for an SHL team
#'
#' @export
#' @returns
#' Returns a data frame with each prospect player page link and the team holding their rights
#'

draftedProspectScraper <- function(link){
  ### Reads the information
  topic <- xml2::read_html(link)

  ###  Reading the player information from topic title
  SHLRIGHTS <-
    teamInfo %>%
    dplyr::slice(
      topic %>%
        rvest::html_nodes(".navigation") %>%
        rvest::html_text() %>%
        stringr::str_squish() %>%
        dplyr::nth(1) %>%
        stringr::str_detect(
          ## Takes team information from a separate data set
          pattern = teamInfo$team
        ) %>%
        which()
    ) %>%
    dplyr::select(.data$team) %>%
    unlist() %>%
    unname()

  prospectPages <-
    topic %>%
    rvest::html_nodes(".tborder2") %>%
    rvest::html_nodes(".inline_row") %>%
    rvest::html_nodes(".subject_old") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")

  data.frame(
    Prospect = paste("https://simulationhockey.com/", prospectPages, sep = ""),
    Rights = SHLRIGHTS
  ) %>%
    return()
}

#' Scrapes all player page data
#'
#' @param player Player page link
#'
#' @export
#'
#' @returns
#' Returns a data.frame with all information (except attributes) from a player page
#'
playerScraper <-
  function(player){
    ### Takes the player link scraped from the team pages
    ##  If it is a complete link with the base url there it scrapes it directly
    ##  For use with teamLinkScraper and playerLinkScraper then only the endings are used, requiring the baseLink addition
    if(stringr::str_detect(player, "simulationhockey")){

    } else{
      baseLink <- "https://simulationhockey.com/"

      player <- paste(baseLink, player, sep = "")

    }

    ### Reads the information
    topic <- xml2::read_html(player)

    ###  Reading the player information from topic title
    title <-
      topic %>%
      rvest::html_nodes("title") %>%
      rvest::html_text() %>%
      stringr::str_split(pattern = " - |- ") %>%
      unlist() %>%
      stringr::str_squish()

    if(length(title) == 2){
      NAME <-
        title %>%
        dplyr::nth(2)

      CLASS <-
        title %>%
        stringr::str_extract_all(pattern = "S[0-9]+") %>%
        unlist() %>%
        dplyr::nth(1)

      if(length(CLASS)==0){
        CLASS <- "Unspecified"
      }

      POSITION <-
        title %>%
        dplyr::nth(1) %>%
        stringr::str_split(pattern = "]|\\)") %>%
        unlist() %>%
        stringr::str_squish() %>%
        dplyr::nth(2)

    } else if(length(title) == 3){
      NAME <-
        title %>%
        dplyr::nth(3)

      CLASS <-
        title %>%
        stringr::str_extract_all(pattern = "S[0-9]+") %>%
        unlist() %>%
        dplyr::nth(1)

      if(length(CLASS)==0){
        CLASS <- "Unspecified"
      }

      POSITION <-
        title %>%
        dplyr::nth(2)

    } else {
      ## If something is wrong with the title splits, return NA for each of these.
      NAME <- NA
      CLASS <- NA
      POSITION <- NA
    }

    # ### Checks if the name includes a nickname
    # if(NAME %>% is.na()){
    #   #Do nothing
    #
    #   NICKNAME <- NA
    # } else if (NAME %>% stringr::str_detect(pattern = "\"")){
    #   NICKNAME <-
    #     NAME %>%
    #     stringr::str_extract_all(pattern = "\"[A-z ]+\"", simplify = TRUE)
    #
    #   NAME <-
    #     NAME %>%
    #     stringr::str_remove_all(pattern = "\"[A-z ]+\"") %>%
    #     stringr::str_squish()
    # } else {
    #   NICKNAME <- NA
    # }

    ### Reading the TPE from the post title
    TPE <-
      topic %>%
      rvest::html_nodes("small") %>%
      dplyr::nth(1) %>%
      rvest::html_text() %>%
      stringr::str_extract_all(pattern = "[0-9]+") %>%
      unlist() %>%
      as.numeric()

    if(length(TPE) == 0){
      TPE = NA
    }


    ###  Extract user information

    ## Specifies the user defined tags that will be removed from the user name
    userTags <-
      paste0(
        "SHL Commissioner|SHL GM|SHL HO|Co-Commissioner|Commissioner|",
        "SMJHL Intern|SMJHL GM|SMJHL HO|SMJHL Commissioner|",
        "IIHF Commissioner|IIHF Federation Head|WJC Commissioner|",
        "SMJHL All-Star Committee|SHL All-Star Committee|HOF Committee|",
        "SMJHL Awards Committee|SHL Awards Committee|Awards Committee|",
        "Rookie Mentor Committee|Appeals Committee|All-Star Committee|",
        "Registered|Rookie|Historian|Recruitment Team|Deep Dives Head|",

        paste(teamInfo$team, collapse = "|"),"|","Team [A-z ]+|",

        "Head Office|Coach|Budget Director|Graphic Graders|Moderators|",
        "Player Updaters|PGS Grader|Player Progression Director|",
        "Fantasy League Manager|Simmer|Head Updater|Deep Dive Head|",
        "Owner|Media Graders|Bank Manager|Mentor|",
        "Site Management|Trading Card Team|Trading Card Admins"
      )

    USER <-
      topic %>%
      rvest::html_nodes(".profile-username") %>%
      dplyr::nth(1) %>%
      rvest::html_text() %>%
      stringr::str_split("\n") %>%
      unlist() %>%
      ## Usually the user name starts with a \n so the second cell holds the user info
      dplyr::nth(2) %>%
      stringr::str_remove(
        pattern = userTags
      )

    USERLINK <-
      topic %>%
      rvest::html_nodes(".profile-username") %>%
      dplyr::nth(1) %>%
      rvest::html_nodes(xpath = "./a") %>%
      rvest::html_attr("href")

    USERINFO <-
      USERLINK %>%
      userScraper()

    USERDATA <-
      topic %>%
      rvest::html_nodes("#mainwidth2") %>%
      dplyr::nth(1) %>%
      rvest::html_nodes(".float_right") %>%
      rvest::html_text() %>%
      stringr::str_squish() %>%
      .[1:2]

    names(USERDATA) <- c("Posts", "Threads")

    ### Extract player information

    PLAYERTEAM <-
      teamInfo %>%
      dplyr::slice(
        topic %>%
          rvest::html_nodes(".navigation") %>%
          rvest::html_text() %>%
          stringr::str_squish() %>%
          dplyr::nth(1) %>%
          stringr::str_detect(
            ## Takes team information from a separate data set
            pattern = teamInfo$team
          ) %>%
          which()
      )


    if((PLAYERTEAM %>% nrow()) == 0){
      PLAYERTEAM <-
        PLAYERTEAM %>%
        dplyr::add_row()
    } else {
      PLAYERTEAM <-
        PLAYERTEAM %>%
        dplyr::filter(
          .data$Inaugural.Season == max(.data$Inaugural.Season)
        )
    }

    postData <-
      topic %>%
      rvest::html_nodes("div#two") %>%
      dplyr::nth(1) %>%
      rvest::html_nodes(".post_body")

    ### Checks if information is written in special fonts
    checkText <- postData %>%
      rvest::html_nodes(".mycode_font") %>%
      rvest::html_text()

    if(length(checkText) == 0){
      postData <-
        postData %>%
        rvest::html_text2()
    } else if(stringr::str_detect(string = checkText[1], pattern = "First Name")){

      postData <-
        postData %>%
        rvest::html_nodes(".mycode_font") %>%
        rvest::html_text2() %>%
        paste0(collapse = "\n")
    } else {
      postData <-
        postData %>%
        rvest::html_text2()
    }

    FIRSTNAME <-
      postData %>%
      stringr::str_match(pattern = "First Name:(.*?)\\n") %>%
      unlist() %>%
      dplyr::nth(2)

    LASTNAME <-
      postData %>%
      stringr::str_match(pattern = "Last Name:(.*?)\\n") %>%
      unlist() %>%
      dplyr::nth(2)

    HANDEDNESS <-
      postData %>%
      stringr::str_match(pattern = "(Shoots|Hand[A-z]+):(.*?)\\n") %>%
      unlist() %>%
      dplyr::last()

    RECRUITEDBY <-
      postData %>%
      stringr::str_match(pattern = "Recruited[A-z ]+:(.*?)\\n") %>%
      unlist() %>%
      dplyr::nth(2)

    HEIGHT <-
      postData %>%
      stringr::str_match(pattern = "Height:(.*?)\\n") %>%
      unlist() %>%
      dplyr::nth(2)

    WEIGHT <-
      postData %>%
      stringr::str_match(pattern = "Weight:(.*?)\\n") %>%
      unlist() %>%
      dplyr::nth(2)

    RENDER <-
      postData %>%
      stringr::str_match(pattern = "Player Render:(.*?)\\n") %>%
      unlist() %>%
      dplyr::nth(2)

    JERSEYNR <-
      postData %>%
      stringr::str_match(pattern = "Jersey[A-z ]+:(.*?)\\n") %>%
      unlist() %>%
      dplyr::nth(2)

    BIRTHPLACE <-
      postData %>%
      stringr::str_match(pattern = "Birth[A-z]+:(.*?)(\\n|Player)") %>%
      unlist() %>%
      dplyr::nth(2)

    PLAYERINFO <-
      cbind(
        FIRSTNAME,
        LASTNAME,
        HANDEDNESS,
        JERSEYNR,
        RECRUITEDBY,
        RENDER,
        BIRTHPLACE,
        HEIGHT,
        WEIGHT
      ) %>%
      stringr::str_trim() %>%
      t() %>%
      as.data.frame()

    colnames(PLAYERINFO) <-
      c(
        "First Name",
        "Last Name",
        "Handedness",
        "Jersey Nr.",
        "Recruited By",
        "Player Render",
        "Birthplace",
        "Height",
        "Weight"
      )


    ### Player Attributes
    if(stringr::str_detect(POSITION, "Goal")){
      ATTRIBUTES <-
        postData %>%
        stringr::str_split("Blocker", simplify = TRUE) %>%
        dplyr::nth(2) %>%
        stringr::str_split("\\*Professionalism", simplify = TRUE) %>%
        dplyr::nth(1) %>%
        stringr::str_remove_all("Goalie Ratings|Mental Ratings") %>%
        stringr::str_split(":|\\n+", simplify = TRUE) %>%
        c(., "15") %>%
        stringr::str_squish() %>%
        matrix(ncol = 2, byrow = TRUE) %>%
        t() %>%
        data.frame()

      colnames(ATTRIBUTES) <- ATTRIBUTES[1,]
      colnames(ATTRIBUTES)[1] <- "Blocker"
      colnames(ATTRIBUTES)[length(colnames(ATTRIBUTES))] <- "*Professionalism"

      ATTRIBUTES <-
        ATTRIBUTES[-1,] %>%
        dplyr::mutate(
          dplyr::across(
            dplyr::everything(),
            as.numeric
          )
        )
    } else {
      ATTRIBUTES <-
        postData %>%
        stringr::str_split("Screening", simplify = TRUE) %>%
        dplyr::nth(2) %>%
        stringr::str_split("\\*Professionalism", simplify = TRUE) %>%
        dplyr::nth(1) %>%
        stringr::str_remove_all("Defensive Ratings|Mental Ratings|Physical Ratings") %>%
        stringr::str_split(":|\\n+", simplify = TRUE) %>%
        c(., "15") %>%
        stringr::str_squish() %>%
        matrix(ncol = 2, byrow = TRUE) %>%
        t() %>%
        data.frame()

      ATTRIBUTES <-
        ATTRIBUTES[,!apply(X = ATTRIBUTES, MARGIN = 2, FUN = function(x){sum(x == "")==2})]

      colnames(ATTRIBUTES) <- ATTRIBUTES[1,]
      colnames(ATTRIBUTES)[1] <- "Screening"
      colnames(ATTRIBUTES)[length(colnames(ATTRIBUTES))] <- "*Professionalism"

      ATTRIBUTES <-
        ATTRIBUTES[-1,] %>%
        dplyr::mutate(
          dplyr::across(
            dplyr::everything(),
            as.numeric
          )
        )
    }
{
    #
    # if(stringr::str_detect(postData, pattern = "Attributes")){
    #   postData <-
    #     postData %>%
    #     stringr::str_split(pattern = "Player Attributes|Payer Attributes") %>%
    #     unlist() %>%
    #     stringi::stri_remove_empty()
    # } else {
    #   postData <-
    #     postData %>%
    #     stringr::str_split(pattern = "Points") %>%
    #     unlist() %>%
    #     stringi::stri_remove_empty()
    # }
    #
    # infoIndex <-
    #   postData %>%
    #   stringr::str_detect("First Name") %>%
    #   which()
    #
    # if(postData %>% length() > 2){
    #   PLAYERINFO <-
    #     postData %>%
    #     dplyr::nth(infoIndex) %>%
    #     stringr::str_split(
    #       pattern = ":|\\n"
    #     )
    # } else {
    #   PLAYERINFO <-
    #     postData %>%
    #     dplyr::nth(1) %>%
    #     stringr::str_split(
    #       pattern = ":|\\n"
    #     )
    # }
    #
    # ## Checks if a title of Player information is present in the text
    # ## If so then remove first two elements of the vector
    # ## Otherwise only remove first element.
    # if(
    #   stringr::str_detect(
    #     PLAYERINFO %>% paste0(collapse = ""),
    #     pattern = "Player Information"
    #     )
    #   ){
    #   PLAYERINFO <-
    #     PLAYERINFO %>%
    #     unlist() %>%
    #     .data[-(1:2)] %>%
    #     stringr::str_squish() %>%
    #     matrix(nrow = 2) %>%
    #     janitor::row_to_names(1) %>%
    #     dplyr::as_tibble()
    # } else {
    #   PLAYERINFO <-
    #     PLAYERINFO %>%
    #     unlist() %>%
    #     .data[-1] %>%
    #     stringr::str_squish() %>%
    #     matrix(nrow = 2) %>%
    #     janitor::row_to_names(1) %>%
    #     dplyr::as_tibble()
    # }
    #
    # colnames(PLAYERINFO)[colnames(PLAYERINFO) %>% stringr::str_detect(pattern = "Height")] <- "Height"
    # colnames(PLAYERINFO)[colnames(PLAYERINFO) %>% stringr::str_detect(pattern = "Weight")] <- "Weight"
    # colnames(PLAYERINFO)[colnames(PLAYERINFO) %>% stringr::str_detect(pattern = "Hand")] <- "Handedness"
    # colnames(PLAYERINFO)[colnames(PLAYERINFO) %>% stringr::str_detect(pattern = "Shoots")] <- "Handedness"
    # colnames(PLAYERINFO)[colnames(PLAYERINFO) %>% stringr::str_detect(pattern = "Recruited")] <- "Recruited"
    # colnames(PLAYERINFO)[colnames(PLAYERINFO) %>% stringr::str_detect(pattern = "Jersey")] <- "Jersey Nr."
    # colnames(PLAYERINFO)[colnames(PLAYERINFO) %>% stringr::str_detect(pattern = "Birth[A-z]+")] <- "Birthplace"
    #
}
    PLAYERINFO <-
      PLAYERINFO %>%
      dplyr::mutate(
        `IIHF Nation` =
          if(exists('Birthplace', where = .)){
            dplyr::case_when(
              stringr::str_detect(Birthplace, pattern = "Sweden") ~ "Sweden",
              stringr::str_detect(Birthplace, pattern = "Canada|Ontario") ~ "Canada",
              stringr::str_detect(Birthplace, pattern = "USA|United States|Michigan|NY|N.Y.|Georgia") ~ "USA",
              stringr::str_detect(Birthplace, pattern = "Finland") ~ "Finland",
              stringr::str_detect(Birthplace, pattern = "Russia") ~ "Russia",
              stringr::str_detect(Birthplace, pattern = "Austria") ~ "Austria",
              stringr::str_detect(Birthplace, pattern = "Czechia|Czech Republic|CZE|Czechoslovakia") ~ "Czechia",
              stringr::str_detect(Birthplace, pattern = "Germany") ~ "Germany",
              stringr::str_detect(Birthplace, pattern = "England|Wales|Scotland|Northern Ireland|United Kingdom|Great Britain") ~ "Great Britain",
              stringr::str_detect(Birthplace, pattern = "Ireland") ~ "Ireland",
              stringr::str_detect(Birthplace, pattern = "Japan") ~ "Japan",
              stringr::str_detect(Birthplace, pattern = "Latvia") ~ "Latvia",
              stringr::str_detect(Birthplace, pattern = "Norway") ~ "Norway",
              stringr::str_detect(Birthplace, pattern = "Switzerland") ~ "Switzerland",
              TRUE ~ "Unassigned"
            )
          } else {
            "Unassigned"
          }
      )
{
    ####### CODE FOR SCRAPING INFORMATION THAT IS NO LONGER USED    #######
    # ## Specifies which tags to use for the player information splits
    # playerMeta <-
    #   paste0(
    #     "First Name:|First Name :|Last Name:|Position:|Born:|",
    #     "Birth Date:|Handedness:|Shoots:|",
    #     "Recruited By:|Recruited by:|Player Render:|Jersey Number:|",
    #     "Height:|Height \\(ft.\\):|Weight:|Weight \\(lbs.\\):|Birthplace:|Player"
    #     )
    #
    # playerRemove <-
    #   paste0(
    #     "Offensive Ratings|Defensive Ratings|Mental Ratings|Physical Ratings"
    #   )
    #
    # ####### Some players have born or birth date as one of the player information, which means
    # ####### that 12 on row 184 should be 13 to cover Birthplace
    #
    # PLAYERINFO <-
    #   postData %>%
    #   dplyr::nth(1) %>%
    #   stringr::str_split(
    #     pattern = playerMeta
    #   ) %>%
    #   unlist() %>%
    #   stringr::str_squish() %>%
    #   dplyr::as_tibble() %>%
    #   dplyr::slice((min(nrow(.),12)-9):min(nrow(.),12)) %>%
    #   unlist()
    #
    # ## Some players have their Player Information in a different order...
    # if(PLAYERINFO[1] == "Alexis" && PLAYERINFO[2] == "Saint-Michel"){
    #   PLAYERINFO <-
    #     PLAYERINFO[c(1, 2, 3, 4, 5, 9, 10, 7, 8, 6)]
    # } else if(PLAYERINFO[1] == "Otis B." && PLAYERINFO[2] == "Driftwood"){
    #   PLAYERINFO <-
    #     PLAYERINFO[-4]
    #
    #   PLAYERINFO[6:10] <-
    #     PLAYERINFO[5:9]
    #
    #   PLAYERINFO[6] <- NA
    # }
    #
    # ## Sets the named vector to the specified order split from earlier
    # if(length(PLAYERINFO)==10){
    #   names(PLAYERINFO) <-
    #     c(
    #       "First Name",
    #       "Last Name",
    #       "Position",
    #       "Handedness",
    #       "Recruited",
    #       "Render",
    #       "Jersey Nr.",
    #       "Height",
    #       "Weight",
    #       "Birthplace"
    #     )
    # }
    #
    ####### CODE FOR SCRAPING ATTRIBUTES THAT IS NO LONGER USED    #######
    #
    #
    # if(!(PLAYERINFO["Position"] %in% c("Goaltender", "Goalie", "G"))){
    #   playerRatings<-
    #     paste0(
    #       "Screening:|Getting Open:|Passing:|Puckhandling:|",
    #       "Shooting Accuracy:|Shooting Range:|Offensive Read:|",
    #       "Checking:|Hitting:|Positioning:|Stickchecking:|",
    #       "Shot Blocking:|Faceoffs:|Defensive Read:|Acceleration:|",
    #       "Agility:|Balance:|Speed:|Stamina:|Strength:|Fighting:|",
    #       "Aggression:|Bravery:|\\*"
    #     )
    #
    #   PLAYERRATINGS<-
    #     postData %>%
    #     dplyr::nth(2) %>%
    #     stringr::str_split(
    #       pattern = playerRatings
    #     ) %>%
    #     unlist() %>%
    #     stringr::str_remove_all(
    #       pattern = playerRemove
    #     ) %>%
    #     stringr::str_squish() %>%
    #     dplyr::as_tibble() %>%
    #     dplyr::slice(2:24) %>%
    #     unlist() %>%
    #     as.numeric()
    #
    #   if(length(PLAYERRATINGS)==23){
    #     names(PLAYERRATINGS) <-
    #       c(
    #         "Screening",
    #         "Getting Open",
    #         "Passing",
    #         "Puckhandling",
    #         "Shooting Accuracy",
    #         "Shooting Range",
    #         "Offensive Read",
    #         "Checking",
    #         "Hitting",
    #         "Positioning",
    #         "Stickchecking",
    #         "Shot Blocking",
    #         "Faceoffs",
    #         "Defensive Read",
    #         "Acceleration",
    #         "Agility",
    #         "Balance",
    #         "Speed",
    #         "Stamina",
    #         "Strength",
    #         "Fighting",
    #         "Aggression",
    #         "Bravery"
    #       )
    #   }
    #
    # } else {
    #   playerRatings<-
    #     paste0(
    #       "Blocker:|Glove:|Passing:|Poke Check:|",
    #       "Positioning:|Rebound:|Recovery:|",
    #       "Puckhandling:|Low Shots:|Reflexes:|Skating:|",
    #       "Mental Toughness:|Goalie Stamina:|\\*"
    #     )
    #
    #   PLAYERRATINGS<-
    #     postData %>%
    #     dplyr::nth(2) %>%
    #     stringr::str_split(
    #       pattern = playerRatings
    #     ) %>%
    #     unlist() %>%
    #     stringr::str_remove_all(
    #       pattern = playerRemove
    #     ) %>%
    #     stringr::str_squish() %>%
    #     dplyr::as_tibble() %>%
    #     dplyr::slice(c(2:12, 14, 18)) %>%
    #     unlist() %>%
    #     as.numeric()
    #
    #   if(length(PLAYERRATINGS)==13){
    #     names(PLAYERRATINGS) <-
    #       c(
    #         "Blocker",
    #         "Glove",
    #         "Passing",
    #         "Poke Check",
    #         "Positioning",
    #         "Rebound",
    #         "Recovery",
    #         "Puckhandling",
    #         "Low Shots",
    #         "Reflexes",
    #         "Skating",
    #         "Mental Toughness",
    #         "Goalie Stamina"
    #       )
    #   }
    # }
    #
    # USEDTPE <-
    #   PLAYERRATINGS %>%
    #   dplyr::as_tibble() %>%
    #   dplyr::left_join(
    #     tpeCost,
    #     by = c("value" = "Skill.level")
    #   ) %>%
    #   summarize(
    #     sum = sum(TPE)
    #   ) %>%
    #   unlist()
    #######
      }

    ### Combines and structures the scraped data into a data.frame
    ##  Commented parts are from the discontinued playerRatings scraper that is no longer used
    data <-
      data.frame(
        NAME,
        # NICKNAME,
        CLASS,
        POSITION,
        TPE,
        LINK = player,
        USER,
        USERLINK,
        USERINFO,
        USERDATA %>% t(),
        #USEDTPE,
        PLAYERTEAM,
        PLAYERINFO,
        ATTRIBUTES
        #PLAYERRATINGS %>% t()
      )

    return(data)
  }

#' Scrapes user data
#'
#' @param link Forum link to a user
#'
#' @export
#' @returns
#' Data table with user data
#'

userScraper <- function(link){
  base_link <- "https://simulationhockey.com/"

  ### Reads the information on the user page
  topic <- xml2::read_html(link)

  ### Searches for the date of the user's last post
  lastPost <-
    topic %>%

    ## Finds link to all posts
    rvest::html_elements("a.button") %>%
    dplyr::nth(2) %>%
    rvest::html_attr("href")

  if(lastPost == "#"){
    topic <- xml2::read_html(link)

    lastPost <-
      topic %>%

      ## Finds link to all posts
      rvest::html_elements("a.button") %>%
      dplyr::nth(2) %>%
      rvest::html_attr("href")
  }

  lastPost <-
    paste(
      base_link,
      lastPost,
      sep = ""
    ) %>%

    ## Reads the information from the search results
    xml2::read_html() %>%
    rvest::html_elements("td.hide span") %>%
    dplyr::nth(5) %>%

    ## Finds date of the last post and converts it to date format
    rvest::html_text() %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      value =
        dplyr::case_when(
          stringr::str_detect(value, pattern = "minute") ~ lubridate::today(),
          stringr::str_detect(value, pattern = "hour") ~ lubridate::today(),
          stringr::str_detect(value, pattern = "Today") ~ lubridate::today(),
          stringr::str_detect(value, pattern = "Yesterday") ~ lubridate::today()-1,
          TRUE ~ value %>% stringr::str_extract(pattern = "[0-9]+-[0-9]+-[0-9]+") %>% lubridate::as_date(format = "%m-%d-%Y")
        )
    )

  print(lastPost)


  ### Reads table information
  table <-
    topic %>%

    ## Finds the html element with the tables
    rvest::html_elements("div#two table.tborder") %>%

    ## Selects the second table in the list
    dplyr::nth(1) %>%

    ## Converts it (wrong) to a data.frame
    rvest::html_table() %>%

    ## Data wrangling to get it to correct format
    dplyr::slice(-1) %>%
    dplyr::select(.data$X1) %>%
    unlist() %>%

    ## Splits headers from values, looks for either : followed by info or : at the end of string
    stringr::str_split(pattern = ":[^0-9]|:$") %>%
    unlist() %>%

    ## Creates a new matrix with the headers
    matrix(ncol = 2, byrow = TRUE) %>%
    t() %>%
    tibble::as_tibble(.name_repair = "minimal") %>%
    janitor::row_to_names(1) %>%

    ## Removes \n from the text and removes unnecessary white space
    dplyr::mutate_all(
      ~ stringr::str_replace_all(.x, pattern = "\n", replacement = "") %>%
        stringr::str_squish()
    ) %>%

    ## Converts dates to a date format
    dplyr::mutate(
      `Last Visit` =
        dplyr::case_when(
          stringr::str_detect(`Last Visit`, pattern = "minute") ~ lubridate::today(),
          stringr::str_detect(`Last Visit`, pattern = "hour") ~ lubridate::today(),
          stringr::str_detect(`Last Visit`, pattern = "Today") ~ lubridate::today(),
          stringr::str_detect(`Last Visit`, pattern = "Yesterday") ~ lubridate::today()-1,
          TRUE ~`Last Visit` %>%
            stringr::str_extract(pattern = "[0-9]+-[0-9]+-[0-9]+") %>%
            lubridate::as_date(`Last Visit`, format = "%m-%d-%Y")
        ),
      Joined =
        dplyr::case_when(
          stringr::str_detect(`Joined`, pattern = "minute") ~ lubridate::today(),
          stringr::str_detect(`Joined`, pattern = "hour") ~ lubridate::today(),
          stringr::str_detect(`Joined`, pattern = "Today") ~ lubridate::today(),
          stringr::str_detect(`Joined`, pattern = "Yesterday") ~ lubridate::today()-1,
          TRUE ~ lubridate::as_date(`Joined`, format = "%m-%d-%Y")
        ),
      `Online For` =
        sapply(
          X = .data$`Online For`,
          FUN = function(x){
            if(x == "(Hidden)"){
              return(NA)
            }

            x <-
              x %>%
              stringr::str_split(",") %>%
              unlist()

            year <-
              x[stringr::str_detect(x, pattern = "Year")] %>%
              stringr::str_extract_all(pattern = "[0-9]+", simplify = TRUE) %>%
              as.numeric()

            if(length(year) == 0){
              year <- 0
            }

            month <-
              x[stringr::str_detect(x, pattern = "Month")] %>%
              stringr::str_extract_all(pattern = "[0-9]+", simplify = TRUE) %>%
              as.numeric()

            if(length(month) == 0){
              month <- 0
            }

            week <-
              x[stringr::str_detect(x, pattern = "Week")] %>%
              stringr::str_extract_all(pattern = "[0-9]+", simplify = TRUE) %>%
              as.numeric()

            if(length(week) == 0){
              week <- 0
            }

            day <-
              x[stringr::str_detect(x, pattern = "Day")] %>%
              stringr::str_extract_all(pattern = "[0-9]+", simplify = TRUE) %>%
              as.numeric()

            if(length(day) == 0){
              day <- 0
            }

            hour <-
              x[stringr::str_detect(x, pattern = "Hour")] %>%
              stringr::str_extract_all(pattern = "[0-9]+", simplify = TRUE) %>%
              as.numeric()

            if(length(hour) == 0){
              hour <- 0
            }

            minute <-
              x[stringr::str_detect(x, pattern = "Minute")] %>%
              stringr::str_extract_all(pattern = "[0-9]+", simplify = TRUE) %>%
              as.numeric()

            if(length(minute) == 0){
              minute <- 0
            }

            second <-
              x[stringr::str_detect(x, pattern = "Second")] %>%
              stringr::str_extract_all(pattern = "[0-9]+", simplify = TRUE) %>%
              as.numeric()

            if(length(second) == 0){
              second <- 0
            }

            second + 60 * (minute + 60 * (hour + 24 * (day + 7 * (week + 4.345 * (month + 12*year)))))
          }
        )
    ) %>%

    ## Checks if a user is considered IA (30 days of not posting)
    dplyr::mutate(
      # `Last Post` = lastPost %>% unlist() %>% lubridate::as_date(),
      Active =
        dplyr::case_when(
          lubridate::today() - (lastPost %>% unlist()) > 30 ~ "IA",
          TRUE ~ "Active"
        )
    )

  ## Checks if Reputation is present in the user data otherwise adds it.

  if(!("Reputation" %in% colnames(table))){
    table$Reputation = NA
  }

  table <-
    table %>%
    dplyr::mutate(
      Reputation =
        stringr::str_extract(
          .data$Reputation,
          pattern = "[0-9]+"
        ) %>%
        as.numeric(),
      `Bank Balance` =
        stringr::str_extract_all(
          .data$`Bank Balance`,
          pattern = "[0-9]+",
          simplify = TRUE
        ) %>%
        paste0(collapse = "") %>%
        as.numeric()
    )

  return(table)
}

