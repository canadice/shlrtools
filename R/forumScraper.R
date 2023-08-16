#' Combines the league links from SHL and SMJHL
#'
#' @export
#'

leagueLinks <- function(){
  ## All links on the forum
  smjhl <- "https://simulationhockey.com/forumdisplay.php?fid=5"
  shl_east <- "https://simulationhockey.com/forumdisplay.php?fid=8"
  shl_west <- "https://simulationhockey.com/forumdisplay.php?fid=9"

  return(
    c(
      smjhl,
      shl_west,
      shl_east
    )
  )
}

#' Combines all undrafted Prospects and Free Agents links
#'
#'
#' @export
#'

prospectFALinks <- function(){
  link <- "https://simulationhockey.com/forumdisplay.php?fid=63"

  undraftedProspects <-
    c(
      link,
      paste(link, "&page=2", sep = ""),
      paste(link, "&page=3", sep = ""),
      paste(link, "&page=4", sep = "")
    )


  link <- "https://simulationhockey.com/forumdisplay.php?fid=43"

  shlFA <-
    c(
      link,
      paste(
        link,
        paste("&page=", 2:35, sep = ""),
        sep = ""
      )
    )

  link <- "https://simulationhockey.com/forumdisplay.php?fid=64"

  smjhlFA <-
    c(
      link,
      paste(
        link,
        paste("&page=", 2:25, sep = ""),
        sep = ""
      )
    )

  return(
    c(
      undraftedProspects,
      shlFA,
      smjhlFA
    )
  )
}

#' Combines all drafted Prospects for each SHL team
#'
#'
#' @export
#'

draftedProspects <- function(){
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
}

#' The main forum scraper
#'
#' @description Making use of created scraper functions, this function scrapes and aggregates all info from the forum
#' @param parallell Boolean argument that controls if parallellization should occur.
#'
#' @export
#'
#' @returns
#' Returns a data frame with all player information from the forum.
#'

scraper <- function(parallell = FALSE){
  ## Current season
  currentSeason <-
    playerLoader(0)$players %>%
    dplyr::select(season) %>%
    unique() %>%
    unname()

  if(parallell){
    cl <-
      parallel::makeCluster(getOption("cl.cores", 4))

    parallel::clusterExport(
      cl,
      varlist =
        c(
          "%>%",
          "teamInfo",
          "tpeCost"
        )
    )

    playerLinks <-
      parallel::clusterApply(
        cl,
        shlrtools::leagueLinks(),
        fun = shlrtools::teamScraper
      ) %>%
      unlist() %>%
      parallel::clusterApply(
        cl = cl,
        x = .,
        fun = shlrtools::playerLinkScraper
      ) %>%
      unlist() %>%
      c(
        .,
        c(
          shlrtools::prospectFALinks()
        ) %>%
          sapply(FUN = shlrtools::prospectsFAScraper) %>%
          unlist()
      ) %>%
      unique() %>%
      stringi::stri_remove_na()

    playerData <-
      parallel::clusterApply(
        cl = cl,
        x = playerLinks,
        fun = shlrtools::playerScraper
      )
  } else {

    playerLinks <-
      lapply(
        shlrtools::leagueLinks(),
        FUN = shlrtools::teamScraper
      ) %>%
      unlist() %>%
      lapply(
        X = .,
        FUN = shlrtools::playerLinkScraper
      ) %>%
      unlist() %>%
      c(
        .,
        c(
          shlrtools::prospectFALinks()
        ) %>%
          sapply(FUN = shlrtools::prospectsFAScraper) %>%
          unlist()
      ) %>%
      unique() %>%
      stringi::stri_remove_na()

    playerData <-
      lapply(
        X = playerLinks,
        FUN = function(x){
          scrape <- tryCatch(shlrtools::playerScraper(x), error = function(e) paste(x, "produces this error: ", e))

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
      ) %>%
      return()
  }


  return(playerData)
}


