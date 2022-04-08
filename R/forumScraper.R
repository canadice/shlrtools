
#################################################################
##              The main forum scraper of the SHL              ##
#################################################################

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

prospectFALinks <- function(){
  undraftedProspects <-
    "https://simulationhockey.com/forumdisplay.php?fid=63" %>%
    c(
      .,
      paste(., "&page=2", sep = ""),
      paste(., "&page=3", sep = ""),
      paste(., "&page=4", sep = "")
    )

  shlFA <-
    "https://simulationhockey.com/forumdisplay.php?fid=43" %>%
    c(
      .,
      paste(
        .,
        paste("&page=", 2:35, sep = ""),
        sep = ""
      )
    )

  smjhlFA <-
    "https://simulationhockey.com/forumdisplay.php?fid=64" %>%
    c(
      .,
      paste(
        .,
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


scraper <- function(){
  ## Current season
  currentSeason <-
    playerLoader(0)$players %>%
    select(season) %>%
    unique() %>%
    unname()


  cl <-
    parallel::makeCluster(getOption("cl.cores", 4))

  parallel::clusterExport(
    cl,
    varlist =
      c(
        "%>%"
      )
  )

  playerLinks <-
    parallel::clusterApply(
      cl,
      shlrtools::leagueLinks(),
      fun = function(x){
        x %>%
          shlrtools::teamScraper() %>%
          shlrtools::playerScraper()
      }
    ) %>%
    c(
      .,
      parallel::clusterApply(
        cl,
        shlrtools::prospectFALinks(),
        fun = shlrtools::prospectsFAScraper
      ) %>%
        unlist()
    ) %>%
    unique() %>%
    stringi::stri_remove_na()










}
