#' Rescales standardized values
#'
#' @param x Standardized value
#'
#' @export

reScale <- function(x){
  x * attr(x, 'scaled:scale') + attr(x, 'scaled:center')
}

#' Creates a function that loads the data used for the player cards
#'
#' @param league League ID for the chosen league
#' @param season Season to take the data from
#'
#' @export
#'
#' @returns
#' Data frame of the chosen league and season

dataLoader <- function(league, season = NULL){
  ### Reads all players from the index
  players <- indStatsLoader(league, season = season, type = "regular")

  ### Reads team information from the index
  teams <-
    teamLoader(league, season = season) %>%
    dplyr::left_join(
      addTeamLogo(teamInfo) %>%
        dplyr::select(
          .data$team,
          .data$logoImage
        ),
      by = c("name" = "team")
    )


  ### Splits the data sets ### MOVE THIS ONE TO LOWER WHEN BUILDING GOALIE FUNCTIONS
  goalies <-
    players$goalies

  ##----------------------------------------------------------------
  ##                      Adding new variables                     -
  ##----------------------------------------------------------------

  skaters <-
    players$players %>%
    tidyr::unnest(cols = "advancedStats") %>%

    dplyr::rename_with(
      .cols = .data$PDO:.data$FFPctRel,
      ~ paste("advancedStats", .x, sep = ".")
    ) %>%

    ## Creates takeaway to giveaway ratio
    ## Creates group for forwards and defencemen
    dplyr::mutate(
      `TA_GA` =
        dplyr::case_when(
          .data$giveaways > 0 ~ .data$takeaways/.data$giveaways,
          .data$giveaways == 0 & .data$takeaways > 0 ~ 100,
          TRUE ~ 0
        ),
      posGroup =
        dplyr::case_when(
          .data$position %in% c("LD", "RD") ~ "Def",
          TRUE ~ "For"
        )
    ) %>%
    dplyr::relocate(
      .data$`TA_GA`,
      .after = "takeaways"
    ) %>%
    ## Converts time on ice variables to numeric values in seconds
    dplyr::mutate(
      dplyr::across(
        dplyr::contains("timeOnIce"),
        ~ .x %>%
          lubridate::ms() %>%
          as.numeric()
      ),
      ## Creates Even Strength Time On Ice
      esTimeOnIce = .data$timeOnIce - .data$ppTimeOnIce - .data$shTimeOnIce
    ) %>%
    dplyr::select(
      -.data$id, -.data$league, -.data$season
    ) %>%

    ## Calculates the Time On Ice rank within the team and skater group
    dplyr::group_by(.data$team, .data$posGroup) %>%
    dplyr::mutate(
      esRank = rank(-.data$esTimeOnIce, ties.method = "first"),
      ppRank = rank(-.data$ppTimeOnIce, ties.method = "first"),
      shRank = rank(-.data$shTimeOnIce, ties.method = "first")
    ) %>%
    ## Adds the nth text to the rank
    dplyr::mutate(
      dplyr::across(
        dplyr::contains("Rank"),
        ~ dplyr::case_when(
          .x %in% c(11,12,13) ~ paste(.x, "th", sep = ""),
          .x %% 10 == 1 ~ paste(.x, "st", sep = ""),
          .x %% 10 == 2 ~ paste(.x, "nd", sep = ""),
          .x %% 10 == 3 ~ paste(.x, "rd", sep = ""),
          TRUE ~ paste(.x, "th", sep = "")
        )
      )
    ) %>%
    dplyr::ungroup() %>%
    ## Relocates variables
    dplyr::relocate(
      .data$esRank:.data$shRank,
      .after = .data$team
    ) %>%
    dplyr::relocate(
      .data$esTimeOnIce,
      .after = .data$timeOnIce
    )

  ##---------------------------------------------------------------
  ##                  Aggregating team statistics                 -
  ##---------------------------------------------------------------

  teamSkater <-
    skaters %>%
    dplyr::group_by(.data$team) %>%
    ## Calculates the sum and mean where applicable ### SHOULD MOVE GAME RATINGS TO MEAN
    dplyr::summarize(
      dplyr::across(
        .data$timeOnIce:.data$defensiveGameRating,
        sum
      ),
      dplyr::across(
        dplyr::contains("advancedStats"),
        mean
      )
    )

  ##----------------------------------------------------------------
  ##    Transforming statistics in units of standard deviation     -
  ##----------------------------------------------------------------

  teamSkaterZ <-
    teamSkater %>%
    dplyr::mutate(
      dplyr::across(
        .data$timeOnIce:.data$advancedStats.FFPctRel,
        ~ .x %>%
          as.numeric() %>%
          scale()
      )
    ) %>%

    ## Changes direction of AGAINST stats, larger values are worse so they become negative
    dplyr::mutate(
      dplyr::across(
        dplyr::contains(
          "advancedStats.GA60"
        ),
        ~ -.x
      ),
      dplyr::across(
        dplyr::contains(
          "advancedStats.SA60"
        ),
        ~ -.x
      ),
      dplyr::across(
        dplyr::contains(
          "advancedStats.CA"
        ),
        ~ -.x
      ),
      dplyr::across(
        dplyr::contains(
          "advancedStats.FA"
        ),
        ~ -.x
      )
    )

  ### Standardizing skaters
  skatersZ <-
    skaters %>%
    ## Calculates the normalized value (z-score) of all players' variables
    ## The z-score is the distance in units of standard deviation from the league mean
    dplyr::mutate(
      dplyr::across(
        .data$timeOnIce:.data$advancedStats.FFPctRel,
        ~ .x %>%
          as.numeric() %>%
          scale()
      )
    ) %>%
    ## Changes direction of AGAINST stats, larger values are worse so they become negative
    dplyr::mutate(
      dplyr::across(
        dplyr::contains(
          "advancedStats.GA60"
        ),
        ~ -.x
      ),
      dplyr::across(
        dplyr::contains(
          "advancedStats.SA60"
        ),
        ~ -.x
      ),
      dplyr::across(
        dplyr::contains(
          "advancedStats.CA"
        ),
        ~ -.x
      ),
      dplyr::across(
        dplyr::contains(
          "advancedStats.FA"
        ),
        ~ -.x
      )
    )

  ### Calculates the percentile of the standardized values
  skatersZRank <-
    skatersZ %>%
    dplyr::mutate(
      dplyr::across(
        .data$timeOnIce:.data$advancedStats.FFPctRel,
        ~ dplyr::percent_rank(.x)
      )
    )

  ### Adds percentile data to the standardized stats
  skatersZ <-
    skatersZ %>%
    dplyr::left_join(
      skatersZRank,
      by =
        c("name", "position",
          "team", "esRank",
          "ppRank", "shRank",
          "gamesPlayed", "posGroup"),
      suffix = c(".ind", ".rank")
    )

  ### Removes text from the colnames
  colnames(skatersZ) <-
    colnames(skatersZ) %>%
    stringr::str_remove_all("advancedStats.")

  colnames(teamSkaterZ) <-
    colnames(teamSkaterZ) %>%
    stringr::str_remove_all("advancedStats.")

  ### Returns list of standardized skater and team stats as well as merged teamInfo with index data
  list(
    skaters = skatersZ,
    team = teamSkaterZ,
    teamInfo = teams,
    season = players$players$season %>% unique()
  ) %>%
    return()
  ##----------------------------------------------------------------
}

#' Creates a function that filters and processes the league data given a chosen player
#'
#' @param chosen Player chosen to visualize
#' @param leagueData The league data of the chosen league
#'
#' @returns
#' Returns a data frame with the data of the chosen player for the player card
#'
#'

playerData <- function(chosen, leagueData){
  ## The league data used as an input is output from dataLoader()

  skatersZ <- leagueData$skaters

  teamSkaterZ <- leagueData$team

  teams <- leagueData$teamInfo

  ## Checks if the chosen player exists in the league data
  if(!any(skatersZ$name == chosen)){
    stop(
      c(
        "The player does not exist in the data. ",
        paste(
          "You might have meant:",
          agrep(
            pattern = chosen,
            x = skatersZ$name,
            value = TRUE
          )
        )
      )
    )
  }

  ## Selects the statistics that will be used for the player card
  selectedStats <-
    c(
      "goals",
      "assists",
      "points",
      "ppPoints",
      "hits",
      "TA_GA",
      "shotsBlocked",
      "CFPct",
      "FFPct"
    )

  ## Joins individual and team standardized values
  skatersZ <-
    skatersZ %>%
    dplyr::left_join(
      ## The team statistic is added with a suffix to distinguish the two statistics
      ## This is required as the individual statistics already has .ind so no duplicates will be joined
      teamSkaterZ %>%
        dplyr::rename_with(
          .cols = -.data$team,
          ~ paste0(.x,".teams")
        ),
      by = c("team")
    )

  ##----------------------------------------------------------------
  ##        Creates the data for the initial visualization                -
  ##----------------------------------------------------------------

  visData <-
    skatersZ %>%

    ## Filters the chosen player
    dplyr::filter(
      .data$name == chosen
    ) %>%

    ## Joins team information, specifically the team colors
    dplyr::left_join(
      teams %>%
        dplyr::select(
          .data$abbreviation,
          .data$colors
        ),
      by = c("team" = "abbreviation")
    ) %>%
    dplyr::relocate(
      c(.data$colors),
      .after = .data$team
    ) %>%

    ## Rescales time on ice statistics to original scale
    dplyr::mutate(
      dplyr::across(
        dplyr::contains("timeOnIce.ind"),
        ~ reScale(.x) %>%
          ## Converts the value to a lubridate format
          lubridate::seconds_to_period()
      ),

      ## Adds a 0 to values that are less than 10 in order to create a good output format
      ## 1:1 or 01:01 for 1 minute and 1 second
      dplyr::across(
        dplyr::contains("timeOnIce.ind"),
        ~ paste(
          dplyr::if_else(
            lubridate::minute(.x) > 9,
            lubridate::minute(.x) %>% round(0) %>% as.character(),
            paste(0, lubridate::minute(.x) %>% round(0), sep = "")
          ),
          dplyr::if_else(
            lubridate::second(.x) > 9,
            lubridate::second(.x) %>% round(0) %>% as.character(),
            paste(0, lubridate::second(.x) %>% round(0), sep = "")
          ),
          sep = ":")
      )
    ) %>%

    ## Unnests the data frame to one level
    tidyr::unnest(
      cols = "colors"
    ) %>%
    # ## This does not work anymore, wants a list as second argument
    # do.call(
    #   what = data.frame,
    #   args = .data
    # ) %>%

    ## Adds team information again but this time only the logo image
    dplyr::left_join(
      addTeamLogo(teamInfo) %>%
        ## Both Anaheim and Tampa Bay Barracuda occur twice so this picks the latest graphics
        dplyr::group_by(.data$abbr) %>%
        dplyr::filter(
          .data$Inaugural.Season == max(.data$Inaugural.Season)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(
          .data$abbr,
          .data$logoImage
        ),
      by = c("team" = "abbr")
    ) %>%
    dplyr::relocate(
      .data$logoImage,
      .after = .data$team
    )

  ##----------------------------------------------------------------
  ##        Creates the data for the statistic visualization       -
  ##----------------------------------------------------------------

  ### Further processes the visualization data to value, group and id columns (pivot_longer)
  visDataLong <-
    visData %>%

    ## Only pivots the statistic variables, not identifiers
    tidyr::pivot_longer(
      -c(
        .data$name:.data$gamesPlayed,
        dplyr::contains("timeOnIce"),
        .data$posGroup
      ),
      ## The .value indicates that the value variable will be split based on a suffix
      names_to = c("stat", ".value"),
      ## The value split is based on column names with . as a splitting character
      names_pattern = "([^\\.]+)\\.(.*)$"
    ) %>%
    ## Renames the two value variables from pivot_longer to better names
    dplyr::rename(
      Individual = .data$ind,
      Team = .data$teams
    ) %>%

    ## Pivots even more to get individual and team statistics as observations instead of variables
    ## measure_type indicates if the statistic is individual or team
    ## score indicates what the value of the statistic is (normalized)
    tidyr::pivot_longer(
      c(.data$Individual, .data$Team),
      names_to = "measure_type",
      values_to = "score"
    ) %>%

    ## Filter the selected stats for the player card
    dplyr::filter(
      .data$stat %in% selectedStats
    ) %>%
    dplyr::mutate(
      ## Reformats the statistic (group) variable to a factor with a specific order set in the order of selectedStats
      ## Changes the labels to easier read ones
      stat =
        factor(
          .data$stat,
          levels =
            selectedStats,
          labels =
            c(
              "Goals",
              "Assists",
              "Points",
              "PP Points",
              "Hits",
              "TkA/GvA",
              "Blocked Shots",
              "Corsi For %",
              "Fenwick For %"
            )
        ),
      ## Round the percentile to 3 decimal places
      rank = .data$rank %>% round(3),
      ## Pastes the percentile value alongside the statistic name for use as a facet title
      statPerc =
        paste0(.data$stat, "\n(", .data$rank*100, "%)") %>%
        factor(
          levels = unique(.)
        )
    ) %>%
    ## Removes all Time On Ice stats from the visualization
    dplyr::select(
      -(.data$timeOnIce.rank:.data$shTimeOnIce.teams)
    )

  list(
    initialData = visData,
    statData = visDataLong
  ) %>%
    return()
}

#' Creates a function that outputs a specific player card from a league
#'
#' @param chosen Player chosen to visualize
#' @param leagueData The league data of the chosen league
#'
#' @export
#'
#' @returns
#' Returns an image of the player card

playerCard <- function(chosen, leagueData){

  data <- playerData(chosen, leagueData)

  visData <- data$initialData

  visDataLong <- data$statData

  ## Checks if the player name is too long for the default size (14)
  if(nchar(chosen) > 12){
    mainTextSize <- 10
  } else {
    mainTextSize <- 14
  }

  ### Creates the initial base for the visualization
  ##  Uses the visualization data on a plain background colored by the team colors
  main <-
    ggplot2::ggplot(visData) + ggplot2::theme_minimal() +

    ## Creates the title
    ggplot2::annotate(
      "text",
      x = 4.25,
      y = 29.5,
      label = paste(visData$name, " (", visData$position,")", sep = ""),
      color = "white", # visData$colors.text was used but dark on dark occurred
      size = mainTextSize,
      family = "title",
      hjust = 0.5
    ) +

    ## Sets the dimensions of the plot
    ggplot2::coord_cartesian(
      ylim = c(0, 30),
      xlim = c(0, 10)
    ) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme(
      panel.background =
        ggplot2::element_rect(
          fill = visData$primary
        ),
      panel.grid = ggplot2::element_blank(),
      plot.margin = ggplot2::unit(c(0,0,-0.75,-0.75), "cm")
    ) +
    ggplot2::scale_x_continuous(breaks = NULL) +
    ggplot2::scale_y_continuous(breaks = NULL)




  ##---------------------------------------------------------------
  ##              Creates the statistic visualization             -
  ##---------------------------------------------------------------

  statistics <-
    ggplot2::ggplot(visDataLong) +

    ## The y (height) is the standardized score
    ggplot2::aes(x = 1, y = .data$score) +

    ## A bar chart where the fill is defined by the score
    ## Group defines that the individual and team scores should be included
    ## dodge puts the two bars side by side
    ggplot2::geom_bar(
      ggplot2::aes(fill = .data$score, group = .data$measure_type),
      stat = "identity",
      position = "dodge"
    ) +

    ## Creates a facet grid based on the statistic (including the percentile)
    ggplot2::facet_wrap(
      ggplot2::vars(.data$statPerc),
      nrow = 3
    ) +

    ## Sets the viewport of the diagram to always show from -3 to 3 standard deviations from the mean
    ggplot2::coord_cartesian(
      ylim = c(-3, 3)
    ) +

    ## Changes theme
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    ) +

    ## Removes x-axis scale
    ggplot2::scale_x_discrete(
      breaks = NULL
    ) +

    ## Changes y-axis scale to show breaks at every 1.5 steps
    ggplot2::scale_y_continuous(
      breaks =
        seq(-3, 3, 1.5)
    ) +

    ## The fill of the bars are set by their score from muted red (bad) to muted green (good)
    ggplot2::scale_fill_gradientn(
      ggplot2::aes(fill = .data$score),
      limits = c(-3,3),
      colors =
        c(
          scales::muted("red"),
          "white",
          scales::muted("green")
        ),
      guide = "none",
      ## If the score is outside the limits the value is squished to the min/max (otherwise it would be gray)
      oob = scales::squish
    ) +

    ## As the team bar is not of interest, a new fill scale is added
    ggnewscale::new_scale_fill() +

    # ## This new one is a bar chart on top of the earlier, filled by measure_type
    ggplot2::geom_bar(
      ggplot2::aes(fill = .data$measure_type),
      color = "black",
      stat = "identity",
      position = "dodge"
    ) +
    ## The individual bars is filled with NA to show the earlier bar color, team bar is just gray
    ggplot2::scale_fill_manual(
      "",
      values = c(ggplot2::alpha("white", 0.0), "gray") # Team bar tested with visData$colors.secondary, but it was too colorful
    ) +
    ## Creates a tougher horizontal line on y = 0 to indicate league mean
    ggplot2::geom_hline(yintercept = 0, size = 1) +

    ## Revises the theme to make the text and data easy to read
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "white"),
      strip.text = ggplot2::element_text(size = 12),

      plot.background = ggplot2::element_rect(fill = NA, color = NA),

      text = ggplot2::element_text(color = "white", family = "body"),

      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(color = "white", size = 10),
      axis.ticks.y = ggplot2::element_line(color = "white"),

      legend.background = ggplot2::element_rect(fill = NA, color = NA),
      legend.text = ggplot2::element_text(size = 10)
    ) +
    ggplot2::labs(x = "", y = "z-score")

  ##----------------------------------------------------------------
  ##          Building the card using ggplot and svg files         -
  ##----------------------------------------------------------------

  ggCard <-
    ## Starts with the plain background and title
    main +

    ## Add the statistic data
    ggplot2::annotation_custom(
      statistics %>% ggplot2::ggplotGrob(),
      xmin = 0,
      xmax = 10.5,
      ymin = 1,
      ymax = 26
    ) +

    ## Adds informative text on how to read the data on the bottom
    ggplot2::annotate(
      "text",
      x = 0,
      y = 0.25,
      hjust = 0,
      label =
        c("A z-score is the player's, or team's average, unit of standard deviation from the league mean value.\n\n",
          "The time on ice ranking is based on positional (forward/defenseman) ranking in the team."),
      # \nA high Corsi against is bad, so the scale is reversed to correspond to the same color gradient."
      size = 3.5,
      color = "white"
    ) +

    ## Adds subtitle
    ggplot2::annotate(
      "text",
      x = 1.5,
      y = 27,
      label = "Statistic and percentile of players in the league",
      size = 6,
      color = "white",
      family = "body",
      # fontface = 2,
      hjust = 0
    ) +

    ## Adds author in bottom right
    ggplot2::annotate(
      "text",
      x = 10,
      y = 0.25,
      hjust = 0.5,
      label = "Created by: \nCanadice",
      size = 3.5,
      color = "white"
    ) +

    ## Adds season of the data
    ggplot2::annotate(
      "text",
      x = 0.20,
      y = 27,
      label = paste("S", leagueData$season, sep = ""),
      size = 8,
      color = "white",
      family = "body"
    ) +

    ## Adds the time on ice as information along the right hand side of the card
    ggplot2::annotate(
      "text",
      x = 9.7,
      y = 21,
      family = "body",
      color = "white",
      size = 4.25,
      label =
        paste(
          paste(
            c(
              "ESTOI",
              "PPTOI",
              "SHTOI"
            ),
            c(
              visData$esTimeOnIce.ind,
              visData$ppTimeOnIce.ind,
              visData$shTimeOnIce.ind
            ),
            sep = " - "
          ),
          paste(
            .data,
            paste(
              "(",
              c(
                visData$esRank,
                visData$ppRank,
                visData$shRank
              ),
              ")",
              sep = ""
            ),
            sep = "\n"
          )
        ) %>%
        paste(collapse = "\n")
    )

  ### Initialize the drawing into the variable card
  card <- magick::image_graph(res = 96)
  print(ggCard)
  grDevices::dev.off()

  ### Using magick to add the svg image of the logo to the top right of the card
  card %>%
    magick::image_composite(
      visData$logoImage[[1]] %>%
        magick::image_resize(100),
      offset = "+680+15",
    )
}
