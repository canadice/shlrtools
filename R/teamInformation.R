#' Loads svg images to each team
#'
#' @param teamInfo Data frame of the team information
#'
#' @returns
#' Returns a data.frame with an image pointer of the svg logo

addTeamLogo <- function(teamInfo){
  raw <- "https://raw.githubusercontent.com/canadice/shl/main/"

  teamInfo %>%
    dplyr::mutate(
      logoImage =
        dplyr::case_when(
          .data$team == "Calgary Dragons" ~ magick::image_read_svg(paste(raw, "graphics/Calgary.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Hamilton Steelhawks" ~ magick::image_read_svg(paste(raw, "graphics/Hamilton.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Manhattan Rage" ~ magick::image_read_svg(paste(raw, "graphics/Manhattan.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Toronto North Stars" ~ magick::image_read_svg(paste(raw, "graphics/Toronto.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Los Angeles Panthers" ~ magick::image_read_svg(paste(raw, "graphics/Los_Angeles.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Edmonton Blizzard" ~ magick::image_read_svg(paste(raw, "graphics/Edmonton.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Texas Renegades" ~ magick::image_read_svg(paste(raw, "graphics/Texas.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "New England Wolfpack" ~ magick::image_read_svg(paste(raw, "graphics/New_England.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Buffalo Stampede" ~ magick::image_read_svg(paste(raw, "graphics/Buffalo.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "San Francisco Pride" ~ magick::image_read_svg(paste(raw, "graphics/San_Francisco.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Chicago Syndicate" ~ magick::image_read_svg(paste(raw, "graphics/Chicago.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "New Orleans Specters" ~ magick::image_read_svg(paste(raw, "graphics/New_Orleans.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Baltimore Platoon" ~ magick::image_read_svg(paste(raw, "graphics/Baltimore.svg", sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Atlanta Inferno" ~ magick::image_read_svg(paste(raw, "graphics/Atlanta.svg", sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Minnesota Monarchs" ~ magick::image_read_svg(paste(raw, "graphics/Minnesota.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Seattle Argonauts" ~ magick::image_read_svg(paste(raw, "graphics/Seattle.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Tampa Bay Barracuda" ~ magick::image_read_svg(paste(raw, "graphics/Tampa_Bay.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Winnipeg Aurora" ~ magick::image_read_svg(paste(raw, "graphics/Winnipeg.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Montreal Patriotes" ~ magick::image_read_svg(paste(raw, "graphics/Montreal.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Philadelphia Forge" ~ magick::image_read_svg(paste(raw, "graphics/Philadelphia.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Vancouver Whalers" ~ magick::image_read_svg(paste(raw, "graphics/Vancouver.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Kelowna Knights" ~ magick::image_read_svg(paste(raw, "graphics/Kelowna.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Detroit Falcons" ~ magick::image_read_svg(paste(raw, "graphics/Detroit.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "St. Louis Scarecrows" ~ magick::image_read_svg(paste(raw, "graphics/St_Louis.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Anchorage Armada" ~ magick::image_read_svg(paste(raw, "graphics/Anchorage.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Colorado Raptors" ~ magick::image_read_svg(paste(raw, "graphics/Colorado.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Anaheim Outlaws" & Inaugural.Season == 45 ~ magick::image_read_svg(paste(raw, "graphics/Anaheim.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Carolina Kraken" ~ magick::image_read_svg(paste(raw, "graphics/Carolina.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Newfoundland Berserkers" ~ magick::image_read_svg(paste(raw, "graphics/Newfoundland.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Maine Timber" ~ magick::image_read_svg(paste(raw, "graphics/Maine.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Nevada Battleborn" ~ magick::image_read_svg(paste(raw, "graphics/Nevada.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Quebec City Citadelles" ~ magick::image_read_svg(paste(raw, "graphics/Quebec_City.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Austria" ~ magick::image_read_svg(paste(raw, "graphics/Austria.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Canada" ~ magick::image_read_svg(paste(raw, "graphics/Canada.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Czechia" ~ magick::image_read_svg(paste(raw, "graphics/Czechia.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Finland" ~ magick::image_read_svg(paste(raw, "graphics/Finland.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Germany" ~ magick::image_read_svg(paste(raw, "graphics/Germany.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Great Britain" ~ magick::image_read_svg(paste(raw, "graphics/Great_Britain.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Ireland" ~ magick::image_read_svg(paste(raw, "graphics/Ireland.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Japan" ~ magick::image_read_svg(paste(raw, "graphics/Japan.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Latvia" ~ magick::image_read_svg(paste(raw, "graphics/Latvia.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Norway" ~ magick::image_read_svg(paste(raw, "graphics/Norway.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Russia" ~ magick::image_read_svg(paste(raw, "graphics/Russia.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Sweden" ~ magick::image_read_svg(paste(raw, "graphics/Sweden.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Switzerland" ~ magick::image_read_svg(paste(raw, "graphics/Switzerland.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "USA" ~ magick::image_read_svg(paste(raw, "graphics/United_States.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Unassigned" ~ magick::image_read_svg(paste(raw, "graphics/SHL_old.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "British Isles" ~ magick::image_read_svg(paste(raw, "graphics/British_Isles.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "DACH" ~ magick::image_read_svg(paste(raw, "graphics/DACH.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "North America" ~ magick::image_read_svg(paste(raw, "graphics/North_America.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "UCORCAL" ~ magick::image_read_svg(paste(raw, "graphics/UCORCAL.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "World" ~ magick::image_read_svg(paste(raw, "graphics/World.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Young Guns" ~ magick::image_read_svg(paste(raw, "graphics/Young_Guns.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Canada Red" ~ magick::image_read_svg(paste(raw, "graphics/Canada_Red.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Canada White" ~ magick::image_read_svg(paste(raw, "graphics/Canada_White.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "United States Blue" ~ magick::image_read_svg(paste(raw, "graphics/United_States_Blue.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "United States Red" ~ magick::image_read_svg(paste(raw, "graphics/United_States_Red.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Great Falls Grizzlies" ~ magick::image_read_svg(paste(raw, "graphics/Great_Falls.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Regina Elk" ~ magick::image_read_svg(paste(raw, "graphics/Regina.svg",sep = ""), width = 600, height = 600) %>% list(),
          .data$team == "Yukon Malamutes" ~ image_read_svg(paste(raw, "graphics/Yukon.svg",sep = ""), width = 600, height = 600) %>% list(),
          TRUE ~ NA  %>% list()
        )
    )
}
