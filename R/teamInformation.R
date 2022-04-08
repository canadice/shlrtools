
##################################################################
##                 Loads svg files of the logos                 ##
##################################################################
#* @export
addTeamLogo <- function(teamInfo){
  raw <- "https://raw.githubusercontent.com/canadice/shl/main/"

  teamInfo %>%
    dplyr::mutate(
      logoImage =
        dplyr::case_when(
          team == "Calgary Dragons" ~ magick::image_read_svg(paste(raw, "graphics/Calgary.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Hamilton Steelhawks" ~ magick::image_read_svg(paste(raw, "graphics/Hamilton.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Manhattan Rage" ~ magick::image_read_svg(paste(raw, "graphics/Manhattan.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Toronto North Stars" ~ magick::image_read_svg(paste(raw, "graphics/Toronto.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Los Angeles Panthers" ~ magick::image_read_svg(paste(raw, "graphics/Los_Angeles.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Edmonton Blizzard" ~ magick::image_read_svg(paste(raw, "graphics/Edmonton.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Texas Renegades" ~ magick::image_read_svg(paste(raw, "graphics/Texas.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "New England Wolfpack" ~ magick::image_read_svg(paste(raw, "graphics/New_England.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Buffalo Stampede" ~ magick::image_read_svg(paste(raw, "graphics/Buffalo.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "San Francisco Pride" ~ magick::image_read_svg(paste(raw, "graphics/San_Francisco.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Chicago Syndicate" ~ magick::image_read_svg(paste(raw, "graphics/Chicago.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "New Orleans Specters" ~ magick::image_read_svg(paste(raw, "graphics/New_Orleans.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Baltimore Platoon" ~ magick::image_read_svg(paste(raw, "graphics/Baltimore.svg", sep = ""), width = 600, height = 600) %>% list(),
          team == "Atlanta Inferno" ~ magick::image_read_svg(paste(raw, "graphics/Atlanta.svg", sep = ""), width = 600, height = 600) %>% list(),
          team == "Minnesota Monarchs" ~ magick::image_read_svg(paste(raw, "graphics/Minnesota.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Seattle Argonauts" ~ magick::image_read_svg(paste(raw, "graphics/Seattle.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Tampa Bay Barracuda" ~ magick::image_read_svg(paste(raw, "graphics/Tampa_Bay.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Winnipeg Aurora" ~ magick::image_read_svg(paste(raw, "graphics/Winnipeg.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Montreal Patriotes" ~ magick::image_read_svg(paste(raw, "graphics/Montreal.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Philadelphia Forge" ~ magick::image_read_svg(paste(raw, "graphics/Philadelphia.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Vancouver Whalers" ~ magick::image_read_svg(paste(raw, "graphics/Vancouver.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Kelowna Knights" ~ magick::image_read_svg(paste(raw, "graphics/Kelowna.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Detroit Falcons" ~ magick::image_read_svg(paste(raw, "graphics/Detroit.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "St. Louis Scarecrows" ~ magick::image_read_svg(paste(raw, "graphics/St_Louis.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Anchorage Armada" ~ magick::image_read_svg(paste(raw, "graphics/Anchorage.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Colorado Raptors" ~ magick::image_read_svg(paste(raw, "graphics/Colorado.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Anaheim Outlaws" & Inaugural.Season == 45 ~ magick::image_read_svg(paste(raw, "graphics/Anaheim.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Carolina Kraken" ~ magick::image_read_svg(paste(raw, "graphics/Carolina.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Newfoundland Berserkers" ~ magick::image_read_svg(paste(raw, "graphics/Newfoundland.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Maine Timber" ~ magick::image_read_svg(paste(raw, "graphics/Maine.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Nevada Battleborn" ~ magick::image_read_svg(paste(raw, "graphics/Nevada.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Quebec City Citadelles" ~ magick::image_read_svg(paste(raw, "graphics/Quebec_City.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Austria" ~ magick::image_read_svg(paste(raw, "graphics/Austria.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Canada" ~ magick::image_read_svg(paste(raw, "graphics/Canada.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Czechia" ~ magick::image_read_svg(paste(raw, "graphics/Czechia.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Finland" ~ magick::image_read_svg(paste(raw, "graphics/Finland.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Germany" ~ magick::image_read_svg(paste(raw, "graphics/Germany.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Great Britain" ~ magick::image_read_svg(paste(raw, "graphics/Great_Britain.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Ireland" ~ magick::image_read_svg(paste(raw, "graphics/Ireland.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Japan" ~ magick::image_read_svg(paste(raw, "graphics/Japan.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Latvia" ~ magick::image_read_svg(paste(raw, "graphics/Latvia.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Norway" ~ magick::image_read_svg(paste(raw, "graphics/Norway.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Russia" ~ magick::image_read_svg(paste(raw, "graphics/Russia.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Sweden" ~ magick::image_read_svg(paste(raw, "graphics/Sweden.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Switzerland" ~ magick::image_read_svg(paste(raw, "graphics/Switzerland.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "USA" ~ magick::image_read_svg(paste(raw, "graphics/United_States.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Unassigned" ~ magick::image_read_svg(paste(raw, "graphics/SHL_old.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "British Isles" ~ magick::image_read_svg(paste(raw, "graphics/British_Isles.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "DACH" ~ magick::image_read_svg(paste(raw, "graphics/DACH.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "North America" ~ magick::image_read_svg(paste(raw, "graphics/North_America.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "UCORCAL" ~ magick::image_read_svg(paste(raw, "graphics/UCORCAL.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "World" ~ magick::image_read_svg(paste(raw, "graphics/World.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Young Guns" ~ magick::image_read_svg(paste(raw, "graphics/Young_Guns.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Canada Red" ~ magick::image_read_svg(paste(raw, "graphics/Canada_Red.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Canada White" ~ magick::image_read_svg(paste(raw, "graphics/Canada_White.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "United States Blue" ~ magick::image_read_svg(paste(raw, "graphics/United_States_Blue.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "United States Red" ~ magick::image_read_svg(paste(raw, "graphics/United_States_Red.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Great Falls Grizzlies" ~ magick::image_read_svg(paste(raw, "graphics/Great_Falls.svg",sep = ""), width = 600, height = 600) %>% list(),
          team == "Regina Elk" ~ magick::image_read_svg(paste(raw, "graphics/Regina.svg",sep = ""), width = 600, height = 600) %>% list(),
          TRUE ~ NA  %>% list()
        )
    )
}
