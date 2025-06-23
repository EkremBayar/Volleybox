# 1. Packages -------------------------------------------------------------
library(tidyverse)
library(rvest)
library(RPostgres)
library(foreach)
library(parallel)
library(lubridate)
library(doParallel)



# 2. Volleybox Get Players ------------------------------------------------
url <- "https://women.volleybox.net/ajax/get_players"
page <- read_html(url)

get_players <- data.frame(
  Player = page %>% html_nodes("a") %>% html_text() %>% str_squish(),
  Url =  page %>% html_nodes("a") %>% html_attr("href")
) %>% 
  mutate(
    Id = str_split_i(Url, "-", -1),
    Detail = str_split_i(Url, "/", -1),
    Detail = str_to_title(str_squish(str_replace_all(str_remove_all(Detail, Id), "-", " ")))
  ) %>% 
  select(Id, Player, Detail, Url)
rm(url, page)


# 3.  Volleybox Player Bio ------------------------------------------------
get_player_bio <- function(url){
  page <- read_html(url)
  
  # Id
  pid <- str_split_i(url, "-", -1)
  
  # Player Name
  name <- page %>% html_node("h1") %>% html_text() %>% str_remove_all("TOP 100|TOP 200|TOP 500") %>% str_squish()
  nickname <- str_split_i(str_split_i(name, "\\(", 2), "\\)", 1)
  name <- str_split_i(name, "\\(", 1) %>% str_squish()
  
  # Top100
  top100_badge <- page %>% html_nodes(".item-medal-filled.item-medal-filled-1") %>% html_text()
  top100_badge <- ifelse(length(top100_badge) == 0, "No", top100_badge)
  top100_badge <- ifelse(str_to_upper(top100_badge) == "TOP 100", "Yes", "No")
  
  top200_badge <- page %>% html_nodes(".item-medal-filled.item-medal-filled-2") %>% html_text()
  top200_badge <- ifelse(length(top200_badge) == 0, "No", top200_badge)
  top200_badge <- ifelse(str_to_upper(top200_badge) == "TOP 200", "Yes", "No")
  
  top500_badge <- page %>% html_nodes(".item-medal-filled.item-medal-filled-3") %>% html_text()
  top500_badge <- ifelse(length(top500_badge) == 0, "No", top500_badge)
  top500_badge <- ifelse(str_to_upper(top500_badge) == "TOP 500", "Yes", "No")
  
  top200_badge <- ifelse(top100_badge == "Yes" & top200_badge == "No", "Yes", top200_badge)
  top500_badge <- ifelse(top200_badge == "Yes" & top500_badge == "No", "Yes", top500_badge)
  top500_badge <- ifelse(top100_badge == "Yes" & top500_badge == "No", "Yes", top500_badge)
  
  # Registered on Volleybox?
  registered <- page %>% html_node("div.user-rounded-icon-container i") %>% html_attr("title")
  registered <- ifelse(!is.na(registered), "Yes", "No")
  
  # Profile Img
  img <- page %>% html_node(".profile_header_container") %>% html_node("img") %>% html_attr("src")
  
  bio1 <- data.frame(
    V1 = c("Id", "Name", "Nickname", "Top100", "Top200", "Top500", "Registered", "Url","Image"),
    V2 = c(pid, name, nickname, top100_badge, top200_badge, top500_badge, registered, url, img)
  )
  
  
  # Bio
  bio2 <- data.frame(
    V1 = page %>% html_nodes(".new_box.pRelative dt.info-header.tar") %>% html_text(),
    V2 = page %>% html_nodes(".new_box.pRelative dd.info-data") %>% html_text()
  )
  
  bio <- bind_rows(bio1, bio2)
  rm(pid, name, nickname, top100_badge, registered, img, bio1, bio2)
  
  # Agent
  agent <- page %>% html_nodes(".new_box.pRelative div.box_content.dTableCell") %>% html_text() %>% str_squish()
  agent <- ifelse(length(agent) == 0, NA_character_, agent)
  
  # Social Media
  sm <- page %>% html_nodes(".profile-buttons-container a") %>% html_attr("href")
  if(length(sm) > 0){
    
    twitter <- sm[str_detect(sm, "twitter")]
    facebook <- sm[str_detect(sm, "facebook")]
    instagram <- sm[str_detect(sm, "instagram")]
    youtube <- sm[str_detect(sm, "youtube")]
    linkedin <- sm[str_detect(sm, "linkedin")]
    website <- sm[!str_detect(sm, "linkedin|youtube|instagram|facebook|twitter")]
    
    twitter <- ifelse(length(twitter) == 0, NA_character_, twitter)
    facebook <- ifelse(length(facebook) == 0, NA_character_, facebook)
    instagram <- ifelse(length(instagram) == 0, NA_character_, instagram)
    youtube <- ifelse(length(youtube) == 0, NA_character_, youtube)
    linkedin <- ifelse(length(linkedin) == 0, NA_character_, linkedin)
    website <- ifelse(length(website) == 0, NA_character_, website)
  }else{
    twitter <- NA_character_
    facebook <- NA_character_
    instagram <- NA_character_
    youtube <- NA_character_
    linkedin <- NA_character_
    website <- NA_character_
  }
  sm <- data.frame(
    V1 = c("Agent", "Twitter", "Facebook", "Instagram", "Youtube", "Linkedin", "Website"),
    V2 = c(agent, twitter, facebook, instagram, youtube, linkedin, website)
  )
  
  bio <- bind_rows(bio, sm)
  rm(agent, twitter, facebook, instagram, youtube, linkedin, website)
  
  # Tabs
  tabs <- page %>% html_nodes("ul#tabs-player") %>% html_nodes("li span") %>% as.character() %>% str_remove_all("<span>|</span>|</var>") %>% str_split("<var>", simplify = T) %>% as.data.frame()
  bio <- bind_rows(bio, tabs)
  rm(tabs)
  
  # Experience
  ce <- page %>% html_nodes(".new_box.player-experience") %>% html_nodes(".team_arena_box")
  if(length(ce)>0){  
    
    ce <- lapply(ce, function(x){
    
      data.frame(
        Season = x %>% html_nodes(".div-1-1 div.seasons") %>% html_text() %>% str_squish(),
        Club = x %>% html_nodes(".div-1-1 a.arenaName") %>% html_text() %>% str_squish(),
        ClubUrl = x %>% html_nodes(".div-1-1 a.arenaName") %>% html_attr("href"),
        ClubLogo = img <- x %>% html_node("a.image_link img") %>% html_attr("data-src"),
        LeagueCountry = x %>% html_nodes(".div-1-1 img.flag") %>% html_attr("alt") ,
        LeagueCountryFlag = x %>% html_nodes(".div-1-1 img.flag") %>% html_attr("src")
        
      )
      
    }) %>% bind_rows() %>% 
      mutate(ClubId = str_split_i(str_split_i(ClubUrl, "/", -2), "-", -1)) %>% 
      select(Season:Club, ClubId, ClubUrl:LeagueCountryFlag) %>% 
      # Last Team
      head(1) %>% 
      # Active Player
      mutate(ActivePlayer = ifelse(str_detect(Season, "2023/24|still"), "Yes", "No"))
  }else{
    ce<-tibble()
  }

  
  
  bio <- as.data.frame(t(bio), row.names = F)
  names(bio) <- bio[1, ]
  bio <- bio[2, ]
  bio <- bio %>% 
    mutate(
      Ranking = tryCatch({as.integer(Ranking)}, error = function(e){NA_integer_}),
      `Ranking in 2023` = tryCatch({as.integer(`Ranking in 2023`)}, error = function(e){NA_integer_}),
      Birthdate = tryCatch({ymd(Birthdate)}, error = function(e){NA_Date_}),
      Views = tryCatch({as.integer(Views)}, error = function(e){NA_integer_}),
      Timeline = tryCatch({as.integer(Timeline)}, error = function(e){NA_integer_}),
      Clubs = tryCatch({as.integer(Clubs)}, error = function(e){NA_integer_}),
      Teammates = tryCatch({as.integer(Teammates)}, error = function(e){NA_integer_}),
      Awards = tryCatch({as.integer(Awards)}, error = function(e){NA_integer_}),
      Tournaments = tryCatch({as.integer(Tournaments)}, error = function(e){NA_integer_}),
      Matches = tryCatch({as.integer(Matches)}, error = function(e){NA_integer_}),
      Followers = tryCatch({as.integer(Followers)}, error = function(e){NA_integer_}),
      Height = tryCatch({as.integer(str_remove_all(Height, "cm"))}, error = function(e){NA_integer_}),
      Weight = tryCatch({as.integer(str_remove_all(Weight, "kg"))}, error = function(e){NA_integer_}),
      Spike = tryCatch({as.integer(str_remove_all(Spike, "cm"))}, error = function(e){NA_integer_}),
      Block = tryCatch({as.integer(str_remove_all(Block, "cm"))}, error = function(e){NA_integer_})
    ) %>% 
    rename(
      `Height (cm)` = Height,
      `Weight (kg)` = Weight,
      `Spike (cm)` = Spike,
      `Block (cm)` = Block,
    )
  if(nrow(ce)>0){
    bio <- bind_cols(bio, ce)
  }
  
  
  return(bio)
}


# url <- "https://women.volleybox.net/hatice-gizem-orge-p1719"
# url <- "https://women.volleybox.net/tijana-boskovic-p1961"
# get_player_bio(url)



# 4. Parallel Programming -------------------------------------------------
# parallel::detectCores()
cl <- makeCluster(10, outfile = "")
registerDoParallel(cl, cores = 10)  
iterations <- nrow(get_players)
      
pbio_df3 <- foreach (
  i=1:5818, #.noexport="con", 
  .combine=bind_rows, # Eğer liste değilde dataframe bastırırsan oto bind rows yapar
  .packages = c("tidyverse", "lubridate", "rvest")
) %dopar% {
  
  print(i)
  url <- b[i, "Url"]
  
  tryCatch({
    
    get_player_bio(url)
    
  },error = function(e){message(paste0(i, e))})
  
}
stopCluster(cl)


1

write.csv(bind_rows(pbio_df2, pbio_df3), "volleybox.csv", row.names = F)



a<-bind_rows(pbio_df2, pbio_df3)

b<-get_players %>% filter(!Id %in% a$Id)

d <-bind_rows(a, pbio_df3)


d <- d %>% 
  mutate(
    ActivePlayer = ifelse(str_detect(Season, "2023/24|still"), "Yes", ActivePlayer))

d <- d %>% 
  mutate(
    Top200 = ifelse(str_detect(Name, "TOP 200"), "Yes", "No"),
    Top500 = ifelse(str_detect(Name, "TOP 500"), "Yes", "No"),
    Name = str_remove_all(Name, "TOP 100|TOP 200|TOP 500") 
  )

d$Top200 <- ifelse(d$Top100 == "Yes" & d$Top200 == "No", "Yes", d$Top200)
d$Top500 <- ifelse(d$Top200 == "Yes" & d$Top500 == "No", "Yes", d$Top500)
d$Top500 <- ifelse(d$Top100 == "Yes" & d$Top500 == "No", "Yes", d$Top500)


d <- d %>% 
  select(
    "Id", "Name", "Nickname", "Maiden name", "Birthdate", "Nationality", "Position", "Dominant hand","Height (cm)", "Weight (kg)" , "Spike (cm)", "Block (cm)",
    "Url", "Image", "Season","Club","ClubId","ClubUrl", "ClubLogo","LeagueCountry","LeagueCountryFlag", "Agent", "Top100", "Top200", "Top500", "ActivePlayer",
    "Twitter","Facebook","Instagram","Youtube","Linkedin", "Website",
    "Timeline", "Clubs", "Teammates", "Tournaments", "Matches","Awards", 
    "Ranking", "Ranking in 2023", "Followers", "Player ranking", "Coach ranking",
    "Registered", "Views",  "Last activity",  "Added by",
    "User","Recommendations", "Partners" 
      
  )

d$`Coach ranking` <- as.integer(d$`Coach ranking`)

str(d)


write.csv(d, "volleybox_women_players.csv", row.names = F)


library(DT)

data <- d

data2 <- data %>% filter(str_detect(LeagueCountry, "Turkey")) %>% 
  mutate(
    Name = str_squish(str_remove_all(Name, "0")),
    Player = paste0('<img src="',Image,'" height="60"></img>', "<a style = 'font-size:13px; position: relative; bottom: 25px; left:10px;' href='", Url,"' target='_blank'>", Name,"</a>"),
    Club = paste0('<img src="',ClubLogo,'" height="40"></img>', "<a style = 'font-size:13px; position: relative; bottom: 14px; left:10px;' href='", ClubUrl,"' target='_blank'>", Club,"</a>", '<img style = "position: relative; bottom: 11px; left:15px;" src="',LeagueCountryFlag,'" height="15"></img>')
  ) %>% 
  arrange(Name) %>% 
  select(Player, Club, Season, "Birthdate", "Nationality", "Position", "Dominant hand","Height (cm)", "Weight (kg)" , "Spike (cm)", "Block (cm)", Agent, "Clubs", "Teammates", "Tournaments", "Matches","Awards", 
         "Ranking", "Player ranking", "Top100", "Top200", "Top500", "Registered", "ActivePlayer")

datatable(
  data2,
  rownames = F,
  escape = F,
  class = 'cell-border stripe compact nowrap',
  plugins = "ellipsis",
  filter = list(position = 'top', clear = TRUE, plain = F),
  options = list(
    searchHighlight = TRUE,
    autoWidth = TRUE,
    scrollX = TRUE, scrollY = 700,
    scrollCollapse=TRUE,
    pageLength = 50,
    lengthMenu = c(50, 100, 150, 200, 250),
    search = list(regex = TRUE, caseInsensitive = TRUE),
    columnDefs = list(
      list(className = 'dt-center', targets=2:(ncol(data2)-1)),
      list(width = '250px', targets = c(0,1))
    )
  )
) 




dt_header_ui <- function(datatable_id){
  res <- tagList(
    tags$style(type = "text/css",".noUi-connect {background: #39a642;}"), #lacivert 002749
    tags$style(HTML(paste0("#", datatable_id, ' table.dataTable tbody tr.selected>* {box-shadow: inset 0 0 0 9999px #C6E0B4; color: black;}'))),
    tags$head(tags$style(paste0("#", datatable_id," thead th{background-color: #002749; color: white;}"))),
    #tags$head(tags$style("#responses_table tbody td {border-top: 0.1px solid #002749;border-left: 0.1px solid #002749;border-right: 0.1px solid #002749;}")),
    tags$head(tags$style("#", datatable_id, " .dataTables_length {float:right;}"))
  )
  return(res)
}


df <- sbtmlist() %>% 
  select(-national_team, -national_team_status, -national_goals, -national_caps, -instagram, -twitter) %>% 
  mutate(Photo = paste0('<center><img src="',image_url,'" height="40"></img></center>'),
         Player = paste0("<a href='", url,"' target='_blank'>", Player,"</a>")) %>% 
  select(-Season, -SBId, -TMId, -Most.Recent.Match, -url, -SB_Team, -Competition) %>% 
  select(Photo,Player:Agent) %>% 
  rename_all(., list(~gsub("[[:punct:]]", " ", .)))

dtbutton <- function(tbl){
  function(i){
    sprintf(
      paste0('<button id="button_%s_%d" style="background-color:white;" type="button" onclick="%s"><img src="', "playeradd.png", '" width="20px"></button>'),
      tbl, i, "Shiny.setInputValue('button', this.id, {priority: 'event'});")
  }
}
#<i class="fa-solid fa-user-plus"></i>
df$Info <- sapply(1:nrow(df), dtbutton("button"))
df <- df %>% select(Info, Photo:Agent)

optlist1 <- list(
  
  columnDefs = list(
    list(className = 'dt-center', targets="_all"),
    list(targets = 3:(ncol(df)-1), render = JS("$.fn.dataTable.render.ellipsis( 10, false )"))
  )
)

datatable(
  df,
  rownames = FALSE, # Remove rownames
  escape = FALSE, #Hyperlink
  class = 'cell-border stripe compact ',
  plugins = "ellipsis",
  #class = "cell-border stripe display nowrap compact", # Borders
  selection = "single", # Satır seçimi
  filter = list(position = 'top', clear = TRUE, plain = F),
  #Options
  options = optlist1
) %>%
  formatCurrency("Market Value", currency = "€", digits = 0)










