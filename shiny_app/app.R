library(shiny)
library(shinydashboard)
library(tidyverse)
# library(GAlogger)  # this isn't working since I updated R
library(rvest)
library(knitr)
library(kableExtra)


# start tracking ----------------------------------------------------------
# source("my_google_tracking_id.R")  # just contains my tracking ID (excluded from github)
# my_id <- my_google_tracking_id()
# ga_set_tracking_id(my_id)
# ga_set_approval(consent=TRUE)
# ga_collect_pageview(page="/test", title="test")


# define functions --------------------------------------------------------
get_league_id <- function(season) {
  if (season == 2018) { return("912438") }
}

scrape_team_info <- function(season) {
  league_id <- get_league_id(2018)
  
  # scrape the "managers" page
  url <- paste0("https://football.fantasysports.yahoo.com/f1/", league_id, "/teams")
  
  # get the HTML tables
  tables <- url %>% 
    read_html() %>% 
    html_nodes("table")
  
  # select the team names out of the correct table
  team.df <- tables[2] %>% 
    html_table() %>% 
    map_df(~ .) %>% 
    rename(name = `Team Name`) %>% 
    select(name)
  
  # get the team IDs from the HTML classes for the table rows
  team.ids <- tables[2] %>% 
    html_nodes("tr") %>% 
    html_attr("class") %>%  # classes are "team-1", "team-2", etc.
    str_split("-") %>% 
    map_chr(~ .x[2]) %>% 
    na.exclude() %>% 
    as.integer()
  
  team.df$id <- team.ids
  
  return(team.df)
}

team_id_to_name <- Vectorize(function(id, team_df=team.df) {
  team_df <- team_df %>% 
    bind_rows(data.frame(name="--BYE--", id=0, seed=0, stringsAsFactors=FALSE))
  return(team_df[which(team_df$id == id), "name"])
})

team_name_to_id <- Vectorize(function(name, team_df=team.df) {
  team_df <- team_df %>% 
    bind_rows(data.frame(name="--BYE--", id=0, seed=0, stringsAsFactors=FALSE))
  return(team_df[which(team_df$name == name), "id"])
})

team_seed_to_id <- Vectorize(function(seed, team_df=team.df) {
  team_df <- team_df %>% 
    bind_rows(data.frame(name="--BYE--", id=0, seed=0, stringsAsFactors=FALSE))
  return(team_df[which(team_df$seed == seed), "id"])
})

team_id_to_seed <- Vectorize(function(id, team_df=team.df) {
  team_df <- team_df %>% 
    bind_rows(data.frame(name="--BYE--", id=0, seed=0, stringsAsFactors=FALSE))
  return(team_df[which(team_df$id == id), "seed"])
})

scrape_team_from_roster <- function(team_id, week, season) {
  if (team_id == 0) { 
    df <- data.frame(pos="", name="", fan_pts="", proj_pts="", stringsAsFactors=FALSE)
    df <- df[-1,]
    return(df)
  }
  
  # scrape HTML
  league_id <- get_league_id(season)
  url <- paste0("https://football.fantasysports.yahoo.com/f1/", league_id, "/", 
                team_id, "/team?&week=", week)
  html_out <- read_html(url)
  
  # get the relevant tables with player and scoring data, then format data frames
  tables <- html_nodes(html_out, "table")
  offense.df <- tables[2] %>% roster_table_to_df()
  kicker.df <- tables[3] %>% roster_table_to_df()
  defense.df <- tables[4] %>% roster_table_to_df()
  
  # combine entire roster into single data frame
  roster.df <- offense.df %>% 
    bind_rows(kicker.df) %>% 
    bind_rows(defense.df)
  
  return(roster.df)
}

roster_table_to_df <- function(table) {
  out <- table %>% 
    html_table() 
  
  # assign the column names, then fix the column names
  df <- out[[1]]
  colnames(df) <- df[1,] %>% 
    str_to_lower() %>% 
    str_replace(" ", "_")
  df <- df[-1,]
  
  def.flag <- "DEF" %in% df$pos
  
  # extract the name
  df$name <- df[,2] %>% 
    str_split("\n") %>% 
    map_chr(~ .x[2]) %>% 
    trimws() %>% 
    str_split(" ") %>% 
    map_chr(~ paste0(paste(.x[1:(length(.x)-3)], collapse=" "),
                     ifelse(def.flag, "", paste0(" (", toupper(.x[length(.x)-2]), ")"))))
  
  df <- df[!duplicated(colnames(df))] %>% 
    mutate_at(c("fan_pts", "proj_pts"), as.numeric) %>% 
    select(pos, name, fan_pts, proj_pts)
  
  return(df)
}

get_active_roster <- function(roster_df=NULL, team_id=NULL, week=NULL, season=NULL,
                              bench_flag=FALSE) {
  if ((!is.null(roster_df) && nrow(roster_df) == 0) | (!is.null(team_id) && (team_id == 0))) { 
    df <- data.frame(pos="", name="", fan_pts="", proj_pts="", i=1, stringsAsFactors=FALSE)
    df <- df[-1,]
    return(df)
  }
  
  if (is.null(c(roster_df, team_id, week, season))) {
    return(NULL)
  }
  
  if (is.null(roster_df)) {
    roster.df <- scrape_team_from_roster(team_id=team_id, week=week, season=season)
  }
  
  if (bench_flag) {
    df <- roster_df %>% 
      filter(pos %in% c("BN", "IR")) %>% 
      group_by(pos) %>% 
      mutate(i = 1:n()) %>% 
      ungroup()
  } else {
    df <- roster_df %>% 
      filter(!(pos %in% c("BN", "IR"))) %>% 
      group_by(pos) %>% 
      mutate(i = 1:n()) %>% 
      ungroup()
  }
  
  return(df)
}

get_bench_roster <- function(roster_df=NULL, team_id=NULL, week=NULL, season=NULL) {
  return(get_active_roster(roster_df=roster_df, team_id=team_id, week=week,
                           season=season, bench_flag=TRUE))
}

get_matchup_df <- function(roster1=NULL, roster2=NULL, team1=NULL, team2=NULL, 
                           week=NULL, season=NULL) {
  if (is.null(c(roster1, roster2, team1, team2, week, season))) {
    return(NULL)
  }
  
  if (is.null(roster1)) {
    roster1 <- scrape_team_from_roster(team_id=team1, week=week, season=season)
  }
  
  if (is.null(roster2)) {
    roster2 <- scrape_team_from_roster(team_id=team2, week=week, season=season)
  }
  
  active1 <- get_active_roster(roster_df=roster1)
  active2 <- get_active_roster(roster_df=roster2)
  bench1  <- get_bench_roster(roster_df=roster1)
  bench2  <- get_bench_roster(roster_df=roster2)
  
  active.df <- active1 %>% 
    left_join(active2, by=c("pos", "i"), suffix=c(".1", ".2")) %>% 
    select(name.1, proj_pts.1, fan_pts.1, pos, fan_pts.2, proj_pts.2, name.2) %>% 
    mutate_at(c("fan_pts.1", "fan_pts.2", "proj_pts.1", "proj_pts.2"), as.numeric)
  
  bench.df <- bench1 %>% 
    left_join(bench2, by=c("pos", "i"), suffix=c(".1", ".2")) %>% 
    select(name.1, proj_pts.1, fan_pts.1, pos, fan_pts.2, proj_pts.2, name.2) %>% 
    mutate_at(c("fan_pts.1", "fan_pts.2", "proj_pts.1", "proj_pts.2"), as.numeric)
  
  df <- active.df %>% 
    bind_rows(data.frame(name.1="", proj_pts.1=sum(active.df$proj_pts.1, na.rm=TRUE), 
                         fan_pts.1=sum(active.df$fan_pts.1, na.rm=TRUE), pos="Total",
                         name.2="", proj_pts.2=sum(active.df$proj_pts.2, na.rm=TRUE), 
                         fan_pts.2=sum(active.df$fan_pts.2, na.rm=TRUE),
                         stringsAsFactors=FALSE)) %>% 
    bind_rows(bench.df)
  
  return(df)
}

result <- function(team_vec, team1=team_vec[1], team2=team_vec[2],
                   week, outcome) {
  return("")  # TODO: write this function
}

generate_html <- function(url, output_dir) {
  html <- url %>% read_html()
  
  # write relevant parts of HTML to temporary files
  html %>%
    html_nodes("head") %>%
    write_html(paste0(output_dir, "/head.html"))
  
  divs <- html %>%
    html_nodes("div")
  
  sections <- html %>%
    html_nodes("section")
  
  which.div <- which(html_attr(divs, "id") == "matchup")
  which.section <- which(html_attr(sections, "id") == "matchup-header")
  
  divs[which.div] %>%
    write_html(paste0(output_dir, "/div.html"))
  
  sections[which.section] %>%
    write_html(paste0(output_dir, "/section.html"))
  
  '<html id="Stencil" class="NoJs template-html5 Sticky-off Desktop" lang="en-US" xmlns:fb="https://www.facebook.com/2008/fbml">' %>% 
  # "<!DOCTYPE html>" %>%
    # paste('<html id="Stencil" class="NoJs template-html5 Sticky-off Desktop" lang="en-US" xmlns:fb="https://www.facebook.com/2008/fbml">', sep="\n") %>%
    paste(read_file(paste0(output_dir, "/head.html")), sep="\n") %>%
    paste(read_file(paste0(output_dir, "/section.html")), sep="\n") %>%
    paste(read_file(paste0(output_dir, "/div.html")), sep="\n") %>%
    # add the script that makes the "show bench" button work
    paste(read_file("./toggle_script.html"), sep="\n") %>%
    # this line messes up the format of everything in the app (I've included some lines in a separate CSS file)
    str_replace('<link href="https://sp.yimg.com/ua/assets/css/icingv2.cGopRQV4VIw1I.css" type="text/css" rel="stylesheet">', "") %>%
    # remove the player notes
    # remove "compare managers"
    str_replace(">Compare Managers<", "><") %>%
    # center "orig proj"
    # remove "--hidden--" manager names
    str_replace_all('<div>--hidden--</div>', "") %>%
    # add </html> at end of file
    paste("</html>", sep="\n") %>%
    # TODO: remove the favorite/underdog bars and percentages
    
    # write the file
    write_file(paste0(output_dir, "/out.html"))
  # write_file("./out.html")
}

# preprocessing -----------------------------------------------------------
season <- 2018
team.df <- scrape_team_info(season=season) %>% 
  left_join(read_csv(paste0("./", season, "_seeds.csv"), 
                     col_types=cols(), progress=FALSE),
            by=c("id" = "team_id"))

# a bit hard-coded for now, but improve this
# week 13 matchups are by seed
playoffs <- list(
  # quarterfinals (week 14) are by seed
  quarter1 = c(1, 8) %>% team_seed_to_id(),
  quarter2 = c(4, 5) %>% team_seed_to_id(),
  quarter3 = c(3, 6) %>% team_seed_to_id(),
  quarter4 = c(2, 7) %>% team_seed_to_id(),
  # semifinals (week 15) are by quarterfinal winner
  semi1 = c(result(quarter1, week=14, outcome="W"), result(quarter2, week=14, outcome="W")),
  semi2 = c(result(quarter3, week=14, outcome="W"), result(quarter4, week=14, outcome="W")),
  cons1 = c(result(quarter1, week=14, outcome="L"), result(quarter2, week=14, outcome="L")),
  cons2 = c(result(quarter3, week=14, outcome="L"), result(quarter4, week=14, outcome="L")),
  # finals (week 16) are by semifinal winner
  final = c(result(semi1, week=15, outcome="W"), result(semi2, week=15, outcome="W")),
  third = c(result(semi1, week=15, outcome="L"), result(semi2, week=15, outcome="L")),
  fifth = c(result(cons1, week=15, outcome="W"), result(cons2, week=15, outcome="W")),
  seventh = c(result(cons1, week=15, outcome="L"), result(cons2, week=15, outcome="L"))
)

consolation <- list(
  quarter1 = c(9, 0) %>% team_seed_to_id(),
  quarter2 = c(12, 13) %>% team_seed_to_id(),
  quarter3 = c(11, 14) %>% team_seed_to_id(),
  quarter4 = c(10, 0) %>% team_seed_to_id(),
  # seeds 9 and 10 move on automatically due to byes, consolation is last place game
  semi1 = c(team_seed_to_id(9), result(quarter2, week=14, outcome="W")),
  semi2 = c(result(quarter3, week=14, outcome="W"), team_seed_to_id(10)),
  thirteenth = c(result(quarter2, week=14, outcome="L"), result(quarter3, week=14, outcome="L")),
  # only have 9th and 11th place games in last round
  ninth = c(result(semi1, week=15, outcome="W"), result(semi2, week=15, outcome="W")),
  eleventh = c(result(semi1, week=15, outcome="L"), result(semi2, week=15, outcome="L"))
)

# do something like this if you want to preload the rosters
# roster.list <- list(week14=list(), week15=list(), week16=list())
# for (i in 1:14) {
#   roster.list$week14[[i]] <- scrape_team_from_roster(team_id=i, week=14, season=season)
# }

# create the file HTML file that will be displayed
temp.dir <- tempdir()
# write_file("", path=paste0(temp.dir, "/out.html"))


# variables for ui and server ---------------------------------------------

league.id <- get_league_id(season)

matchup.choices <- c()
for (i in paste0("quarter", 1:4)) {
  matchup.str <- paste(paste0("#", team_id_to_seed(playoffs[[i]])), 
                       team_id_to_name(playoffs[[i]]),
                       collapse=" vs. ")
  
  matchup.choices <- c(matchup.choices, matchup.str)
}

for (i in paste0("quarter", 1:4)) {
  matchup.str <- paste(paste0("#", team_id_to_seed(consolation[[i]])), 
                       team_id_to_name(consolation[[i]]),
                       collapse=" vs. ")
  
  matchup.choices <- c(matchup.choices, matchup.str)
}

matchup.choices <- matchup.choices %>% 
  str_replace("#0 ", "")

week.choices <- c("14 - Quarterfinal"
                  # ,"15 - Semifinal"
                  # ,"16 - Final"
)
                  



# ui ----------------------------------------------------------------------
ui <- dashboardPage(skin="purple",
  dashboardHeader(title="2018 Playoffs and Consolation Bracket",
                  titleWidth=400)
  
  ,dashboardSidebar(
    sidebarMenu(
      id="tabs"
      ,menuItem("Matchups", tabName="matchup")
      # ,menuItem("Team Page", tabName="team")
      # ,menuItem("Playoff Seeding", tabName="seeding")
      # ,menuItem("Matchup (simple)", tabName="simplematchup")
    )
  )
  
  ,dashboardBody(
    
    includeCSS("./styling.css"),
    
    tabItems(
      tabItem(
        tabName="matchup"
        
        ,div(style="display:inline-block", 
             selectInput("week", "Week", choices=week.choices, width="150px"))
        ,div(style="display:inline-block",
             # selectInput("matchup", "Matchup", choices=matchup.choices, width="400px"))
             selectizeInput("matchup", "Matchup", choices=matchup.choices, width="400px",
                            options = list(
                              placeholder = 'Select a matchup',
                              onInitialize = I('function() { this.setValue(""); }'))))
        
        # not sure why this needs to be included since it's in the HTML file, but it fixes the format
        # (something weird happens when using includeHTML inside renderUI)
        ,tags$html(id="Stencil", class="NoJs template-html5 Sticky-off Desktop")
        
        # ,includeHTML(paste0(temp.dir, "/out.html"))
        ,htmlOutput("html")
      )
      
      # ,tabItem(
      #   tabName="team"
      #   ,h2("Team Roster")
      #   
      #   ,selectInput("team", "Team", choices=team.df$name)
      #   ,selectInput("teamweek", "Week", choices=1:16)
      #   
      #   ,dataTableOutput("team")
      # ) # end tabItem
      # 
      # ,tabItem(
      #   tabName="seeding"
      #   
      #   ,dataTableOutput("team.df")
      # ) # end tabItem
      # 
      # ,tabItem(
      #   tabName="simplematchup"
      # 
      #   ,selectInput("weeksimple", "Week", choices=14)
      #   ,selectInput("matchupsimple", "Matchup", choices=matchup.choices)
      # 
      #   ,tableOutput("matchup")
      # )
      
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage


# server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # show Yahoo matchup page for selected teams
  # TODO: try to make it work so it doesn't show a blank screen at the default selectInput value
  output$html <- renderUI({
    progress <- Progress$new()
    on.exit(progress$close())
    progress$set(message = "Loading...")
    
    week <- input$week %>% 
      str_extract("[0-9]+") %>% 
      as.numeric()
    matchup <- input$matchup
    
    # this is necessary so it shows something the first time you select a matchup
    if (matchup == "") { matchup <- matchup.choices[1] }
    
    i <- which(matchup.choices == matchup)
    if (i <= 4) {
      team.ids <- playoffs[[paste0("quarter", i)]]
    } else {
      team.ids <- consolation[[paste0("quarter", i-4)]]
    }
    
    team1 <- team.ids[1]
    team2 <- team.ids[2]
    
    # week=0 works for the current week
    # TODO: make this work for past weeks as well
    url <- paste0("https://football.fantasysports.yahoo.com/f1/", league.id,
                  "/matchup?week=0&mid1=", team1, "&mid2=", team2)
    
    html <- url %>% read_html()
    
    # write relevant parts of HTML to temporary files
    generate_html(url=url, output_dir=temp.dir)
    
    includeHTML(paste0(temp.dir, "/out.html"))
  })
  
  # output$team <- renderDataTable({
  #   scrape_team_from_roster(team_id=team_name_to_id(name=input$team), week=input$teamweek, season=season)
  # })
  
  # output$team.df <- renderDataTable({
  #   team.df %>% 
  #     arrange(seed)
  # })
  
  # output$matchup <- function () {
  #   
  #   progress <- Progress$new()
  #   on.exit(progress$close())
  #   progress$set(message = "Loading...")
  #   
  #   options(knitr.kable.NA = '')
  #   
  #   week <- input$weeksimple
  #   
  #   i <- which(matchup.choices == input$matchupsimple)
  #   if (i <= 4) {
  #     team.ids <- playoffs[[paste0("quarter", i)]]
  #   } else {
  #     team.ids <- consolation[[paste0("quarter", i-4)]]
  #   }
  #   
  #   team1 <- team.ids[1]
  #   team2 <- team.ids[2]
  #   
  #   roster1 <- scrape_team_from_roster(team_id=team1, week=week, season=season)  # TODO: change so it's not hard coded to week 14
  #   roster2 <- scrape_team_from_roster(team_id=team2, week=week, season=season)
  #   
  #   matchup.df <- get_matchup_df(roster1=roster1, roster2=roster2)
  #   matchup.df %>% 
  #     kable() %>% 
  #     kable_styling()
  #     
  # }
  
}

shinyApp(ui, server)
