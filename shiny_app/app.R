library(shiny)
library(shinydashboard)
library(tidyverse)
# library(GAlogger)  # this isn't working since I updated R
library(rvest)
library(knitr)
library(kableExtra)
library(lubridate)


# start tracking ----------------------------------------------------------
# source("my_google_tracking_id.R")  # just contains my tracking ID (excluded from github)
# my_id <- my_google_tracking_id()
# ga_set_tracking_id(my_id)
# ga_set_approval(consent=TRUE)
# ga_collect_pageview(page="/test", title="test")


# define functions --------------------------------------------------------
get_max_completed_week <- function(time=Sys.time()) {
  time.14 <- as_datetime("2018-12-11 00:00:00")
  time.15 <- as_datetime("2018-12-18 00:00:00")
  time.16 <- as_datetime("2018-12-25 00:00:00")
  tz(time.14) <- "EST"
  tz(time.15) <- "EST"
  tz(time.16) <- "EST"
  
  if (time < time.14) {
    return(13)
  } else if (time < time.15) {
    return(14)
  } else if (time < time.16) {
    return(15)
  } else {
    return(16)
  }
}

scrape_team_info <- function() {
  # scrape the "managers" page
  url <- paste0("https://football.fantasysports.yahoo.com/f1/912438/teams")
  
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

team_id_to_seedname <- function(id, team_df=team.df) {
  team_df <- team_df %>% 
    bind_rows(data.frame(name="--BYE--", id=0, seed=0, stringsAsFactors=FALSE))
  
  out <- paste(paste0("#", team_id_to_seed(id)), team_id_to_name(id)) %>% 
    str_replace("#0 ", "")
  
  return(out)
}

matchup_str_to_ids <- function(matchup_str, team_df=team.df) {
  out <- matchup_str %>% 
    str_extract_all("#[0-9]+") %>% 
    map(~ str_extract(., "[0-9]+"))
  
  out <- as.numeric(out[[1]])
  
  # add 0 if the string contained "--BYE--" (won't have a seed number in the string)
  if (length(out) == 1) {
    out <- c(out, 0)
  }
  
  out <- team_seed_to_id(out)
  
  return(out)
}

matchup_result <- function(team_vec=c(NA, NA), week, team1=team_vec[1], team2=team_vec[2]) {
  max_comp_week <- get_max_completed_week()
  
  if (is.na(team1) | is.na(team2)) {
    winner <- NA
    loser <- NA
  } else if (week > max_comp_week) {
    winner <- NA
    loser <- NA
  } else if (0 %in% team_vec) {
    winner <- team_vec[which(team_vec != 0)]
    loser <- NA
  } else {
    scores <- c(get_team_score(team=team1, week=week),
                get_team_score(team=team2, week=week))
    
    winner <- team_vec[which.max(scores)]
    loser  <- team_vec[which.min(scores)]
  }
  return(c(W=winner, L=loser))
}

get_team_score <- function(team, week) {
  url <- paste0("https://football.fantasysports.yahoo.com/f1/912438/", team, "/team?&week=", week)
  html <- read_html(url)
  
  # this worked at first but stopped working for consolation teams (always showed 0)
  # score <- html %>% 
  #   html_text() %>% 
  #   str_extract(paste0("Week ", week, " Total: [0-9.]+")) %>% 
  #   str_extract(paste0("Total Week ", week, " "))
  #   str_extract_all("[0-9.]+") %>% 
  #   map_chr(~ .x[2]) %>% 
  #   as.numeric()
  
  # switching to this instead
  nodes <- html %>% 
    html_nodes("strong")
  
  which.node <- which(html_attr(nodes, "class") == "Pstart-lg")
  
  score <- nodes[which.node] %>% 
    html_text() %>% 
    as.numeric()
  
  return(score)
}

build_playoff_bracket <- function() {
  playoffs <- list(
    # quarterfinals (week 14) are by seed
    quarter1 = c(1, 8) %>% team_seed_to_id(),
    quarter2 = c(4, 5) %>% team_seed_to_id(),
    quarter3 = c(3, 6) %>% team_seed_to_id(),
    quarter4 = c(2, 7) %>% team_seed_to_id()
  )
  # semifinals (week 15) are by quarterfinal winner
  tmp1 <- matchup_result(playoffs$quarter1, week=14)
  tmp2 <- matchup_result(playoffs$quarter2, week=14)
  tmp3 <- matchup_result(playoffs$quarter3, week=14)
  tmp4 <- matchup_result(playoffs$quarter4, week=14)
  playoffs$semi1 <- c(tmp1["W"], tmp2["W"]) %>% unname()
  playoffs$semi2 <- c(tmp3["W"], tmp4["W"]) %>% unname()
  playoffs$cons1 <- c(tmp1["L"], tmp2["L"]) %>% unname()
  playoffs$cons2 <- c(tmp3["L"], tmp4["L"]) %>% unname()
  # finals (week 16) are by semifinal winner
  tmp1 <- matchup_result(playoffs$semi1, week=15)
  tmp2 <- matchup_result(playoffs$semi2, week=15)
  tmp3 <- matchup_result(playoffs$cons1, week=15)
  tmp4 <- matchup_result(playoffs$cons2, week=15)
  playoffs$final <- c(tmp1["W"], tmp2["W"]) %>% unname()
  playoffs$third <- c(tmp1["L"], tmp2["L"]) %>% unname()
  playoffs$fifth <- c(tmp3["W"], tmp4["W"]) %>% unname()
  playoffs$seventh <- c(tmp3["L"], tmp4["L"]) %>% unname()
  # get final standings
  tmp1 <- matchup_result(playoffs$final, week=16)
  tmp2 <- matchup_result(playoffs$third, week=16)
  tmp3 <- matchup_result(playoffs$fifth, week=16)
  tmp4 <- matchup_result(playoffs$seventh, week=16)
  playoffs$standings <- c(
    tmp1["W"],
    tmp1["L"],
    tmp2["W"],
    tmp2["L"],
    tmp3["W"],
    tmp3["L"],
    tmp4["W"],
    tmp4["L"]
  ) %>% unname()
  
  return(playoffs)
}

build_consolation_bracket <- function() {
  consolation <- list(
    quarter1 = c(9, 0) %>% team_seed_to_id(),
    quarter2 = c(12, 13) %>% team_seed_to_id(),
    quarter3 = c(11, 14) %>% team_seed_to_id(),
    quarter4 = c(10, 0) %>% team_seed_to_id()
  )
  # seeds 9 and 10 move on automatically due to byes, consolation is last place game
  tmp1 <- c("W"=9) %>% team_seed_to_id()
  tmp2 <- matchup_result(consolation$quarter2, week=14)
  tmp3 <- matchup_result(consolation$quarter3, week=14)
  tmp4 <- c("W"=10) %>% team_seed_to_id()
  consolation$semi1 <- c(tmp1["W"], tmp2["W"]) %>% unname()
  consolation$semi2 <- c(tmp3["W"], tmp4["W"]) %>% unname()
  consolation$thirteenth <- c(tmp2["L"], tmp3["L"]) %>% unname()
  # only have 9th and 11th place games in last round
  tmp1 <- matchup_result(consolation$semi1, week=15)
  tmp2 <- matchup_result(consolation$semi2, week=15)
  tmp3 <- matchup_result(consolation$thirteenth, week=15)
  consolation$ninth <- c(tmp1["W"], tmp2["W"]) %>% unname()
  consolation$eleventh <- c(tmp1["L"], tmp2["L"]) %>% unname()
  # get final standings
  tmp1 <- matchup_result(consolation$ninth, week=16)
  tmp2 <- matchup_result(consolation$eleventh, week=16)
  consolation$standings <- c(
    tmp1["W"],
    tmp1["L"],
    tmp2["W"],
    tmp2["L"],
    tmp3["W"],
    tmp3["L"]
  ) %>% unname()
  
  return(consolation)
}

format_directory_path <- function(path) {
  require(stringr, quietly=TRUE, warn.conflicts=FALSE)
  if (str_sub(path, -1) != "/") {
    path <- paste0(path, "/")
  }
  return(path)
}

generate_html <- function(week, team1, team2, output_dir=tempdir(), empty_flag=FALSE) {
  output_dir <- format_directory_path(output_dir)
  
  url <- paste0("https://football.fantasysports.yahoo.com/f1/912438",
                "/matchup?week=", week, "&mid1=", team1, "&mid2=", team2)
  
  html <- url %>% read_html()
  
  # need to use week=0 for current week if games are in progress
  # check if the necessary HTML nodes exist and use week=0 if they don't (if which.div == integer(0))
  divs <- html %>%
    html_nodes("div")
  which.div <- which(html_attr(divs, "id") == "matchup")
  
  if (week == get_max_completed_week() & length(which.div) == 0 & week != 0) {
    return(generate_html(week=0, team1=team1, team2=team2, output_dir=output_dir, empty_flag=empty_flag))
  }
  
  # write relevant parts of HTML to temporary files
  html %>%
    html_nodes("head") %>%
    write_html(paste0(output_dir, "/head.html"))
  
  # if we don't have a matchup selected, just write the <head> section
  if (empty_flag) {
    '<html id="Stencil" class="NoJs template-html5 Sticky-off Desktop" lang="en-US" xmlns:fb="https://www.facebook.com/2008/fbml">' %>% 
      paste(read_file(paste0(output_dir, "/head.html")), sep="\n") %>%
      paste("</html>", sep="\n") %>%
      str_replace('<link href="https://sp.yimg.com/ua/assets/css/icingv2.cGopRQV4VIw1I.css" type="text/css" rel="stylesheet">', "") %>% 
      write_file(paste0(output_dir, "/out.html"))
  } else {
    sections <- html %>%
      html_nodes("section")
    which.section <- which(html_attr(sections, "id") == "matchup-header")
    
    divs[which.div] %>%
      write_html(paste0(output_dir, "/div.html"))
    
    sections[which.section] %>%
      write_html(paste0(output_dir, "/section.html"))
    
    '<html id="Stencil" class="NoJs template-html5 Sticky-off Desktop" lang="en-US" xmlns:fb="https://www.facebook.com/2008/fbml">' %>% 
      paste(read_file(paste0(output_dir, "/head.html")), sep="\n") %>%
      paste(read_file(paste0(output_dir, "/section.html")), sep="\n") %>%
      paste(read_file(paste0(output_dir, "/div.html")), sep="\n") %>%
      # add the script that makes the "show bench" button work
      paste(read_file("./toggle_script.html"), sep="\n") %>%
      # add </html> at end of file
      paste("</html>", sep="\n") %>%
      # this line messes up the format of everything in the app (I've included some lines in a separate CSS file)
      str_replace('<link href="https://sp.yimg.com/ua/assets/css/icingv2.cGopRQV4VIw1I.css" type="text/css" rel="stylesheet">', "") %>%
      # remove the player notes
      # remove "compare managers"
      str_replace(">Compare Managers<", "><") %>%
      # center "orig proj"
      # remove "--hidden--" manager names
      str_replace_all('<div>--hidden--</div>', "") %>%
      # str_replace("Sean's Swell Team", "Sean's Dumb Team") %>% 
      # remove the favorite/underdog bars and percentages
      str_replace_all('<div class="Inlineblock Bg-positive Fz-xxs Py-xxs My-xxs" style="width: .+</div>', "") %>% 
      str_replace_all('<div class="Inlineblock Bg-negative Fz-xxs Py-xxs My-xxs" style="width: .+</div>', "") %>% 
      str_replace_all('>[0-9]+ Players Remaining', '') %>% 
      str_replace_all('<span class="Pend-med">Favorite </span>[0-9]+%', '') %>% 
      str_replace_all('>[0-9]+%<span class="Pstart-med"> Underdog</span>', '') %>% 
      str_replace_all('<span class="Pend-med">Underdog </span>[0-9]+%', '') %>% 
      str_replace_all('>[0-9]+%<span class="Pstart-med"> Favorite</span>', '') %>% 
      # remove non-ASCII characters (weather icons) because they don't render appropriately
      str_replace_all('>[^[:ascii:]]<', '') %>% 
      
      # write the file
      write_file(paste0(output_dir, "/out.html"))
  }
}

# preprocessing -----------------------------------------------------------

team.df <- scrape_team_info() %>% 
  left_join(read_csv(paste0("./2018_seeds.csv"), col_types=cols(), progress=FALSE),
            by=c("id" = "team_id"))

playoffs <- build_playoff_bracket()
consolation <- build_consolation_bracket()

# matchup choices for each week
max.week <- get_max_completed_week()
week.choices <- "14 - Quarterfinal"

choices.14 <- c(
  playoffs$quarter1 %>% team_id_to_seedname() %>% paste(collapse=" vs. "),
  playoffs$quarter2 %>% team_id_to_seedname() %>% paste(collapse=" vs. "),
  playoffs$quarter3 %>% team_id_to_seedname() %>% paste(collapse=" vs. "),
  playoffs$quarter4 %>% team_id_to_seedname() %>% paste(collapse=" vs. "),
  consolation$quarter1 %>% team_id_to_seedname() %>% paste(collapse=" vs. "),
  consolation$quarter2 %>% team_id_to_seedname() %>% paste(collapse=" vs. "),
  consolation$quarter3 %>% team_id_to_seedname() %>% paste(collapse=" vs. "),
  consolation$quarter4 %>% team_id_to_seedname() %>% paste(collapse=" vs. ")
)
default.choices <- choices.14

if (max.week >= 14) {
  week.choices <- c("15 - Semifinal", week.choices)
  choices.15 <- c(
    playoffs$semi1 %>% team_id_to_seedname() %>% paste(collapse=" vs. "),
    playoffs$semi2 %>% team_id_to_seedname() %>% paste(collapse=" vs. "),
    playoffs$cons1 %>% team_id_to_seedname() %>% paste(collapse=" vs. "),
    playoffs$cons2 %>% team_id_to_seedname() %>% paste(collapse=" vs. "),
    consolation$semi1 %>% team_id_to_seedname() %>% paste(collapse=" vs. "),
    consolation$semi2 %>% team_id_to_seedname() %>% paste(collapse=" vs. "),
    consolation$thirteenth %>% team_id_to_seedname() %>% paste(collapse=" vs. ")
  )
  default.choices <- choices.15
}

if (max.week >= 15) {
  week.choices <- c("16 - Final", week.choices)
  choices.16 <- c(
    playoffs$final %>% team_id_to_seedname() %>% paste(collapse=" vs. "),
    playoffs$third %>% team_id_to_seedname() %>% paste(collapse=" vs. "),
    playoffs$fifth %>% team_id_to_seedname() %>% paste(collapse=" vs. "),
    playoffs$seventh %>% team_id_to_seedname() %>% paste(collapse=" vs. "),
    consolation$ninth %>% team_id_to_seedname() %>% paste(collapse=" vs. "),
    consolation$eleventh %>% team_id_to_seedname() %>% paste(collapse=" vs. ")
  )
  default.choices <- choices.16
}

# directory to store HTML files that will be displayed
temp.dir <- tempdir()




# ui ----------------------------------------------------------------------
ui <- dashboardPage(skin="purple",
  dashboardHeader(title="2018 Playoffs and Consolation Bracket",
                  titleWidth=400)
  
  ,dashboardSidebar(
    sidebarMenu(
      id="tabs"
      ,menuItem("Matchups", tabName="matchup")
      ,menuItem("Playoff Seeding", tabName="seeding")
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
             selectizeInput("matchup", "Matchup", choices=default.choices, width="400px",
                            options = list(
                              placeholder = 'Select a matchup',
                              onInitialize = I('function() { this.setValue(""); }'))))
        
        # not sure why this needs to be included since it's in the HTML file, but it fixes the format
        # (something weird happens when using includeHTML inside renderUI)
        ,tags$html(id="Stencil", class="NoJs template-html5 Sticky-off Desktop")
        
        ,htmlOutput("htmlmatchup")
      ) # end tabItem
      
      ,tabItem(
        tabName="seeding"

        ,tableOutput("team.df")
      ) # end tabItem
      
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage


# server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  vals <- reactiveValues()
  vals$matchup_choices <- default.choices
  
  # update the matchup list whenever the selected week changes
  observeEvent(
    input$week,
    {
      week <- input$week %>% 
        str_extract("[0-9]+") %>% 
        as.numeric()
      
      if (week == 14) {
        vals$matchup_choices <- choices.14
      } else if (week == 15) {
        vals$matchup_choices <- choices.15
      } else {
        vals$matchup_choices <- choices.16
      }
      
      updateSelectizeInput(session, "matchup", choices=vals$matchup_choices)
    }
  )
  
  # show Yahoo matchup page for selected teams
  output$htmlmatchup <- renderUI({
    progress <- Progress$new()
    on.exit(progress$close())
    progress$set(message = "Loading...")
    
    week <- input$week %>% 
      str_extract("[0-9]+") %>% 
      as.numeric()
    
    matchup <- input$matchup
    
    # this is necessary so it shows something the first time you select a matchup
    if (matchup == "") { 
      matchup <- vals$matchup_choices[1]
      empty.flag <- TRUE
    } else {
      empty.flag <- FALSE
    }
    
    team.ids <- matchup_str_to_ids(matchup)
    
    team1 <- team.ids[1]
    team2 <- team.ids[2]
    
    # write relevant parts of HTML to temporary files
    generate_html(week=week, team1=team1, team2=team2, output_dir=temp.dir, empty_flag=empty.flag)
    
    includeHTML(paste0(temp.dir, "/out.html"))
  }) # end HTML matchup
  
  # show teams and seeds
  output$team.df <- function() {
    team.df %>%
      select(name, seed) %>% 
      rename(`Team Name` = name,
             Seed = seed) %>% 
      arrange(Seed) %>% 
      kable() %>% 
      kable_styling(bootstrap_options=c("striped", "condensed"),
                    full_width=FALSE, position="left") %>% 
      row_spec(0, bold=TRUE)
  } # end team seeding
}

shinyApp(ui, server)
