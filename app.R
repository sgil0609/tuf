library(shiny)
library(shinydashboard)
library(DT)
library(duckdb)
library(DBI)
library(dplyr)
library(shinyjs)

# Initialize database and create tables
init_database <- function() {
  con <- dbConnect(duckdb::duckdb(), "fantasy_draft.db")
  
  # Create tables if they don't exist
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS players (
      id INTEGER PRIMARY KEY,
      name VARCHAR,
      sport VARCHAR,
      position VARCHAR,
      team VARCHAR,
      available BOOLEAN DEFAULT TRUE
    )
  ")
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS drafters (
      id INTEGER PRIMARY KEY,
      name VARCHAR,
      draft_position INTEGER
    )
  ")
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS draft_picks (
      id INTEGER PRIMARY KEY,
      pick_number INTEGER,
      drafter_id INTEGER,
      player_id INTEGER,
      timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS draft_state (
      id INTEGER PRIMARY KEY DEFAULT 1,
      current_pick INTEGER DEFAULT 1,
      current_drafter INTEGER DEFAULT 1,
      pick_start_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Insert sample data if tables are empty
  if (nrow(dbGetQuery(con, "SELECT * FROM players")) == 0) {
    insert_sample_data(con)
  }
  
  if (nrow(dbGetQuery(con, "SELECT * FROM drafters")) == 0) {
    insert_drafters(con)
  }
  
  if (nrow(dbGetQuery(con, "SELECT * FROM draft_state")) == 0) {
    dbExecute(con, "INSERT INTO draft_state (current_pick, current_drafter) VALUES (1, 1)")
  }
  
  dbDisconnect(con)
}

# Insert sample player data
insert_sample_data <- function(con) {
  # NFL Players
  nfl_players <- data.frame(
    name = c(
      "Patrick Mahomes", "Josh Allen", "Lamar Jackson", "Aaron Rodgers", "Tom Brady",
      "Derrick Henry", "Christian McCaffrey", "Dalvin Cook", "Alvin Kamara", "Ezekiel Elliott",
      "Davante Adams", "Tyreek Hill", "DeAndre Hopkins", "Stefon Diggs", "Calvin Ridley",
      "Travis Kelce", "George Kittle", "Darren Waller", "Mark Andrews", "Rob Gronkowski",
      "Justin Tucker", "Harrison Butker", "Younghoe Koo", "Daniel Carlson", "Matt Gay",
      "Pittsburgh Steelers", "Los Angeles Rams", "Buffalo Bills", "Tampa Bay Buccaneers", "New England Patriots"
    ),
    sport = "NFL",
    position = c(
      rep("QB", 5), rep("RB", 5), rep("WR", 5), rep("TE", 5), rep("K", 5), rep("DEF", 5)
    ),
    team = c(
      "KC", "BUF", "BAL", "GB", "TB",
      "TEN", "CAR", "MIN", "NO", "DAL",
      "GB", "MIA", "ARI", "BUF", "ATL",
      "KC", "SF", "LV", "BAL", "TB",
      "BAL", "KC", "ATL", "LV", "LAR",
      "PIT", "LAR", "BUF", "TB", "NE"
    )
  )
  
  # NBA Players
  nba_players <- data.frame(
    name = c(
      "Stephen Curry", "Damian Lillard", "Kyrie Irving", "Luka Doncic", "Trae Young",
      "James Harden", "Bradley Beal", "Devin Booker", "Donovan Mitchell", "CJ McCollum",
      "LeBron James", "Kevin Durant", "Kawhi Leonard", "Paul George", "Jimmy Butler",
      "Giannis Antetokounmpo", "Anthony Davis", "Karl-Anthony Towns", "Nikola Jokic", "Joel Embiid",
      "Rudy Gobert", "Clint Capela", "Myles Turner", "Robert Williams", "Jarrett Allen"
    ),
    sport = "NBA",
    position = c(
      rep("PG", 5), rep("SG", 5), rep("SF", 5), rep("PF", 5), rep("C", 5)
    ),
    team = c(
      "GSW", "POR", "BKN", "DAL", "ATL",
      "PHI", "WAS", "PHX", "UTA", "POR",
      "LAL", "BKN", "LAC", "LAC", "MIA",
      "MIL", "LAL", "MIN", "DEN", "PHI",
      "UTA", "ATL", "IND", "BOS", "CLE"
    )
  )
  
  # MLB Players
  mlb_players <- data.frame(
    name = c(
      "Salvador Perez", "J.T. Realmuto", "Will Smith", "Tyler Stephenson", "Willson Contreras",
      "Vladimir Guerrero Jr.", "Matt Olson", "Jose Altuve", "Gleyber Torres", "Marcus Semien",
      "Manny Machado", "Rafael Devers", "Jose Ramirez", "Alex Bregman", "Nolan Arenado",
      "Fernando Tatis Jr.", "Trea Turner", "Bo Bichette", "Carlos Correa", "Xander Bogaerts",
      "Ronald Acuna Jr.", "Mike Trout", "Mookie Betts", "Juan Soto", "Kyle Tucker",
      "Jacob deGrom", "Gerrit Cole", "Shane Bieber", "Walker Buehler", "Brandon Woodruff",
      "Josh Hader", "Emmanuel Clase", "Edwin Diaz", "Liam Hendriks", "Ryan Pressly",
      "Yordan Alvarez", "Aaron Judge", "Freddie Freeman", "Corey Seager", "Francisco Lindor"
    ),
    sport = "MLB",
    position = c(
      rep("C", 5), rep("1B", 5), rep("2B", 5), rep("3B", 5), rep("SS", 5), rep("OF", 5), rep("SP", 5), rep("RP", 5)
    ),
    team = c(
      "KC", "PHI", "LAD", "CIN", "STL",
      "TOR", "ATL", "HOU", "NYY", "TEX",
      "SD", "BOS", "CLE", "HOU", "STL",
      "SD", "LAD", "TOR", "MIN", "BOS",
      "ATL", "LAA", "LAD", "WAS", "HOU",
      "NYM", "NYY", "CLE", "LAD", "MIL",
      "SD", "CLE", "NYM", "CWS", "HOU",
      "HOU", "NYY", "ATL", "TEX", "NYM"
    )
  )
  
  # Combine all players
  all_players <- rbind(nfl_players, nba_players, mlb_players)
  all_players$id <- 1:nrow(all_players)
  
  # Insert into database
  dbWriteTable(con, "players", all_players, append = TRUE)
}

# Insert drafter data
insert_drafters <- function(con) {
  drafters <- data.frame(
    id = 1:10,
    name = paste("Drafter", 1:10),
    draft_position = 1:10
  )
  dbWriteTable(con, "drafters", drafters, append = TRUE)
}

# Calculate snake draft order
get_snake_draft_order <- function(pick_number) {
  round_num <- ceiling(pick_number / 10)
  position_in_round <- ((pick_number - 1) %% 10) + 1
  
  if (round_num %% 2 == 1) {  # Odd rounds: 1-10
    return(position_in_round)
  } else {  # Even rounds: 10-1
    return(11 - position_in_round)
  }
}

# Get current draft state
get_draft_state <- function() {
  con <- dbConnect(duckdb::duckdb(), "fantasy_draft.db")
  state <- dbGetQuery(con, "SELECT * FROM draft_state WHERE id = 1")
  dbDisconnect(con)
  return(state)
}

# Update draft state
update_draft_state <- function(pick_number, drafter_id) {
  con <- dbConnect(duckdb::duckdb(), "fantasy_draft.db")
  dbExecute(con, "
    UPDATE draft_state 
    SET current_pick = ?, current_drafter = ?, pick_start_time = CURRENT_TIMESTAMP 
    WHERE id = 1
  ", list(pick_number, drafter_id))
  dbDisconnect(con)
}

# Get available players with filters
get_available_players <- function(sport_filter = NULL, position_filter = NULL) {
  con <- dbConnect(duckdb::duckdb(), "fantasy_draft.db")
  
  query <- "SELECT * FROM players WHERE available = TRUE"
  params <- list()
  
  if (!is.null(sport_filter) && sport_filter != "All") {
    query <- paste(query, "AND sport = ?")
    params <- append(params, sport_filter)
  }
  
  if (!is.null(position_filter) && position_filter != "All") {
    query <- paste(query, "AND position = ?")
    params <- append(params, position_filter)
  }
  
  query <- paste(query, "ORDER BY sport, position, name")
  
  if (length(params) > 0) {
    players <- dbGetQuery(con, query, params)
  } else {
    players <- dbGetQuery(con, query)
  }
  
  dbDisconnect(con)
  return(players)
}

# Get drafter roster counts
get_roster_counts <- function(drafter_id) {
  con <- dbConnect(duckdb::duckdb(), "fantasy_draft.db")
  
  counts <- dbGetQuery(con, "
    SELECT p.sport, COUNT(*) as count
    FROM draft_picks dp
    JOIN players p ON dp.player_id = p.id
    WHERE dp.drafter_id = ?
    GROUP BY p.sport
  ", list(drafter_id))
  
  dbDisconnect(con)
  
  # Convert to named list with defaults
  result <- list(NFL = 0, NBA = 0, MLB = 0)
  if (nrow(counts) > 0) {
    for (i in 1:nrow(counts)) {
      result[[counts$sport[i]]] <- counts$count[i]
    }
  }
  
  return(result)
}

# Check if pick is valid (doesn't exceed roster limits)
is_valid_pick <- function(drafter_id, sport) {
  counts <- get_roster_counts(drafter_id)
  limits <- list(NFL = 13, NBA = 14, MLB = 25)
  
  return(counts[[sport]] < limits[[sport]])
}

# Make a draft pick
make_draft_pick <- function(drafter_id, player_id) {
  con <- dbConnect(duckdb::duckdb(), "fantasy_draft.db")
  
  # Get current state
  state <- dbGetQuery(con, "SELECT * FROM draft_state WHERE id = 1")
  current_pick <- state$current_pick
  
  # Get next available ID for draft_picks
  max_id <- dbGetQuery(con, "SELECT COALESCE(MAX(id), 0) as max_id FROM draft_picks")$max_id
  next_id <- max_id + 1
  
  # Insert the pick with explicit ID
  dbExecute(con, "
    INSERT INTO draft_picks (id, pick_number, drafter_id, player_id)
    VALUES (?, ?, ?, ?)
  ", list(next_id, current_pick, drafter_id, player_id))
  
  # Mark player as unavailable
  dbExecute(con, "UPDATE players SET available = FALSE WHERE id = ?", list(player_id))
  
  # Update draft state for next pick
  next_pick <- current_pick + 1
  next_drafter <- get_snake_draft_order(next_pick)
  
  if (next_pick <= 400) {  # 40 rounds * 10 players
    update_draft_state(next_pick, next_drafter)
  }
  
  dbDisconnect(con)
}

# Get recent picks
get_recent_picks <- function(limit = 10) {
  con <- dbConnect(duckdb::duckdb(), "fantasy_draft.db")
  
  picks <- dbGetQuery(con, "
    SELECT dp.pick_number, d.name as drafter_name, p.name as player_name, 
           p.sport, p.position, p.team, dp.timestamp
    FROM draft_picks dp
    JOIN drafters d ON dp.drafter_id = d.id
    JOIN players p ON dp.player_id = p.id
    ORDER BY dp.pick_number DESC
    LIMIT ?
  ", list(limit))
  
  dbDisconnect(con)
  return(picks)
}

# Get drafter roster by sport
get_drafter_roster <- function(drafter_id, sport) {
  con <- dbConnect(duckdb::duckdb(), "fantasy_draft.db")
  
  roster <- dbGetQuery(con, "
    SELECT p.position, p.name as player_name, p.team
    FROM draft_picks dp
    JOIN players p ON dp.player_id = p.id
    WHERE dp.drafter_id = ? AND p.sport = ?
    ORDER BY p.position, p.name
  ", list(drafter_id, sport))
  
  dbDisconnect(con)
  return(roster)
}

# Initialize database on startup
init_database()

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Fantasy Draft App"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Draft", tabName = "draft", icon = icon("football-ball")),
      menuItem("Rosters", tabName = "rosters", icon = icon("users"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    tabItems(
      tabItem(tabName = "draft",
        fluidRow(
          # Current Pick Info
          box(
            title = "Current Pick", status = "primary", solidHeader = TRUE, width = 6,
            h3(textOutput("current_pick_info")),
            h4(textOutput("current_drafter")),
            h4(textOutput("pick_timer"))
          ),
          
          # Draft Progress
          box(
            title = "Draft Progress", status = "info", solidHeader = TRUE, width = 6,
            h4(textOutput("draft_progress"))
          )
        ),
        
        fluidRow(
          # Player Selection
          box(
            title = "Available Players", status = "success", solidHeader = TRUE, width = 8,
            
            fluidRow(
              column(4,
                selectInput("sport_filter", "Sport:",
                  choices = c("All", "NFL", "NBA", "MLB"),
                  selected = "All"
                )
              ),
              column(4,
                selectInput("position_filter", "Position:",
                  choices = "All",
                  selected = "All"
                )
              ),
              column(4,
                br(),
                actionButton("refresh_players", "Refresh", class = "btn-info")
              )
            ),
            
            DT::dataTableOutput("players_table")
          ),
          
          # Draft Info Sidebar
          box(
            title = "Draft Information", status = "warning", solidHeader = TRUE, width = 4,
            
            h4("Upcoming Picks:"),
            tableOutput("upcoming_picks"),
            
            hr(),
            
            h4("Recent Picks:"),
            tableOutput("recent_picks"),
            
            hr(),
            
            h4("Roster Limits:"),
            p("NFL: 13 players"),
            p("NBA: 14 players"), 
            p("MLB: 25 players")
          )
        )
      ),
      
      tabItem(tabName = "rosters",
        fluidRow(
          box(
            title = "Drafter Selection", status = "primary", solidHeader = TRUE, width = 12,
            selectInput("selected_drafter", "Select Drafter:",
              choices = setNames(1:10, paste("Drafter", 1:10)),
              selected = 1
            )
          )
        ),
        
        fluidRow(
          # NFL Roster
          box(
            title = "NFL Roster", status = "success", solidHeader = TRUE, width = 4,
            tableOutput("nfl_roster")
          ),
          
          # NBA Roster
          box(
            title = "NBA Roster", status = "info", solidHeader = TRUE, width = 4,
            tableOutput("nba_roster")
          ),
          
          # MLB Roster
          box(
            title = "MLB Roster", status = "warning", solidHeader = TRUE, width = 4,
            tableOutput("mlb_roster")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    draft_state = get_draft_state(),
    pick_start_time = Sys.time(),
    picks_updated = 0  # Counter to trigger reactive updates
  )
  
  # Update position filter based on sport selection
  observe({
    sport <- input$sport_filter
    
    if (sport == "NFL") {
      positions <- c("All", "QB", "RB", "WR", "TE", "K", "DEF")
    } else if (sport == "NBA") {
      positions <- c("All", "PG", "SG", "SF", "PF", "C")
    } else if (sport == "MLB") {
      positions <- c("All", "C", "1B", "2B", "3B", "SS", "OF", "SP", "RP")
    } else {
      positions <- "All"
    }
    
    updateSelectInput(session, "position_filter", choices = positions, selected = "All")
  })
  
  # Timer that updates every second
  observe({
    invalidateLater(1000, session)
    values$draft_state <- get_draft_state()
    values$pick_start_time <- as.POSIXct(values$draft_state$pick_start_time)
  })
  
  # Current pick information
  output$current_pick_info <- renderText({
    state <- values$draft_state
    paste("Pick", state$current_pick, "of 400")
  })
  
  output$current_drafter <- renderText({
    state <- values$draft_state
    paste("Current Drafter:", paste("Drafter", state$current_drafter))
  })
  
  output$pick_timer <- renderText({
    if (!is.null(values$pick_start_time)) {
      elapsed <- as.numeric(difftime(Sys.time(), values$pick_start_time, units = "secs"))
      minutes <- floor(elapsed / 60)
      seconds <- floor(elapsed %% 60)
      paste("Time on Clock:", sprintf("%02d:%02d", minutes, seconds))
    } else {
      "Time on Clock: 00:00"
    }
  })
  
  output$draft_progress <- renderText({
    state <- values$draft_state
    round_num <- ceiling(state$current_pick / 10)
    paste("Round", round_num, "of 40")
  })
  
  # Available players table
  output$players_table <- DT::renderDataTable({
    input$refresh_players  # Dependency for refresh button
    values$picks_updated   # Dependency on picks to auto-refresh when players are drafted
    
    players <- get_available_players(input$sport_filter, input$position_filter)
    
    DT::datatable(
      players[, c("name", "sport", "position", "team")],
      selection = "single",
      options = list(
        pageLength = 15,
        scrollY = "400px",
        scrollCollapse = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Handle player selection - show confirmation modal
  observeEvent(input$players_table_rows_selected, {
    if (length(input$players_table_rows_selected) > 0) {
      players <- get_available_players(input$sport_filter, input$position_filter)
      selected_player <- players[input$players_table_rows_selected, ]
      
      state <- values$draft_state
      current_drafter <- state$current_drafter
      
      # Store selected player info for confirmation
      values$selected_player <- selected_player
      values$current_drafter <- current_drafter
      
      # Check if it's a valid pick (roster limits)
      if (is_valid_pick(current_drafter, selected_player$sport)) {
        # Show confirmation modal
        showModal(modalDialog(
          title = "Confirm Draft Pick",
          paste("Draft", selected_player$name, "(", selected_player$sport, selected_player$position, "-", selected_player$team, ") to Drafter", current_drafter, "?"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_draft", "Draft", class = "btn-success")
          ),
          easyClose = TRUE
        ))
      } else {
        # Show error message
        limits <- list(NFL = 13, NBA = 14, MLB = 25)
        showNotification(
          paste("Cannot draft", selected_player$name, "- Drafter", current_drafter, 
                "has reached the limit for", selected_player$sport, 
                "players (", limits[[selected_player$sport]], ")"),
          type = "error",
          duration = 5
        )
        
        # Deselect the row
        DT::selectRows(DT::dataTableProxy("players_table"), NULL)
      }
    }
  })
  
  # Handle draft confirmation
  observeEvent(input$confirm_draft, {
    if (!is.null(values$selected_player)) {
      # Make the pick
      make_draft_pick(values$current_drafter, values$selected_player$id)
      
      # Update the picks counter to trigger reactive updates
      values$picks_updated <- values$picks_updated + 1
      
      # Show success message
      showNotification(
        paste("Drafted:", values$selected_player$name, "to Drafter", values$current_drafter),
        type = "message",
        duration = 3
      )
      
      # Clear selected player
      values$selected_player <- NULL
      values$current_drafter <- NULL
      
      # Close modal and deselect row
      removeModal()
      DT::selectRows(DT::dataTableProxy("players_table"), NULL)
    }
  })
  
  # Upcoming picks
  output$upcoming_picks <- renderTable({
    state <- values$draft_state
    current_pick <- state$current_pick
    
    upcoming <- data.frame(
      Pick = (current_pick):(current_pick + 4),
      Drafter = paste("Drafter", sapply((current_pick):(current_pick + 4), get_snake_draft_order))
    )
    
    upcoming[upcoming$Pick <= 400, ]
  }, striped = TRUE, hover = TRUE)
  
  # Recent picks
  output$recent_picks <- renderTable({
    # Add dependency on picks_updated to trigger reactive updates
    values$picks_updated
    
    recent <- get_recent_picks(5)
    if (nrow(recent) > 0) {
      data.frame(
        Pick = recent$pick_number,
        Player = recent$player_name,
        Drafter = recent$drafter_name
      )
    } else {
      data.frame(Pick = character(0), Player = character(0), Drafter = character(0))
    }
  }, striped = TRUE, hover = TRUE)
  
  # NFL Roster
  output$nfl_roster <- renderTable({
    values$picks_updated  # Dependency to update when picks are made
    
    drafter_id <- as.numeric(input$selected_drafter)
    roster <- get_drafter_roster(drafter_id, "NFL")
    
    if (nrow(roster) > 0) {
      data.frame(
        Position = roster$position,
        Player = roster$player_name,
        Team = roster$team
      )
    } else {
      data.frame(Position = character(0), Player = character(0), Team = character(0))
    }
  }, striped = TRUE, hover = TRUE)
  
  # NBA Roster
  output$nba_roster <- renderTable({
    values$picks_updated  # Dependency to update when picks are made
    
    drafter_id <- as.numeric(input$selected_drafter)
    roster <- get_drafter_roster(drafter_id, "NBA")
    
    if (nrow(roster) > 0) {
      data.frame(
        Position = roster$position,
        Player = roster$player_name,
        Team = roster$team
      )
    } else {
      data.frame(Position = character(0), Player = character(0), Team = character(0))
    }
  }, striped = TRUE, hover = TRUE)
  
  # MLB Roster
  output$mlb_roster <- renderTable({
    values$picks_updated  # Dependency to update when picks are made
    
    drafter_id <- as.numeric(input$selected_drafter)
    roster <- get_drafter_roster(drafter_id, "MLB")
    
    if (nrow(roster) > 0) {
      data.frame(
        Position = roster$position,
        Player = roster$player_name,
        Team = roster$team
      )
    } else {
      data.frame(Position = character(0), Player = character(0), Team = character(0))
    }
  }, striped = TRUE, hover = TRUE)
}

# Run the app
shinyApp(ui = ui, server = server)
