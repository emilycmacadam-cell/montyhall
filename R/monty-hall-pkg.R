#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#' Select a door.
#' @description
#' 'select_door()' randomly selects one of the three doors in the Monty Hall game.
#' @details
#' The function randomly selects one door from doors 1, 2, and 3 to represent the contestant's initial choice.
#' @param ... no arguments are used by the function
#' @return The function returns a numeric value returns a numeric value between 1 and 3 indicating the selected door.
#' @examples
#'   select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Open a door with a goat.
#' @description
#' 'open_goat_door()' selects a door with a goat behind it to open after the contestant makes an initial selection.
#' @details
#' If the contestant initially selects the car, the function randomly selects one of the two doors containing goats. If the contestant initially selects a goat, the function opens the other door containing a goat.
#' @param game A length 3 character vector indicating the positions of the goats and the car.
#' @param a.pick A numeric value between 1 and 3 indicating the contestant's initial door selection.
#' @return The function returns a numeric value between 1 and 3 indicating the door that was opened.
#' @examples
#' game <- c("goat", "car", "goat")
#' open_goat_door(game, 1)
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Stay with or change the selected door.
#' @description
#' 'change_door()' determines the contestant's final door selection based on whether they choose to stay or switch.
#' @details
#' If stay is TRUE, the contestant keeps their original door selection. If stay is FALSE, the contestant switches to the remaining unopened door.
#' @param stay A logical value indicating whether the contestant stays with their original selection. The default is TRUE.
#' @param opened.door A numeric value between 1 and 3 indicating the door opened to reveal a goat.
#' @param a.pick A numeric value between 1 and 3 indicating the contestant's initial door selection.
#' @return The function returns a numeric value between 1 and 3 indicating the contestant's final door selection.
#' @examples
#' change_door(stay=TRUE, opened.door=3, a.pick=1)
#' change_door(stay=FALSE, opened.door=3, a.pick=1)
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine the winner of the game.
#' @description
#' 'determine_winner()' determines whether the contestant wins or loses based on their final door selection.
#' @details
#' The function checks the contestant's final selected door. If the car is behind the selected door, the outcome is a win. If a goat is behind the selected door, the outcome is a loss.
#' @param final.pick A numeric value between 1 and 3 indicating the contestant's final door selection.
#' @param game A length 3 character vector indicating the positions of the goats and the car.
#' @return The function returns a character value of "WIN" if the selected door contains the car or "LOSE" if it contains a goat.
#' @examples
#' game <- c("goat", "car", "goat")
#' determine_winner(2, game)
#' determine_winner(1, game)
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' Play one Monty Hall game.
#' @description
#' 'play_game()' simulates one complete Monty Hall game using both the stay and switch strategies.
#' @details
#' The function creates a new game, randomly selects an initial door, opens a door containing a goat, and determines the outcome for both staying with the original selection and switching to the remaining unopened door.
#' @param ... no arguments are used by the funcction.
#' @return The function returns a data frame containing the stay and switch strategies and the outcome of each strategy.
#' @examples
#' play_game()
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Play multiple Monty Hall games.
#' @description
#' 'play_n_games()' simulates multiple Monty Hall games and compares the outcomes of the stay and switch strategies.
#' @details
#' The function repeatedly calls 'play_game()' for the specified number of games and combines the results into a single data frame. It also calculates and prints the proportion of wins and losses for the stay and switch strategies.
#' @param n A numeric value indicating the number of games to simulate. The default is 100.
#' @return The function returns a data frame containing the strategy and outcome for each simulated game.
#' @examples
#' play_n_games(n=5)
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
