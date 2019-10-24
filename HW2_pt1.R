# This function plays bulls and cows
bulls_and_cows <-function() {
  num_guess <- 0
  
  ###this is a comment 
  # This function returns four random digits to be guessed
  generate_computer_vector <-function() {
    return(c(sample(0:9,4)))
  }

  computer_choice <- generate_computer_vector()

  # This function prompts a user to guess four digits and return the numbers
  get_guess <- function(num_guess) {
    numbers_string <- readline("Please enter four numbers > ")
    print(paste("You have ", 9-num_guess, "remaining guesses"))
    #num_guess <- num_guess + 1
    return(numbers_string)
  }
  
  # This function checks whether the under input is of length four and if there is any redundant digit.
  validated <- function(user_input){
    is_len_four <- (4 == length(user_input)) ## check if length is four
    if (is_len_four != TRUE){
      print("Your input is not four digits. Please enter four digit number.")
    }
    is_redundant <- (TRUE %in% duplicated(user_input)) ## return TRUE when there is a duplicated element
    if (is_redundant == TRUE){
      print("Your input has redundant digits. Please choose four distinct digits.")
    }
    input_status <- c(is_len_four, is_redundant)
    return(input_status)
    }
  
  # This function has two sub-functions: number_bulls and number_cows that count the number of Bulls and Cows respectively.
  number_bulls_and_cows <-function(computer_choice,user_choice){
    # This function returns the number of bulls
    number_bulls<-function(user_choice,computer_choice){
      bull <- 0 # Initialize/Reset bull to zero
      for (i in 1:4){
        if (user_choice[i] %in% computer_choice & match(user_choice[i],computer_choice)==i){ # Check whether a digit in user_choice matches a digit in computer_choice
            bull <- bull + 1 #increment Bull by 1
          }
      }
      return(bull)
    }
    bulls <- number_bulls(user_choice = user_choice, computer_choice = computer_choice)

    # This function returns the number of cows
    number_cows<-function(computer_choice,user_choice){
      cow <- 0 # Initialize/Reset bull to zero
      for (i in 1:4){
        if (user_choice[i] %in% computer_choice & match(user_choice[i],computer_choice)!=i){ # Check whether a digit in user_choice matches a digit in computer_choice
          cow <- cow + 1 #increment Bull by 1
        }
      }
      return(cow)
    }
    cows <- number_cows(user_choice = user_choice, computer_choice = computer_choice)
    bulls_cows <- c(bulls,cows) # a vector containing number of bulls and cows
    return(bulls_cows)
  }
  
  do_response<-function(num_bulls_cows){
    if (num_bulls_cows[1] != 4){
      print("Try again")
    }
  }
  
  invalidity_tracker <- numeric(0) # This variable stores validity of the input. If valid, add 0 and if invalid add 1
  while(num_guess<10){
    input_status <- c(FALSE, TRUE) # Dafault input validity
    #print(paste("Computer chose: ", computer_choice))
    numbers_string <- get_guess(num_guess)
    user_choice <- as.integer(unlist(strsplit(numbers_string, " ")))
    user_choice <- as.numeric(strsplit(as.character(user_choice), "")[[1]])
    input_status <- validated(user_choice)
    if (input_status[1] == TRUE & input_status[2] == FALSE){ # If input is valid do the following 
      invalidity_tracker <- c(invalidity_tracker,0) # add 0 to the invalidity tracker
      num_guess <- num_guess+1
      num_bulls_cows <- number_bulls_and_cows(computer_choice,user_choice) #Imprement number_bulls_and_cows function
      print(paste(num_bulls_cows[1], " Bulls", num_bulls_cows[2], " Cows")) # Report number of bulls and cows
      do_response(num_bulls_cows = num_bulls_cows)
      if (num_bulls_cows[1] == 4){ # If correct guess is made break the loop early
        print(paste("You guessed the right number, and it took ",num_guess," guess(es)"))
        break
      }
    }
    else if (input_status[1] == FALSE | input_status[2] == TRUE){ # If input is invalid, do the following
      invalidity_tracker <- c(invalidity_tracker,1) # Add 1 to the invalidity tracker
      num_guess <- num_guess+1
      if (sum(tail(invalidity_tracker,3)) == 3){ # Check if the invalid input was made three times in a row.
        print("You entered invalid inputs three times in a row. Game over.")
        break
      }
    }
    if (num_guess==10){ # When number of guesses reach 10, print answer.
      print(paste("The correct answer was: ", paste(computer_choice, collapse = " ")))
    }
  }
}


