bulls_and_cows <-function() {
  print("Test")
  num_guess <- 0
  ###this is a comment 
  # This function returns four random digits to be guessed
  generate_computer_vector <-function() {
    return(c(sample(0:9,4)))
  }

  computer_choice <- generate_computer_vector()
  #print(paste("Computer chose: ", computer_choice))

  # This function prompts a user to guess four digits and return the numbers
  get_guess <- function(num_guess) {
    numbers_string <- readline("Please enter four numbers > ")
    print(paste("You have ", 9-num_guess, "remaining guesses"))
    #num_guess <- num_guess + 1
    return(numbers_string)
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
  while(num_guess<10){
    numbers_string <- get_guess(num_guess)
    user_choice <- as.integer(unlist(strsplit(numbers_string, " ")))
    user_choice <- as.numeric(strsplit(as.character(user_choice), "")[[1]])
    num_guess <- num_guess + 1
    num_bulls_cows <- number_bulls_and_cows(computer_choice,user_choice) #Imprement number_bulls_and_cows function
    print(paste(num_bulls_cows[1], " Bulls", num_bulls_cows[2], " Cows")) # Report number of bulls and cows
    if (num_bulls_cows[1] == 4){ # If correct guess is made break the loop early
      print(paste("You guessed the right number, and it took ",num_guess," number of guess(es)"))
      break
    }
    if(num_guess==10){ # If correct guess was not made in 10 attempts, let the user know the answer.
      print(paste("The correct answer was: ", paste(computer_choice, collapse = " ")))
    }
  }
}


