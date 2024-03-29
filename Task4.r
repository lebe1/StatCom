game_of_craps <- function(){
    dice_number <- floor(runif(1, min=1, max=7))
    if (dice_number == 1){
      return("Sorry, you lost!")
    }
    else if (dice_number == 6){
      return("Yeah, you won in the first attempt!")
    }
    else {
      for (i in 1:3){
        new_dice_number <- floor(runif(1, min=1, max=7))
        if (dice_number == new_dice_number){
          return("Yeah, you won rolling the same 'point number' again!")
        }
    }
  } 
  return("Sorry, you lost!")
}

game_of_craps()

