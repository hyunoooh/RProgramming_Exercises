# Applied Statistical Programming
# Problem Set 03: Let's Make a Deal
# Due Feb. 23, 1:00 PM
# Author: Hyunjoo Oh
# --------------------------------------------------------------




########## Using the S3 class system ###########################

### 1. Define a new S3 class: door
# Create a vector named "prize.set" that contains the different prizes
prize.set <- c("car", "goat", "goat")

# Create a function that makes objects that contian the number, 1, 2, or 3, and the prize.
# The number indicates which door a candidate chooses.
fun.door <- function(number){  # a candidate will choose the number,
  prize <- sample(prize.set, 3, replace=FALSE) # the prizes will be placed behind each door.
                              # Once they are placed, their matching should not be changed.
  door <- list(number, 
               prize=prize[number]) # a door is consisted of its number and the prize
  class(door) <- "door"  # setting the class "door"
  print(door) # call the members and check each object
}

fun.door(1) # the door #1 is created.
fun.door(2) # the door #2 is created.
fun.door(3) # the door #3 is created.




### 2. Create a method for door objects that is called PlayGame
# Create the method
PlayGame <- function(x){
  X <- fun.door(x) # a candidate calls the door number
  if(X$prize == "car"){
    print('Congratulations!')}
  else{
    print('Sorry, you chose the wrong door.')
  }
}

PlayGame(1) # test by calling the door number 1
PlayGame(2) # test by calling the door number 2
PlayGame(3) # test by calling the door number 3




### 3. Using the S4 class system
# contruct a function by which we can create a door object

setMethod(f = "PlayGame", 
          function(door){
            setClass("Door",
                     slots = list(number = "numeric",
                                  prize = "character"))
            prize.set <- c("car", "goat", "goat")
            door <- new("Door", 
                     number=sample(1:3, 1, replace = FALSE), 
                     prize=sample(prize.set, 1, replace = FALSE))
            return(is.integer(door@number))
            return(is.integer(door@prize))
            if(door@prize == "car"){
              print('Congratulations!')}
            else{
              print('Sorry, you chose the wrong door.')
            }
})

PlayGame(1) # test by calling the door number 1
PlayGame(2) # test by calling the door number 2
PlayGame(3) # test by calling the door number 3
