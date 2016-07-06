# The idea of putting message() along necessary steps in a function is that this way,
# you know which step in the function is causing the error.
# Once the function hits an error, all the other strings you've asked message() to print no
# longer shows up.
# Print() is also commonly used by programmers.

# Another good practice is to delete your message('test') statements as you go.


# message() vs print()

message("ABC", "DEF") # string concatenated
print("ABCDEF")

suppressMessages(message("ABC")) # run it if curious, but just ignore it...not relevant LOL
# Messages give informative output
# Suppressed by the user (?suppressMessages())
# e.g. messages let the user know what value the function has chosen for an important missing argument.

testit <- function() {
  message("testing package startup messages")
  packageStartupMessage("initializing ...", appendLF = FALSE) # not relevant # appendLF: append new line to string?
  Sys.sleep(1) # suspend execution for time interval (in seconds)
  packageStartupMessage(" done")
}

testit() # run the function. noticed that we created a function with no arguments
suppressPackageStartupMessages(testit()) # put the name of the function within the message!
suppressMessages(testit())