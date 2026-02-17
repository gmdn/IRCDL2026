library(shiny)
library(htmltools)
library(jsonlite)

hex_to_rgba <- function(hex, alpha = 0.35) {
  hex <- gsub("#", "", hex)
  r <- strtoi(substr(hex, 1, 2), 16L)
  g <- strtoi(substr(hex, 3, 4), 16L)
  b <- strtoi(substr(hex, 5, 6), 16L)
  sprintf("rgba(%d,%d,%d,%.3f)", r, g, b, alpha)
}