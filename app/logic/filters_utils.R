# Filter utils.R
box::use(
  shiny[moduleServer, NS,
        sliderInput, selectInput, textAreaInput, checkboxInput, fluidRow, tabPanel]
)

# make ui ----
#' @export
make_ui <- function(x, var, nice_var, id, session) {
  ns <- NS(id)
  if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    fluidRow(
      sliderInput(session$ns(var), nice_var, min = rng[1], max = rng[2], value = rng, step = 0.01),
      checkboxInput(session$ns(paste0("na_switch_", var)), 'Include NAs', TRUE)
    )
  } else if (is.factor(x)) {
    levs <- levels(x)
    fluidRow(
      selectInput(session$ns(var), nice_var, choices = levs, selected = levs, multiple = TRUE),
      checkboxInput(session$ns(paste0("na_switch_", var)), 'Include NAs', TRUE)
    )
  } else if (is.character(x)) {
    fluidRow(
      textAreaInput(session$ns(var), nice_var, ""),
      checkboxInput(session$ns(paste0("na_switch_", var)), 'Include NAs', TRUE)
    )
  } else {
    # Not supported
    NULL
  }
}

# filters server ----
#' @export
# filter_var <- function(x, val, switch_val) {
#   if (is.numeric(x)) {
#     if (switch_val == TRUE) {
#       x >= val[1] & x <= val[2] | is.na(x)
#     } else if (switch_val == FALSE) {
#       !is.na(x) & x >= val[1] & x <= val[2]
#     }
#   } else if (is.factor(x)) {
#     if (switch_val == TRUE) {
#       x %in% val | is.na(x)
#     } else if (switch_val == FALSE) {
#       !is.na(x) & x %in% val
#     }
#   } else if (is.character(x)) {
#     # Split the input string by semicolon and remove spaces, convert to lowercase
#     if (nchar(val) > 0) {
#       input_vals <- unlist(strsplit(tolower(val), ";\\s*"))
#       !is.na(x) & tolower(x) %in% input_vals
#     } else {
#       TRUE
#     }
#   } else {
#     # No control, so don't filter
#     TRUE
#   }
# }
#' @export
filter_var <- function(x, val, switch_val) {
  if (is.numeric(x)) {
    if (switch_val == TRUE) {
      x >= val[1] & x <= val[2] | is.na(x)
    } else if (switch_val == FALSE) {
      !is.na(x) & x >= val[1] & x <= val[2]
    }
  } else if (is.factor(x)) {
    if (switch_val == TRUE) {
      x %in% val | is.na(x)
    } else if (switch_val == FALSE) {
      !is.na(x) & x %in% val
    }
  } else if (is.character(x)) {
    # Split the input string by semicolon and remove spaces, convert to lowercase
    if (switch_val == TRUE) {
      if (nchar(val) > 0) {
        input_vals <- unlist(strsplit(tolower(val), ";\\s*"))
        !is.na(x) & tolower(x) %in% input_vals | is.na(x)
      } else {
        TRUE
      }
    } else if (switch_val == FALSE) {
      if (nchar(val) > 0) {
        input_vals <- unlist(strsplit(tolower(val), ";\\s*"))
        !is.na(x) & tolower(x) %in% input_vals
      } else {
        !is.na(x)
      }
    }
    
  } else {
    # No control, so don't filter
    TRUE
  }
}