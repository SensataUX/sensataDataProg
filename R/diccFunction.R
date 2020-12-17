#' Function to create sensata dictionary
#'
#' This function allows you to create a dictionary from Sensata's data exported from Mongo.
#' @param dat datafile downloaded from Mongo
#' @param exclude question (ex. q2) to be excluded from the dictionary (ex because is an exit screen)
#' @param f file path to save the dictionaries
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe dictionary and exported to .csv and .Rdata useful for data cleaning
#' @keywords dictionary diccionario sensata
#' @import tidyverse
#'
#' @examples
#' diccFunction("IND_data.csv", q2)
#' @export
# TODO ¿Cómo asegurarse que las preguntas que no salen queden bien? Opciones de respuesta.


diccFunction <- function(dat, exclude = NULL, f = NULL) {
  # Loading packages
  require(tidyverse)

  # Loading data
  d <- read_csv(dat)

  # Exclude vars if they are any to exclude, and selecting the vars needed for dictionary
  if (is.null(exclude)) {
    ques_d <- d %>%
      select(ends_with(".question")) %>%
      unique()
    iden_d <- d %>% select(ends_with(".identifier"))  %>%
      unique()
    type_d <- d %>%
      select(ends_with(".type")) %>%
      select(!(starts_with("geolocation")))  %>%
      unique()
    qid_d <- d %>% select(ends_with(".qid"))  %>%
      unique()
  } else {
    d <- d %>% select(!contains(paste0(exclude, ".", sep = "")))
    ques_d <- d %>%
      select(ends_with(".question")) %>%
      unique()
    iden_d <- d %>% select(ends_with(".identifier"))
    type_d <- d %>%
      select(ends_with(".type")) %>%
      select(!(starts_with("geolocation")))
    qid_d <- d %>% select(ends_with(".qid"))
  }

  # fixing names
  ques_d <- ques_d %>% rename_with(~ gsub("newResponses.", "", .x, fixed = T))
  iden_d <- iden_d %>% rename_with(~ gsub("newResponses.", "", .x, fixed = T))
  type_d <- type_d %>% rename_with(~ gsub("newResponses.", "", .x, fixed = T))
  qid_d <- qid_d %>% rename_with(~ gsub("newResponses.", "", .x, fixed = T))

  ques_d <- ques_d %>% rename_with(~ gsub(".question", "", .x, fixed = T))
  iden_d <- iden_d %>% rename_with(~ gsub(".identifier", "", .x, fixed = T))
  type_d <- type_d %>% rename_with(~ gsub(".type", "", .x, fixed = T))
  qid_d <- qid_d %>% rename_with(~ gsub(".qid", "", .x, fixed = T))

  ques_d <- ques_d %>% fill(everything(), .direction = "down")
  ques_d <- ques_d %>% tail(n = 1)

  iden_d <- iden_d %>% fill(everything(), .direction = "down")
  iden_d <- iden_d %>% tail(n = 1)

  type_d <- type_d %>% fill(everything(), .direction = "down")
  type_d <- type_d %>% tail(n = 1)

  qid_d <- qid_d %>% fill(everything(), .direction = "down")
  qid_d <- qid_d %>% tail(n = 1)

  # pivoting data

  ques_d <- ques_d %>% pivot_longer(
    cols = starts_with("q"),
    names_to = "Q",
    values_to = "question"
  )

  iden_d <- iden_d %>% pivot_longer(
    cols = starts_with("q"),
    names_to = "Q",
    values_to = "identifier"
  )

  type_d <- type_d %>% pivot_longer(
    cols = starts_with("q"),
    names_to = "Q",
    values_to = "type"
  )

  qid_d <- qid_d %>% pivot_longer(
    cols = starts_with("q"),
    names_to = "Q",
    values_to = "qid"
  )

  dic_data <- ques_d %>%
    full_join(iden_d) %>%
    full_join(type_d) %>%
    full_join(qid_d)
  dic_data <- dic_data %>% relocate(Q, identifier, qid, type)
  if (!is.null(f)) {
    write_csv(dic_data, file = paste0(f, ".csv"))
    save(dic_data, file = paste0(f, ".Rdata"))
    dic_data
  } else {
    dic_data
  }
}
