#' Function to create conjoint data by rounds
#'
#' This function eliminates extra characters, usually used for conjoint. For now it only eliminates the characters from the conjoint options field
#' @param df conjoint Dataframe already scrubbed and cleaned of extra characters
#' @param nRounds number of rounds/task of the conjoint
#' @param nInd number of individuals
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return dataframe with rows as task done
#' @keywords conjoint prepare sensata
#'
#'
#' @examples
#' CJData <- conjoint2Tasks(CJData, nRounds = 3, nInd = 42284)
#' @export


# Changing type of timeToCompletion
conjoint2Tasks <- function(df, nRounds, nInd) {
  df <- df %>%
    mutate(across(ends_with("timeToCompletion"),
                  as.character))

  # Create data by tasks
  df <- df %>%
    pivot_longer(-id, names_to = "colname", values_to = "val") %>%
    mutate(colname = gsub("\\.\\d\\.","_",colname)) %>%
    pivot_wider(id_cols = id, names_from = colname, values_from = val, values_fn = list) %>%
    unnest(cols = c(colnames(.)))

  # Create round var
  df$round <- rep(seq(1, nRounds, 1), nInd)

  df <- df %>% relocate(id, round)

  df <- df %>%
    mutate(across(ends_with("timeToCompletion"),
                  as.numeric))
}
