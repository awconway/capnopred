#' Convert data frame to set of sequences
#' @export
#' @importFrom dplyr pull filter
#' @importFrom TraMineR seqdef
#' @importFrom zoo na.trim
as.seq <- function(data) {
  ids <- data %>%
    pull(ID) %>%
    unique()
  t_max <- data$Time %>%
    max()
  seq.data <- matrix(NA, nrow = length(ids), ncol = t_max)

  for (i in 1:length(ids)) {
    states <- data %>%
      filter(ID == ids[i]) %>%
      pull("state")
    seq.data[i, 1:length(states)] <- states
  }

  seq.data <- seqdef(seq.data)
  return(seq.data)
}

#' Extract information about duration of states
#'
#' @export
#' @importFrom dplyr bind_rows as_tibble group_by mutate lag
#' @importFrom TraMineR seqdss seqdur
state.dur <- function(seq.data) {
  dss <- seqdss(seq.data) %>% data.frame()
  dss[dss == "%" | dss == "*"] <- NA
  dss <- dss %>% type.convert()

  dur <- seqdur(seq.data)

  state.dur <- list()
  for (i in 1:nrow(dur)) {
    state.dur[[i]] <- rbind(i, dss[i, ], dur[i, ]) %>%
      t() %>%
      unname() %>%
      data.frame() %>%
      zoo::na.trim()
  }



#' @export

  state.dur <- bind_rows(state.dur)
  names(state.dur) <- c("ID", "state", "dur")
  state.dur <- state.dur %>% as_tibble()

  # Add start time variable
  state.dur <- state.dur %>%
    group_by(ID) %>%
    mutate(start = lag(cumsum(dur) + 1)) %>%
    mutate(start = replace(start, start %>% is.na(), 1))

  return(state.dur)
}
