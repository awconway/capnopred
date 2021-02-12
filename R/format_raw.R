# load raw data

#' @export

format_raw <- function(patient, capno_data) {
  patient <- readRDS(patient)
  capno_data <- readRDS(capno_data)

  N <- length(names(capno_data))
  resp.data <- list()

  # Extract data from each patient and store in list resp.data
  for (i in 1:N) {
    id <- names(capno_data)[i]
    patient.info <- patient %>%
     filter(record_id == id)

    df <- capno_data[[i]]
    df <- df %>%
      select("Time", "state", "Midazolam", "Fentanyl") %>%
      mutate(
        Time = seq(1:nrow(df)),
        ID = i,
        Midazolam = as.integer(Midazolam),
        Fentanyl = as.integer(Fentanyl),
        Age = patient.info$Age,
        BMI = patient.info$BMI,
        Sex = patient.info$`Female=1`,
        Smoke = patient.info$`Current smoker=1`,
        OSA = patient.info$`OSA=1`,
        ASA = patient.info$`ASA Class`
      )

    resp.data[[i]] <- df
  }

  # Collapse resp.data into one data.frame
  resp.data <- bind_rows(resp.data) %>%
   select("ID", everything())

  #  Recode respiratory states to simplified scale

  # hyperventilation to normal
  resp.data <- resp.data %>%
    mutate(state = replace(state, state == 9, 0))
  # <CO2PB to hypoventilation
  resp.data <- resp.data %>%
    mutate(state = replace(state, state == 8, 1))
  # RRlow<CO2 to bradypnoea
  resp.data <- resp.data %>%
    mutate(state = replace(state, state == 7, 3))
  # RRlow>CO2 to bradypnoea
  resp.data <- resp.data %>%
    mutate(state = replace(state, state == 6, 3))
  # Periodic breathing to normal
  resp.data <- resp.data %>%
    mutate(state = replace(state, state == 4, 0))
  # <CO2 to hypoventilation
  resp.data <- resp.data %>%
    mutate(state = replace(state, state == 2, 1))

  # Enumerate states according to
  # 1 = Normal
  # 2 = Hypoventilation
  # 3 = Bradypnea
  # 4 = Apnea

  resp.data <- resp.data %>%
  mutate(state = replace(state, state == 1, 2))
  resp.data <- resp.data %>%
   mutate(state = replace(state, state == 0, 1))
  resp.data <- resp.data %>%
   mutate(state = replace(state, state == 5, 4))

  return(resp.data)
}