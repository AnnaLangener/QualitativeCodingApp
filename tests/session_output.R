# Run from the project root with Rscript tests/session_output.R.
source(file.path("R", "coding_app.R"))
source(file.path("R", "coding_modes.R"))
source(file.path("R", "session_settings.R"))

expect_error <- function(expression, message) {
  error <- tryCatch({ force(expression); NULL }, error = identity)
  stopifnot(inherits(error, "error"), grepl(message, conditionMessage(error), fixed = TRUE))
}

test_session_output <- function(mode, legacy) {
  directory <- tempfile("session-output-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  source_path <- file.path(directory, "source.csv")
  output_path <- file.path(directory, "coded.csv")
  # Interleave excluded columns and use a custom ID name and unsorted rows.
  data <- data.frame(
    Hidden = c("x", "y", "z"),
    Response = c("second", "other participant", "first"),
    Subject = c(7, 8, 7),
    Day = c(1, 1, 1),
    Context = c("home", "work", "outside"),
    Time1 = c("10:00", "09:00", "08:00"),
    Obs = c(2, 1, 1),
    check.names = FALSE
  )
  utils::write.csv(data, source_path, row.names = FALSE)
  book <- list(name = "book.csv", content_csv = table_csv(data.frame(Code = c("A", "B"))))
  arguments <- list(
    mode = mode, user = "coder", participant_id = "7",
    participant_id_column = "Subject", data_file = source_path,
    coding_path = output_path, display_columns = c("Response", "Subject"),
    coding_variables = c(first = "Emotion"), codebook_files = list(first = book)
  )
  do.call(create_coding_mode, arguments)
  settings <- read_session_settings(sidecar_path(output_path))
  coding_columns <- session_storage_columns(settings)
  saved <- csv_table(output_path)
  stopifnot(identical(names(saved), c("Response", "Subject", coding_columns)),
            identical(saved$Response, c("first", "second")),
            all(is.na(saved[, coding_columns, drop = FALSE])))
  for (column in coding_columns) saved[[column]] <- paste(column, c("A ; B", "note"))
  coding_values <- saved[, coding_columns, drop = FALSE]
  if (legacy != "filtered") {
    source_rows <- select_participant_data(load_coding_data(source_path, names(data), "Subject"), "Subject", "7")
    if (legacy == "timestamp") {
      source_rows <- source_rows[, c("Response", "Subject", "Time1"), drop = FALSE]
    }
    saved <- cbind(source_rows, coding_values)
  }
  write_session_output(saved, output_path, settings)
  settings <- read_session_settings(sidecar_path(output_path))

  # Add and remove display columns at the same time as adding a concept.
  arguments$resume_settings <- settings
  arguments$display_columns <- c("Context", "Day")
  arguments$coding_variables <- c(first = "Emotion", second = "Activity")
  arguments$codebook_files$second <- book
  do.call(create_coding_mode, arguments)
  resumed <- csv_table(output_path)
  updated <- read_session_settings(sidecar_path(output_path))
  updated_columns <- session_storage_columns(updated)
  stopifnot(
    identical(names(resumed), c("Subject", "Day", "Context", updated_columns)),
    identical(resumed$Context, c("outside", "home")),
    identical(resumed[, coding_columns, drop = FALSE], coding_values),
    all(is.na(resumed[, setdiff(updated_columns, coding_columns), drop = FALSE])),
    identical(updated$display_columns, as.list(arguments$display_columns)),
    identical(updated$started_at, settings$started_at),
    identical(output_hash(output_path), updated$output$sha256)
  )

  # Repeated continuation can restore a removed column from the source.
  arguments$resume_settings <- updated
  arguments$display_columns <- c("Obs", "Time1", "Response")
  do.call(create_coding_mode, arguments)
  resumed <- csv_table(output_path)
  updated <- read_session_settings(sidecar_path(output_path))
  stopifnot(
    identical(names(resumed), c("Response", "Subject", "Time1", "Obs", updated_columns)),
    identical(resumed$Response, c("first", "second")),
    identical(resumed[, coding_columns, drop = FALSE], coding_values),
    identical(output_hash(output_path), updated$output$sha256)
  )
  arguments$resume_settings <- updated
  # An unchanged continuation must also succeed.
  do.call(create_coding_mode, arguments)

  invalid <- arguments
  invalid$user <- "another coder"
  expect_error(do.call(create_coding_mode, invalid), "Continuation keeps")
  invalid <- arguments
  invalid$coding_variables <- c(first = "Renamed", second = "Activity")
  expect_error(do.call(create_coding_mode, invalid), "Continuation keeps")

  changed <- resumed
  changed[1, coding_columns[[1]]] <- "external change"
  utils::write.csv(changed, output_path, row.names = FALSE)
  expect_error(do.call(create_coding_mode, arguments), "output data does not match")
  write_session_output(resumed, output_path, updated)
  data$Hidden[[1]] <- "changed source"
  utils::write.csv(data, source_path, row.names = FALSE)
  expect_error(do.call(create_coding_mode, arguments), "source data does not match")
  cat("Passed:", mode, paste0(" (", legacy, " output)"), "\n")
}

for (mode in c("deductive", "inductive_phase1", "inductive_phase2")) {
  for (legacy in c("filtered", "timestamp", "full")) test_session_output(mode, legacy)
}
