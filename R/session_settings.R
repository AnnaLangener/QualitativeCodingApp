# Sidecars deliberately contain only settings, file fingerprints, and codebooks.
# Hashes use UTF-8 CSV with a fixed column/row order and no file metadata.
table_csv <- function(data) {
  data <- as.data.frame(data, check.names = FALSE)
  data[] <- lapply(data, as.character)
  connection <- textConnection("csv", "w", local = TRUE)
  on.exit(close(connection))
  utils::write.csv(data, connection, row.names = FALSE, na = "NA")
  paste0(paste(csv, collapse = "\n"), "\n")
}

csv_table <- function(path = NULL, text = NULL) {
  arguments <- list(check.names = FALSE, colClasses = "character",
                    stringsAsFactors = FALSE, na.strings = "NA")
  if (is.null(text)) arguments$file <- path else arguments$text <- text
  do.call(utils::read.csv, arguments)
}

content_hash <- function(data) {
  digest::digest(enc2utf8(table_csv(data)), algo = "sha256", serialize = FALSE)
}

source_hash <- function(path) {
  data <- if (tolower(tools::file_ext(path)) == "csv") csv_table(path) else
    read_tabular_file(path, "The data file")
  content_hash(data)
}

output_hash <- function(path) content_hash(csv_table(path))

session_storage_columns <- function(settings) {
  variables <- vapply(settings$coding_variables, `[[`, character(1), "name")
  make.names(switch(settings$activity,
    deductive = c(paste0("Code_", variables), "Comments: general", "Comments: depth"),
    inductive_phase1 = c("Familiarization note: beep", "Familiarization note: day",
                        paste0("Proposed_", variables)),
    inductive_phase2 = c("Familiarization note: beep", "Familiarization note: day",
                        paste0("Proposed_", variables), paste0("Code_", variables))
  ))
}

sidecar_path <- function(coding_path) {
  paste0(tools::file_path_sans_ext(coding_path), ".json")
}

coding_output_path <- function(data_file, coder, participant_id) {
  file.path(dirname(data_file), paste0(
    tools::file_path_sans_ext(basename(data_file)), "_",
    validate_storage_identifier(participant_id, "Participant ID"), "_",
    validate_storage_identifier(coder, "Coder"), ".csv"
  ))
}

output_path_taken <- function(path) {
  file.exists(path) || file.exists(sidecar_path(path))
}

next_output_path <- function(path) {
  candidate <- path
  suffix <- 0L
  while (output_path_taken(candidate)) {
    suffix <- suffix + 1L
    candidate <- paste0(tools::file_path_sans_ext(path), "_", suffix, ".csv")
  }
  candidate
}

codebook_settings <- function(file, label) {
  if (is.list(file) && !is.null(file$content_csv)) {
    require_columns(csv_table(text = file$content_csv), "Code", label)
    return(list(name = file$name, content_csv = file$content_csv))
  }
  name <- if (is.character(file)) basename(file) else file$name[[1]]
  path <- if (is.character(file)) file else file$datapath[[1]]
  book <- if (tolower(tools::file_ext(name)) == "csv") {
    require_columns(csv_table(path), "Code", label)
  } else {
    load_codebook(file, label)
  }
  list(name = name, content_csv = table_csv(book))
}

new_session_settings <- function(mode, coder, data_file, coding_path,
                                 participant_id_column, participant_id,
                                 display_columns, coding_variables, codebooks) {
  list(
    schema_version = 1L,
    activity = mode,
    coder = coder,
    started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
    hash_format = "sha256-canonical-csv-v1",
    source = list(name = basename(data_file), sha256 = source_hash(data_file)),
    output = list(name = basename(coding_path), sha256 = ""),
    participant = list(column = participant_id_column, id = participant_id),
    display_columns = unname(as.list(display_columns)),
    coding_variables = lapply(seq_along(coding_variables), function(i) {
      list(name = unname(coding_variables[[i]]), codebook = codebooks[[i]])
    })
  )
}

read_session_settings <- function(path) {
  settings <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  scalar <- function(value) {
    is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
  }
  filename <- function(value) {
    scalar(value) && !grepl("[<>:\"/\\\\|?*]", value) && !value %in% c(".", "..")
  }
  hash <- function(value) scalar(value) && grepl("^[0-9a-f]{64}$", value)
  valid <- is.list(settings) && identical(settings$schema_version, 1L) &&
    scalar(settings$activity) && settings$activity %in%
      c("deductive", "inductive_phase1", "inductive_phase2") &&
    scalar(settings$coder) && scalar(settings$started_at) &&
    identical(settings$hash_format, "sha256-canonical-csv-v1") &&
    filename(settings$source$name) && hash(settings$source$sha256) &&
    filename(settings$output$name) && grepl("\\.csv$", settings$output$name) &&
    hash(settings$output$sha256) && scalar(settings$participant$column) &&
    scalar(settings$participant$id) && is.list(settings$display_columns) &&
    length(settings$display_columns) > 0L &&
    all(vapply(settings$display_columns, scalar, logical(1))) &&
    is.list(settings$coding_variables) && length(settings$coding_variables) > 0L
  if (!isTRUE(valid)) stop("This is not a valid coding-session JSON settings file.")
  validate_storage_identifier(settings$coder, "Coder")
  validate_storage_identifier(settings$participant$id, "Participant ID")
  variables <- vapply(settings$coding_variables, function(variable) {
    if (!is.list(variable) || !scalar(variable$name)) stop("Invalid coding variable in settings file.")
    if (settings$activity != "inductive_phase1") {
      if (!is.list(variable$codebook) || !filename(variable$codebook$name) ||
          !scalar(variable$codebook$content_csv)) stop("Missing codebook in settings file.")
      require_columns(csv_table(text = variable$codebook$content_csv), "Code", "The embedded codebook")
    }
    variable$name
  }, character(1))
  validate_coding_variables(stats::setNames(variables, seq_along(variables)))
  # Reconstruct an allowlist: arbitrary JSON properties are never copied to output.
  list(
    schema_version = 1L, activity = settings$activity, coder = settings$coder,
    started_at = settings$started_at, hash_format = settings$hash_format,
    source = list(name = settings$source$name, sha256 = settings$source$sha256),
    output = list(name = settings$output$name, sha256 = settings$output$sha256),
    participant = list(column = settings$participant$column, id = settings$participant$id),
    display_columns = unname(settings$display_columns),
    coding_variables = lapply(settings$coding_variables, function(variable) {
      list(name = variable$name, codebook = if (settings$activity == "inductive_phase1") NULL else
        list(name = variable$codebook$name, content_csv = variable$codebook$content_csv))
    })
  )
}

verify_session_files <- function(settings, data_file, coding_path) {
  if (!file.exists(coding_path)) stop("The saved output file could not be found.")
  if (!identical(basename(coding_path), settings$output$name) ||
      !identical(output_hash(coding_path), settings$output$sha256)) {
    stop("Continuation is not permitted: the output data does not match the hash in the JSON settings file.")
  }
  if (!identical(source_hash(data_file), settings$source$sha256)) {
    stop("Continuation is not permitted: the source data does not match the hash in the JSON settings file.")
  }
}

write_session_output <- function(data, coding_path, settings) {
  # Prepare both files before publishing either; a partial failure fails closed on resume.
  csv_temp <- tempfile(".coding-", tmpdir = dirname(coding_path), fileext = ".csv")
  json_temp <- tempfile(".coding-", tmpdir = dirname(coding_path), fileext = ".json")
  on.exit(unlink(c(csv_temp, json_temp)))
  writeLines(enc2utf8(table_csv(data)), csv_temp, useBytes = TRUE, sep = "")
  settings$output$sha256 <- output_hash(csv_temp)
  jsonlite::write_json(settings, json_temp, auto_unbox = TRUE, pretty = TRUE, null = "null")
  if (!file.copy(csv_temp, coding_path, overwrite = TRUE) ||
      !file.copy(json_temp, sidecar_path(coding_path), overwrite = TRUE)) {
    stop("Could not save the coding CSV and JSON settings. Check that the output folder is writable.")
  }
  invisible(settings)
}
