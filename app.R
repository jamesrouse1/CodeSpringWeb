library(shiny)

DT_AVAILABLE <- requireNamespace("DT", quietly = TRUE)
BASE64_AVAILABLE <- requireNamespace("base64enc", quietly = TRUE)

table_output <- function(output_id) {
  if (DT_AVAILABLE) DT::dataTableOutput(output_id) else tableOutput(output_id)
}

render_csl_table <- function(expr, page_length = 25, editable = FALSE) {
  if (DT_AVAILABLE) {
    DT::renderDataTable({
      df <- expr
      if (!NROW(df)) df <- data.frame()
      DT::datatable(df, editable = editable, rownames = FALSE, options = list(scrollX = TRUE, pageLength = page_length))
    })
  } else {
    renderTable({
      df <- expr
      if (!NROW(df)) return(data.frame())
      utils::head(df, 500)
    }, striped = TRUE, bordered = TRUE, spacing = "s")
  }
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x)) || !nzchar(as.character(x)[1])) y else x
}

clean_name <- function(x, fallback = "sample") {
  x <- gsub("[^A-Za-z0-9_]+", "_", trimws(as.character(x)))
  x <- gsub("^_+|_+$", "", x)
  ifelse(nzchar(x), x, fallback)
}

find_codespringlab_root <- function() {
  env_root <- Sys.getenv("CSL_CODESPRINGLAB_ROOT", unset = "")
  candidates <- unique(c(
    env_root,
    getwd(),
    dirname(getwd()),
    path.expand("~/CodeSpringLab"),
    path.expand("~/CSH/CodeSpringLab"),
    "/grid/bsr/home/rouse/CodeSpringLab",
    "/Users/rouse/CSH/CodeSpringLab"
  ))
  for (candidate in candidates[nzchar(candidates)]) {
    if (dir.exists(file.path(candidate, "scripts_DoNotTouch"))) {
      return(normalizePath(candidate, winslash = "/", mustWork = FALSE))
    }
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

CSL_ROOT <- find_codespringlab_root()
SCRIPTS_DIR <- file.path(CSL_ROOT, "scripts_DoNotTouch")
APP_HOME <- path.expand(Sys.getenv("CSL_WEB_HOME", unset = "~/.codespringweb"))
dir.create(APP_HOME, recursive = TRUE, showWarnings = FALSE)
JOBS_PATH <- file.path(APP_HOME, "jobs.tsv")
LOGO_CSL_PATH <- file.path(SCRIPTS_DIR, "Logo_CSL.png")
LOGO_PATH <- file.path(SCRIPTS_DIR, "Logo.png")
FLOWCHART_PATH <- file.path(SCRIPTS_DIR, "flowchart.png")
if (file.exists(LOGO_CSL_PATH)) addResourcePath("csl_logo", dirname(LOGO_CSL_PATH))
if (file.exists(LOGO_PATH)) addResourcePath("codespring_logo", dirname(LOGO_PATH))
if (file.exists(FLOWCHART_PATH)) addResourcePath("codespring_flowchart", dirname(FLOWCHART_PATH))

cleanup_previous_shiny_processes <- function() {
  if (identical(Sys.getenv("CSL_WEB_AUTOKILL_SHINY", unset = "1"), "0")) return(invisible(character(0)))
  current_pid <- as.integer(Sys.getpid())
  current_user <- Sys.info()[["user"]] %||% ""
  killed <- character(0)

  run_quiet <- function(command, args) {
    suppressWarnings(tryCatch(system2(command, args, stdout = TRUE, stderr = FALSE), error = function(e) character(0)))
  }

  pid_command <- function(pid) {
    paste(run_quiet("ps", c("-p", as.character(pid), "-o", "command=")), collapse = " ")
  }

  pid_user <- function(pid) {
    trimws(paste(run_quiet("ps", c("-p", as.character(pid), "-o", "user=")), collapse = " "))
  }

  looks_like_r_shiny <- function(cmd) {
    grepl("(^|/)(R|Rscript)(\\s|$)|/exec/R(\\s|$)|shiny::runApp|runApp\\(|CodeSpringWeb|scripts_DoNotTouch/Shiny|RNASEQ_SHINY", cmd)
  }

  kill_pid <- function(pid, reason, signal = tools::SIGTERM) {
    pid <- suppressWarnings(as.integer(pid))
    if (is.na(pid) || pid <= 1 || identical(pid, current_pid)) return(invisible(FALSE))
    ok <- tryCatch({
      tools::pskill(pid, signal)
      TRUE
    }, error = function(e) FALSE)
    if (ok) {
      label <- if (identical(signal, tools::SIGKILL)) "SIGKILL" else "SIGTERM"
      killed <<- unique(c(killed, paste0("pid:", pid, " (", reason, ", ", label, ")")))
    }
    invisible(ok)
  }

  listener_pids <- function(port) {
    if (!nzchar(Sys.which("lsof"))) return(character(0))
    pids <- run_quiet("lsof", c("-nP", paste0("-iTCP:", port), "-sTCP:LISTEN", "-t"))
    unique(trimws(pids[nzchar(pids)]))
  }

  stop_listener <- function(pid, reason, signal = tools::SIGTERM, require_shiny = TRUE) {
    cmd <- pid_command(pid)
    user <- pid_user(pid)
    same_user <- !nzchar(current_user) || !nzchar(user) || identical(user, current_user)
    if (!same_user) return(invisible(FALSE))
    if (!require_shiny || looks_like_r_shiny(cmd)) {
      reason <- paste0(reason, if (nzchar(cmd)) paste0(", command: ", substr(cmd, 1, 120)) else "")
      return(kill_pid(pid, reason, signal))
    }
    invisible(FALSE)
  }

  pidfiles <- list.files(APP_HOME, pattern = "^codespringweb_.*\\.pid$|^rnaseq_shiny_.*\\.pid$", full.names = TRUE)
  for (pf in pidfiles) {
    pid <- suppressWarnings(as.integer(readLines(pf, warn = FALSE, n = 1)))
    kill_pid(pid, paste0("pidfile:", basename(pf)))
    unlink(pf, force = TRUE)
  }

  ps_lines <- run_quiet("ps", c("-eo", "pid=,command="))
  for (line in ps_lines) {
    line <- trimws(line)
    m <- regexec("^([0-9]+)\\s+(.+)$", line)
    hit <- regmatches(line, m)[[1]]
    if (length(hit) != 3) next
    pid <- suppressWarnings(as.integer(hit[2]))
    cmd <- hit[3]
    if (looks_like_r_shiny(cmd) && !identical(pid, current_pid)) kill_pid(pid, "R/Shiny process")
  }

  shiny_ports <- 3838:3850
  web_ports <- 8501:8515
  for (port in shiny_ports) {
    for (pid in listener_pids(port)) stop_listener(pid, paste0("Shiny port:", port), require_shiny = TRUE)
  }
  for (port in web_ports) {
    for (pid in listener_pids(port)) stop_listener(pid, paste0("CodeSpringWeb port:", port), require_shiny = FALSE)
  }

  Sys.sleep(0.7)
  for (port in shiny_ports) {
    for (pid in listener_pids(port)) stop_listener(pid, paste0("Shiny port still busy:", port), tools::SIGKILL, require_shiny = TRUE)
  }
  for (port in web_ports) {
    for (pid in listener_pids(port)) stop_listener(pid, paste0("CodeSpringWeb port still busy:", port), tools::SIGKILL, require_shiny = FALSE)
  }

  Sys.sleep(0.3)
  busy_web <- unlist(lapply(web_ports, function(port) {
    pids <- listener_pids(port)
    if (!length(pids)) return(character(0))
    paste0(port, "=", paste(pids, collapse = ","))
  }), use.names = FALSE)
  if (length(busy_web)) {
    cat("WARNING: these CodeSpringWeb ports are still busy after cleanup: ", paste(busy_web, collapse = "; "), "\n", sep = "")
  }

  pid_path <- file.path(APP_HOME, paste0("codespringweb_", current_pid, ".pid"))
  writeLines(as.character(current_pid), pid_path)
  if (length(killed)) {
    cat("Stopped previous CodeSpring/R Shiny sessions before starting CodeSpringWeb: ", paste(killed, collapse = ", "), "\n", sep = "")
  } else {
    cat("Checked for previous CodeSpring/R Shiny sessions; none needed cleanup.\n")
  }
  invisible(killed)
}

analysis_label <- function(x) {
  x <- tolower(as.character(x %||% "rna"))
  if (grepl("atac", x)) return("ATAC-seq")
  if (grepl("chip", x)) return("ChIP-seq")
  "RNA-seq"
}

analysis_key <- function(x) {
  x <- tolower(as.character(x %||% "rna"))
  if (grepl("atac", x)) return("atac")
  if (grepl("chip", x)) return("chip")
  "rna"
}

analysis_notebook_dir <- function(key) {
  switch(analysis_key(key), atac = "bulkATACseq", chip = "bulkChIPseq", rna = "bulkRNAseq")
}

parse_py_config <- function(path) {
  values <- list()
  if (!file.exists(path)) return(values)
  lines <- readLines(path, warn = FALSE)
  for (line in lines) {
    line <- trimws(line)
    if (!nzchar(line) || grepl("^#", line)) next
    m <- regexec("^([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*(.*)$", line)
    hit <- regmatches(line, m)[[1]]
    if (length(hit) != 3) next
    key <- hit[2]
    val <- trimws(hit[3])
    val <- sub("\\s+#.*$", "", val)
    if ((startsWith(val, "'") && endsWith(val, "'")) || (startsWith(val, "\"") && endsWith(val, "\""))) {
      val <- substr(val, 2, nchar(val) - 1)
    }
    values[[key]] <- val
  }
  values
}

resolve_legacy_path <- function(value, key = "rna") {
  value <- trimws(as.character(value %||% ""))
  if (!nzchar(value)) return("")
  value <- path.expand(value)
  if (startsWith(value, "/")) return(normalizePath(value, winslash = "/", mustWork = FALSE))
  base <- file.path(CSL_ROOT, analysis_notebook_dir(key))
  normalizePath(file.path(base, value), winslash = "/", mustWork = FALSE)
}

with_slash <- function(path) {
  path <- trimws(as.character(path %||% ""))
  if (!nzchar(path)) return(path)
  paste0(sub("/+$", "", path), "/")
}

design_path_from_dir <- function(path) {
  path <- trimws(as.character(path %||% ""))
  if (!nzchar(path)) return("")
  if (basename(path) == "design_matrix.txt") return(path)
  file.path(path, "design_matrix.txt")
}

legacy_project_from_config <- function(path) {
  vals <- parse_py_config(path)
  if (!length(vals) && basename(path) != "config.py") return(NULL)
  key <- analysis_key(vals$analysis_type %||% basename(dirname(path)))
  project_name <- vals$project_name %||% tools::file_path_sans_ext(basename(path))
  if (!nzchar(project_name)) return(NULL)
  results_root <- resolve_legacy_path(vals$results_directory %||% "../../csl_results/", key)
  visualizer_data_dir <- resolve_legacy_path(vals$visualizer_data_dir %||% "", key)
  if (nzchar(visualizer_data_dir) && basename(visualizer_data_dir) == "data" && basename(dirname(visualizer_data_dir)) == project_name) {
    results_root <- dirname(dirname(visualizer_data_dir))
  }
  inpath_design <- resolve_legacy_path(vals$inpath_design %||% "", key)
  fastq_dir <- resolve_legacy_path(vals$read_path_destination %||% vals$read_path_original %||% "", key)
  pairing <- tolower(vals$pairing %||% "y")
  data_dir <- if (nzchar(visualizer_data_dir)) visualizer_data_dir else file.path(results_root, project_name, "data")
  list(
    id = paste0(key, "/", clean_name(project_name, "project")),
    name = clean_name(project_name, "project"),
    label = project_name,
    analysis = analysis_label(key),
    analysis_key = key,
    genome = tolower(vals$genome %||% "mouse"),
    paired_end = !(pairing %in% c("n", "no", "false", "single", "se")),
    results_root = results_root,
    data_dir = data_dir,
    fastq_dir = fastq_dir,
    design_matrix_path = design_path_from_dir(inpath_design),
    source_config = normalizePath(path, winslash = "/", mustWork = FALSE),
    source = "CodeSpringLab config"
  )
}

discover_projects <- function() {
  roots <- c(file.path(SCRIPTS_DIR, "project_configs"), file.path(CSL_ROOT, "project_configs"))
  files <- character(0)
  for (root in roots) {
    if (dir.exists(root)) files <- c(files, list.files(root, pattern = "\\.py$", recursive = TRUE, full.names = TRUE))
  }
  active <- file.path(SCRIPTS_DIR, "config.py")
  if (file.exists(active)) files <- c(files, active)
  files <- unique(normalizePath(files, winslash = "/", mustWork = FALSE))
  projects <- Filter(Negate(is.null), lapply(files, legacy_project_from_config))
  if (!length(projects)) {
    projects <- list(list(
      id = "rna/example_dataset",
      name = "example_dataset",
      label = "example_dataset",
      analysis = "RNA-seq",
      analysis_key = "rna",
      genome = "mouse",
      paired_end = TRUE,
      results_root = normalizePath(path.expand("~/csl_results"), winslash = "/", mustWork = FALSE),
      data_dir = normalizePath(path.expand("~/csl_results/example_dataset/data"), winslash = "/", mustWork = FALSE),
      fastq_dir = "",
      design_matrix_path = normalizePath(path.expand("~/csl_results/example_dataset/data/manifest/design_matrix.txt"), winslash = "/", mustWork = FALSE),
      source_config = "",
      source = "default"
    ))
  }
  names(projects) <- vapply(projects, `[[`, character(1), "id")
  projects
}

new_project_from_inputs <- function(input) {
  key <- analysis_key(input$new_project_analysis %||% input$analysis %||% "RNA-seq")
  project_name <- clean_name(input$new_project_name %||% paste0("new_", key, "_project"), paste0("new_", key, "_project"))
  label <- input$new_project_name %||% project_name
  results_root <- normalizePath(path.expand(input$new_results_root %||% "~/csl_results"), winslash = "/", mustWork = FALSE)
  data_dir <- file.path(results_root, project_name, "data")
  design_path <- trimws(input$new_design_matrix_path %||% "")
  if (!nzchar(design_path)) design_path <- file.path(data_dir, "manifest", "design_matrix.txt")
  design_path <- normalizePath(path.expand(design_path), winslash = "/", mustWork = FALSE)
  fastq_dir <- normalizePath(path.expand(input$new_fastq_dir %||% ""), winslash = "/", mustWork = FALSE)
  paired <- !tolower(input$new_paired_end %||% "paired") %in% c("single", "se", "n", "no", "false")
  list(
    id = paste0(key, "/", project_name),
    name = project_name,
    label = label,
    analysis = analysis_label(key),
    analysis_key = key,
    genome = tolower(input$new_genome %||% "mouse"),
    paired_end = paired,
    results_root = results_root,
    data_dir = data_dir,
    fastq_dir = fastq_dir,
    design_matrix_path = design_path,
    source_config = "",
    source = "new project"
  )
}

project_config_dir <- function(key) {
  file.path(SCRIPTS_DIR, "project_configs", analysis_key(key))
}

write_project_config <- function(project) {
  cfg_dir <- project_config_dir(project$analysis_key)
  dir.create(cfg_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(project$data_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(project$design_matrix_path), recursive = TRUE, showWarnings = FALSE)
  cfg_path <- file.path(cfg_dir, paste0(clean_name(project$name, "project"), ".py"))
  lines <- c(
    sprintf("analysis_type = %s", deparse(project$analysis_key)),
    sprintf("project_name = %s", deparse(project$name)),
    sprintf("results_directory = %s", deparse(with_slash(project$results_root))),
    sprintf("visualizer_data_dir = %s", deparse(project$data_dir)),
    sprintf("inpath_design = %s", deparse(dirname(project$design_matrix_path))),
    sprintf("read_path_original = %s", deparse(project$fastq_dir)),
    sprintf("read_path_destination = %s", deparse(project$fastq_dir)),
    sprintf("genome = %s", deparse(project$genome)),
    sprintf("pairing = %s", deparse(if (isTRUE(project$paired_end)) "y" else "n"))
  )
  writeLines(lines, cfg_path)
  cfg_path
}

safe_read_table <- function(path, n = Inf) {
  if (!file.exists(path)) return(data.frame())
  ext <- tolower(tools::file_ext(path))
  sep <- if (ext == "csv") "," else "\t"
  tryCatch({
    utils::read.table(
      path,
      sep = sep,
      header = TRUE,
      quote = "\"",
      comment.char = "",
      check.names = FALSE,
      nrows = if (is.finite(n)) n else -1
    )
  }, error = function(e) {
    tryCatch({
      utils::read.table(
        path,
        sep = "",
        header = TRUE,
        quote = "\"",
        comment.char = "",
        check.names = FALSE,
        nrows = if (is.finite(n)) n else -1
      )
    }, error = function(e2) data.frame())
  })
}

render_data_table <- function(df, page_length = 25, height = NULL) {
  if (!NROW(df)) return(tags$div(class = "empty-box", "No rows available."))
  if (DT_AVAILABLE) {
    DT::datatable(
      df,
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = page_length, scrollX = TRUE, scrollY = height %||% "520px")
    )
  } else {
    tableOutput(NULL)
  }
}

download_table <- function(df, file) {
  utils::write.csv(df, file, row.names = FALSE)
}

fastq_suffix_regex <- "\\.(fastq\\.gz|fq\\.gz|fastq|fq)$"

fastq_files <- function(folder) {
  if (!dir.exists(folder)) return(character(0))
  files <- list.files(folder, full.names = FALSE)
  files[grepl(fastq_suffix_regex, tolower(files))]
}

mate_name <- function(x, mate = 2) {
  stem <- sub(fastq_suffix_regex, "", x, ignore.case = TRUE)
  suffix <- regmatches(x, regexpr(fastq_suffix_regex, x, ignore.case = TRUE))
  if (!length(suffix) || suffix == "-1") suffix <- ""
  if (mate == 2) {
    out <- sub("([._-]R)1([._-]?[0-9]*)$", "\\12\\2", stem, ignore.case = TRUE)
    if (identical(out, stem)) out <- sub("([._-])1$", "\\12", stem)
  } else {
    out <- sub("([._-]R)2([._-]?[0-9]*)$", "\\11\\2", stem, ignore.case = TRUE)
    if (identical(out, stem)) out <- sub("([._-])2$", "\\11", stem)
  }
  if (identical(out, stem)) return(NA_character_)
  paste0(out, suffix)
}

infer_sample <- function(x) {
  stem <- sub(fastq_suffix_regex, "", basename(x), ignore.case = TRUE)
  stem <- sub("([._-]R)[12]([._-]?[0-9]*)$", "", stem, ignore.case = TRUE)
  stem <- sub("([._-])[12]$", "", stem)
  clean_name(stem)
}

scan_fastqs <- function(folder, paired = TRUE, metadata_cols = "treatment") {
  files <- fastq_files(folder)
  rows <- list()
  used <- character(0)
  if (paired) {
    for (r1 in files) {
      if (r1 %in% used) next
      r2 <- mate_name(r1, 2)
      if (!is.na(r2) && r2 %in% files) {
        rows[[length(rows) + 1]] <- data.frame(include = TRUE, sample = infer_sample(r1), filename = paste(r1, r2, sep = ","), status = "paired")
        used <- c(used, r1, r2)
      } else if (grepl("([._-]R)1|([._-])1", r1, ignore.case = TRUE)) {
        rows[[length(rows) + 1]] <- data.frame(include = FALSE, sample = infer_sample(r1), filename = r1, status = "missing R2")
        used <- c(used, r1)
      }
    }
  } else {
    for (f in files) rows[[length(rows) + 1]] <- data.frame(include = TRUE, sample = infer_sample(f), filename = f, status = "single")
  }
  df <- if (length(rows)) do.call(rbind, rows) else data.frame(include = logical(), sample = character(), filename = character(), status = character())
  for (col in metadata_cols) if (!col %in% names(df)) df[[col]] <- ""
  df[, c("include", "sample", metadata_cols, "filename", "status"), drop = FALSE]
}

design_matrix_columns <- function(df) {
  if (!NROW(df)) return(c("include", "sample", "treatment", "filename", "status"))
  c("include", "sample", setdiff(names(df), c("include", "sample", "filename", "status")), "filename", "status")
}

as_design_bool <- function(x) {
  if (is.logical(x)) return(isTRUE(x))
  tolower(as.character(x %||% "")) %in% c("true", "t", "1", "yes", "y")
}

design_input_id <- function(row, col) {
  paste0("design_", col, "_", row)
}

collect_design_inputs <- function(input, df) {
  if (!NROW(df)) return(df)
  cols <- design_matrix_columns(df)
  for (i in seq_len(NROW(df))) {
    for (col in cols) {
      id <- design_input_id(i, col)
      val <- input[[id]]
      if (is.null(val)) next
      if (identical(col, "include")) {
        df[[col]][i] <- isTRUE(val)
      } else {
        df[[col]][i] <- as.character(val)
      }
    }
  }
  df
}

design_matrix_ui <- function(df) {
  if (!NROW(df)) return(div(class = "empty-box", "Scan a FASTQ folder or select a project with an existing design_matrix.txt."))
  cols <- design_matrix_columns(df)
  df <- df[, cols, drop = FALSE]
  tags$div(
    class = "design-table-scroll",
    tags$table(
      class = "design-matrix-table",
      tags$thead(tags$tr(lapply(cols, tags$th))),
      tags$tbody(lapply(seq_len(NROW(df)), function(i) {
        tags$tr(lapply(cols, function(col) {
          value <- df[[col]][i]
          tags$td(
            if (identical(col, "include")) {
              checkboxInput(design_input_id(i, col), NULL, value = as_design_bool(value), width = "70px")
            } else if (identical(col, "status")) {
              tags$span(class = "status-path", as.character(value %||% ""))
            } else {
              textInput(
                design_input_id(i, col),
                NULL,
                value = as.character(value %||% ""),
                width = if (identical(col, "filename")) "420px" else "180px"
              )
            }
          )
        }))
      }))
    )
  )
}

write_design_matrix <- function(project, df, metadata_cols) {
  if (!"include" %in% names(df)) df$include <- TRUE
  keep <- df[vapply(df$include, as_design_bool, logical(1)), , drop = FALSE]
  if (!NROW(keep)) stop("No samples are included.")
  keep$sample <- clean_name(keep$sample)
  out <- project$design_matrix_path
  if (!nzchar(out) || basename(out) != "design_matrix.txt") {
    out <- file.path(project$data_dir, "manifest", "design_matrix.txt")
  }
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  keep <- keep[, c("sample", metadata_cols, "filename"), drop = FALSE]
  utils::write.table(keep, out, sep = "\t", row.names = FALSE, quote = FALSE)
  out
}

project_design_df <- function(project) {
  df <- safe_read_table(project$design_matrix_path)
  if (!NROW(df)) return(data.frame())
  if (!"sample" %in% names(df)) names(df)[1] <- "sample"
  df
}

design_compare_columns <- function(project) {
  df <- project_design_df(project)
  if (!NROW(df)) return(character(0))
  nms <- names(df)
  sample_i <- match("sample", nms)
  filename_i <- match("filename", nms)
  if (!is.na(sample_i) && !is.na(filename_i) && filename_i > sample_i + 1) {
    cols <- nms[(sample_i + 1):(filename_i - 1)]
  } else {
    cols <- setdiff(nms, c("sample", "filename", "include", "status"))
  }
  setdiff(cols, c("include", "status"))
}

design_compare_values <- function(project, col) {
  df <- project_design_df(project)
  if (!NROW(df) || !nzchar(col %||% "") || !col %in% names(df)) return(character(0))
  vals <- unique(as.character(df[[col]]))
  vals <- vals[!is.na(vals) & nzchar(vals)]
  sort(vals)
}

deseq_design_for_column <- function(project, compare_col) {
  df <- project_design_df(project)
  if (!NROW(df)) stop("No design matrix found.")
  if (!compare_col %in% names(df)) stop("Selected comparison column is not in design matrix: ", compare_col)
  if (!"filename" %in% names(df)) df$filename <- df$sample
  keep <- df[, c("sample", compare_col, "filename"), drop = FALSE]
  out_dir <- file.path(project$data_dir, "manifest", paste0("deseq2_", clean_name(compare_col, "comparison")))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out <- file.path(out_dir, "design_matrix.txt")
  utils::write.table(keep, out, sep = "\t", row.names = FALSE, quote = FALSE)
  out
}

count_files <- function(path, pattern) {
  if (!dir.exists(path)) return(0)
  length(list.files(path, pattern = pattern, recursive = TRUE, full.names = TRUE))
}

extract_job_id <- function(x) {
  m <- regexpr("job_id:[[:space:]]*[0-9]+", x)
  if (m < 0) return("")
  sub("job_id:[[:space:]]*", "", regmatches(x, m))
}

job_history <- function(project) {
  if (!file.exists(JOBS_PATH)) return(data.frame())
  jobs <- tryCatch(utils::read.delim(JOBS_PATH, check.names = FALSE, stringsAsFactors = FALSE), error = function(e) data.frame())
  if (!NROW(jobs) || !"project" %in% names(jobs) || !"step" %in% names(jobs) || !"output" %in% names(jobs)) return(data.frame())
  jobs <- jobs[jobs$project == project$name, , drop = FALSE]
  if (!NROW(jobs)) return(data.frame())
  jobs$job_id <- vapply(as.character(jobs$output), extract_job_id, character(1))
  jobs$slurm_state <- ifelse(nzchar(jobs$job_id), "Submitted", "No job id")
  ids <- unique(jobs$job_id[nzchar(jobs$job_id)])
  if (length(ids) && nzchar(Sys.which("squeue"))) {
    sq <- tryCatch(system2("squeue", c("-h", "-j", paste(ids, collapse = ","), "-o", "%A|%T"), stdout = TRUE, stderr = FALSE), error = function(e) character(0))
    sq <- sq[nzchar(sq)]
    if (length(sq)) {
      parts <- strsplit(sq, "|", fixed = TRUE)
      state_map <- setNames(vapply(parts, function(x) if (length(x) >= 2) x[2] else "Active", character(1)),
                            vapply(parts, function(x) x[1], character(1)))
      matched <- jobs$job_id %in% names(state_map)
      jobs$slurm_state[matched] <- unname(state_map[jobs$job_id[matched]])
      jobs$slurm_state[!matched & nzchar(jobs$job_id)] <- "Finished or not in queue"
    } else {
      jobs$slurm_state[nzchar(jobs$job_id)] <- "Finished or not in queue"
    }
  }
  extract_output_field <- function(x, key) {
    pat <- paste0(".*", key, ":[[:space:]]*([^\\n]+).*")
    val <- sub(pat, "\\1", x)
    ifelse(identical(val, x), "", val)
  }
  jobs$input_mode <- vapply(as.character(jobs$output), extract_output_field, character(1), key = "input_mode")
  jobs$stdout <- vapply(as.character(jobs$output), extract_output_field, character(1), key = "stdout")
  jobs$stderr <- vapply(as.character(jobs$output), extract_output_field, character(1), key = "stderr")
  keep <- intersect(c("time", "step", "job_id", "slurm_state", "input_mode", "stdout", "stderr"), names(jobs))
  jobs[, keep, drop = FALSE]
}

last_job_modes <- function(project) {
  jobs <- job_history(project)
  if (!NROW(jobs) || !"input_mode" %in% names(jobs)) return(setNames(character(0), character(0)))
  jobs <- jobs[nzchar(jobs$input_mode), , drop = FALSE]
  if (!NROW(jobs)) return(setNames(character(0), character(0)))
  out <- tapply(jobs$input_mode, jobs$step, function(x) tail(x, 1))
  unlist(out)
}

log_file_choices <- function(project) {
  vals <- character(0)
  add_log_choice <- function(label, path) {
    path <- as.character(path %||% "")
    path <- path[!is.na(path) & nzchar(path)]
    if (!length(path)) return(invisible(NULL))
    for (one_path in path) vals[[paste(label, basename(one_path))]] <<- one_path
    invisible(NULL)
  }

  project_log_dir <- file.path(dirname(project$data_dir), "log")
  if (dir.exists(project_log_dir)) {
    files <- list.files(project_log_dir, pattern = "^(output|error)_.*\\.txt$", full.names = TRUE)
    add_log_choice("Project log", files)
  }

  jobs <- job_history(project)
  if (NROW(jobs)) {
    for (i in seq_len(NROW(jobs))) {
      label_base <- paste(jobs$time[i] %||% "", jobs$step[i] %||% "", jobs$job_id[i] %||% "")
      if ("stdout" %in% names(jobs)) add_log_choice(paste(label_base, "stdout"), jobs$stdout[i])
      if ("stderr" %in% names(jobs)) add_log_choice(paste(label_base, "stderr"), jobs$stderr[i])
    }
  }

  vals <- as.character(vals)
  vals <- vals[!is.na(vals) & nzchar(vals)]
  vals <- vals[file.exists(vals)]
  vals <- vals[!duplicated(vals)]
  vals
}

read_log_excerpt <- function(path, mode = "tail", n = 120) {
  if (!nzchar(path %||% "") || !file.exists(path)) return("")
  lines <- readLines(path, warn = FALSE)
  mode <- mode %||% "tail"
  if (identical(mode, "head")) lines <- utils::head(lines, n)
  else if (identical(mode, "full")) lines <- lines
  else lines <- utils::tail(lines, n)
  paste(lines, collapse = "\n")
}

active_job_steps <- function(project) {
  jobs <- job_history(project)
  if (!NROW(jobs) || !"slurm_state" %in% names(jobs)) return(character(0))
  active_states <- c("PENDING", "CONFIGURING", "COMPLETING", "RUNNING", "SUSPENDED", "Submitted")
  unique(jobs$step[jobs$slurm_state %in% active_states])
}

normalize_pipeline_status <- function(status) {
  status <- as.character(status)
  ifelse(status %in% c("Complete"), "Complete", ifelse(status %in% c("Active"), "Active", "Not started"))
}

project_status <- function(project) {
  data_dir <- project$data_dir
  design <- project$design_matrix_path
  raw <- data.frame(
    step = c("Setup", "Design matrix", "FASTQ reads", "FastQC", "Cutadapt", "STAR", "Kallisto", "featureCounts", "Count matrix", "DESeq2", "GSEA"),
    status = c(
      if (nzchar(project$name)) "Complete" else "Not started",
      if (file.exists(design)) "Complete" else "Not started",
      if (dir.exists(project$fastq_dir) && length(fastq_files(project$fastq_dir))) "Complete" else "Not started",
      if (count_files(file.path(data_dir, "fastqc"), "\\.html$") + count_files(file.path(data_dir, "fastqc_cutadapt"), "\\.html$") > 0) "Complete" else "Not started",
      if (count_files(file.path(data_dir, "cutadapt"), fastq_suffix_regex) > 0) "Complete" else "Not started",
      if (count_files(file.path(data_dir, "star"), "Aligned\\.sortedByCoord\\.out\\.bam$") > 0) "Complete" else "Not started",
      if (count_files(file.path(data_dir, "kallisto"), "abundance\\.tsv$") > 0) "Complete" else "Not started",
      if (count_files(file.path(data_dir, "featurecounts"), "_counts\\.txt$") > 0) "Complete" else "Not started",
      if (file.exists(file.path(data_dir, "counts", "count_matrix.txt"))) "Complete" else "Not started",
      if (count_files(file.path(data_dir, "deseq2"), "DEG|normalized") > 0) "Complete" else "Not started",
      if (count_files(file.path(data_dir, "gseapy"), "\\.(csv|txt|png|pdf)$") > 0) "Complete" else "Not started"
    ),
    path = c(
      dirname(data_dir),
      design,
      project$fastq_dir,
      file.path(data_dir, "fastqc"),
      file.path(data_dir, "cutadapt"),
      file.path(data_dir, "star"),
      file.path(data_dir, "kallisto"),
      file.path(data_dir, "featurecounts"),
      file.path(data_dir, "counts", "count_matrix.txt"),
      file.path(data_dir, "deseq2"),
      file.path(data_dir, "gseapy")
    ),
    stringsAsFactors = FALSE
  )
  modes <- last_job_modes(project)
  raw$input <- unname(modes[raw$step])
  raw$input[is.na(raw$input)] <- ""
  active <- active_job_steps(project)
  raw$status[raw$step %in% active & raw$status != "Complete"] <- "Active"
  raw$status <- normalize_pipeline_status(raw$status)
  raw
}

status_rank <- function(status) {
  match(status, c("Active", "Complete", "Not started"), nomatch = 99)
}

status_pill <- function(status) {
  cls <- switch(status, "Active" = "active", "Complete" = "complete", "not-started")
  tags$span(class = paste("status-pill", cls), status)
}

status_cards <- function(df) {
  if (!NROW(df)) return(div(class = "empty-box", "No steps available."))
  df <- df[order(status_rank(df$status), df$step), , drop = FALSE]
  tagList(lapply(seq_len(NROW(df)), function(i) {
    div(class = "status-card",
        div(class = "status-card-top",
            tags$strong(df$step[i]),
            status_pill(df$status[i])
        ),
        div(class = "status-path", df$path[i]),
        if ("input" %in% names(df) && nzchar(df$input[i])) div(class = "status-path", paste("Last input:", df$input[i])) else NULL
    )
  }))
}

sample_progress <- function(project) {
  design <- safe_read_table(project$design_matrix_path)
  if (!NROW(design) || !"sample" %in% names(design)) return(data.frame())
  data_dir <- project$data_dir
  rows <- lapply(seq_len(NROW(design)), function(i) {
    sample <- as.character(design$sample[i])
    data.frame(
      sample = sample,
      FastQC = if (count_files(file.path(data_dir, "fastqc"), paste0(sample, ".*\\.html$")) > 0 || count_files(file.path(data_dir, "fastqc_cutadapt"), paste0(sample, ".*\\.html$")) > 0) "ready" else "missing",
      Trim = if (count_files(file.path(data_dir, "cutadapt"), paste0(sample, ".*", fastq_suffix_regex)) > 0) "ready" else "missing",
      STAR = if (file.exists(file.path(data_dir, "star", sample, paste0(sample, "Aligned.sortedByCoord.out.bam")))) "ready" else "missing",
      Kallisto = if (file.exists(file.path(data_dir, "kallisto", sample, "abundance.tsv"))) "ready" else "missing",
      featureCounts = if (file.exists(file.path(data_dir, "featurecounts", sample, paste0(sample, "_counts.txt")))) "ready" else "missing",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

save_job <- function(project, step, command, output = "") {
  row <- data.frame(
    time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    project = project$name,
    analysis = project$analysis,
    step = step,
    command = paste(command, collapse = " "),
    output = output,
    stringsAsFactors = FALSE
  )
  utils::write.table(row, JOBS_PATH, sep = "\t", row.names = FALSE, quote = TRUE, append = file.exists(JOBS_PATH), col.names = !file.exists(JOBS_PATH))
}

rna_workdir <- function(project) {
  normalizePath(file.path(CSL_ROOT, analysis_notebook_dir(project$analysis_key)), winslash = "/", mustWork = FALSE)
}

genome_resources <- function(project) {
  genome <- tolower(project$genome %||% "mouse")
  if (genome == "human") {
    list(
      label = "human hg38 / GENCODE v42 annotation; kallisto transcript index v45",
      star_index = "/grid/bsr/data/data/utama/genome/hg38_p13_gencode/hg38_p13_gencode_rel42_all_starindex",
      kallisto_index = "/grid/bsr/data/data/utama/genome/hg38_p13_gencode/gencode.v45.transcripts.idx",
      gtf = "/grid/bsr/data/data/utama/genome/hg38_p13_gencode/gencode.v42.chr_patch_hapl_scaff.annotation.gtf",
      strand_bed = "/grid/bsr/data/data/utama/genome/hg38_p13_gencode/gencode.v42.chr_patch_hapl_scaff.annotation_forStrandDetect_geneID.bed"
    )
  } else {
    list(
      label = "mouse GRCm39 / GENCODE M29",
      star_index = "/grid/bsr/data/data/utama/genome/GRCm39_M29_gencode/GRCm39_M29_gencode_starindex",
      kallisto_index = "/grid/bsr/data/data/utama/genome/GRCm39_M29_gencode/gencode.vM29.transcripts.idx",
      gtf = "/grid/bsr/data/data/utama/genome/GRCm39_M29_gencode/gencode.vM29.annotation.gtf",
      strand_bed = "/grid/bsr/data/data/utama/genome/GRCm39_M29_gencode/gencode.vM29.annotation_forStrandDetect_geneID.bed"
    )
  }
}

gencode_label <- function(project) {
  genome_resources(project)$label
}

resolve_read_path <- function(base, value) {
  value <- trimws(as.character(value %||% ""))
  if (!nzchar(value)) return("")
  if (startsWith(path.expand(value), "/")) return(path.expand(value))
  file.path(base, basename(value))
}

sample_fastq_pairs <- function(project, trimmed = FALSE) {
  design <- safe_read_table(project$design_matrix_path)
  if (!NROW(design) || !"sample" %in% names(design) || !"filename" %in% names(design)) return(data.frame())
  base <- if (trimmed) file.path(project$data_dir, "cutadapt") else project$fastq_dir
  rows <- lapply(seq_len(NROW(design)), function(i) {
    parts <- trimws(unlist(strsplit(as.character(design$filename[i]), ",")))
    parts <- parts[nzchar(parts)]
    if (!length(parts)) return(NULL)
    r1 <- resolve_read_path(base, parts[1])
    r2 <- if (project$paired_end && length(parts) >= 2) resolve_read_path(base, parts[2]) else r1
    data.frame(sample = as.character(design$sample[i]), r1 = r1, r2 = r2, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(out)) data.frame() else out
}

parse_sbatch_job_id <- function(output) {
  m <- regexpr("Submitted batch job[[:space:]]+[0-9]+", output)
  if (m < 0) return("")
  sub(".*Submitted batch job[[:space:]]+", "", regmatches(output, m))
}

submit_sbatch <- function(project, step, script, args, log_name, input_mode = "") {
  log_dir <- file.path(dirname(project$data_dir), "log")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  stdout <- file.path(log_dir, paste0("output_", log_name, "_", stamp, ".txt"))
  stderr <- file.path(log_dir, paste0("error_", log_name, "_", stamp, ".txt"))
  cmd <- c("sbatch", "-e", stderr, "-o", stdout, script, args)
  if (Sys.which("sbatch") == "") {
    msg <- "sbatch was not found. Run on the server to submit jobs."
    save_job(project, step, cmd, msg)
    return(msg)
  }
  wd <- rna_workdir(project)
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  dir.create(wd, recursive = TRUE, showWarnings = FALSE)
  setwd(wd)
  out <- tryCatch(system2(cmd[1], cmd[-1], stdout = TRUE, stderr = TRUE), error = function(e) conditionMessage(e))
  out_text <- paste(out, collapse = "\n")
  job_id <- parse_sbatch_job_id(out_text)
  save_job(project, step, cmd, paste(c(out_text, if (nzchar(job_id)) paste("job_id:", job_id), if (nzchar(input_mode)) paste("input_mode:", input_mode), paste("stdout:", stdout), paste("stderr:", stderr)), collapse = "\n"))
  paste(c(paste("Command:", paste(cmd, collapse = " ")), out_text, if (nzchar(job_id)) paste("Job ID:", job_id), if (nzchar(input_mode)) paste("Input mode:", input_mode), paste("stdout:", stdout), paste("stderr:", stderr)), collapse = "\n")
}

missing_read_message <- function(project, pairs) {
  if (!NROW(pairs)) return("No samples/read files found in design matrix.")
  reads <- unique(c(pairs$r1, if (isTRUE(project$paired_end)) pairs$r2 else character(0)))
  missing <- reads[nzchar(reads) & !file.exists(reads)]
  if (length(missing)) {
    return(paste(c("These read files do not exist. Check the FASTQ folder and design_matrix.txt filenames:", missing), collapse = "\n"))
  }
  ""
}

submit_fastqc_jobs <- function(project, trimmed = FALSE) {
  outdir <- file.path(project$data_dir, if (trimmed) "fastqc_cutadapt" else "fastqc")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  pairs <- sample_fastq_pairs(project, trimmed)
  msg <- missing_read_message(project, pairs)
  if (nzchar(msg)) return(msg)
  reads <- unique(c(pairs$r1, if (project$paired_end) pairs$r2 else character(0)))
  script <- file.path(SCRIPTS_DIR, "FastQC", "qsub_fastqc.sh")
  input_mode <- if (trimmed) "trimmed reads" else "raw reads"
  paste(vapply(reads, function(read) submit_sbatch(project, "FastQC", script, c(read, outdir, project$name), "fastQC", input_mode), character(1)), collapse = "\n")
}

submit_cutadapt_jobs <- function(project, adapter1, adapter2, min_length) {
  outdir <- file.path(project$data_dir, "cutadapt")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  pairs <- sample_fastq_pairs(project, FALSE)
  msg <- missing_read_message(project, pairs)
  if (nzchar(msg)) return(msg)
  script <- file.path(SCRIPTS_DIR, if (project$paired_end) "cutadapt_PE/qsub_cutadapt_PE.sh" else "cutadapt_SE/qsub_cutadapt_SE.sh")
  paste(apply(pairs, 1, function(row) {
    trimmed1 <- file.path(outdir, basename(row[["r1"]]))
    trimmed2 <- if (project$paired_end) file.path(outdir, basename(row[["r2"]])) else trimmed1
    read2 <- if (project$paired_end) row[["r2"]] else row[["r1"]]
    submit_sbatch(project, "Cutadapt", script, c(min_length, adapter1, adapter2, trimmed1, trimmed2, row[["r1"]], read2, project$name), "cutadapt", "raw reads")
  }), collapse = "\n")
}

submit_star_jobs <- function(project, trimmed = FALSE) {
  res <- genome_resources(project)
  outdir <- file.path(project$data_dir, "star")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  pairs <- sample_fastq_pairs(project, trimmed)
  msg <- missing_read_message(project, pairs)
  if (nzchar(msg)) return(msg)
  script <- file.path(SCRIPTS_DIR, "STAR", if (project$paired_end) "qsub_star_PE.sh" else "qsub_star_SE.sh")
  paste(apply(pairs, 1, function(row) {
    sample_dir <- file.path(outdir, row[["sample"]])
    dir.create(sample_dir, recursive = TRUE, showWarnings = FALSE)
    out_prefix <- file.path(sample_dir, row[["sample"]])
    input_mode <- if (trimmed) "trimmed reads" else "raw reads"
    submit_sbatch(project, "STAR", script, c(out_prefix, res$star_index, row[["r1"]], row[["r2"]], project$name), "star", input_mode)
  }), collapse = "\n")
}

submit_kallisto_jobs <- function(project, trimmed = FALSE) {
  res <- genome_resources(project)
  outdir <- file.path(project$data_dir, "kallisto")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  pairs <- sample_fastq_pairs(project, trimmed)
  msg <- missing_read_message(project, pairs)
  if (nzchar(msg)) return(msg)
  script <- file.path(SCRIPTS_DIR, "Kallisto", if (project$paired_end) "qsub_kallisto_PE.sh" else "qsub_kallisto_SE.sh")
  paste(apply(pairs, 1, function(row) {
    sample_dir <- file.path(outdir, row[["sample"]])
    dir.create(sample_dir, recursive = TRUE, showWarnings = FALSE)
    input_mode <- if (trimmed) "trimmed reads" else "raw reads"
    submit_sbatch(project, "Kallisto", script, c(sample_dir, res$kallisto_index, row[["r1"]], row[["r2"]], project$name), "kallisto", input_mode)
  }), collapse = "\n")
}

submit_featurecounts_jobs <- function(project, feature = "gene_id") {
  res <- genome_resources(project)
  design <- safe_read_table(project$design_matrix_path)
  if (!NROW(design) || !"sample" %in% names(design)) return("No samples found in design matrix.")
  outdir <- file.path(project$data_dir, "featurecounts")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  script <- file.path(SCRIPTS_DIR, "featureCounts", if (project$paired_end) "qsub_featurecounts_PE.sh" else "qsub_featurecounts_SE.sh")
  paste(vapply(as.character(design$sample), function(sample) {
    sample_dir <- file.path(outdir, sample)
    dir.create(sample_dir, recursive = TRUE, showWarnings = FALSE)
    bam <- file.path(project$data_dir, "star", sample, paste0(sample, "Aligned.sortedByCoord.out.bam"))
    count_prefix <- file.path(sample_dir, sample)
    submit_sbatch(project, "featureCounts", script, c(bam, res$gtf, feature, count_prefix, res$strand_bed, project$name), "featurecounts", paste("STAR BAM; feature", feature))
  }, character(1)), collapse = "\n")
}

submit_deseq2_job <- function(project, compare_col, reference, comparison, redundant = "NoRedundant") {
  outdir <- file.path(project$data_dir, "deseq2")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  script <- file.path(SCRIPTS_DIR, "DESeq2", "qsub_deseq2.sh")
  rscript <- file.path(SCRIPTS_DIR, "DESeq2", "DESeq2.R")
  count_matrix <- file.path(project$data_dir, "counts", "count_matrix.txt")
  design_matrix <- deseq_design_for_column(project, compare_col)
  submit_sbatch(project, "DESeq2", script, c(rscript, count_matrix, design_matrix, outdir, reference, comparison, redundant, project$name), "deseq2", paste(compare_col, reference, "vs", comparison))
}

write_native_shiny_config <- function(project) {
  cfg_dir <- file.path(APP_HOME, "native_configs")
  dir.create(cfg_dir, recursive = TRUE, showWarnings = FALSE)
  cfg <- file.path(cfg_dir, paste0(clean_name(project$id, "project"), "_shiny_results_config.R"))
  lines <- c(
    sprintf("project_name <- %s", deparse(project$name)),
    sprintf("results_root <- %s", deparse(project$results_root)),
    sprintf("data_dir <- %s", deparse(project$data_dir)),
    sprintf("design_matrix_path <- %s", deparse(project$design_matrix_path)),
    sprintf("app_dir <- %s", deparse(file.path(SCRIPTS_DIR, "Shiny"))),
    sprintf("logo_search_dirs <- c(%s)", paste(vapply(c(SCRIPTS_DIR, file.path(SCRIPTS_DIR, "Shiny")), deparse, character(1)), collapse = ", "))
  )
  writeLines(lines, cfg)
  cfg
}

load_native_rnaseq_viewer <- function(project) {
  if (!identical(project$analysis_key, "rna")) {
    return(list(id = project$id, ui = div(class = "empty-box", "The native Results Explorer is currently available for RNA-seq projects."), server = function(input, output, session) NULL))
  }
  app_file <- file.path(SCRIPTS_DIR, "Shiny", "app_server.R")
  if (!file.exists(app_file)) {
    return(list(id = project$id, ui = div(class = "empty-box", "Could not find CodeSpringLab's native Shiny app_server.R."), server = function(input, output, session) NULL))
  }
  cfg <- write_native_shiny_config(project)
  old_cfg <- Sys.getenv("RNASEQ_SHINY_CONFIG", unset = NA_character_)
  old_wd <- getwd()
  on.exit({
    if (is.na(old_cfg)) Sys.unsetenv("RNASEQ_SHINY_CONFIG") else Sys.setenv(RNASEQ_SHINY_CONFIG = old_cfg)
    setwd(old_wd)
  }, add = TRUE)
  Sys.setenv(RNASEQ_SHINY_CONFIG = cfg)
  setwd(file.path(SCRIPTS_DIR, "Shiny"))
  env <- new.env(parent = globalenv())
  sys.source(app_file, envir = env)
  list(
    id = paste(project$id, normalizePath(cfg, winslash = "/", mustWork = FALSE), sep = "::"),
    ui = div(class = "native-results-host", env$ui),
    server = env$server
  )
}

run_step_meta <- function() {
  data.frame(
    order = seq_len(6),
    step = c("FastQC", "Cutadapt", "STAR", "Kallisto", "featureCounts", "DESeq2"),
    description = c(
      "Generate per-read quality reports.",
      "Trim adapters and short reads.",
      "Align reads and write BAM files.",
      "Quantify transcript abundance.",
      "Create gene-level count files.",
      "Run differential expression and normalized counts."
    ),
    button = c("run_fastqc", "run_cutadapt", "run_star", "run_kallisto", "run_featurecounts", "run_deseq2"),
    label = c("Run FastQC", "Run cutadapt", "Run STAR", "Run Kallisto", "Run featureCounts", "Run DESeq2"),
    stringsAsFactors = FALSE
  )
}

pipeline_stepper_ui <- function(project) {
  status <- project_status(project)
  meta <- run_step_meta()
  div(class = "pipeline-stepper", lapply(seq_len(NROW(meta)), function(i) {
    st <- status$status[match(meta$step[i], status$step)] %||% "Not started"
    mode <- status$input[match(meta$step[i], status$step)] %||% ""
    cls <- switch(st, "Complete" = "complete", "Active" = "active", "not-started")
    div(class = paste("pipeline-step", cls),
        div(class = "step-index", meta$order[i]),
        div(class = "step-main", tags$strong(meta$step[i]), tags$span(st), if (nzchar(mode)) tags$em(mode) else NULL)
    )
  }))
}

tool_panel <- function(step, status, description, controls, button_id, button_label) {
  st <- status$status[match(step, status$step)] %||% "Not started"
  mode <- status$input[match(step, status$step)] %||% ""
  cls <- switch(st, "Complete" = "complete", "Active" = "active", "not-started")
  tags$details(
    class = paste("tool-panel", cls),
    open = identical(st, "Active") || identical(st, "Not started"),
    tags$summary(
      div(class = "tool-summary",
          div(tags$strong(step), tags$span(description)),
          div(class = "tool-right", status_pill(st), if (nzchar(mode)) tags$small(mode) else NULL)
      )
    ),
    div(class = "tool-body",
        controls,
        actionButton(button_id, button_label, class = "btn-primary")
    )
  )
}

list_result_files <- function(project, pattern = "\\.(txt|csv|tsv|html|png|pdf)$") {
  if (!dir.exists(project$data_dir)) return(character(0))
  list.files(project$data_dir, pattern = pattern, recursive = TRUE, full.names = TRUE)
}

image_or_file_ui <- function(path, height = "900px") {
  if (!file.exists(path)) return(tags$div(class = "empty-box", "File not found."))
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("png", "jpg", "jpeg", "webp") && BASE64_AVAILABLE) {
    mime <- if (ext == "png") "image/png" else "image/jpeg"
    tags$img(src = paste0("data:", mime, ";base64,", base64enc::base64encode(path)), style = "max-width:100%; border:1px solid #d8dde8; border-radius:8px;")
  } else if (ext == "html") {
    html <- paste(readLines(path, warn = FALSE), collapse = "\n")
    tags$iframe(srcdoc = htmltools::HTML(html), style = paste0("width:100%; height:", height, "; border:1px solid #d8dde8; border-radius:8px;"))
  } else if (ext == "pdf" && BASE64_AVAILABLE) {
    tags$iframe(src = paste0("data:application/pdf;base64,", base64enc::base64encode(path)), style = paste0("width:100%; height:", height, "; border:1px solid #d8dde8; border-radius:8px;"))
  } else {
    tags$div(class = "empty-box", tags$p(basename(path)), tags$p(path))
  }
}

app_css <- "
body { background:#eef3f8; color:#17202f; }
.container-fluid { width:100%; max-width:none; padding:18px 22px 28px 22px; }
.navbar, .navbar-default { background:#0f1724 !important; border:0; }
.navbar-default .navbar-nav > li > a, .navbar-default .navbar-brand { color:#f8fafc !important; }
.well, .panel, .tab-content { border-radius:8px; border-color:#d8dde8; }
.csl-header { background:linear-gradient(135deg,#0f2742 0%,#145f78 58%,#1f8f7a 100%); color:white; border:0; border-radius:8px; padding:28px 34px; margin-bottom:16px; min-height:128px; display:flex; align-items:center; justify-content:space-between; gap:26px; }
.brand-lockup { display:flex; align-items:center; gap:22px; }
.brand-lockup img { background:white; border-radius:8px; padding:9px; max-height:78px; max-width:210px; object-fit:contain; }
.csl-header h2 { margin:0 0 6px 0; font-weight:800; font-size:32px; color:white; }
.csl-header .muted { color:#dceaf4; }
.muted { color:#657084; }
.empty-box { background:white; border:1px solid #d8dde8; border-radius:8px; padding:18px; color:#657084; }
.btn-primary { background:#1f5eff; border-color:#1f5eff; }
.status-toolbar { display:flex; gap:12px; align-items:flex-end; flex-wrap:wrap; margin-bottom:16px; }
.status-grid { display:grid; grid-template-columns:repeat(auto-fit, minmax(250px, 1fr)); gap:12px; margin-bottom:18px; }
.status-card, .run-card, .tool-panel { background:white; border:1px solid #d8dde8; border-radius:8px; padding:14px; box-shadow:0 1px 2px rgba(15,23,36,0.04); }
.status-card-top, .run-card-top { display:flex; justify-content:space-between; align-items:center; gap:10px; margin-bottom:8px; }
.status-path { color:#657084; font-size:12px; overflow-wrap:anywhere; }
.status-pill { display:inline-flex; align-items:center; border-radius:999px; padding:4px 9px; font-size:12px; font-weight:700; white-space:nowrap; }
.status-pill.active { color:#7c3d00; background:#fff4d6; border:1px solid #f0c36d; }
.status-pill.complete { color:#0b6b3a; background:#def7e8; border:1px solid #8fd8ad; }
.status-pill.not-started { color:#526070; background:#eef2f7; border:1px solid #cfd7e3; }
.run-grid { display:grid; grid-template-columns:1fr; gap:12px; margin-top:14px; }
.run-card p { min-height:38px; margin-bottom:12px; }
.tool-panel { padding:0; overflow:hidden; }
.tool-panel summary { cursor:pointer; list-style:none; padding:14px 16px; }
.tool-panel summary::-webkit-details-marker { display:none; }
.tool-panel.complete { border-left:5px solid #27ae60; }
.tool-panel.active { border-left:5px solid #d99a15; }
.tool-panel.not-started { border-left:5px solid #d55745; }
.tool-summary { display:flex; justify-content:space-between; align-items:center; gap:16px; }
.tool-summary strong { display:block; font-size:16px; }
.tool-summary span { color:#657084; font-size:13px; }
.tool-right { display:flex; align-items:center; gap:8px; flex-wrap:wrap; justify-content:flex-end; }
.tool-right small { color:#657084; }
.tool-body { padding:0 16px 16px 16px; border-top:1px solid #edf1f6; }
.tool-body .form-group { margin-bottom:10px; }
.resource-strip { display:grid; grid-template-columns:1.2fr 1fr; gap:12px; align-items:stretch; margin:12px 0 14px 0; }
.resource-card { background:white; border:1px solid #d8dde8; border-radius:8px; padding:14px; }
.resource-card img { max-width:100%; max-height:150px; object-fit:contain; }
.progress-note { color:#657084; margin-bottom:10px; }
.job-table-wrap { margin-top:16px; }
.design-table-scroll { overflow-x:auto; background:white; border:1px solid #d8dde8; border-radius:8px; padding:10px; }
.design-matrix-table { border-collapse:separate; border-spacing:0 6px; min-width:100%; }
.design-matrix-table th { font-size:12px; color:#657084; font-weight:700; padding:0 8px 4px 8px; }
.design-matrix-table td { vertical-align:middle; padding:0 8px; }
.design-matrix-table .form-group { margin-bottom:0; }
.pipeline-stepper { display:grid; grid-template-columns:repeat(auto-fit, minmax(150px, 1fr)); gap:10px; margin:12px 0 18px 0; }
.pipeline-step { border:1px solid #d8dde8; border-radius:8px; padding:10px; display:flex; gap:10px; align-items:center; background:#fff4f3; }
.pipeline-step.complete { background:#def7e8; border-color:#8fd8ad; }
.pipeline-step.active { background:#fff4d6; border-color:#f0c36d; }
.step-index { width:28px; height:28px; border-radius:50%; background:white; display:flex; align-items:center; justify-content:center; font-weight:700; }
.step-main { display:flex; flex-direction:column; line-height:1.2; }
.step-main span, .step-main em { font-size:12px; color:#657084; margin-top:3px; font-style:normal; }
.log-viewer { max-height:620px; overflow:auto; background:#0d1623; color:#d9e8ff; border-radius:8px; border:1px solid #1f3857; padding:14px; }
.native-results-host { margin: 0 !important; width:100% !important; }
.native-results-host > .container-fluid { max-width: none !important; width: 100% !important; margin: 0 !important; padding: 6px 0 18px 0 !important; }
.native-results-host .app-shell { border-radius: 10px !important; box-shadow: none !important; margin:0 !important; }
.native-results-host .hero { padding: 18px 22px 16px 22px !important; }
.native-results-host .main-tabs { padding: 14px 14px 20px 14px !important; }
"

ui <- fluidPage(
  tags$head(tags$style(HTML(app_css))),
  div(class = "csl-header",
      div(class = "brand-lockup",
          if (file.exists(LOGO_PATH)) tags$img(src = file.path("codespring_logo", basename(LOGO_PATH))),
          div(h2("CodeSpringWeb"), div(class = "muted", "Shiny control center for CodeSpringLab projects: configure, run, track, and visualize results from one port."))
      ),
      if (file.exists(LOGO_CSL_PATH)) tags$img(src = file.path("csl_logo", basename(LOGO_CSL_PATH)), style = "max-height:82px;max-width:230px;background:white;border-radius:8px;padding:9px;object-fit:contain;")
  ),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      selectInput("analysis", "Analysis", choices = c("RNA-seq", "ATAC-seq", "ChIP-seq", "All analyses"), selected = "RNA-seq"),
      uiOutput("project_ui"),
      uiOutput("new_project_ui"),
      tags$hr(),
      div(class = "muted", sprintf("CodeSpringLab root: %s", CSL_ROOT)),
      tags$hr(),
      h4("Selected Project"),
      verbatimTextOutput("project_paths")
    ),
    mainPanel(
      width = 10,
      tabsetPanel(
        id = "web_main_tabs",
        tabPanel("Setup", br(), h3("Project Setup"), tableOutput("setup_table"), uiOutput("source_config_ui")),
        tabPanel("Design Matrix", br(), h3("Design Matrix Builder"),
                 tags$p(class = "muted", "Scan the raw FASTQ folder, then edit include/sample/metadata cells directly. Filenames stay on the right so the run steps know which reads belong to each sample."),
                 fluidRow(
                   column(8, textInput("metadata_cols", "Metadata columns", value = "treatment", placeholder = "treatment, batch, replicate")),
                   column(4, br(), actionButton("scan_fastqs", "Scan FASTQ folder", class = "btn-primary"))
                 ),
                 uiOutput("design_editor_ui"),
                 br(),
                 actionButton("save_design", "Save design_matrix.txt", class = "btn-primary"),
                 verbatimTextOutput("design_save_status")),
        tabPanel("Progress", br(),
                 h3("Pipeline Progress"),
                 div(class = "status-toolbar",
                     selectInput("progress_status_filter", "Show steps", choices = c("All", "Active", "Complete", "Not started"), selected = "All"),
                     actionButton("refresh_progress", "Refresh now", class = "btn-primary")
                 ),
                 textOutput("progress_updated"),
                 uiOutput("pipeline_stepper"),
                 uiOutput("status_cards_ui"),
                 table_output("status_table"),
                 br(),
                 h4("Sample Progress"),
                 table_output("sample_progress_table"),
                 div(class = "job-table-wrap", h4("Submitted Jobs"), table_output("active_jobs_table"))),
        tabPanel("Run Pipeline", br(), h3("Run Pipeline"),
                 tags$p(class = "muted", "Each tool has its own settings. Jobs are submitted with SLURM sbatch and keep running after this app or browser is closed."),
                 uiOutput("run_resource_strip"),
                 uiOutput("run_pipeline_stepper"),
                 uiOutput("run_step_cards"),
                 br(),
                 verbatimTextOutput("run_output")),
        tabPanel("Results Explorer", uiOutput("native_results_ui")),
        tabPanel("Logs", br(), h3("Submitted Jobs"), table_output("jobs_table"), br(), uiOutput("log_file_ui"), tags$pre(class = "log-viewer", textOutput("selected_log_text")))
      )
    )
  )
)

server <- function(input, output, session) {
  projects <- reactiveVal(discover_projects())
  design_state <- reactiveVal(data.frame())
  run_message <- reactiveVal("")
  progress_refresh <- reactiveVal(Sys.time())
  native_registered_id <- reactiveVal("")

  filtered_projects <- reactive({
    p <- projects()
    analysis <- input$analysis
    if (!length(analysis) || is.null(analysis) || !nzchar(analysis) || identical(analysis, "All analyses")) return(p)
    p[vapply(p, function(x) identical(x$analysis, analysis), logical(1))]
  })

  output$project_ui <- renderUI({
    p <- filtered_projects()
    labels <- if (length(p)) vapply(p, function(x) paste0(x$label, " (", x$analysis, if (nzchar(x$source_config)) " · CSL config" else "", ")"), character(1)) else character(0)
    choices <- c(stats::setNames(labels, labels), "Start a new project" = "__new__")
    selectInput("project_id", "Project config", choices = choices, selected = if (length(labels)) labels[[1]] else "__new__")
  })

  output$new_project_ui <- renderUI({
    if (!identical(input$project_id, "__new__")) return(NULL)
    tagList(
      tags$hr(),
      h4("New Project"),
      textInput("new_project_name", "Project name", value = "new_rnaseq_project"),
      selectInput("new_project_analysis", "Analysis type", choices = c("RNA-seq", "ATAC-seq", "ChIP-seq"), selected = if (identical(input$analysis, "All analyses")) "RNA-seq" else input$analysis),
      selectInput("new_genome", "Genome", choices = c("mouse", "human"), selected = "mouse"),
      radioButtons("new_paired_end", "Reads", choices = c("Paired-end" = "paired", "Single-end" = "single"), selected = "paired"),
      textInput("new_fastq_dir", "Raw FASTQ folder", value = ""),
      textInput("new_results_root", "Results root", value = "~/csl_results"),
      textInput("new_design_matrix_path", "Design matrix path", value = ""),
      actionButton("create_project_config", "Create project config", class = "btn-primary"),
      textOutput("create_project_status")
    )
  })

  current_project <- reactive({
    if (identical(input$project_id, "__new__")) return(new_project_from_inputs(input))
    p <- filtered_projects()
    req(length(p) > 0)
    labels <- vapply(p, function(x) paste0(x$label, " (", x$analysis, if (nzchar(x$source_config)) " · CSL config" else "", ")"), character(1))
    selected <- input$project_id
    if (!length(selected) || is.null(selected) || !nzchar(selected)) {
      idx <- 1
    } else {
      idx <- match(selected, labels)
      if (!length(idx) || is.na(idx)) idx <- 1
    }
    p[[idx]]
  })

  output$project_paths <- renderText({
    p <- current_project()
    paste(c(
      paste("Project:", p$label),
      paste("Analysis:", p$analysis),
      paste("Genome:", p$genome),
      paste("Data:", p$data_dir),
      paste("Design:", p$design_matrix_path),
      paste("FASTQ:", p$fastq_dir)
    ), collapse = "\n")
  })

  output$setup_table <- renderTable({
    p <- current_project()
    data.frame(
      field = c("Project", "Analysis", "Genome", "GENCODE/index", "Paired-end", "Results root", "Data folder", "FASTQ folder", "Design matrix"),
      value = c(p$label, p$analysis, p$genome, gencode_label(p), as.character(p$paired_end), p$results_root, p$data_dir, p$fastq_dir, p$design_matrix_path),
      stringsAsFactors = FALSE
    )
  })

  output$source_config_ui <- renderUI({
    p <- current_project()
    if (!nzchar(p$source_config)) return(NULL)
    tagList(h4("Imported CodeSpringLab Config"), tags$pre(p$source_config))
  })

  output$create_project_status <- renderText("")
  observeEvent(input$create_project_config, {
    p <- new_project_from_inputs(input)
    msg <- tryCatch({
      cfg <- write_project_config(p)
      projects(discover_projects())
      paste("Created project config:", cfg)
    }, error = function(e) paste("ERROR:", conditionMessage(e)))
    output$create_project_status <- renderText(msg)
  })

  observeEvent(input$scan_fastqs, {
    p <- current_project()
    cols <- clean_name(unlist(strsplit(input$metadata_cols, ",")))
    cols <- cols[nzchar(cols) & !cols %in% c("sample", "filename", "include", "status")]
    if (!length(cols)) cols <- "treatment"
    design_state(scan_fastqs(p$fastq_dir, p$paired_end, cols))
  })

  observeEvent(current_project(), {
    p <- current_project()
    if (file.exists(p$design_matrix_path)) {
      df <- safe_read_table(p$design_matrix_path)
      if (NROW(df)) {
        df$include <- TRUE
        df$status <- "saved"
        df <- df[, c("include", setdiff(names(df), c("include", "status")), "status"), drop = FALSE]
        design_state(df)
      }
    }
  }, ignoreInit = FALSE)

  output$design_editor_ui <- renderUI({
    df <- design_state()
    if (!NROW(df)) df <- data.frame(include = logical(), sample = character(), treatment = character(), filename = character(), status = character())
    design_matrix_ui(df)
  })

  output$design_save_status <- renderText("")
  observeEvent(input$save_design, {
    p <- current_project()
    df <- collect_design_inputs(input, design_state())
    design_state(df)
    metadata <- setdiff(names(df), c("include", "sample", "filename", "status"))
    msg <- tryCatch({
      design_path <- write_design_matrix(p, df, metadata)
      if (identical(input$project_id, "__new__")) {
        cfg <- write_project_config(p)
        projects(discover_projects())
        paste("Saved design matrix:", design_path, "\nCreated project config:", cfg)
      } else {
        paste("Saved design matrix:", design_path)
      }
    }, error = function(e) paste("ERROR:", conditionMessage(e)))
    output$design_save_status <- renderText(msg)
  })

  observe({
    invalidateLater(10000, session)
    progress_refresh(Sys.time())
  })

  output$progress_updated <- renderText({
    paste("Auto-refreshes every 10 seconds. Last checked:", format(progress_refresh(), "%Y-%m-%d %H:%M:%S"))
  })

  progress_status <- reactive({
    progress_refresh()
    df <- project_status(current_project())
    filt <- input$progress_status_filter %||% "All"
    if (!identical(filt, "All")) df <- df[df$status == filt, , drop = FALSE]
    df[order(status_rank(df$status), df$step), , drop = FALSE]
  })

  observeEvent(input$refresh_progress, {
    progress_refresh(Sys.time())
  })

  output$pipeline_stepper <- renderUI({
    pipeline_stepper_ui(current_project())
  })

  output$status_cards_ui <- renderUI({
    div(class = "status-grid", status_cards(progress_status()))
  })

  output$status_table <- render_csl_table(progress_status(), page_length = 20)

  output$sample_progress_table <- render_csl_table({
    progress_refresh()
    sample_progress(current_project())
  }, page_length = 25)

  output$active_jobs_table <- render_csl_table({
    progress_refresh()
    job_history(current_project())
  }, page_length = 10)

  output$run_pipeline_stepper <- renderUI({
    progress_refresh()
    pipeline_stepper_ui(current_project())
  })

  output$run_resource_strip <- renderUI({
    p <- current_project()
    div(class = "resource-strip",
        div(class = "resource-card",
            tags$strong("Genome resources"),
            tags$p(class = "muted", gencode_label(p)),
            tags$p(class = "status-path", genome_resources(p)$gtf)
        ),
        div(class = "resource-card",
            if (file.exists(FLOWCHART_PATH)) tags$img(src = file.path("codespring_flowchart", basename(FLOWCHART_PATH))) else tags$p("Pipeline flowchart")
        )
    )
  })

  output$run_step_cards <- renderUI({
    progress_refresh()
    status <- project_status(current_project())
    div(class = "run-grid",
      tool_panel("FastQC", status, "Quality reports for raw or trimmed reads.",
        tagList(checkboxInput("fastqc_use_trimmed", "Use trimmed reads", value = FALSE)),
        "run_fastqc", "Submit FastQC"),
      tool_panel("Cutadapt", status, "Trim adapters and short reads from raw FASTQs.",
        tagList(
          textInput("cutadapt_adapter1", "R1 adapter", value = "AGATCGGAAGAGCACACGTCTGAACTCCAGTCA"),
          textInput("cutadapt_adapter2", "R2 adapter", value = "AGATCGGAAGAGCGTCGTGTAGGGAAAGAGTGT"),
          textInput("cutadapt_min_length", "Minimum read length", value = "20")
        ),
        "run_cutadapt", "Submit cutadapt"),
      tool_panel("STAR", status, "Align raw or trimmed reads to the selected genome index.",
        tagList(checkboxInput("star_use_trimmed", "Use trimmed reads", value = TRUE)),
        "run_star", "Submit STAR"),
      tool_panel("Kallisto", status, "Quantify transcript abundance from raw or trimmed reads.",
        tagList(checkboxInput("kallisto_use_trimmed", "Use trimmed reads", value = TRUE)),
        "run_kallisto", "Submit Kallisto"),
      tool_panel("featureCounts", status, "Quantify STAR BAM files with the selected GTF attribute.",
        tagList(selectInput("feature_attr", "featureCounts attribute", choices = c("gene_id", "gene_name"), selected = "gene_id")),
        "run_featurecounts", "Submit featureCounts"),
      tool_panel("DESeq2", status, "Run differential expression from count_matrix.txt.",
        uiOutput("deseq_controls_ui"),
        "run_deseq2", "Submit DESeq2")
    )
  })

  output$deseq_controls_ui <- renderUI({
    p <- current_project()
    cols <- design_compare_columns(p)
    if (!length(cols)) return(div(class = "empty-box", "No comparison columns found between sample and filename in design_matrix.txt."))
    selected_col <- input$deseq_compare_col %||% if ("treatment" %in% cols) "treatment" else cols[[1]]
    if (!selected_col %in% cols) selected_col <- cols[[1]]
    vals <- design_compare_values(p, selected_col)
    ref <- input$deseq_reference %||% if (length(vals)) vals[[1]] else ""
    comp <- input$deseq_comparison %||% if (length(vals) > 1) vals[[2]] else ref
    tagList(
      selectInput("deseq_compare_col", "Comparison column", choices = cols, selected = selected_col),
      selectInput("deseq_reference", "Reference/baseline", choices = vals, selected = ref),
      selectInput("deseq_comparison", "Comparison", choices = vals, selected = comp)
    )
  })

  observeEvent(input$run_fastqc, {
    run_message(submit_fastqc_jobs(current_project(), isTRUE(input$fastqc_use_trimmed)))
    progress_refresh(Sys.time())
  })
  observeEvent(input$run_cutadapt, {
    run_message(submit_cutadapt_jobs(current_project(), input$cutadapt_adapter1, input$cutadapt_adapter2, input$cutadapt_min_length))
    progress_refresh(Sys.time())
  })
  observeEvent(input$run_star, {
    run_message(submit_star_jobs(current_project(), isTRUE(input$star_use_trimmed)))
    progress_refresh(Sys.time())
  })
  observeEvent(input$run_kallisto, {
    run_message(submit_kallisto_jobs(current_project(), isTRUE(input$kallisto_use_trimmed)))
    progress_refresh(Sys.time())
  })
  observeEvent(input$run_featurecounts, {
    run_message(submit_featurecounts_jobs(current_project(), input$feature_attr))
    progress_refresh(Sys.time())
  })
  observeEvent(input$run_deseq2, {
    if (identical(input$deseq_reference, input$deseq_comparison)) {
      run_message("Reference and comparison must be different.")
    } else {
      run_message(submit_deseq2_job(current_project(), input$deseq_compare_col, input$deseq_reference, input$deseq_comparison, "NoRedundant"))
    }
    progress_refresh(Sys.time())
  })
  output$run_output <- renderText(run_message())


  native_results_app <- reactive({
    load_native_rnaseq_viewer(current_project())
  })

  output$native_results_ui <- renderUI({
    native_results_app()$ui
  })

  observeEvent(native_results_app(), {
    app <- native_results_app()
    if (!identical(native_registered_id(), app$id)) {
      app$server(input, output, session)
      native_registered_id(app$id)
    }
  }, ignoreInit = FALSE)

  output$results_overview <- render_csl_table(project_status(current_project()), page_length = 20)
  output$design_table <- render_csl_table(safe_read_table(current_project()$design_matrix_path), page_length = 25)
  output$fastqc_select_ui <- renderUI({
    p <- current_project()
    files <- c(list.files(file.path(p$data_dir, "fastqc"), pattern = "\\.html$", full.names = TRUE),
               list.files(file.path(p$data_dir, "fastqc_cutadapt"), pattern = "\\.html$", full.names = TRUE))
    selectInput("fastqc_file", "FastQC report", choices = files, selected = files[1] %||% character(0))
  })
  output$fastqc_view <- renderUI({ req(input$fastqc_file); image_or_file_ui(input$fastqc_file, "1050px") })
  output$star_summary <- render_csl_table(safe_read_table(file.path(current_project()$data_dir, "star_summary", "summary_matrix.txt")), page_length = 25)
  output$featurecounts_summary <- render_csl_table(safe_read_table(file.path(current_project()$data_dir, "counts", "featurecounts_summary.txt")), page_length = 25)
  output$count_matrix <- render_csl_table(safe_read_table(file.path(current_project()$data_dir, "counts", "count_matrix.txt"), 5000), page_length = 25)

  file_select <- function(id, label, dir, pattern) {
    files <- if (dir.exists(dir)) list.files(dir, pattern = pattern, recursive = TRUE, full.names = TRUE) else character(0)
    selectInput(id, label, choices = files, selected = files[1] %||% character(0))
  }
  output$rsem_file_ui <- renderUI({ file_select("rsem_file", "RSEM table", file.path(current_project()$data_dir, "rsem"), "\\.(txt|csv|results)$") })
  output$rsem_table <- render_csl_table({ req(input$rsem_file); safe_read_table(input$rsem_file, 5000) }, page_length = 25)
  output$kallisto_file_ui <- renderUI({ file_select("kallisto_file", "Kallisto table", file.path(current_project()$data_dir, "kallisto"), "\\.(tsv|txt|csv)$") })
  output$kallisto_table <- render_csl_table({ req(input$kallisto_file); safe_read_table(input$kallisto_file, 5000) }, page_length = 25)
  output$norm_file_ui <- renderUI({ file_select("norm_file", "DESeq2 normalized counts", file.path(current_project()$data_dir, "deseq2"), "normalized.*\\.(txt|csv)$") })
  output$norm_table <- render_csl_table({ req(input$norm_file); safe_read_table(input$norm_file, 5000) }, page_length = 25)
  output$deseq_file_ui <- renderUI({ file_select("deseq_file", "DESeq2 file", file.path(current_project()$data_dir, "deseq2"), "\\.(txt|csv|png|pdf)$") })
  output$deseq_file_view <- renderUI({
    req(input$deseq_file)
    if (tolower(tools::file_ext(input$deseq_file)) %in% c("txt", "csv", "tsv")) {
      table_output("deseq_selected_table")
    } else image_or_file_ui(input$deseq_file)
  })
  output$deseq_selected_table <- render_csl_table({ req(input$deseq_file); safe_read_table(input$deseq_file, 5000) }, page_length = 25)
  output$gsea_file_ui <- renderUI({ file_select("gsea_file", "GSEA file", file.path(current_project()$data_dir, "gseapy"), "\\.(txt|csv|png|pdf)$") })
  output$gsea_file_view <- renderUI({
    req(input$gsea_file)
    if (tolower(tools::file_ext(input$gsea_file)) %in% c("txt", "csv", "tsv")) {
      table_output("gsea_selected_table")
    } else image_or_file_ui(input$gsea_file, "950px")
  })
  output$gsea_selected_table <- render_csl_table({ req(input$gsea_file); safe_read_table(input$gsea_file, 5000) }, page_length = 25)
  output$all_file_ui <- renderUI({ file_select("all_file", "Result file", current_project()$data_dir, "\\.(txt|csv|tsv|html|png|pdf)$") })
  output$all_file_view <- renderUI({ req(input$all_file); image_or_file_ui(input$all_file) })
  output$jobs_table <- render_csl_table({
    if (!file.exists(JOBS_PATH)) return(data.frame())
    utils::read.delim(JOBS_PATH, check.names = FALSE)
  }, page_length = 25)

  output$log_file_ui <- renderUI({
    choices <- log_file_choices(current_project())
    if (!length(choices)) return(div(class = "empty-box", paste("No stdout/stderr log files were found in", file.path(dirname(current_project()$data_dir), "log"))))
    tagList(
      selectInput("selected_log_file", "Open job log", choices = choices),
      radioButtons("log_view_mode", "View", choices = c("Tail" = "tail", "Head" = "head", "Full" = "full"), selected = "tail", inline = TRUE)
    )
  })

  output$selected_log_text <- renderText({
    req(input$selected_log_file)
    read_log_excerpt(input$selected_log_file, input$log_view_mode %||% "tail")
  })
}

shinyApp(ui, server, onStart = cleanup_previous_shiny_processes)
