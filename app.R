library(shiny)

DT_AVAILABLE <- requireNamespace("DT", quietly = TRUE)
BASE64_AVAILABLE <- requireNamespace("base64enc", quietly = TRUE)

table_output <- function(output_id) {
  if (DT_AVAILABLE) DT::dataTableOutput(output_id) else tableOutput(output_id)
}

pvalue_columns <- function(df) {
  if (is.null(df) || !NCOL(df)) return(character(0))
  names(df)[grepl("(^p$|pvalue|p\\.value|p_value|p-val|p\\.val|padj|adj\\.p|fdr|qvalue|q\\.value|q_value|q-val)", names(df), ignore.case = TRUE)]
}

pvalue_render_js <- function(digits = 3) {
  DT::JS(sprintf(
    "function(data, type, row, meta) {
       if (type === 'display' || type === 'filter') {
         var x = parseFloat(data);
         if (!isNaN(x) && isFinite(x)) return x.toExponential(%d);
       }
       return data;
     }",
    digits
  ))
}

format_pvalues_for_display <- function(df, digits = 3) {
  for (column in pvalue_columns(df)) {
    values <- suppressWarnings(as.numeric(df[[column]]))
    if (!length(values) || all(is.na(values) & !is.na(df[[column]]))) next
    df[[column]] <- ifelse(is.na(values), NA_character_, formatC(values, format = "e", digits = digits))
  }
  df
}

render_csl_table <- function(expr, page_length = 50, editable = FALSE, scroll_y = "520px", escape = TRUE) {
  expr_call <- substitute(expr)
  expr_env <- parent.frame()
  if (DT_AVAILABLE) {
    DT::renderDataTable({
      df <- eval(expr_call, envir = expr_env)
      if (!NROW(df)) df <- data.frame()
      pvalue_cols_all <- pvalue_columns(df)
      pvalue_targets <- match(pvalue_cols_all, names(df), nomatch = 0) - 1
      pvalue_targets <- pvalue_targets[pvalue_targets >= 0]
      column_defs <- c(
        list(list(width = "118px", targets = "_all")),
        lapply(pvalue_targets, function(target) list(targets = target, render = pvalue_render_js(3)))
      )
      widget <- DT::datatable(
        df,
        editable = editable,
        rownames = FALSE,
        escape = escape,
        options = list(
          scrollX = TRUE,
          scrollY = scroll_y,
          pageLength = page_length,
          lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100", "All")),
          paging = TRUE,
          pagingType = "full_numbers",
          dom = "lfrtip",
          autoWidth = FALSE,
          columnDefs = column_defs
        )
      )
      numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      if (length(numeric_cols)) {
        pvalue_cols <- intersect(numeric_cols, pvalue_cols_all)
        integer_cols <- numeric_cols[vapply(df[numeric_cols], function(x) {
          finite <- x[is.finite(x) & !is.na(x)]
          length(finite) == 0 || all(abs(finite - round(finite)) < 1e-8)
        }, logical(1))]
        decimal_cols <- setdiff(numeric_cols, c(integer_cols, pvalue_cols))
        if (length(integer_cols)) widget <- DT::formatRound(widget, columns = integer_cols, digits = 0)
        if (length(decimal_cols)) widget <- DT::formatRound(widget, columns = decimal_cols, digits = 2)
        if (length(pvalue_cols)) widget <- DT::formatSignif(widget, columns = pvalue_cols, digits = 3)
      }
      widget
    }, server = FALSE)
  } else {
    renderTable({
      df <- eval(expr_call, envir = expr_env)
      if (!NROW(df)) return(data.frame())
      df <- format_pvalues_for_display(df)
      utils::head(df, 50)
    }, striped = TRUE, bordered = TRUE, spacing = "s")
  }
}

render_methods_table <- function(expr, page_length = 25, scroll_y = "520px") {
  expr_call <- substitute(expr)
  expr_env <- parent.frame()
  if (DT_AVAILABLE) {
    DT::renderDataTable({
      df <- eval(expr_call, envir = expr_env)
      if (!NROW(df)) df <- data.frame()
      widths <- c("90px", "190px", "360px", "260px", "320px", "420px")
      column_defs <- lapply(seq_len(min(NCOL(df), length(widths))), function(i) {
        list(width = widths[[i]], targets = i - 1)
      })
      DT::datatable(
        df,
        rownames = FALSE,
        escape = TRUE,
        class = "compact stripe hover methods-dt",
        options = list(
          scrollX = TRUE,
          scrollY = scroll_y,
          pageLength = page_length,
          lengthMenu = list(c(10, 25, 50, -1), c("10", "25", "50", "All")),
          paging = TRUE,
          pagingType = "full_numbers",
          dom = "lfrtip",
          autoWidth = FALSE,
          columnDefs = column_defs
        )
      )
    }, server = FALSE)
  } else {
    renderTable({
      df <- eval(expr_call, envir = expr_env)
      if (!NROW(df)) return(data.frame())
      df <- format_pvalues_for_display(df)
      df
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

effective_unix_user <- function() {
  candidates <- c(
    Sys.info()[["effective_user"]] %||% "",
    tryCatch(trimws(system2("id", "-un", stdout = TRUE, stderr = FALSE)[1]), error = function(e) ""),
    Sys.info()[["user"]] %||% ""
  )
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  if (!length(candidates)) stop("Could not determine the effective Unix user running CodeSpringApp.")
  candidates[[1]]
}

unix_home_for_user <- function(user) {
  expanded <- path.expand(paste0("~", user))
  if (nzchar(expanded) && !identical(expanded, paste0("~", user)) && dir.exists(expanded)) {
    return(normalizePath(expanded, winslash = "/", mustWork = FALSE))
  }
  if (nzchar(Sys.which("getent"))) {
    entry <- tryCatch(system2("getent", c("passwd", user), stdout = TRUE, stderr = FALSE), error = function(e) character(0))
    if (length(entry)) {
      fields <- strsplit(entry[[1]], ":", fixed = TRUE)[[1]]
      if (length(fields) >= 6 && dir.exists(fields[[6]])) return(normalizePath(fields[[6]], winslash = "/", mustWork = FALSE))
    }
  }
  stop("Could not determine the home directory for Unix user ", user, ".")
}

path_is_within <- function(path, root) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- sub("/+$", "", normalizePath(root, winslash = "/", mustWork = FALSE))
  identical(path, root) || startsWith(path, paste0(root, "/"))
}

CURRENT_USER <- effective_unix_user()
CURRENT_HOME <- unix_home_for_user(CURRENT_USER)
ACCESS_TOKEN <- Sys.getenv("CSL_WEB_ACCESS_TOKEN", unset = "")
IDLE_SHUTDOWN_SECONDS <- suppressWarnings(as.numeric(Sys.getenv("CSL_WEB_IDLE_SHUTDOWN_SECONDS", unset = "300")))
if (!is.finite(IDLE_SHUTDOWN_SECONDS) || IDLE_SHUTDOWN_SECONDS < 0) IDLE_SHUTDOWN_SECONDS <- 300
APP_RUNTIME <- new.env(parent = emptyenv())
APP_RUNTIME$active_sessions <- 0L
APP_RUNTIME$idle_generation <- 0L

access_token_valid <- function(query_string) {
  if (!nzchar(ACCESS_TOKEN)) return(TRUE)
  query_string <- sub("^\\?", "", as.character(query_string %||% ""))
  supplied <- tryCatch(shiny::parseQueryString(query_string)[["token"]] %||% "", error = function(e) "")
  identical(as.character(supplied), ACCESS_TOKEN)
}

register_authorized_session <- function(session) {
  APP_RUNTIME$active_sessions <- APP_RUNTIME$active_sessions + 1L
  APP_RUNTIME$idle_generation <- APP_RUNTIME$idle_generation + 1L
  session$onSessionEnded(function() {
    APP_RUNTIME$active_sessions <- max(0L, APP_RUNTIME$active_sessions - 1L)
    APP_RUNTIME$idle_generation <- APP_RUNTIME$idle_generation + 1L
    generation <- APP_RUNTIME$idle_generation
    if (APP_RUNTIME$active_sessions == 0L && IDLE_SHUTDOWN_SECONDS > 0) {
      later::later(function() {
        if (APP_RUNTIME$active_sessions == 0L && identical(APP_RUNTIME$idle_generation, generation)) {
          shiny::stopApp()
        }
      }, delay = IDLE_SHUTDOWN_SECONDS)
    }
  })
  invisible(NULL)
}

find_codespringlab_root <- function() {
  env_root <- Sys.getenv("CSL_CODESPRINGLAB_ROOT", unset = "")
  if (nzchar(env_root) && !path_is_within(env_root, CURRENT_HOME)) {
    stop(
      "Refusing CSL_CODESPRINGLAB_ROOT outside the effective user's home (",
      CURRENT_HOME, "): ", env_root
    )
  }
  candidates <- unique(c(
    env_root,
    getwd(),
    dirname(getwd()),
    file.path(CURRENT_HOME, "CodeSpringLab"),
    file.path(CURRENT_HOME, "CSH", "CodeSpringLab")
  ))
  candidates <- candidates[nzchar(candidates)]
  candidates <- candidates[vapply(candidates, path_is_within, logical(1), root = CURRENT_HOME)]
  for (candidate in candidates) {
    if (dir.exists(file.path(candidate, "scripts_DoNotTouch"))) {
      return(normalizePath(candidate, winslash = "/", mustWork = FALSE))
    }
  }
  stop(
    "CodeSpringLab was not found for the current user. Install it at ~/CodeSpringLab ",
    "or launch CodeSpringApp with CSL_CODESPRINGLAB_ROOT=/path/to/CodeSpringLab."
  )
}

CSL_ROOT <- find_codespringlab_root()
SCRIPTS_DIR <- file.path(CSL_ROOT, "scripts_DoNotTouch")
requested_app_home <- Sys.getenv("CSL_WEB_HOME", unset = "")
if (nzchar(requested_app_home) && !path_is_within(requested_app_home, CURRENT_HOME)) {
  stop(
    "Refusing CSL_WEB_HOME outside the effective user's home (",
    CURRENT_HOME, "): ", requested_app_home
  )
}
APP_HOME <- normalizePath(if (nzchar(requested_app_home)) requested_app_home else file.path(CURRENT_HOME, ".codespringweb"), winslash = "/", mustWork = FALSE)
dir.create(APP_HOME, recursive = TRUE, showWarnings = FALSE)
JOBS_PATH <- file.path(APP_HOME, "jobs.tsv")
LAST_PROJECT_PATH <- file.path(APP_HOME, "last_project_id.txt")
PROJECT_CONFIG_ROOT <- file.path(APP_HOME, "project_configs")
DEFAULT_RESULTS_ROOT <- normalizePath(file.path(CURRENT_HOME, "csl_results"), winslash = "/", mustWork = FALSE)
RNA_EXAMPLE_FASTQ_DIR <- normalizePath(file.path(SCRIPTS_DIR, "test", "fastq"), winslash = "/", mustWork = FALSE)
RNA_EXAMPLE_DESIGN_DIR <- normalizePath(file.path(SCRIPTS_DIR, "test", "manifest"), winslash = "/", mustWork = FALSE)
ATAC_EXAMPLE_FASTQ_DIR <- normalizePath(file.path(SCRIPTS_DIR, "test", "fastq_atac"), winslash = "/", mustWork = FALSE)
ATAC_EXAMPLE_DESIGN_DIR <- normalizePath(file.path(SCRIPTS_DIR, "test", "manifest_atac"), winslash = "/", mustWork = FALSE)
PROGRESS_REFRESH_MS <- 5000
JOB_HISTORY_CACHE_SECONDS <- 10
JOB_HISTORY_CACHE <- new.env(parent = emptyenv())
METRIC_LINES_CACHE <- new.env(parent = emptyenv())
CUTRUN_DEFAULT_SPIKEIN_INDEX <- "/grid/bsr/data/data/utama/genome/ecoli_k12/bowtie2_index/ecoli_k12_mg1655"
CUTRUN_DEFAULT_SPIKEIN_NAME <- "ecoli"
CUTRUN_SPIKEIN_GENOME_CHOICES <- c("E. coli K-12 MG1655" = CUTRUN_DEFAULT_SPIKEIN_INDEX)
GSEAPY_GENESET_OPTIONS <- c(
  "MSigDB_Hallmark_2020",
  "KEGG_2021_Human",
  "GO_Biological_Process_2025",
  "Reactome_Pathways_2024",
  "ARCHS4_TFs_Coexp",
  "ENCODE_TF_ChIP-seq_2015",
  "ENCODE_Histone_Modifications_2015",
  "FANTOM6_lncRNA_KD_DEGs",
  "miRTarBase_2017",
  "TRANSFAC_and_JASPAR_PWMs",
  "GTEx_Tissues_V8_2023",
  "CellMarker_2024",
  "Cancer_Cell_Line_Encyclopedia",
  "ClinVar_2019",
  "GTEx_Aging_Signatures_2021",
  "Proteomics_Drug_Atlas_2023"
)
LOGO_CSL_PATH <- file.path(SCRIPTS_DIR, "Logo_CSL.png")
LOGO_PATH <- file.path(SCRIPTS_DIR, "Logo.png")
FLOWCHART_PATH <- file.path(SCRIPTS_DIR, "flowchart.png")
if (file.exists(LOGO_CSL_PATH)) addResourcePath("csl_logo", dirname(LOGO_CSL_PATH))
if (file.exists(LOGO_PATH)) addResourcePath("codespring_logo", dirname(LOGO_PATH))
if (file.exists(FLOWCHART_PATH)) addResourcePath("codespring_flowchart", dirname(FLOWCHART_PATH))

cleanup_previous_shiny_processes <- function() {
  if (identical(Sys.getenv("CSL_WEB_AUTOKILL_SHINY", unset = "1"), "0")) return(invisible(character(0)))
  current_pid <- as.integer(Sys.getpid())
  current_user <- CURRENT_USER
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
  if (grepl("cut.?run|cutandrun", x)) return("CUT&RUN")
  if (grepl("atac", x)) return("ATAC-seq")
  if (grepl("chip", x)) return("ChIP-seq")
  "RNA-seq"
}

analysis_key <- function(x) {
  x <- tolower(as.character(x %||% "rna"))
  if (grepl("cut.?run|cutandrun", x)) return("cutrun")
  if (grepl("atac", x)) return("atac")
  if (grepl("chip", x)) return("chip")
  "rna"
}

analysis_notebook_dir <- function(key) {
  switch(analysis_key(key), atac = "bulkATACseq", chip = "bulkChIPseq", cutrun = "bulkCUTRUNseq", rna = "bulkRNAseq")
}

is_cutrun_project <- function(project) {
  identical(analysis_key(project$analysis_key %||% project$analysis), "cutrun")
}

is_atac_project <- function(project) {
  identical(analysis_key(project$analysis_key %||% project$analysis), "atac")
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

parse_fastq_dirs <- function(value, normalize = TRUE) {
  value <- as.character(value %||% character(0))
  parts <- unlist(strsplit(value, "[\r\n;]+", perl = TRUE), use.names = FALSE)
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  if (!isTRUE(normalize)) return(unique(parts))
  unique(normalizePath(path.expand(parts), winslash = "/", mustWork = FALSE))
}

project_fastq_dirs <- function(project) {
  dirs <- parse_fastq_dirs(project$fastq_dirs %||% project$fastq_dir %||% "")
  if (!length(dirs) && nzchar(project$fastq_dir %||% "")) dirs <- parse_fastq_dirs(project$fastq_dir)
  dirs
}

with_slash <- function(path) {
  path <- trimws(as.character(path %||% ""))
  if (!nzchar(path)) return(path)
  paste0(sub("/+$", "", path), "/")
}

read_last_project_id <- function() {
  if (!file.exists(LAST_PROJECT_PATH)) return("__new__")
  value <- trimws(readLines(LAST_PROJECT_PATH, warn = FALSE, n = 1))
  if (length(value) && nzchar(value[[1]])) value[[1]] else "__new__"
}

write_last_project_id <- function(project_id) {
  project_id <- as.character(project_id %||% "__new__")
  dir.create(dirname(LAST_PROJECT_PATH), recursive = TRUE, showWarnings = FALSE)
  writeLines(project_id, LAST_PROJECT_PATH)
  invisible(project_id)
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
  configured_fastq_dirs <- parse_fastq_dirs(vals$read_paths %||% vals$read_path_destination %||% vals$read_path_original %||% "", normalize = FALSE)
  fastq_dirs <- vapply(configured_fastq_dirs, resolve_legacy_path, character(1), key = key)
  fastq_dir <- if (length(fastq_dirs)) fastq_dirs[[1]] else ""
  pairing <- tolower(vals$pairing %||% "y")
  data_dir <- if (nzchar(visualizer_data_dir)) visualizer_data_dir else file.path(results_root, project_name, "data")
  list(
    id = paste0(key, "/", clean_name(project_name, "project")),
    name = clean_name(project_name, "project"),
    label = project_name,
    analysis = analysis_label(key),
    analysis_key = key,
    genome = tolower(vals$genome %||% "mouse"),
    genome_version = vals$genome_version %||% vals$reference_genome %||% "",
    paired_end = !(pairing %in% c("n", "no", "false", "single", "se")),
    results_root = results_root,
    data_dir = data_dir,
    fastq_dir = fastq_dir,
    fastq_dirs = fastq_dirs,
    design_matrix_path = design_path_from_dir(inpath_design),
    source_config = normalizePath(path, winslash = "/", mustWork = FALSE),
    source = "CodeSpringLab config"
  )
}

legacy_project_config_files <- function() {
  roots <- unique(c(file.path(SCRIPTS_DIR, "project_configs"), file.path(CSL_ROOT, "project_configs")))
  files <- unlist(lapply(roots, function(root) {
    if (dir.exists(root)) list.files(root, pattern = "\\.py$", recursive = TRUE, full.names = TRUE) else character(0)
  }), use.names = FALSE)
  unique(normalizePath(files, winslash = "/", mustWork = FALSE))
}

migrate_user_legacy_projects <- function() {
  legacy_files <- legacy_project_config_files()
  if (!length(legacy_files) || !file.exists(JOBS_PATH)) return(invisible(character(0)))

  jobs <- tryCatch(utils::read.delim(JOBS_PATH, check.names = FALSE, stringsAsFactors = FALSE, fill = TRUE), error = function(e) data.frame())
  if (!NROW(jobs)) return(invisible(character(0)))
  known_data_dirs <- if ("data_dir" %in% names(jobs)) {
    values <- as.character(jobs$data_dir)
    values <- values[!is.na(values) & nzchar(values)]
    unique(normalizePath(values, winslash = "/", mustWork = FALSE))
  } else character(0)

  migrated <- character(0)
  for (path in legacy_files) {
    project <- legacy_project_from_config(path)
    if (is.null(project)) next
    project_data_dir <- normalizePath(project$data_dir %||% "", winslash = "/", mustWork = FALSE)
    belongs_to_user <- nzchar(project_data_dir) && project_data_dir %in% known_data_dirs
    if (!belongs_to_user) next

    destination_dir <- file.path(PROJECT_CONFIG_ROOT, project$analysis_key)
    destination <- file.path(destination_dir, paste0(clean_name(project$name, "project"), ".py"))
    if (file.exists(destination)) next
    dir.create(destination_dir, recursive = TRUE, showWarnings = FALSE)
    if (isTRUE(file.copy(path, destination, overwrite = FALSE))) migrated <- c(migrated, destination)
  }
  invisible(migrated)
}

example_dataset_paths <- function(key) {
  switch(
    analysis_key(key),
    rna = list(name = "example_rnaseq", fastq_dir = RNA_EXAMPLE_FASTQ_DIR, design_dir = RNA_EXAMPLE_DESIGN_DIR),
    atac = list(name = "example_atac", fastq_dir = ATAC_EXAMPLE_FASTQ_DIR, design_dir = ATAC_EXAMPLE_DESIGN_DIR),
    NULL
  )
}

is_bundled_example_design <- function(path) {
  path <- normalizePath(path %||% "", winslash = "/", mustWork = FALSE)
  examples <- normalizePath(
    c(file.path(RNA_EXAMPLE_DESIGN_DIR, "design_matrix.txt"), file.path(ATAC_EXAMPLE_DESIGN_DIR, "design_matrix.txt")),
    winslash = "/", mustWork = FALSE
  )
  nzchar(path) && path %in% examples
}

discover_projects <- function() {
  # Project configs are user state. Keeping discovery inside APP_HOME prevents
  # configs accidentally committed to CodeSpringLab from appearing for every
  # person who clones the repositories.
  # Existing users are migrated only when a legacy config matches their own
  # private jobs.tsv history, so another person's saved projects stay hidden.
  migrate_user_legacy_projects()
  roots <- PROJECT_CONFIG_ROOT
  files <- character(0)
  for (root in roots) {
    if (dir.exists(root)) files <- c(files, list.files(root, pattern = "\\.py$", recursive = TRUE, full.names = TRUE))
  }
  files <- unique(normalizePath(files, winslash = "/", mustWork = FALSE))
  projects <- Filter(Negate(is.null), lapply(files, legacy_project_from_config))
  if (!length(projects)) return(list())
  names(projects) <- vapply(projects, `[[`, character(1), "id")
  projects
}

new_project_from_inputs <- function(input) {
  key <- analysis_key(input$new_project_analysis %||% input$analysis %||% "RNA-seq")
  project_name <- clean_name(input$new_project_name %||% paste0("new_", key, "_project"), paste0("new_", key, "_project"))
  label <- input$new_project_name %||% project_name
  results_root <- normalizePath(path.expand(input$new_results_root %||% DEFAULT_RESULTS_ROOT), winslash = "/", mustWork = FALSE)
  data_dir <- file.path(results_root, project_name, "data")
  design_path <- trimws(input$new_design_matrix_path %||% "")
  if (!nzchar(design_path)) design_path <- file.path(data_dir, "manifest", "design_matrix.txt")
  else if (dir.exists(path.expand(design_path))) {
    design_path <- file.path(normalizePath(path.expand(design_path), winslash = "/", mustWork = FALSE), "design_matrix.txt")
  } else if (basename(design_path) != "design_matrix.txt") {
    design_path <- file.path(normalizePath(dirname(path.expand(design_path)), winslash = "/", mustWork = FALSE), "design_matrix.txt")
  }
  design_path <- normalizePath(path.expand(design_path), winslash = "/", mustWork = FALSE)
  fastq_mode <- tolower(input$new_fastq_location_mode %||% "one")
  fastq_dirs <- if (identical(fastq_mode, "multiple")) parse_fastq_dirs(input$new_fastq_dirs %||% "") else parse_fastq_dirs(input$new_fastq_dir %||% "")
  fastq_dir <- if (length(fastq_dirs)) fastq_dirs[[1]] else ""
  paired <- !tolower(input$new_paired_end %||% "paired") %in% c("single", "se", "n", "no", "false")
  list(
    id = paste0(key, "/", project_name),
    name = project_name,
    label = label,
    analysis = analysis_label(key),
    analysis_key = key,
    genome = tolower(input$new_species %||% "mouse"),
    genome_version = input$new_genome_version %||% "",
    paired_end = paired,
    results_root = results_root,
    data_dir = data_dir,
    fastq_dir = fastq_dir,
    fastq_dirs = fastq_dirs,
    design_matrix_path = design_path,
    source_config = "",
    source = "new project"
  )
}

project_config_dir <- function(key) {
  file.path(PROJECT_CONFIG_ROOT, analysis_key(key))
}

project_config_roots <- function() {
  normalizePath(PROJECT_CONFIG_ROOT, winslash = "/", mustWork = FALSE)
}

is_managed_project_config <- function(path) {
  path <- trimws(as.character(path %||% ""))
  if (!nzchar(path)) return(FALSE)
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!file.exists(path) || basename(path) == "config.py") return(FALSE)
  any(vapply(project_config_roots(), function(root) startsWith(path, paste0(sub("/+$", "", root), "/")), logical(1)))
}

delete_projects <- function(projects_to_delete, delete_data = FALSE) {
  if (!length(projects_to_delete)) return("No projects selected.")
  messages <- character(0)
  for (project in projects_to_delete) {
    cancelled <- cancel_active_project_jobs(project)
    if (nzchar(cancelled)) messages <- c(messages, cancelled)
    cfg <- project$source_config %||% ""
    if (is_managed_project_config(cfg)) {
      ok <- unlink(cfg, force = TRUE) == 0
      messages <- c(messages, paste(if (ok) "Deleted config:" else "Could not delete config:", cfg))
    } else {
      messages <- c(messages, paste("Skipped unmanaged config:", if (nzchar(cfg)) cfg else project$label))
    }
    if (isTRUE(delete_data)) {
      deleted <- delete_project_results(project)
      messages <- c(messages, deleted$message)
    }
  }
  paste(messages, collapse = "\n")
}

project_result_dir <- function(project) {
  data_dir <- normalizePath(project$data_dir %||% "", winslash = "/", mustWork = FALSE)
  if (nzchar(data_dir) && basename(data_dir) %in% c("data", "log", "shiny")) return(dirname(data_dir))
  file.path(normalizePath(project$results_root %||% DEFAULT_RESULTS_ROOT, winslash = "/", mustWork = FALSE), project$name %||% project$label)
}

project_result_dir_is_safe <- function(project, path = project_result_dir(project)) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- normalizePath(project$results_root %||% "", winslash = "/", mustWork = FALSE)
  nzchar(path) &&
    nzchar(root) &&
    startsWith(path, paste0(sub("/+$", "", root), "/")) &&
    basename(path) == (project$name %||% project$label)
}

dir_has_contents <- function(path) {
  if (!dir.exists(path)) return(FALSE)
  entries <- list.files(path, all.files = TRUE, no.. = TRUE)
  entries <- setdiff(entries, c(".DS_Store", "Thumbs.db", "desktop.ini", ".gitkeep"))
  length(entries) > 0
}

prune_project_job_history <- function(project) {
  if (!file.exists(JOBS_PATH)) return(invisible(0))
  jobs <- tryCatch(utils::read.delim(JOBS_PATH, check.names = FALSE, stringsAsFactors = FALSE, fill = TRUE), error = function(e) data.frame())
  if (!NROW(jobs)) return(invisible(0))
  remove <- rep(FALSE, NROW(jobs))
  if ("project_id" %in% names(jobs) && nzchar(project$id %||% "")) remove <- remove | jobs$project_id == project$id
  if ("data_dir" %in% names(jobs) && nzchar(project$data_dir %||% "")) remove <- remove | jobs$data_dir == project$data_dir
  if (!any(remove) && "project" %in% names(jobs) && nzchar(project$name %||% "")) remove <- jobs$project == project$name
  removed <- sum(remove)
  jobs <- jobs[!remove, , drop = FALSE]
  utils::write.table(jobs, JOBS_PATH, sep = "\t", row.names = FALSE, quote = TRUE, append = FALSE, col.names = TRUE)
  invisible(removed)
}

delete_project_results <- function(project) {
  result_dir <- project_result_dir(project)
  if (!project_result_dir_is_safe(project, result_dir)) {
    return(list(ok = FALSE, message = paste("Refusing to delete unexpected project path:", result_dir)))
  }
  if (!dir.exists(result_dir)) {
    removed_jobs <- prune_project_job_history(project)
    return(list(ok = TRUE, message = paste("No project results folder found for:", project$label, sprintf("(removed %s old job record%s)", removed_jobs, ifelse(removed_jobs == 1, "", "s")))))
  }
  ok <- unlink(result_dir, recursive = TRUE, force = TRUE) == 0
  removed_jobs <- prune_project_job_history(project)
  list(
    ok = ok,
    message = paste(if (ok) "Deleted entire project folder:" else "Could not delete project folder:", result_dir, sprintf("(removed %s old job record%s)", removed_jobs, ifelse(removed_jobs == 1, "", "s")))
  )
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
    sprintf("read_paths = %s", deparse(paste(project_fastq_dirs(project), collapse = ";"))),
    sprintf("genome = %s", deparse(project$genome)),
    sprintf("genome_version = %s", deparse(genome_reference_key(project))),
    sprintf("pairing = %s", deparse(if (isTRUE(project$paired_end)) "y" else "n"))
  )
  if (is_atac_project(project)) {
    ref <- atac_reference_resources(project)
    lines <- c(lines,
      sprintf("bowtie2_index = %s", deparse(ref$bowtie2_index)),
      sprintf("chrom_sizes = %s", deparse(ref$chrom_sizes)),
      sprintf("effective_genome_size = %s", deparse(ref$effective_genome_size)),
      sprintf("macs2_genome_size = %s", deparse(ref$macs2_genome))
    )
  }
  if (is_cutrun_project(project)) {
    ref <- cutrun_reference_resources(project)
    lines <- c(lines,
      sprintf("bowtie2_index = %s", deparse(ref$bowtie2_index)),
      sprintf("chrom_sizes = %s", deparse(ref$chrom_sizes)),
      sprintf("macs2_genome_size = %s", deparse(ref$macs2_genome)),
      "peakcaller = 'seacr'",
      "seacr_norm = 'non'",
      "seacr_stringency = 'stringent'",
      "minimum_alignment_q_score = '30'",
      "max_fragment_length = '1000'",
      "normalization_mode = 'spikein'",
      "normalisation_mode = 'spikein'",
      sprintf("spikein_index_path = %s", deparse(CUTRUN_DEFAULT_SPIKEIN_INDEX)),
      sprintf("spikein_genome = %s", deparse(CUTRUN_DEFAULT_SPIKEIN_INDEX)),
      sprintf("spikein_name = %s", deparse(CUTRUN_DEFAULT_SPIKEIN_NAME)),
      "spikein_min_reads = '1000'",
      "dedup_target_reads = 'n'",
      "dedup_control_reads = 'y'",
      "remove_mitochondrial_reads = 'y'"
    )
  }
  writeLines(lines, cfg_path)
  cfg_path
}

project_select_choices <- function(projects, analysis = "RNA-seq") {
  p <- projects
  if (length(p)) {
    p <- p[vapply(p, function(x) identical(x$analysis, analysis), logical(1))]
  }
  labels <- if (length(p)) vapply(p, function(x) x$label, character(1)) else character(0)
  ids <- if (length(p)) names(p) else character(0)
  c("Start a new project" = "__new__", stats::setNames(ids, labels))
}

record_preflight_failure <- function(project, step, message, log_name = clean_name(step, "preflight")) {
  log_dir <- file.path(dirname(project$data_dir), "log")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  tool <- clean_name(log_name, clean_name(step, "preflight"))
  stderr <- file.path(log_dir, paste0("error_", tool, "_preflight.txt"))
  submit_log <- file.path(log_dir, paste0("submit_", tool, "_preflight.txt"))
  lines <- c(
    paste("time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste("project:", project$name),
    paste("step:", step),
    "status: pre-submit validation failed",
    paste("data_dir:", project$data_dir),
    paste("fastq_dirs:", paste(project_fastq_dirs(project), collapse = ";")),
    paste("design_matrix:", project$design_matrix_path),
    "",
    message
  )
  writeLines(lines, stderr)
  writeLines(c(lines, paste("stderr:", stderr)), submit_log)
  paste("ERROR: Not submitted.", message, "\nPre-submit error log:", stderr)
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

read_key_value_table <- function(path, key_name = "Metric", value_name = "Value") {
  lines <- read_metric_lines(path)
  lines <- lines[nzchar(lines)]
  if (!length(lines)) return(data.frame())
  parts <- strsplit(lines, "\t", fixed = TRUE)
  rows <- lapply(parts, function(x) {
    if (length(x) < 2) return(NULL)
    data.frame(key = x[[1]], value = paste(x[-1], collapse = "\t"), stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  out <- do.call(rbind, rows)
  names(out) <- c(key_name, value_name)
  out
}

safe_read_result_table <- function(path, n = 5000) {
  if (!file.exists(path)) return(data.frame())
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("bed", "bedgraph", "narrowpeak", "broadpeak")) {
    x <- tryCatch(utils::read.table(path, sep = "\t", header = FALSE, quote = "", comment.char = "", check.names = FALSE, nrows = n), error = function(e) data.frame())
    if (NROW(x)) {
      names(x) <- paste0("V", seq_len(NCOL(x)))
      if (ext == "bedgraph" && NCOL(x) >= 4) names(x)[1:4] <- c("chrom", "start", "end", "score")
      if (ext == "bed" && NCOL(x) >= 3) names(x)[1:3] <- c("chrom", "start", "end")
      if (ext == "narrowpeak" && NCOL(x) >= 10) names(x)[1:10] <- c("chrom", "start", "end", "name", "score", "strand", "signalValue", "pValue", "qValue", "peak")
      if (ext == "broadpeak" && NCOL(x) >= 9) names(x)[1:9] <- c("chrom", "start", "end", "name", "score", "strand", "signalValue", "pValue", "qValue")
    }
    return(x)
  }
  safe_read_table(path, n)
}

render_data_table <- function(df, page_length = 50, height = NULL) {
  if (!NROW(df)) return(tags$div(class = "empty-box", "No rows available."))
  if (DT_AVAILABLE) {
    DT::datatable(
      df,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = page_length,
        lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100", "All")),
        paging = TRUE,
        pagingType = "full_numbers",
        dom = "lfrtip",
        scrollX = TRUE,
        scrollY = height %||% "520px",
        autoWidth = FALSE
      )
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

scan_fastqs <- function(folder, paired = TRUE, metadata_cols = "treatment", infer_samples = FALSE) {
  files <- fastq_files(folder)
  rows <- list()
  used <- character(0)
  if (paired) {
    for (r1 in files) {
      if (r1 %in% used) next
      r2 <- mate_name(r1, 2)
      if (!is.na(r2) && r2 %in% files) {
        rows[[length(rows) + 1]] <- data.frame(include = TRUE, sample = if (isTRUE(infer_samples)) infer_sample(r1) else "", filename = paste(r1, r2, sep = ","), status = "paired")
        used <- c(used, r1, r2)
      } else if (grepl("([._-]R)1|([._-])1", r1, ignore.case = TRUE)) {
        rows[[length(rows) + 1]] <- data.frame(include = FALSE, sample = if (isTRUE(infer_samples)) infer_sample(r1) else "", filename = r1, status = "missing R2")
        used <- c(used, r1)
      }
    }
  } else {
    for (f in files) rows[[length(rows) + 1]] <- data.frame(include = TRUE, sample = if (isTRUE(infer_samples)) infer_sample(f) else "", filename = f, status = "single")
  }
  df <- if (length(rows)) do.call(rbind, rows) else data.frame(include = logical(), sample = character(), filename = character(), status = character())
  for (col in metadata_cols) if (!col %in% names(df)) df[[col]] <- ""
  df[, c("include", "sample", metadata_cols, "filename", "status"), drop = FALSE]
}

scan_fastq_dirs <- function(folders, paired = TRUE, metadata_cols = "treatment", infer_samples = FALSE) {
  folders <- parse_fastq_dirs(folders)
  folders <- folders[dir.exists(folders)]
  if (!length(folders)) return(scan_fastqs("", paired, metadata_cols, infer_samples))
  multiple <- length(folders) > 1L
  scanned <- lapply(folders, function(folder) {
    df <- scan_fastqs(folder, paired, metadata_cols, infer_samples)
    if (multiple && NROW(df)) {
      df$filename <- vapply(as.character(df$filename), function(value) {
        parts <- trimws(unlist(strsplit(value, ",", fixed = TRUE)))
        paste(file.path(folder, basename(parts)), collapse = ",")
      }, character(1))
    }
    df
  })
  out <- do.call(rbind, scanned)
  rownames(out) <- NULL
  out
}

infer_cutrun_metadata <- function(df) {
  if (!NROW(df) || !"sample" %in% names(df)) return(df)
  for (col in c("cell_type", "mark", "target_class", "seacr_stringency", "condition", "replicate", "control_sample")) {
    if (!col %in% names(df)) df[[col]] <- ""
  }
  for (i in seq_len(NROW(df))) {
    sample <- trimws(as.character(df$sample[[i]] %||% ""))
    core <- sub("_S[0-9]+(?:_.*)?$", "", sample, perl = TRUE, ignore.case = TRUE)
    match <- regmatches(core, regexec("^([^_]+)_([^_-]+)[_-](.+?)([0-9]+)$", core, perl = TRUE))[[1]]
    if (length(match) == 5L) {
      inferred <- c(cell_type = match[[2]], mark = match[[3]], condition = match[[4]], replicate = match[[5]])
    } else {
      control_match <- regmatches(core, regexec("^([^_]+)_(IgG|input|control)[_-](.+)$", core, perl = TRUE, ignore.case = TRUE))[[1]]
      if (length(control_match) != 4L) next
      inferred <- c(cell_type = control_match[[2]], mark = control_match[[3]], condition = control_match[[4]])
    }
    for (field in names(inferred)) {
      if (!nzchar(trimws(as.character(df[[field]][[i]] %||% "")))) df[[field]][[i]] <- inferred[[field]]
    }
  }
  control_rows <- grepl("igg|input|control", tolower(as.character(df$mark)))
  inferred_class <- vapply(as.character(df$mark), function(mark) {
    value <- tolower(trimws(mark))
    if (grepl("igg|input|control", value)) return("control")
    if (grepl("h3k27me3|h3k9me3|h3k36me3|h4k20me3", value)) return("histone_broad")
    if (grepl("^(h[1-4]|histone)", value)) return("histone_narrow")
    "tf_or_other"
  }, character(1))
  blank_class <- !nzchar(trimws(as.character(df$target_class)))
  df$target_class[blank_class] <- inferred_class[blank_class]
  blank_stringency <- !nzchar(trimws(as.character(df$seacr_stringency)))
  df$seacr_stringency[blank_stringency] <- "auto"
  for (i in which(!control_rows)) {
    if (nzchar(trimws(as.character(df$control_sample[[i]] %||% "")))) next
    exact <- which(
      control_rows &
        trimws(as.character(df$cell_type)) == trimws(as.character(df$cell_type[[i]])) &
        trimws(as.character(df$condition)) == trimws(as.character(df$condition[[i]]))
    )
    if (length(exact) == 1L) df$control_sample[[i]] <- as.character(df$sample[[exact]])
  }
  df
}

infer_atac_metadata <- function(df) {
  if (!NROW(df) || !"sample" %in% names(df)) return(df)
  for (col in c("condition", "replicate")) if (!col %in% names(df)) df[[col]] <- ""
  for (i in seq_len(NROW(df))) {
    sample <- sub("_S[0-9]+(?:_.*)?$", "", as.character(df$sample[[i]]), perl = TRUE, ignore.case = TRUE)
    hit <- regmatches(sample, regexec("^.+[_-]([^_-]+)[_-]rep([0-9]+)$", sample, perl = TRUE, ignore.case = TRUE))[[1]]
    if (length(hit) != 3L) hit <- regmatches(sample, regexec("^.+[_-]([A-Za-z]+)([0-9]+)$", sample, perl = TRUE))[[1]]
    if (length(hit) == 3L) {
      if (!nzchar(as.character(df$condition[[i]]))) df$condition[[i]] <- hit[[2]]
      if (!nzchar(as.character(df$replicate[[i]]))) df$replicate[[i]] <- hit[[3]]
    }
  }
  df
}

sync_metadata_columns <- function(df, metadata_cols) {
  if (!NROW(df)) {
    df <- data.frame(include = logical(), sample = character(), filename = character(), status = character())
  }
  metadata_cols <- unique(metadata_cols[nzchar(metadata_cols)])
  current_metadata <- setdiff(names(df), c("include", "sample", "filename", "status"))
  for (col in setdiff(metadata_cols, current_metadata)) df[[col]] <- ""
  drop_cols <- setdiff(current_metadata, metadata_cols)
  if (length(drop_cols)) df <- df[, setdiff(names(df), drop_cols), drop = FALSE]
  df[, design_matrix_columns(df), drop = FALSE]
}

design_matrix_columns <- function(df) {
  existing_metadata <- setdiff(names(df), c("include", "sample", "filename", "status"))
  if (!NROW(df)) {
    if (length(existing_metadata)) return(c("include", "sample", existing_metadata, "filename", "status"))
    return(c("include", "sample", "treatment", "filename", "status"))
  }
  c("include", "sample", setdiff(names(df), c("include", "sample", "filename", "status")), "filename", "status")
}

default_metadata_cols <- function(project = NULL, analysis = NULL) {
  key <- if (!is.null(project)) analysis_key(project$analysis_key %||% project$analysis) else analysis_key(analysis %||% "rna")
  if (identical(key, "cutrun")) return(c("cell_type", "mark", "target_class", "seacr_stringency", "condition", "replicate", "control_sample"))
  if (identical(key, "atac")) return(c("cell_type", "condition", "replicate"))
  "treatment"
}

project_metadata_cols <- function(project) {
  defaults <- default_metadata_cols(project)
  path <- project$design_matrix_path %||% ""
  if (!nzchar(path) || !file.exists(path) || dir.exists(path)) return(defaults)
  existing <- safe_read_table(path)
  unique(c(defaults, setdiff(names(existing), c("sample", "filename", "include", "status"))))
}

parse_metadata_cols <- function(x, project = NULL) {
  raw <- unlist(strsplit(as.character(x %||% ""), ",", fixed = TRUE), use.names = FALSE)
  cols <- clean_name(trimws(raw))
  cols <- cols[nzchar(cols) & !cols %in% c("sample", "filename", "include", "status")]
  cols <- unique(cols)
  if (!length(cols)) cols <- default_metadata_cols(project)
  if (!is.null(project) && is_cutrun_project(project)) cols <- unique(c(default_metadata_cols(project), cols))
  cols
}

ensure_design_metadata_columns <- function(df, metadata_cols) {
  if (!NROW(df) && !length(names(df))) {
    df <- data.frame(include = logical(), sample = character(), filename = character(), status = character())
  }
  metadata_cols <- unique(metadata_cols[nzchar(metadata_cols)])
  n <- NROW(df)
  for (base_col in c("include", "sample", "filename", "status")) {
    if (!base_col %in% names(df)) {
      df[[base_col]] <- if (identical(base_col, "include")) rep(TRUE, n) else rep("", n)
    }
  }
  for (col in metadata_cols) {
    if (!col %in% names(df)) df[[col]] <- rep("", n)
  }
  df[, c("include", "sample", metadata_cols, "filename", "status"), drop = FALSE]
}

as_design_bool <- function(x) {
  if (is.logical(x)) return(isTRUE(x))
  tolower(as.character(x %||% "")) %in% c("true", "t", "1", "yes", "y")
}

design_matrix_ui <- function(df, project = NULL) {
  if (!NROW(df)) return(div(class = "empty-box", "Scan a FASTQ folder or select a project with an existing design_matrix.txt."))
  tagList(
    tags$p(class = "muted small-note", "Click a cell to edit. Use TRUE/FALSE in include. The table is paged so large projects do not freeze the app."),
    if (!is.null(project) && is_cutrun_project(project)) tags$p(
      class = "muted small-note",
      "CUT&RUN target_class values: tf_or_other, histone_narrow, histone_broad, or control. seacr_stringency may be auto, stringent, or relaxed. Auto uses the SEACR panel default. Target class documents peak biology but does not silently change SEACR stringency."
    ),
    table_output("design_editor_table")
  )
}

results_design_matrix_path <- function(project) {
  file.path(project$data_dir, "manifest", "design_matrix.txt")
}

write_design_matrix <- function(project, df, metadata_cols) {
  if (!"include" %in% names(df)) df$include <- TRUE
  metadata_cols <- unique(metadata_cols[nzchar(metadata_cols)])
  metadata_cols <- metadata_cols[!metadata_cols %in% c("sample", "filename", "include", "status")]
  if (is_cutrun_project(project)) metadata_cols <- unique(c(default_metadata_cols(project), metadata_cols))
  if (!length(metadata_cols)) metadata_cols <- default_metadata_cols(project)
  df <- ensure_design_metadata_columns(df, metadata_cols)
  keep <- df[vapply(df$include, as_design_bool, logical(1)), , drop = FALSE]
  if (!NROW(keep)) stop("No samples are included.")
  blank_sample <- !nzchar(trimws(as.character(keep$sample %||% "")))
  if (any(blank_sample)) {
    keep$sample[blank_sample] <- vapply(as.character(keep$filename[blank_sample]), function(x) {
      first_file <- trimws(strsplit(x, ",", fixed = TRUE)[[1]][1])
      infer_sample(first_file)
    }, character(1))
  }
  keep$sample <- clean_name(keep$sample)
  if (is_cutrun_project(project)) {
    keep <- infer_cutrun_metadata(keep)
    allowed_classes <- c("tf_or_other", "histone_narrow", "histone_broad", "control")
    allowed_stringency <- c("auto", "stringent", "relaxed")
    invalid_class <- !tolower(trimws(as.character(keep$target_class))) %in% allowed_classes
    invalid_stringency <- !tolower(trimws(as.character(keep$seacr_stringency))) %in% allowed_stringency
    if (any(invalid_class)) stop("Invalid CUT&RUN target_class for: ", paste(keep$sample[invalid_class], collapse = ", "), ". Use tf_or_other, histone_narrow, histone_broad, or control.")
    if (any(invalid_stringency)) stop("Invalid CUT&RUN seacr_stringency for: ", paste(keep$sample[invalid_stringency], collapse = ", "), ". Use auto, stringent, or relaxed.")
    keep$target_class <- tolower(trimws(as.character(keep$target_class)))
    keep$seacr_stringency <- tolower(trimws(as.character(keep$seacr_stringency)))
  }
  out <- results_design_matrix_path(project)
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

design_editor_from_project <- function(project, metadata_cols = NULL) {
  metadata_cols <- metadata_cols %||% default_metadata_cols(project)
  metadata_cols <- unique(c(
    metadata_cols,
    if (file.exists(project$design_matrix_path) && !dir.exists(project$design_matrix_path)) {
      existing <- safe_read_table(project$design_matrix_path)
      setdiff(names(existing), c("sample", "filename", "include", "status"))
    } else character(0)
  ))
  if (file.exists(project$design_matrix_path) && !dir.exists(project$design_matrix_path)) {
    df <- safe_read_table(project$design_matrix_path)
    if (NROW(df)) {
      if (!"sample" %in% names(df)) names(df)[1] <- "sample"
      df$include <- TRUE
      df$status <- "saved"
      df <- df[, c("include", setdiff(names(df), c("include", "status")), "status"), drop = FALSE]
      return(ensure_design_metadata_columns(df, metadata_cols))
    }
  }
  ensure_design_metadata_columns(data.frame(), metadata_cols)
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

atac_diffbind_design <- function(project, compare_col, subset_col = "", subset_value = "") {
  df <- project_design_df(project)
  if (!NROW(df)) stop("No design matrix found.")
  if (!compare_col %in% names(df)) stop("Selected comparison column is not in design matrix: ", compare_col)
  subset_col <- trimws(as.character(subset_col %||% ""))
  subset_value <- trimws(as.character(subset_value %||% ""))
  if (nzchar(subset_col)) {
    if (!subset_col %in% names(df)) stop("Selected subset column is not in design matrix: ", subset_col)
    if (!nzchar(subset_value)) stop("Choose a subset value for ", subset_col, ".")
    df <- df[trimws(as.character(df[[subset_col]])) == subset_value, , drop = FALSE]
    if (!NROW(df)) stop("No samples match ", subset_col, " = ", subset_value, ".")
  }
  if (!"filename" %in% names(df)) df$filename <- df$sample
  keep <- df[, c("sample", compare_col, "filename"), drop = FALSE]
  scope <- if (nzchar(subset_col)) paste(subset_col, subset_value, sep = "_") else "all_samples"
  out_dir <- file.path(project$data_dir, "manifest", "atac_diffbind", clean_name(scope), clean_name(compare_col))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out <- file.path(out_dir, "design_matrix.txt")
  utils::write.table(keep, out, sep = "\t", row.names = FALSE, quote = FALSE)
  out
}

count_files <- function(path, pattern) {
  if (!dir.exists(path)) return(0)
  length(list.files(path, pattern = pattern, recursive = TRUE, full.names = TRUE))
}

cutadapt_outputs_available <- function(project) {
  count_files(file.path(project$data_dir, "cutadapt"), fastq_suffix_regex) > 0
}

trimmed_checkbox_default <- function(project, current_value) {
  if (is.null(current_value)) cutadapt_outputs_available(project) else isTRUE(current_value)
}

extract_job_id <- function(x) {
  m <- regexpr("job_id:[[:space:]]*[0-9]+", x)
  if (m >= 0) return(sub("job_id:[[:space:]]*", "", regmatches(x, m)))
  m <- regexpr("Job ID:[[:space:]]*[0-9]+", x)
  if (m >= 0) return(sub("Job ID:[[:space:]]*", "", regmatches(x, m)))
  m <- regexpr("Submitted batch job[[:space:]]+[0-9]+", x)
  if (m >= 0) return(sub(".*Submitted batch job[[:space:]]+", "", regmatches(x, m)))
  ""
}

job_history_cache_key <- function(project) {
  paste(project$id %||% project$name %||% "project", project$data_dir %||% "", sep = "\r")
}

job_history_file_signature <- function() {
  if (!file.exists(JOBS_PATH)) return("missing")
  info <- file.info(JOBS_PATH)
  paste(info$size[[1]], as.numeric(info$mtime[[1]]), sep = ":")
}

job_history <- function(project, force_refresh = FALSE) {
  cache_key <- job_history_cache_key(project)
  file_signature <- job_history_file_signature()
  if (!isTRUE(force_refresh) && exists(cache_key, envir = JOB_HISTORY_CACHE, inherits = FALSE)) {
    cached <- get(cache_key, envir = JOB_HISTORY_CACHE, inherits = FALSE)
    cache_age <- as.numeric(difftime(Sys.time(), cached$checked, units = "secs"))
    if (identical(cached$file_signature, file_signature) && is.finite(cache_age) && cache_age < JOB_HISTORY_CACHE_SECONDS) {
      return(cached$value)
    }
  }
  if (!file.exists(JOBS_PATH)) return(data.frame())
  jobs <- tryCatch(utils::read.delim(JOBS_PATH, check.names = FALSE, stringsAsFactors = FALSE, fill = TRUE), error = function(e) data.frame())
  if (!NROW(jobs) || !"project" %in% names(jobs) || !"step" %in% names(jobs) || !"output" %in% names(jobs)) return(data.frame())
  if ("project_id" %in% names(jobs) && any(nzchar(jobs$project_id %||% ""))) {
    jobs <- jobs[jobs$project_id == (project$id %||% ""), , drop = FALSE]
  } else {
    jobs <- jobs[jobs$project == project$name, , drop = FALSE]
  }
  if ("data_dir" %in% names(jobs) && any(nzchar(jobs$data_dir %||% ""))) {
    jobs <- jobs[!nzchar(jobs$data_dir %||% "") | jobs$data_dir == (project$data_dir %||% ""), , drop = FALSE]
  }
  if ("analysis" %in% names(jobs) && nzchar(project$analysis %||% "")) {
    jobs <- jobs[jobs$analysis == project$analysis, , drop = FALSE]
  }
  if (!NROW(jobs)) return(data.frame())
  jobs$step[jobs$step == "Count matrix"] <- "featureCounts"
  jobs$step[jobs$step == "RSEM optional"] <- "RSEM (optional)"
  jobs$step[jobs$step == "Kallisto optional"] <- "Kallisto (optional)"
  jobs$job_id <- vapply(as.character(jobs$output), extract_job_id, character(1))
  jobs$slurm_state <- ifelse(nzchar(jobs$job_id), "Submitted", "No job id")
  jobs$elapsed <- ""
  jobs$start_time <- ""
  jobs$end_time <- ""
  jobs$slurm_job_name <- ""
  ids <- unique(jobs$job_id[nzchar(jobs$job_id)])
  if (length(ids) && nzchar(Sys.which("squeue"))) {
    queue_user <- CURRENT_USER
    queue_args <- if (nzchar(queue_user)) {
      c("-h", "-u", queue_user, "-o", "%A|%T|%M|%j")
    } else {
      c("-h", "-j", paste(ids, collapse = ","), "-o", "%A|%T|%M|%j")
    }
    sq <- tryCatch(suppressWarnings(system2("squeue", queue_args, stdout = TRUE, stderr = FALSE, timeout = 10)), error = function(e) character(0))
    sq <- sq[nzchar(sq)]
    if (length(sq)) {
      parts <- strsplit(sq, "|", fixed = TRUE)
      ids_seen <- vapply(parts, function(x) x[1], character(1))
      state_map <- setNames(vapply(parts, function(x) if (length(x) >= 2) x[2] else "Active", character(1)), ids_seen)
      elapsed_map <- setNames(vapply(parts, function(x) if (length(x) >= 3) x[3] else "", character(1)), ids_seen)
      name_map <- setNames(vapply(parts, function(x) if (length(x) >= 4) x[4] else "", character(1)), ids_seen)
      matched <- jobs$job_id %in% names(state_map)
      jobs$slurm_state[matched] <- unname(state_map[jobs$job_id[matched]])
      jobs$elapsed[matched] <- unname(elapsed_map[jobs$job_id[matched]])
      jobs$slurm_job_name[matched] <- unname(name_map[jobs$job_id[matched]])
      jobs$slurm_state[!matched & nzchar(jobs$job_id)] <- "Finished or not in queue"
    } else {
      jobs$slurm_state[nzchar(jobs$job_id)] <- "Finished or not in queue"
    }
  }
  if (length(ids) && nzchar(Sys.which("sacct"))) {
    sac <- tryCatch(suppressWarnings(system2("sacct", c("-n", "-P", "-j", paste(ids, collapse = ","), "--format=JobIDRaw,State,Elapsed,Start,End,JobName"), stdout = TRUE, stderr = FALSE, timeout = 10)), error = function(e) character(0))
    sac <- sac[nzchar(sac)]
    if (length(sac)) {
      parts <- strsplit(sac, "|", fixed = TRUE)
      root_rows <- parts[vapply(parts, function(x) length(x) >= 6 && !grepl("\\.", x[1], fixed = TRUE), logical(1))]
      if (length(root_rows)) {
        sac_ids <- vapply(root_rows, function(x) x[1], character(1))
        sac_state <- setNames(vapply(root_rows, function(x) x[2], character(1)), sac_ids)
        sac_elapsed <- setNames(vapply(root_rows, function(x) x[3], character(1)), sac_ids)
        sac_start <- setNames(vapply(root_rows, function(x) x[4], character(1)), sac_ids)
        sac_end <- setNames(vapply(root_rows, function(x) x[5], character(1)), sac_ids)
        sac_name <- setNames(vapply(root_rows, function(x) x[6], character(1)), sac_ids)
        queued <- jobs$slurm_state %in% c("Submitted", "Finished or not in queue")
        matched <- jobs$job_id %in% sac_ids
        jobs$slurm_state[matched & queued] <- unname(sac_state[jobs$job_id[matched & queued]])
        jobs$elapsed[matched] <- ifelse(nzchar(jobs$elapsed[matched]), jobs$elapsed[matched], unname(sac_elapsed[jobs$job_id[matched]]))
        jobs$start_time[matched] <- unname(sac_start[jobs$job_id[matched]])
        jobs$end_time[matched] <- unname(sac_end[jobs$job_id[matched]])
        missing_name <- matched & !nzchar(jobs$slurm_job_name)
        jobs$slurm_job_name[missing_name] <- unname(sac_name[jobs$job_id[missing_name]])
      }
    }
  }
  jobs$input_mode <- vapply(as.character(jobs$output), extract_output_field, character(1), key = "input_mode")
  jobs$sample <- vapply(as.character(jobs$output), extract_output_field, character(1), key = "sample")
  jobs$target <- vapply(as.character(jobs$output), extract_output_field, character(1), key = "target")
  jobs$stdout <- vapply(as.character(jobs$output), extract_output_field, character(1), key = "stdout")
  jobs$stderr <- vapply(as.character(jobs$output), extract_output_field, character(1), key = "stderr")
  app_cancelled <- grepl("cancelled_by_codespringweb:[[:space:]]*true", as.character(jobs$output), ignore.case = TRUE)
  jobs$slurm_state[grepl("^CANCELLED", jobs$slurm_state, ignore.case = TRUE)] <- "CANCELLED"
  jobs$slurm_state[grepl("^COMPLETED", jobs$slurm_state, ignore.case = TRUE)] <- "COMPLETED"
  jobs$slurm_state[app_cancelled] <- "CANCELLED"
  keep <- intersect(c("time", "step", "sample", "job_id", "slurm_state", "elapsed", "start_time", "end_time", "input_mode", "target", "stdout", "stderr"), names(jobs))
  result <- jobs[, keep, drop = FALSE]
  assign(cache_key, list(checked = Sys.time(), file_signature = file_signature, value = result), envir = JOB_HISTORY_CACHE)
  result
}

job_history_display <- function(project) {
  jobs <- job_history(project)
  if (!NROW(jobs)) return(jobs)
  jobs$logs <- apply(jobs, 1, function(row) {
    labels <- character(0)
    if ("stdout" %in% names(jobs) && nzchar(row[["stdout"]] %||% "")) labels <- c(labels, "output")
    if ("stderr" %in% names(jobs) && nzchar(row[["stderr"]] %||% "")) labels <- c(labels, "error")
    if (length(labels)) paste(labels, collapse = " / ") else ""
  })
  drop <- intersect(c("stdout", "stderr"), names(jobs))
  jobs[, setdiff(names(jobs), drop), drop = FALSE]
}

job_history_progress_display <- function(project) {
  jobs <- job_history(project)
  job_history_progress_display_from_jobs(jobs)
}

job_history_progress_display_from_jobs <- function(jobs) {
  if (!NROW(jobs)) return(jobs)
  keep <- intersect(c("time", "step", "sample", "job_id", "slurm_state", "elapsed", "start_time", "end_time"), names(jobs))
  jobs[, keep, drop = FALSE]
}

included_design_table <- function(project) {
  design <- safe_read_table(project$design_matrix_path)
  if (!NROW(design)) return(design)
  if ("include" %in% names(design)) {
    design <- design[vapply(design$include, as_design_bool, logical(1)), , drop = FALSE]
  }
  design
}

project_samples <- function(project) {
  design <- included_design_table(project)
  if (!NROW(design) || !"sample" %in% names(design)) return(character(0))
  unique(as.character(design$sample[nzchar(as.character(design$sample))]))
}

cancel_active_step_jobs <- function(project, step, samples = NULL) {
  if (Sys.which("scancel") == "") {
    return("ERROR: scancel was not found. Job cancellation must be run on the SLURM server.")
  }
  jobs <- job_history(project)
  if (!NROW(jobs) || !"job_id" %in% names(jobs) || !"step" %in% names(jobs) || !"slurm_state" %in% names(jobs)) {
    return(paste("No tracked jobs found for", step, "in this project."))
  }
  active_states <- active_slurm_states()
  hit <- jobs[
    canonical_job_step(jobs$step) == canonical_job_step(step) &
      jobs$slurm_state %in% active_states &
      nzchar(jobs$job_id),
    ,
    drop = FALSE
  ]
  samples <- unique(as.character(samples %||% character(0)))
  samples <- samples[nzchar(samples)]
  if (length(samples) && canonical_job_step(step) %in% canonical_job_step(sample_level_pipeline_steps()) && "sample" %in% names(hit)) {
    hit <- hit[nzchar(hit$sample) & hit$sample %in% samples, , drop = FALSE]
  }
  ids <- unique(as.character(hit$job_id))
  ids <- ids[nzchar(ids)]
  if (!length(ids)) {
    return(paste("No active", step, "jobs were found for this project."))
  }
  out <- tryCatch(system2("scancel", ids, stdout = TRUE, stderr = TRUE), error = function(e) conditionMessage(e))
  for (id in ids) {
    row <- hit[hit$job_id == id, , drop = FALSE]
    row <- if (NROW(row)) tail(row, 1) else data.frame()
    save_job(
      project,
      step,
      c("scancel", id),
      paste(
        c(
          paste("job_id:", id),
          "cancelled_by_codespringweb: true",
          if (NROW(row) && "sample" %in% names(row) && nzchar(row$sample[1] %||% "")) paste("sample:", row$sample[1]),
          if (NROW(row) && "target" %in% names(row) && nzchar(row$target[1] %||% "")) paste("target:", row$target[1]),
          if (NROW(row) && "input_mode" %in% names(row) && nzchar(row$input_mode[1] %||% "")) paste("input_mode:", row$input_mode[1])
        ),
        collapse = "\n"
      )
    )
  }
  msg <- paste0("Requested cancellation of ", length(ids), " active ", step, " job", if (length(ids) == 1) "" else "s", ": ", paste(ids, collapse = ", "))
  if (length(out) && any(nzchar(out))) msg <- paste(msg, paste(out[nzchar(out)], collapse = "\n"), sep = "\n")
  msg
}

active_squeue_project_jobs <- function(project) {
  if (Sys.which("squeue") == "") return(data.frame())
  prefix <- project_job_name_prefix(project)
  if (!nzchar(prefix)) return(data.frame())
  user <- CURRENT_USER
  args <- c("-h", "-o", "%A|%T|%.200j")
  if (nzchar(user)) args <- c(args, "-u", user)
  out <- tryCatch(system2("squeue", args, stdout = TRUE, stderr = FALSE), error = function(e) character(0))
  out <- out[nzchar(out)]
  if (!length(out)) return(data.frame())
  parts <- strsplit(out, "|", fixed = TRUE)
  rows <- lapply(parts, function(x) {
    if (length(x) < 3) return(NULL)
    data.frame(
      job_id = trimws(x[[1]]),
      slurm_state = trimws(x[[2]]),
      slurm_job_name = trimws(paste(x[-c(1, 2)], collapse = "|")),
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  jobs <- do.call(rbind, rows)
  jobs <- jobs[
    startsWith(jobs$slurm_job_name, prefix) &
      jobs$slurm_state %in% active_slurm_states() &
      nzchar(jobs$job_id),
    ,
    drop = FALSE
  ]
  jobs
}

cancel_active_project_jobs <- function(project) {
  jobs <- job_history(project)
  active_states <- active_slurm_states()
  if (NROW(jobs) && "job_id" %in% names(jobs) && "slurm_state" %in% names(jobs)) {
    hit <- jobs[
      jobs$slurm_state %in% active_states &
        nzchar(jobs$job_id),
      ,
      drop = FALSE
    ]
  } else {
    hit <- data.frame()
  }
  squeue_hit <- active_squeue_project_jobs(project)
  ids <- unique(c(as.character(hit$job_id %||% character(0)), as.character(squeue_hit$job_id %||% character(0))))
  ids <- ids[nzchar(ids)]
  if (!length(ids)) {
    return(paste("No active jobs found for", project$label %||% project$name))
  }
  if (Sys.which("scancel") == "") {
    return("ERROR: scancel was not found. Active project jobs could not be cancelled.")
  }
  out <- tryCatch(system2("scancel", ids, stdout = TRUE, stderr = TRUE), error = function(e) conditionMessage(e))
  for (id in ids) {
    rows <- hit[hit$job_id == id, , drop = FALSE]
    row <- if (NROW(rows)) tail(rows, 1) else data.frame()
    save_job(
      project,
      if (NROW(row) && "step" %in% names(row) && nzchar(row$step[1] %||% "")) row$step[1] else "Project",
      c("scancel", id),
      paste(
        c(
          paste("job_id:", id),
          "cancelled_by_codespringweb: true",
          "project_delete_cleanup: true",
          if (NROW(row) && "sample" %in% names(row) && nzchar(row$sample[1] %||% "")) paste("sample:", row$sample[1]),
          if (NROW(row) && "target" %in% names(row) && nzchar(row$target[1] %||% "")) paste("target:", row$target[1]),
          if (NROW(row) && "input_mode" %in% names(row) && nzchar(row$input_mode[1] %||% "")) paste("input_mode:", row$input_mode[1])
        ),
        collapse = "\n"
      )
    )
  }
  msg <- paste0("Requested cancellation of ", length(ids), " active job", if (length(ids) == 1) "" else "s", " for ", project$label %||% project$name, ": ", paste(ids, collapse = ", "))
  if (length(out) && any(nzchar(out))) msg <- paste(msg, paste(out[nzchar(out)], collapse = "\n"), sep = "\n")
  msg
}

deleted_status_from_status <- function(status, slurm_state = "", output_bytes = NA_real_) {
  status <- as.character(status %||% "")
  slurm_state <- as.character(slurm_state %||% "")
  bytes <- suppressWarnings(as.numeric(output_bytes))
  if (!is.finite(bytes)) bytes <- NA_real_
  failed_states <- c("FAILED", "TIMEOUT", "NODE_FAIL", "OUT_OF_MEMORY", "PREEMPTED", "BOOT_FAIL")
  active_states <- c("PENDING", "CONFIGURING", "COMPLETING", "RUNNING", "SUSPENDED", "Submitted")
  completeish_states <- c("COMPLETED", "Finished or not in queue", "No job id", "")
  if (status %in% c("Cancelled") || slurm_state %in% c("CANCELLED", "CANCELLED+", "CA")) return("Cancelled, Deleted")
  if (status %in% c("Completed", "Complete")) return("Completed, Deleted")
  if (slurm_state %in% failed_states) return("Likely failed, Deleted")
  if (status %in% c("Running", "Waiting", "Running, no growth yet") || slurm_state %in% active_states) return("Likely failed, Deleted")
  if (is.finite(bytes) && bytes >= 100 && slurm_state %in% completeish_states) return("Completed, Deleted")
  if (status %in% c("Likely failed", "Possibly incomplete")) return("Likely failed, Deleted")
  "Likely failed, Deleted"
}

deleted_step_records <- function(project) {
  if (!file.exists(JOBS_PATH)) return(data.frame())
  jobs <- tryCatch(utils::read.delim(JOBS_PATH, check.names = FALSE, stringsAsFactors = FALSE, fill = TRUE), error = function(e) data.frame())
  if (!NROW(jobs) || !"project" %in% names(jobs) || !"step" %in% names(jobs) || !"output" %in% names(jobs)) return(data.frame())
  if ("project_id" %in% names(jobs) && any(nzchar(jobs$project_id %||% ""))) {
    jobs <- jobs[jobs$project_id == (project$id %||% ""), , drop = FALSE]
  } else {
    jobs <- jobs[jobs$project == project$name, , drop = FALSE]
  }
  if ("data_dir" %in% names(jobs) && any(nzchar(jobs$data_dir %||% ""))) {
    jobs <- jobs[!nzchar(jobs$data_dir %||% "") | jobs$data_dir == (project$data_dir %||% ""), , drop = FALSE]
  }
  hit <- grepl("data_deleted_by_codespringweb:[[:space:]]*true", as.character(jobs$output), ignore.case = TRUE)
  jobs <- jobs[hit, , drop = FALSE]
  if (!NROW(jobs)) return(data.frame())
  jobs$step[jobs$step == "RSEM optional"] <- "RSEM (optional)"
  jobs$step[jobs$step == "Kallisto optional"] <- "Kallisto (optional)"
  jobs$sample <- vapply(as.character(jobs$output), extract_output_field, character(1), key = "sample")
  jobs$deleted_status <- vapply(as.character(jobs$output), extract_output_field, character(1), key = "deleted_status")
  jobs$previous_status <- vapply(as.character(jobs$output), extract_output_field, character(1), key = "previous_status")
  jobs$previous_slurm_state <- vapply(as.character(jobs$output), extract_output_field, character(1), key = "previous_slurm_state")
  jobs$previous_output_bytes <- suppressWarnings(as.numeric(vapply(as.character(jobs$output), extract_output_field, character(1), key = "previous_output_bytes")))
  jobs$deleted_status[!nzchar(jobs$deleted_status)] <- "Likely failed, Deleted"
  jobs$deleted_status[
    jobs$deleted_status == "Likely failed, Deleted" &
      jobs$previous_status %in% c("Completed", "Complete")
  ] <- "Completed, Deleted"
  jobs$deleted_status[
    jobs$deleted_status == "Likely failed, Deleted" &
      is.finite(jobs$previous_output_bytes) &
      jobs$previous_output_bytes >= 100 &
      !jobs$previous_status %in% c("Cancelled", "Running", "Waiting", "Running, no growth yet") &
      !jobs$previous_slurm_state %in% c("FAILED", "TIMEOUT", "NODE_FAIL", "OUT_OF_MEMORY", "PREEMPTED", "BOOT_FAIL", "CANCELLED", "CANCELLED+", "CA", "PENDING", "CONFIGURING", "COMPLETING", "RUNNING", "SUSPENDED", "Submitted")
  ] <- "Completed, Deleted"
  jobs$deleted_status[
    jobs$deleted_status == "Likely failed, Deleted" &
      jobs$previous_status %in% c("Cancelled")
  ] <- "Cancelled, Deleted"
  jobs$deleted_at <- jobs$time %||% ""
  jobs[, intersect(c("time", "step", "sample", "deleted_status", "previous_status", "previous_slurm_state", "previous_output_bytes", "deleted_at"), names(jobs)), drop = FALSE]
}

latest_deleted_status_from_records <- function(rec, step, sample = "") {
  if (!NROW(rec) || !"step" %in% names(rec) || !"deleted_status" %in% names(rec)) return("")
  rec <- rec[canonical_job_step(rec$step) == canonical_job_step(step), , drop = FALSE]
  if (nzchar(sample %||% "") && "sample" %in% names(rec)) {
    sample_rec <- rec[nzchar(rec$sample) & rec$sample == sample, , drop = FALSE]
    if (NROW(sample_rec)) rec <- sample_rec
  }
  if (!NROW(rec)) return("")
  tail(as.character(rec$deleted_status), 1)
}

latest_deleted_record_from_records <- function(rec, step, sample = "") {
  if (!NROW(rec) || !"step" %in% names(rec)) return(data.frame())
  rec <- rec[canonical_job_step(rec$step) == canonical_job_step(step), , drop = FALSE]
  if (nzchar(sample %||% "") && "sample" %in% names(rec)) {
    sample_rec <- rec[nzchar(rec$sample) & rec$sample == sample, , drop = FALSE]
    if (NROW(sample_rec)) rec <- sample_rec
  }
  if (NROW(rec)) tail(rec, 1) else data.frame()
}

latest_deleted_status <- function(project, step, sample = "") {
  latest_deleted_status_from_records(deleted_step_records(project), step, sample)
}

job_error_signal <- function(jobs, step, sample = "") {
  if (!NROW(jobs) || !"step" %in% names(jobs)) return(FALSE)
  hit <- jobs[canonical_job_step(jobs$step) == canonical_job_step(step), , drop = FALSE]
  if (nzchar(sample %||% "") && "sample" %in% names(hit)) {
    sample_hit <- hit[nzchar(hit$sample) & hit$sample == sample, , drop = FALSE]
    if (NROW(sample_hit)) hit <- sample_hit
  }
  if (!NROW(hit)) return(FALSE)
  hit <- tail(hit, 1)
  failed_states <- c("FAILED", "TIMEOUT", "NODE_FAIL", "OUT_OF_MEMORY", "PREEMPTED", "BOOT_FAIL")
  if ("slurm_state" %in% names(hit) && any(hit$slurm_state %in% failed_states)) return(TRUE)
  if ("stderr" %in% names(hit)) {
    err <- as.character(hit$stderr)
    err <- err[nzchar(err) & file.exists(err)]
    if (length(err) && any(file.info(err)$size > 0, na.rm = TRUE)) return(TRUE)
  }
  FALSE
}

cutrun_macs_fatal_error_signal <- function(project, jobs, step, sample = "") {
  if (!is_cutrun_project(project) || !identical(canonical_job_step(step), "MACS2 (optional)")) return(FALSE)

  fatal_pattern <- paste(
    c(
      "Traceback \\(most recent call last\\):",
      "Exception ignored in:",
      "KeyError:",
      "ValueError:",
      "TypeError:",
      "OSError:",
      "No space left on device",
      "Segmentation fault",
      "(^|[[:space:]])Killed([[:space:]]|$)"
    ),
    collapse = "|"
  )
  has_fatal_text <- function(path) {
    if (!nzchar(path %||% "") || !file.exists(path) || file_size_for(path) <= 0) return(FALSE)
    lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character(0))
    length(lines) > 0 && any(grepl(fatal_pattern, lines, perl = TRUE))
  }

  # New CUT&RUN MACS2 runs overwrite this per-sample log, so an old appended
  # SLURM stderr cannot keep a successfully repaired sample marked as failed.
  run_log <- file.path(project$data_dir, "macs2", sample, paste0(sample, "_macs2.log"))
  if (file.exists(run_log)) return(has_fatal_text(run_log))

  if (!NROW(jobs) || !"step" %in% names(jobs)) return(FALSE)
  hit <- jobs[canonical_job_step(jobs$step) == canonical_job_step(step), , drop = FALSE]
  if (nzchar(sample %||% "") && "sample" %in% names(hit)) {
    sample_hit <- hit[nzchar(hit$sample) & hit$sample == sample, , drop = FALSE]
    if (NROW(sample_hit)) hit <- sample_hit
  }
  if (!NROW(hit) || !"stderr" %in% names(hit)) return(FALSE)
  has_fatal_text(as.character(tail(hit, 1)$stderr[1] %||% ""))
}

sample_step_data_paths <- function(project, step, samples) {
  data_dir <- project$data_dir
  samples <- unique(as.character(samples %||% character(0)))
  samples <- samples[nzchar(samples)]
  if (!length(samples)) return(character(0))
  counts_dir <- file.path(data_dir, "counts")
  sample_pattern <- function(sample, suffix = ".*") paste0("^", gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", sample, perl = TRUE), suffix)
  count_matches <- function(pattern) {
    if (!dir.exists(counts_dir)) return(character(0))
    list.files(counts_dir, pattern = pattern, full.names = TRUE)
  }
  out <- unlist(lapply(samples, function(sample) {
    switch(canonical_job_step(step),
      "FastQC" = {
        dirs <- c(file.path(data_dir, "fastqc"), file.path(data_dir, "fastqc_cutadapt"))
        unlist(lapply(dirs, function(dir) {
          if (!dir.exists(dir)) return(character(0))
          list.files(dir, pattern = sample_pattern(sample, ".*_fastqc\\.(html|zip)$"), full.names = TRUE, ignore.case = TRUE)
        }), use.names = FALSE)
      },
      "Cutadapt" = {
        dir <- file.path(data_dir, "cutadapt")
        if (!dir.exists(dir)) character(0) else list.files(dir, pattern = sample_pattern(sample, ".*\\.(fastq|fq)(\\.gz)?$"), full.names = TRUE, ignore.case = TRUE)
      },
      "STAR" = file.path(data_dir, "star", sample),
      "Bowtie2" = file.path(data_dir, "bowtie2", sample),
      "SEACR" = {
        root <- file.path(data_dir, "seacr")
        dirs <- if (dir.exists(root)) list.dirs(root, recursive = TRUE, full.names = TRUE) else character(0)
        hits <- dirs[basename(dirs) == sample]
        if (length(hits)) hits else file.path(root, sample)
      },
      "MACS2 (optional)" = file.path(data_dir, "macs2", sample),
      "MACS2 Peaks" = file.path(data_dir, "macs2", sample),
      "featureCounts" = c(
        file.path(data_dir, "featurecounts", sample),
        file.path(counts_dir, "count_matrix.txt"),
        file.path(counts_dir, "count_matrix_gene_name_aggregated.txt"),
        file.path(counts_dir, "featurecounts_summary.txt")
      ),
      "RSEM (optional)" = c(file.path(data_dir, "rsem", sample), count_matches("^rsem_.*")),
      "Kallisto (optional)" = c(file.path(data_dir, "kallisto", sample), count_matches("^kallisto_.*")),
      character(0)
    )
  }), use.names = FALSE)
  unique(out[nzchar(out)])
}

step_data_paths <- function(project, step, samples = NULL) {
  if (canonical_job_step(step) %in% canonical_job_step(sample_level_pipeline_steps())) {
    sample_paths <- sample_step_data_paths(project, step, samples)
    if (length(sample_paths)) return(sample_paths)
  }
  data_dir <- project$data_dir
  counts_dir <- file.path(data_dir, "counts")
  count_matches <- function(pattern) {
    if (!dir.exists(counts_dir)) return(character(0))
    list.files(counts_dir, pattern = pattern, full.names = TRUE)
  }
  switch(canonical_job_step(step),
    "FastQC" = c(file.path(data_dir, "fastqc"), file.path(data_dir, "fastqc_cutadapt")),
    "Cutadapt" = file.path(data_dir, "cutadapt"),
    "STAR" = file.path(data_dir, "star"),
    "Bowtie2" = file.path(data_dir, "bowtie2"),
    "SEACR" = file.path(data_dir, "seacr"),
    "Peak QC" = file.path(data_dir, "cutrun_peak_qc"),
    "Differential Peaks" = file.path(data_dir, if (is_atac_project(project)) "diffbind" else "cutrun_diffbind"),
    "MACS2 Peaks" = file.path(data_dir, "macs2"),
    "MACS2 (optional)" = file.path(data_dir, "macs2"),
    "featureCounts" = c(
      file.path(data_dir, "featurecounts"),
      file.path(counts_dir, "count_matrix.txt"),
      file.path(counts_dir, "featurecounts_summary.txt")
    ),
    "DESeq2" = file.path(data_dir, "deseq2"),
    "GSEA" = file.path(data_dir, "gseapy"),
    "RSEM (optional)" = c(file.path(data_dir, "rsem"), count_matches("^rsem_.*")),
    "Kallisto (optional)" = c(file.path(data_dir, "kallisto"), count_matches("^kallisto_.*")),
    character(0)
  )
}

delete_step_data <- function(project, step, samples = NULL) {
  data_dir <- project$data_dir %||% ""
  if (!nzchar(data_dir) || !dir.exists(data_dir)) {
    return("Project data folder does not exist.")
  }
  data_root <- normalizePath(data_dir, winslash = "/", mustWork = TRUE)
  samples <- unique(as.character(samples %||% character(0)))
  samples <- samples[nzchar(samples)]
  paths <- unique(step_data_paths(project, step, samples))
  paths <- paths[nzchar(paths) & file.exists(paths)]
  if (!length(paths)) return(paste("No existing data outputs found for", step, "in this project."))
  normalized <- normalizePath(paths, winslash = "/", mustWork = TRUE)
  inside <- startsWith(normalized, paste0(sub("/+$", "", data_root), "/"))
  if (any(!inside)) {
    blocked <- paste(normalized[!inside], collapse = "\n")
    return(paste("ERROR: Refusing to delete paths outside the project data folder:", blocked, sep = "\n"))
  }
  jobs <- job_history(project)
  active_states <- active_job_state_map_from_jobs(jobs)
  progress <- tryCatch(sample_progress(project, active_states, data.frame(), jobs = jobs)$table, error = function(e) data.frame())
  sample_steps <- sample_level_pipeline_steps()
  if (canonical_job_step(step) %in% canonical_job_step(sample_steps) && NROW(progress)) {
    hit <- progress[canonical_job_step(progress$step) == canonical_job_step(step), , drop = FALSE]
    if (length(samples) && "sample" %in% names(hit)) hit <- hit[hit$sample %in% samples, , drop = FALSE]
    if (NROW(hit)) {
      for (i in seq_len(NROW(hit))) {
        deleted_status <- deleted_status_from_status(hit$status[i], hit$slurm_state[i], hit$output_bytes[i])
        if (
          job_error_signal(jobs, step, hit$sample[i]) &&
            !deleted_status %in% c("Cancelled, Deleted", "Completed, Deleted")
        ) {
          deleted_status <- "Likely failed, Deleted"
        }
        save_job(
          project,
          step,
          c("delete_step_data", step, hit$sample[i]),
          paste(
            c(
              "data_deleted_by_codespringweb: true",
              paste("sample:", hit$sample[i]),
              paste("deleted_status:", deleted_status),
              paste("previous_status:", hit$status[i]),
              paste("previous_slurm_state:", hit$slurm_state[i] %||% ""),
              paste("previous_output_bytes:", hit$output_bytes[i] %||% 0)
            ),
            collapse = "\n"
          )
        )
      }
    }
  } else {
    step_status <- project_status(project, jobs = jobs, progress = progress, active_states = active_states)
    step_row <- step_status[canonical_job_step(step_status$step) == canonical_job_step(step), , drop = FALSE]
    previous_status <- if (NROW(step_row)) step_row$status[1] else ""
    deleted_status <- deleted_status_from_status(previous_status)
    if (
      job_error_signal(jobs, step) &&
        !deleted_status %in% c("Cancelled, Deleted", "Completed, Deleted")
    ) {
      deleted_status <- "Likely failed, Deleted"
    }
    save_job(
      project,
      step,
      c("delete_step_data", step),
      paste(
        c(
          "data_deleted_by_codespringweb: true",
          paste("deleted_status:", deleted_status),
          paste("previous_status:", previous_status)
        ),
        collapse = "\n"
      )
    )
  }
  ok <- unlink(normalized, recursive = TRUE, force = TRUE)
  still_exists <- normalized[file.exists(normalized)]
  deleted <- setdiff(normalized, still_exists)
  msg <- paste0("Deleted ", length(deleted), " ", step, " data path", if (length(deleted) == 1) "" else "s", ".")
  if (length(deleted)) msg <- paste(msg, paste(deleted, collapse = "\n"), sep = "\n")
  if (length(still_exists)) msg <- paste(msg, "Still exists after delete:", paste(still_exists, collapse = "\n"), sep = "\n")
  if (!is.null(ok) && ok != 0) msg <- paste(msg, "unlink returned a non-zero status.", sep = "\n")
  msg
}

job_filter_choices_from_jobs <- function(jobs) {
  steps <- if (NROW(jobs) && "step" %in% names(jobs)) unique(as.character(jobs$step)) else character(0)
  steps <- steps[!is.na(steps) & nzchar(steps)]
  ordered <- pipeline_order()[pipeline_order() %in% steps]
  extra <- sort(setdiff(steps, ordered))
  c("All", ordered, extra)
}

canonical_job_step <- function(x) {
  x <- trimws(as.character(x %||% ""))
  if (!length(x)) return(character(0))
  x_norm <- gsub("[^a-z0-9]+", "", tolower(gsub("\\(optional\\)", "", x)))
  map <- c(
    fastqc = "FastQC",
    cutadapt = "Cutadapt",
    star = "STAR",
    bowtie2 = "Bowtie2",
    seacr = "SEACR",
    macs2 = "MACS2 (optional)",
    diffbind = "Differential Peaks",
    differentialpeaks = "Differential Peaks",
    macs2peaks = "MACS2 Peaks",
    featurecounts = "featureCounts",
    deseq2 = "DESeq2",
    gsea = "GSEA",
    gseapy = "GSEA",
    rsem = "RSEM (optional)",
    kallisto = "Kallisto (optional)"
  )
  out <- unname(map[x_norm])
  out[is.na(out)] <- x[is.na(out)]
  out
}

filter_jobs_by_tool <- function(jobs, tool) {
  tool <- trimws(as.character(tool %||% "All"))
  if (!NROW(jobs) || identical(tool, "All") || !nzchar(tool) || !"step" %in% names(jobs)) return(jobs)
  jobs[canonical_job_step(jobs$step) == canonical_job_step(tool), , drop = FALSE]
}

empty_job_filter_message <- function(jobs, original_jobs, tool) {
  tool <- trimws(as.character(tool %||% "All"))
  if (NROW(jobs) || identical(tool, "All") || !NROW(original_jobs)) return(jobs)
  cols <- names(original_jobs)
  if (!length(cols)) cols <- c("message")
  out <- as.data.frame(setNames(replicate(length(cols), "", simplify = FALSE), cols), stringsAsFactors = FALSE)
  out[[cols[[1]]]] <- paste("No submitted jobs found for", tool, "in this project.")
  out
}

elapsed_to_seconds <- function(x) {
  x <- trimws(as.character(x %||% ""))
  if (!nzchar(x) || identical(x, "NA")) return(0)
  days <- 0
  if (grepl("-", x, fixed = TRUE)) {
    parts <- strsplit(x, "-", fixed = TRUE)[[1]]
    days <- suppressWarnings(as.numeric(parts[1]))
    if (is.na(days)) days <- 0
    x <- parts[length(parts)]
  }
  bits <- suppressWarnings(as.numeric(strsplit(x, ":", fixed = TRUE)[[1]]))
  bits <- bits[!is.na(bits)]
  secs <- switch(
    as.character(length(bits)),
    "3" = bits[1] * 3600 + bits[2] * 60 + bits[3],
    "2" = bits[1] * 60 + bits[2],
    "1" = bits[1],
    0
  )
  total <- days * 86400 + secs
  if (is.na(total)) total <- 0
  as.integer(total)
}

format_elapsed_seconds <- function(seconds) {
  seconds <- max(0, as.integer(seconds %||% 0))
  days <- seconds %/% 86400
  rest <- seconds %% 86400
  hours <- rest %/% 3600
  minutes <- (rest %% 3600) %/% 60
  secs <- rest %% 60
  if (days > 0) sprintf("%s-%02d:%02d:%02d", days, hours, minutes, secs) else sprintf("%02d:%02d:%02d", hours, minutes, secs)
}

active_slurm_states <- function() {
  c("PENDING", "CONFIGURING", "COMPLETING", "RUNNING", "SUSPENDED", "Submitted")
}

escape_job_table <- function(df) {
  if (!NROW(df)) return(df)
  out <- df
  for (nm in names(out)) {
    if (is.character(out[[nm]])) out[[nm]] <- htmltools::htmlEscape(out[[nm]])
  }
  out
}

prepare_job_table_for_display <- function(jobs) {
  if (!NROW(jobs)) return(jobs)
  out <- escape_job_table(jobs)
  if (!all(c("elapsed", "slurm_state") %in% names(jobs))) return(out)
  active <- as.character(jobs$slurm_state) %in% active_slurm_states()
  if (!any(active)) return(out)
  captured <- as.integer(Sys.time())
  elapsed <- as.character(jobs$elapsed)
  elapsed[!nzchar(elapsed)] <- "00:00:00"
  base <- vapply(elapsed, elapsed_to_seconds, integer(1))
  out$elapsed[active] <- sprintf(
    '<span class="elapsed-live" data-base="%s" data-captured="%s">%s</span>',
    base[active],
    captured,
    vapply(base[active], format_elapsed_seconds, character(1))
  )
  out
}

run_manifest_path <- function(project) {
  file.path(dirname(project$data_dir), "log", "codespringweb_run_manifest.tsv")
}

read_run_manifest <- function(project) {
  path <- run_manifest_path(project)
  if (!file.exists(path)) return(data.frame())
  tryCatch(utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE, fill = TRUE), error = function(e) data.frame())
}

project_methods_summary <- function(project) {
  data.frame(
    Field = c(
      "Project",
      "Analysis",
      "Species",
      "Genome/reference version",
      "Reference key",
      "Read type",
      "Design matrix"
    ),
    Value = c(
      project$label,
      project$analysis,
      genome_species(project),
      project_reference_label(project),
      genome_reference_key(project),
      if (isTRUE(project$paired_end)) "Paired-end" else "Single-end",
      if (file.exists(project$design_matrix_path)) "Provided or created in app" else "Not created yet"
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

module_versions_from_scripts <- function(files, fallback = "listed in CodeSpringLab script") {
  files <- files[file.exists(files)]
  if (!length(files)) return(fallback)
  lines <- unlist(lapply(files, function(path) tryCatch(readLines(path, warn = FALSE), error = function(e) character(0))), use.names = FALSE)
  module_lines <- grep("^\\s*module\\s+load\\s+", lines, value = TRUE)
  modules <- sub("^\\s*module\\s+load\\s+", "", module_lines)
  modules <- trimws(sub("\\s*(#.*)?$", "", modules))
  modules <- unlist(strsplit(modules, "\\s+"), use.names = FALSE)
  modules <- modules[nzchar(modules) & !modules %in% c("EBModules")]
  modules <- unique(modules)
  if (!length(modules)) fallback else paste(modules, collapse = "; ")
}

tool_reference_summary <- function(project) {
  fastqc_modules <- module_versions_from_scripts(file.path(SCRIPTS_DIR, "FastQC", "fastqc.sh"))
  cutadapt_modules <- module_versions_from_scripts(c(
    file.path(SCRIPTS_DIR, "cutadapt_PE", "cutadapt_PE.sh"),
    file.path(SCRIPTS_DIR, "cutadapt_SE", "cutadapt_SE.sh")
  ))
  star_modules <- module_versions_from_scripts(c(
    file.path(SCRIPTS_DIR, "STAR", "star_PE.sh"),
    file.path(SCRIPTS_DIR, "STAR", "star_SE.sh")
  ))
  featurecounts_modules <- module_versions_from_scripts(c(
    file.path(SCRIPTS_DIR, "featureCounts", "featurecounts_PE.sh"),
    file.path(SCRIPTS_DIR, "featureCounts", "featurecounts_SE.sh")
  ))
  deseq_modules <- module_versions_from_scripts(file.path(SCRIPTS_DIR, "DESeq2", "deseq2.sh"), "R module in CodeSpringLab DESeq2 script")
  rsem_modules <- module_versions_from_scripts(c(
    file.path(SCRIPTS_DIR, "RSEM", "RSEM_PE.sh"),
    file.path(SCRIPTS_DIR, "RSEM", "RSEM_SE.sh")
  ))
  kallisto_modules <- module_versions_from_scripts(c(
    file.path(SCRIPTS_DIR, "Kallisto", "kallisto_PE.sh"),
    file.path(SCRIPTS_DIR, "Kallisto", "kallisto_SE.sh")
  ))
  if (is_atac_project(project)) {
    ref <- atac_reference_resources(project)
    bowtie2_modules <- module_versions_from_scripts(file.path(SCRIPTS_DIR, "bowtie2", "bowtie2_PE.sh"))
    macs2_modules <- module_versions_from_scripts(file.path(SCRIPTS_DIR, "MACS2", "macs2_PE.sh"))
    diffbind_modules <- module_versions_from_scripts(file.path(SCRIPTS_DIR, "DiffBind", "diffbind.sh"), "R/DiffBind library used by CodeSpringLab")
    rows <- list(
      c("Reference", "ATAC-seq genome reference", ref$label, paste0(genome_species(project), " / ", genome_reference_key(project)), "Bowtie2, MACS2, Homer, DiffBind", paste("Bowtie2 index:", ref$bowtie2_index, "| Chrom sizes:", ref$chrom_sizes)),
      c("Tool", "FastQC", fastqc_modules, "Read quality control", "Raw or trimmed FASTQ", "Input mode is selected per run; completed samples are skipped on rerun."),
      c("Tool", "cutadapt", cutadapt_modules, "Nextera adapter trimming", "Raw paired-end FASTQ", "The web workflow defaults to the established CodeSpringLab Nextera R1/R2 adapters and a 20 bp minimum length."),
      c("Tool", "Bowtie2", bowtie2_modules, "Paired-end ATAC-seq alignment and track generation", ref$label, "Uses the GRCm39/GENCODE M39 Bowtie2 index, removes PCR duplicates, and creates a direct CPM-normalized bigWig scaled to one million mapped reads. The web app detects missing, empty, or stale post-alignment artifacts and can repair Picard/BED/insert-size/bigWig outputs without repeating alignment."),
      c("Tool", "MACS2", macs2_modules, "Shifted ATAC-seq peak calling", "Duplicate-removed Bowtie2 BED", "Uses --nomodel --shift -100 --extsize 200 --call-summits; the q-value is selected in Run Pipeline. Homer annotation and a TSS heatmap are generated by the established CodeSpringLab runner."),
      c("Tool", "DiffBind/DESeq2", diffbind_modules, "Consensus peaks and differential accessibility", "MACS2 narrowPeak files and duplicate-removed BAM files", "Requires at least two replicates in each selected condition, applies the bundled mm39 blacklist for mouse projects, writes a BED containing DiffBind statistics, and annotates differential peaks with Homer.")
    )
    out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
    colnames(out) <- c("Type", "Name", "Version/reference", "Used for", "Input/reference", "Parameters/settings")
    return(out)
  }
  if (is_cutrun_project(project)) {
    bowtie2_modules <- module_versions_from_scripts(c(
      file.path(SCRIPTS_DIR, "bowtie2", "bowtie2_cutrun_PE.sh"),
      file.path(SCRIPTS_DIR, "bowtie2", "bowtie2_cutrun_SE.sh")
    ))
    seacr_modules <- module_versions_from_scripts(file.path(SCRIPTS_DIR, "SEACR", "seacr_cutrun.sh"), "SEACR_1.3.sh local script; BEDTools/R modules")
    diffbind_modules <- module_versions_from_scripts(file.path(SCRIPTS_DIR, "DiffBind", "cutrun_diffbind.sh"), "R/DiffBind library used by CodeSpringLab")
    macs2_modules <- module_versions_from_scripts(file.path(SCRIPTS_DIR, "MACS2", "macs2_cutrun_PE.sh"), "MACS caller module listed in CodeSpringLab script")
    ref <- cutrun_reference_resources(project)
    rows <- list(
      c("Reference", "CUT&RUN genome reference", ref$label, paste0(genome_species(project), " / ", genome_reference_key(project)), "Bowtie2, SEACR, MACS2", paste("Bowtie2 index:", ref$bowtie2_index, "| Chrom sizes:", ref$chrom_sizes)),
      c("Tool", "FastQC", fastqc_modules, "Read quality control", "Raw or trimmed FASTQ", "Input mode selected per run; reruns skip completed samples and submit failed/deleted samples only."),
      c("Tool", "cutadapt", cutadapt_modules, "Adapter trimming", "Raw FASTQ", "Adapter presets or custom adapters from Run Pipeline; minimum length from Run Pipeline."),
      c("Tool", "Bowtie2", bowtie2_modules, "CUT&RUN fragment alignment and normalization", ref$label, "Defaults: MAPQ 30, max fragment 1000 bp, E. coli K-12 MG1655 spike-in normalization, keep target duplicates, deduplicate IgG/input controls, and remove mitochondrial fragments from peak-calling bedGraphs. CPM and no normalization remain available as explicit alternatives."),
      c("Tool", "SEACR", seacr_modules, "Recommended sparse CUT&RUN peak calling and FRiP QC", "Bowtie2 normalized fragment bedGraphs and optional IgG/input control bedGraph", "Stringent is the primary default; relaxed is available globally or per sample through seacr_stringency in design_matrix.txt. target_class records TF/focal versus narrow- or broad-histone biology but does not silently change SEACR stringency. non is used for spike-in-normalized tracks and norm for CPM/raw tracks. Controls are selected from control_sample or inferred IgG/input rows."),
      c("Tool", "Peak QC", "BEDTools module listed in CodeSpringLab script", "Consensus peaks, peak count matrix, and FRiP summary", "SEACR peak BED files and Bowtie2 BAM files", "Merges SEACR peaks across samples and counts fragments over consensus peaks."),
      c("Tool", "DiffBind/DESeq2", diffbind_modules, "Mark-specific CUT&RUN differential binding", "SEACR peaks, target BAMs, and automatically resolved E. coli spike-in BAMs", "Analyzes each cell type/mark separately and compares every non-reference condition with the selected reference. At least two biological replicates are required in each condition. The ATAC-like default admits a peak supported by one or more replicates; stricter consensus support is available under Advanced. Native SEACR widths are preserved with summits=FALSE; IgG BAMs are not subtracted again."),
      c("Tool", "MACS2", macs2_modules, "Optional peak calling comparison", "Bowtie2 BAM and optional IgG/input control BAM", "Default q-value 0.01. Auto mode uses broad peaks for histone_broad targets and narrow peaks for histone_narrow or tf_or_other targets; a run-wide narrow or broad override remains available.")
    )
    out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
    colnames(out) <- c("Type", "Name", "Version/reference", "Used for", "Input/reference", "Parameters/settings")
    return(out)
  }
  rows <- list(
    c("Reference", "Genome annotation", gencode_label(project), paste0(genome_species(project), " / ", genome_reference_key(project)), "STAR, featureCounts, DESeq2, RSEM, Kallisto", paste("STAR index, GTF, RSEM index, Kallisto index, and strand BED from", genome_reference_key(project))),
    c("Tool", "FastQC", fastqc_modules, "Read quality control", "Raw or trimmed FASTQ", "Input mode selected per run; reruns skip completed samples and submit failed/deleted samples only."),
    c("Tool", "cutadapt", cutadapt_modules, "Adapter trimming", "Raw FASTQ", "Adapter presets or custom adapters from Run Pipeline; minimum length from Run Pipeline."),
    c("Tool", "STAR", star_modules, "Spliced alignment", gencode_label(project), "Uses selected genome STAR index; raw or trimmed FASTQ selected per run."),
    c("Tool", "featureCounts / Subread", featurecounts_modules, "Gene-level counting", gencode_label(project), "Feature attribute defaults to gene_name; count_matrix.txt is built after sample jobs finish."),
    c("Tool", "DESeq2", deseq_modules, "Differential expression", "featureCounts count_matrix.txt", "Comparison column, reference, and comparison are selected per run; redundant genes are removed by CodeSpringLab."),
    c("Tool", "GSEApy", "BSR; Python/3.7.4-GCCcore-8.3.0; gseapy 1.1.4 on bamdev1", "Pathway analysis", "Selected Enrichr/MSigDB-style gene set database", "Gene-set database selected per run; each database writes to its own GSEA output folder."),
    c("Tool", "RSEM", rsem_modules, "Optional gene/transcript quantification", gencode_label(project), "Feature attribute selected per run; matrices are built after sample jobs finish."),
    c("Tool", "Kallisto", kallisto_modules, "Optional transcript abundance quantification", gencode_label(project), "Uses selected transcript index; raw or trimmed FASTQ selected per run; matrices are built after sample jobs finish."),
    c("Tool", "RSeQC", "RSeQC module listed in featureCounts/RSEM scripts; strand BED generated with reference", "Optional strand/QC support", gencode_label(project), "Uses bundled/generated annotation_forStrandDetect_geneID.bed for the selected reference.")
  )
  out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  colnames(out) <- c("Type", "Name", "Version/reference", "Used for", "Input/reference", "Parameters/settings")
  out
}

methods_sentence_for_step <- function(step, manifest_rows, project) {
  refs <- if ("reference" %in% names(manifest_rows)) unique(manifest_rows$reference[nzchar(manifest_rows$reference)]) else character(0)
  modes <- if ("input_mode" %in% names(manifest_rows)) unique(manifest_rows$input_mode[nzchar(manifest_rows$input_mode)]) else character(0)
  ref_text <- if (length(refs)) paste0(" Reference: ", paste(refs, collapse = "; "), ".") else ""
  mode_text <- if (length(modes)) paste0(" Inputs/settings: ", paste(modes, collapse = "; "), ".") else ""
  switch(
    step,
    "FastQC" = paste0("Read quality control was performed with FastQC.", mode_text),
    "Cutadapt" = paste0("Adapter trimming was performed with cutadapt.", mode_text),
    "Bowtie2" = paste0(if (is_atac_project(project)) "ATAC-seq fragments were aligned with Bowtie2 and duplicate-removed CPM bigWigs scaled to one million mapped reads were generated." else "CUT&RUN fragments were aligned with Bowtie2.", ref_text, mode_text),
    "SEACR" = paste0("CUT&RUN peaks were called with SEACR from fragment bedGraphs using the selected global default and any per-sample seacr_stringency overrides. Target class was recorded separately and did not automatically change stringency.", ref_text, mode_text),
    "Differential Peaks" = paste0(if (is_atac_project(project)) "Differential accessibility was tested on a MACS2 consensus peakset using DiffBind with DESeq2." else "Differential CUT&RUN binding was tested separately by cell type and mark using DiffBind with DESeq2. Native SEACR intervals were preserved and spike-in BAMs were reused automatically when available.", ref_text, mode_text),
    "MACS2 Peaks" = paste0("ATAC-seq peaks were called with MACS2 using Tn5-aware shifting and annotated with Homer.", ref_text, mode_text),
    "MACS2 (optional)" = paste0("Optional CUT&RUN peaks were called with MACS2.", ref_text, mode_text),
    "STAR" = paste0("Reads were aligned with STAR using ", gencode_label(project), ".", ref_text, mode_text),
    "featureCounts" = paste0("Gene-level counts were quantified with featureCounts using the selected GTF annotation.", ref_text, mode_text),
    "DESeq2" = paste0("Differential expression analysis was performed with DESeq2.", mode_text),
    "GSEA" = paste0("Gene set enrichment analysis was performed with CodeSpringLab GSEApy using signal-to-noise ranking and gene-set permutations.", ref_text, mode_text),
    "RSEM (optional)" = paste0("Optional transcript/gene quantification was performed with RSEM.", ref_text, mode_text),
    "Kallisto (optional)" = paste0("Optional transcript abundance quantification was performed with Kallisto.", ref_text, mode_text),
    paste0(step, " was run.", ref_text, mode_text)
  )
}

project_methods_text <- function(project) {
  manifest <- read_run_manifest(project)
  status <- project_status(project)
  completed <- status$step[status$status == "Complete"]
  run_steps <- if (NROW(manifest) && "step" %in% names(manifest)) unique(as.character(manifest$step)) else character(0)
  steps <- pipeline_order(project)[pipeline_order(project) %in% unique(c(completed, run_steps))]
  if (!length(steps)) steps <- completed
  lines <- c(
    paste0("Project: ", project$label),
    paste0("Analysis: ", project$analysis),
    paste0("Reference genome: ", project_reference_label(project), " (", genome_reference_key(project), ")."),
    paste0("Species: ", genome_species(project), "."),
    paste0("Read type: ", if (isTRUE(project$paired_end)) "paired-end" else "single-end", "."),
    ""
  )
  if (length(steps)) {
    lines <- c(lines, "Methods by completed/submitted step:")
    for (step in steps) {
      rows <- if (NROW(manifest) && "step" %in% names(manifest)) manifest[manifest$step == step, , drop = FALSE] else data.frame()
      lines <- c(lines, paste0("- ", methods_sentence_for_step(step, rows, project)))
    }
  } else {
    lines <- c(lines, "No submitted or completed pipeline steps were detected for this project yet.")
  }
  paste(lines, collapse = "\n")
}

last_job_modes <- function(project) {
  jobs <- job_history(project)
  last_job_modes_from_jobs(jobs)
}

last_job_modes_from_jobs <- function(jobs) {
  if (!NROW(jobs) || !"input_mode" %in% names(jobs)) return(setNames(character(0), character(0)))
  jobs <- jobs[nzchar(jobs$input_mode), , drop = FALSE]
  if (!NROW(jobs)) return(setNames(character(0), character(0)))
  out <- tapply(jobs$input_mode, jobs$step, function(x) tail(x, 1))
  unlist(out)
}

pretty_tool_name <- function(tool) {
  tool <- as.character(tool %||% "")
  key <- tolower(gsub("[^A-Za-z0-9]+", "_", tool))
  known <- c(
    fastqc = "FastQC",
    fastqc_cutadapt = "FastQC",
    cutadapt = "Cutadapt",
    star = "STAR",
    kallisto = "Kallisto",
    featurecounts = "featureCounts",
    deseq2 = "DESeq2",
    gseapy = "GSEA",
    rsem = "RSEM",
    copyfastq = "Copy FASTQ",
    rnaseq_shiny = "RNA-seq viewer"
  )
  if (key %in% names(known)) return(unname(known[[key]]))
  clean <- gsub("_", " ", key)
  paste(tools::toTitleCase(clean))
}

log_label_from_path <- function(path, fallback = "Log") {
  base <- basename(path %||% "")
  m_submit <- regexec("^submit_([^.]*)\\.txt$", base)
  submit_hit <- regmatches(base, m_submit)[[1]]
  if (length(submit_hit) == 2) {
    tool <- sub("_[0-9]{8}_[0-9]{6}$", "", submit_hit[2])
    return(paste(pretty_tool_name(tool), "submit", base))
  }
  m <- regexec("^(output|error)_([^.]*)\\.txt$", base)
  hit <- regmatches(base, m)[[1]]
  if (length(hit) == 3) {
    log_type <- if (identical(hit[2], "output")) "output" else "error"
    tool <- sub("_[0-9]{8}_[0-9]{6}$", "", hit[3])
    return(paste(pretty_tool_name(tool), log_type, base))
  }
  paste(fallback, base)
}

canonical_log_tool <- function(step, fallback = "Job") {
  step <- as.character(step %||% fallback)
  key <- tolower(gsub("[^a-z0-9]+", "", step))
  if (grepl("fastqc", key)) return("FastQC")
  if (grepl("cutadapt", key)) return("Cutadapt")
  if (grepl("star", key)) return("STAR")
  if (grepl("featurecounts", key)) return("featureCounts")
  if (grepl("deseq2", key)) return("DESeq2")
  if (grepl("gsea", key)) return("GSEA")
  if (grepl("rsem", key)) return("RSEM")
  if (grepl("kallisto", key)) return("Kallisto")
  pretty_tool_name(step)
}

log_scope_from_job <- function(job_row) {
  sample <- trimws(job_row[["sample"]] %||% "")
  if (nzchar(sample)) return(sample)
  mode <- trimws(job_row[["input_mode"]] %||% "")
  if (nzchar(mode)) return(mode)
  target <- trimws(job_row[["target"]] %||% "")
  if (nzchar(target)) return(basename(target))
  "project"
}

parse_log_filename <- function(path) {
  base <- basename(path %||% "")
  hit <- regmatches(base, regexec("^(output|error)_([^_]+)_(.+)\\.txt$", base))[[1]]
  if (length(hit) == 4) {
    return(list(type = hit[[2]], tool = canonical_log_tool(hit[[3]]), scope = hit[[4]]))
  }
  hit <- regmatches(base, regexec("^(output|error)_([^.]*)\\.txt$", base))[[1]]
  if (length(hit) == 3) {
    raw <- sub("_[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]_[0-9][0-9][0-9][0-9][0-9][0-9]$", "", hit[[3]])
    return(list(type = hit[[2]], tool = canonical_log_tool(raw), scope = "project"))
  }
  list(type = "", tool = "Other", scope = "project")
}

log_entries <- function(project) {
  rows <- list()
  design <- safe_read_table(project$design_matrix_path)
  design_samples <- if (NROW(design) && "sample" %in% names(design)) as.character(design$sample) else character(0)
  design_samples <- unique(design_samples[!is.na(design_samples) & nzchar(design_samples)])
  comparison_tools <- c("DESeq2", "GSEA")
  add_row <- function(tool, log_type, scope, path, label = "") {
    path <- as.character(path %||% "")
    if (!nzchar(path) || !file.exists(path)) return(invisible(NULL))
    canonical_tool <- canonical_log_tool(tool)
    scope <- if (nzchar(scope %||% "")) scope else "project"
    scope_type <- if (scope %in% design_samples && !canonical_tool %in% comparison_tools) "Sample" else "Run"
    rows[[length(rows) + 1]] <<- data.frame(
      tool = canonical_tool,
      log_type = log_type,
      scope_type = scope_type,
      scope = scope,
      label = if (nzchar(label %||% "")) label else paste(canonical_tool, log_type, scope),
      path = path,
      stringsAsFactors = FALSE
    )
    invisible(NULL)
  }

  jobs <- job_history(project)
  if (NROW(jobs)) {
    for (i in seq_len(NROW(jobs))) {
      row <- jobs[i, , drop = FALSE]
      scope <- log_scope_from_job(row)
      job_id <- row[["job_id"]] %||% ""
      suffix <- if (nzchar(job_id)) paste0("job ", job_id) else trimws(row[["time"]] %||% "")
      if ("stdout" %in% names(row)) add_row(row[["step"]], "output", scope, row[["stdout"]], trimws(paste(row[["step"]], scope, "output", suffix)))
      if ("stderr" %in% names(row)) add_row(row[["step"]], "error", scope, row[["stderr"]], trimws(paste(row[["step"]], scope, "error", suffix)))
    }
  }

  if (is_atac_project(project) && length(design_samples)) {
    for (sample in design_samples) {
      alignment_log <- file.path(project$data_dir, "bowtie2", sample, paste0(sample, "Log.final.out"))
      add_row("Bowtie2", "output", sample, alignment_log, paste("Bowtie2", sample, "alignment report"))
    }
  }

  project_log_dir <- file.path(dirname(project$data_dir), "log")
  if (dir.exists(project_log_dir)) {
    files <- list.files(project_log_dir, pattern = "^(output|error)_.*\\.txt$", full.names = TRUE)
    known <- if (length(rows)) vapply(rows, function(x) x$path[[1]], character(1)) else character(0)
    for (one_path in setdiff(files, known)) {
      parsed <- parse_log_filename(one_path)
      if (nzchar(parsed$type)) add_row(parsed$tool, parsed$type, parsed$scope, one_path, log_label_from_path(one_path))
    }
  }

  if (!length(rows)) {
    return(data.frame(tool = character(), log_type = character(), scope_type = character(), scope = character(), label = character(), path = character()))
  }
  out <- do.call(rbind, rows)
  out <- out[!duplicated(out$path), , drop = FALSE]
  out[order(out$tool, out$scope, out$log_type, out$label), , drop = FALSE]
}

log_file_choices <- function(project, tool = "All", log_type = "All", scope_type = "All", scope = "All") {
  entries <- log_entries(project)
  if (!NROW(entries)) return(character(0))
  if (!identical(tool %||% "All", "All")) entries <- entries[entries$tool == tool, , drop = FALSE]
  if (!identical(log_type %||% "All", "All")) entries <- entries[entries$log_type == log_type, , drop = FALSE]
  if (!identical(scope_type %||% "All", "All") && "scope_type" %in% names(entries)) entries <- entries[entries$scope_type == scope_type, , drop = FALSE]
  if (!identical(scope %||% "All", "All")) entries <- entries[entries$scope == scope, , drop = FALSE]
  if (!NROW(entries)) return(character(0))
  labels <- paste(entries$scope, entries$log_type, basename(entries$path), sep = " - ")
  stats::setNames(entries$path, labels)
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

first_scalar_string <- function(x, fallback = "") {
  x <- as.character(x %||% fallback)
  x <- x[!is.na(x)]
  if (!length(x) || !nzchar(trimws(x[[1]]))) return(fallback)
  x[[1]]
}

server_browser_listing <- function(path, mode = "dir") {
  path <- path.expand(trimws(first_scalar_string(path, CURRENT_HOME)))
  if (!nzchar(path) || !dir.exists(path)) path <- CURRENT_HOME
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  # Listing a directory requires both read and traverse permission.
  if (file.access(path, mode = 5) != 0) {
    return(list(path = path, status = "unreadable", choices = list()))
  }
  all_entries <- suppressWarnings(list.files(
    path,
    recursive = FALSE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  ))
  entries <- all_entries[!startsWith(basename(all_entries), ".")]
  dirs <- sort(entries[dir.exists(entries)])
  files <- sort(entries[!dir.exists(entries)])
  choices <- list()
  if (length(dirs)) {
    choices[["Folders"]] <- stats::setNames(dirs, paste0("📁 ", basename(dirs)))
  }
  status <- if (length(dirs) || length(files)) {
    "ok"
  } else if (length(all_entries)) {
    "hidden_only"
  } else {
    "empty"
  }
  list(path = path, status = status, choices = choices, dirs = dirs, files = files)
}

server_browser_choices <- function(path, mode = "dir") {
  server_browser_listing(path, mode)$choices
}

browser_start_path <- function(value, mode = "dir") {
  value <- path.expand(trimws(first_scalar_string(value, "")))
  if (nzchar(value) && file.exists(value) && !dir.exists(value)) return(dirname(value))
  if (nzchar(value) && dir.exists(value)) return(value)
  if (nzchar(value) && dir.exists(dirname(value))) return(dirname(value))
  CURRENT_HOME
}

active_job_steps <- function(project) {
  names(active_job_state_map(project))
}

active_job_state_map <- function(project) {
  jobs <- job_history(project)
  active_job_state_map_from_jobs(jobs)
}

active_job_state_map_from_jobs <- function(jobs) {
  if (!NROW(jobs) || !"slurm_state" %in% names(jobs)) return(setNames(character(0), character(0)))
  active_states <- c("PENDING", "CONFIGURING", "COMPLETING", "RUNNING", "SUSPENDED", "Submitted")
  jobs <- jobs[jobs$slurm_state %in% active_states, , drop = FALSE]
  if (!NROW(jobs)) return(setNames(character(0), character(0)))
  out <- tapply(jobs$slurm_state, jobs$step, function(x) tail(x, 1))
  unlist(out)
}

normalize_pipeline_status <- function(status) {
  status <- as.character(status)
  known <- c("Complete", "Active", "Cancelled", "Likely failed", "Completed, Deleted", "Likely failed, Deleted", "Cancelled, Deleted")
  ifelse(status %in% known, status, "Not started")
}

status_signature <- function(status) {
  if (!NROW(status)) return("")
  cols <- intersect(c("step", "status", "input", "detail"), names(status))
  paste(apply(status[, cols, drop = FALSE], 1, paste, collapse = "::"), collapse = "||")
}

project_status <- function(project, jobs = NULL, progress = NULL, active_states = NULL) {
  data_dir <- project$data_dir
  design <- project$design_matrix_path
  if (is.null(jobs)) jobs <- job_history(project)
  if (is.null(active_states)) active_states <- active_job_state_map_from_jobs(jobs)
  if (is_atac_project(project)) {
    raw <- data.frame(
      step = atac_pipeline_order(),
      status = c(
        if (file.exists(design)) "Complete" else "Not started",
        if (count_files(file.path(data_dir, "cutadapt"), fastq_suffix_regex) > 0) "Complete" else "Not started",
        if (count_files(file.path(data_dir, "fastqc"), "\\.html$") + count_files(file.path(data_dir, "fastqc_cutadapt"), "\\.html$") > 0) "Complete" else "Not started",
        if (count_files(file.path(data_dir, "bowtie2"), "_alignment_summary\\.txt$") > 0) "Complete" else "Not started",
        if (count_files(file.path(data_dir, "macs2"), "(narrowPeak|peaks\\.xls)$") > 0) "Complete" else "Not started",
        if (count_files(file.path(data_dir, "diffbind"), "DifferentialPeaks_.*\\.txt$") > 0) "Complete" else "Not started"
      ),
      path = c(design, file.path(data_dir, "cutadapt"), file.path(data_dir, "fastqc"), file.path(data_dir, "bowtie2"), file.path(data_dir, "macs2"), file.path(data_dir, "diffbind")),
      stringsAsFactors = FALSE
    )
    modes <- last_job_modes_from_jobs(jobs)
    raw$input <- unname(modes[raw$step]); raw$input[is.na(raw$input)] <- ""; raw$detail <- ""
    if (is.null(progress)) progress <- tryCatch(sample_progress(project, active_states, data.frame(), jobs = jobs)$table, error = function(e) data.frame())
    if (NROW(progress)) {
      for (step in sample_level_steps_for_project(project)) {
        hit <- progress[progress$step == step, , drop = FALSE]
        if (!NROW(hit)) next
        if (any(hit$status %in% c("Running", "Running, no growth yet", "Waiting"))) {
          raw$status[raw$step == step] <- "Active"
        } else if (any(hit$status == "Likely failed, Deleted")) {
          raw$status[raw$step == step] <- "Likely failed, Deleted"
        } else if (any(hit$status == "Cancelled, Deleted")) {
          raw$status[raw$step == step] <- "Cancelled, Deleted"
        } else if (all(hit$status %in% c("Completed, Deleted", "Optional, not run")) && any(hit$status == "Completed, Deleted")) {
          raw$status[raw$step == step] <- "Completed, Deleted"
        } else if (any(hit$status == "Cancelled")) {
          raw$status[raw$step == step] <- "Cancelled"
        } else if (all(hit$status %in% c("Completed", "Optional, not run")) && any(hit$status == "Completed")) {
          raw$status[raw$step == step] <- "Complete"
        } else if (any(hit$status == "Likely failed")) {
          raw$status[raw$step == step] <- "Likely failed"
        } else {
          raw$status[raw$step == step] <- "Not started"
        }
      }
    }
    active <- names(active_states)
    raw$status[raw$step %in% active] <- "Active"
    raw$status <- normalize_pipeline_status(raw$status)
    return(raw)
  }
  if (is_cutrun_project(project)) {
    raw <- data.frame(
      step = cutrun_pipeline_order(),
      status = c(
        if (file.exists(design)) "Complete" else "Not started",
        if (count_files(file.path(data_dir, "cutadapt"), fastq_suffix_regex) > 0) "Complete" else "Not started",
        if (count_files(file.path(data_dir, "fastqc"), "\\.html$") + count_files(file.path(data_dir, "fastqc_cutadapt"), "\\.html$") > 0) "Complete" else "Not started",
        if (count_files(file.path(data_dir, "bowtie2"), "_alignment_summary\\.txt$") > 0) "Complete" else "Not started",
        if (count_files(file.path(data_dir, "seacr"), "\\.bed$") + count_files(file.path(data_dir, "seacr"), "\\.bedgraph$") > 0) "Complete" else "Not started",
        if (count_files(file.path(data_dir, "cutrun_peak_qc"), "^seacr_consensus_peaks\\.bed$") > 0) "Complete" else "Not started",
        if (file.exists(file.path(data_dir, "cutrun_diffbind", "_COMPLETE")) || count_files(file.path(data_dir, "cutrun_diffbind"), "^_COMPLETE$") > 0) "Complete" else "Not started",
        if (count_files(file.path(data_dir, "macs2"), "(narrowPeak|broadPeak|peaks\\.xls)$") > 0) "Complete" else "Not started"
      ),
      path = c(
        design,
        file.path(data_dir, "cutadapt"),
        file.path(data_dir, "fastqc"),
        file.path(data_dir, "bowtie2"),
        file.path(data_dir, "seacr"),
        file.path(data_dir, "cutrun_peak_qc"),
        file.path(data_dir, "cutrun_diffbind"),
        file.path(data_dir, "macs2")
      ),
      stringsAsFactors = FALSE
    )
    modes <- last_job_modes_from_jobs(jobs)
    raw$input <- unname(modes[raw$step])
    raw$input[is.na(raw$input)] <- ""
    raw$detail <- ""
    if (is.null(progress)) progress <- tryCatch(sample_progress(project, active_states, data.frame(), jobs = jobs)$table, error = function(e) data.frame())
    if (NROW(progress)) {
      for (step in sample_level_steps_for_project(project)) {
        hit <- progress[progress$step == step, , drop = FALSE]
        if (!NROW(hit)) next
        if (any(hit$status == "Likely failed, Deleted")) {
          raw$status[raw$step == step] <- "Likely failed, Deleted"
        } else if (any(hit$status == "Cancelled, Deleted")) {
          raw$status[raw$step == step] <- "Cancelled, Deleted"
        } else if (all(hit$status %in% c("Completed, Deleted", "Optional, not run")) && any(hit$status == "Completed, Deleted")) {
          raw$status[raw$step == step] <- "Completed, Deleted"
        } else if (any(hit$status == "Cancelled")) {
          raw$status[raw$step == step] <- "Cancelled"
        } else if (all(hit$status %in% c("Completed", "Optional, not run")) && any(hit$status == "Completed")) {
          raw$status[raw$step == step] <- "Complete"
        } else if (any(hit$status %in% c("Running", "Running, no growth yet", "Waiting"))) {
          raw$status[raw$step == step] <- "Active"
        } else if (any(hit$status == "Likely failed")) {
          raw$status[raw$step == step] <- "Likely failed"
        } else {
          raw$status[raw$step == step] <- "Not started"
        }
      }
    }
    active <- names(active_states)
    raw$status[raw$step %in% active] <- "Active"
    raw$status <- normalize_pipeline_status(raw$status)
    return(raw)
  }
  feature_count_files <- count_files(file.path(data_dir, "featurecounts"), "_counts\\.txt$")
  feature_matrix_exists <- file.exists(file.path(data_dir, "counts", "count_matrix.txt"))
  raw <- data.frame(
    step = c("Design matrix", "Cutadapt", "FastQC", "STAR", "featureCounts", "DESeq2", "GSEA", "RSEM (optional)", "Kallisto (optional)"),
    status = c(
      if (file.exists(design)) "Complete" else "Not started",
      if (count_files(file.path(data_dir, "cutadapt"), fastq_suffix_regex) > 0) "Complete" else "Not started",
      if (count_files(file.path(data_dir, "fastqc"), "\\.html$") + count_files(file.path(data_dir, "fastqc_cutadapt"), "\\.html$") > 0) "Complete" else "Not started",
      if (count_files(file.path(data_dir, "star"), "Aligned\\.sortedByCoord\\.out\\.bam$") > 0) "Complete" else "Not started",
      if (feature_matrix_exists) "Complete" else if (feature_count_files > 0) "Active" else "Not started",
      if (count_files(file.path(data_dir, "deseq2"), "DEG|normalized") > 0) "Complete" else "Not started",
      if (count_files(file.path(data_dir, "gseapy"), "\\.(csv|txt|png|pdf)$") > 0) "Complete" else "Not started",
      if (count_files(file.path(data_dir, "rsem"), "\\.genes\\.results$") > 0) "Complete" else "Not started",
      if (count_files(file.path(data_dir, "kallisto"), "abundance\\.tsv$") > 0) "Complete" else "Not started"
    ),
    path = c(
      design,
      file.path(data_dir, "cutadapt"),
      file.path(data_dir, "fastqc"),
      file.path(data_dir, "star"),
      file.path(data_dir, "counts", "count_matrix.txt"),
      file.path(data_dir, "deseq2"),
      file.path(data_dir, "gseapy"),
      file.path(data_dir, "rsem"),
      file.path(data_dir, "kallisto")
    ),
    stringsAsFactors = FALSE
  )
  modes <- last_job_modes_from_jobs(jobs)
  raw$input <- unname(modes[raw$step])
  raw$input[is.na(raw$input)] <- ""
  raw$detail <- ""
  for (step in c("DESeq2", "GSEA")) {
    complete <- completed_project_level_runs(project, step, jobs)
    running <- running_project_level_runs(jobs, step)
    cancelled <- cancelled_project_level_runs(jobs, step)
    deleted_status <- latest_deleted_status(project, step)
    if (length(complete) && length(running)) {
      if (identical(step, "GSEA")) running <- setdiff(running, complete) else running <- drop_running_completed_labels(running, complete)
    }
    pieces <- character(0)
    if (length(running)) pieces <- c(pieces, paste("Running:", paste(running, collapse = "; ")))
    if (length(cancelled)) pieces <- c(pieces, paste("Cancelled:", paste(cancelled, collapse = "; ")))
    if (nzchar(deleted_status) && !length(complete) && !length(running)) pieces <- c(pieces, deleted_status)
    if (length(complete)) pieces <- c(pieces, paste("Complete:", paste(complete, collapse = "; ")))
    raw$detail[raw$step == step] <- paste(pieces, collapse = " | ")
    if (nzchar(deleted_status) && !length(complete) && !length(running)) raw$status[raw$step == step] <- deleted_status
    if (length(cancelled)) raw$status[raw$step == step] <- "Cancelled"
    if (length(running)) raw$status[raw$step == step] <- "Active"
    if (identical(step, "GSEA") && !length(complete) && !length(running) && !length(cancelled) && !nzchar(deleted_status)) raw$status[raw$step == step] <- "Not started"
  }
  if (is.null(progress)) progress <- tryCatch(sample_progress(project, active_states, data.frame(), jobs = jobs)$table, error = function(e) data.frame())
  if (NROW(progress)) {
    for (step in c("Cutadapt", "FastQC", "STAR", "featureCounts", "RSEM (optional)", "Kallisto (optional)")) {
      hit <- progress[progress$step == step, , drop = FALSE]
      if (!NROW(hit)) next
      if (any(hit$status == "Likely failed, Deleted")) {
        raw$status[raw$step == step] <- "Likely failed, Deleted"
      } else if (any(hit$status == "Cancelled, Deleted")) {
        raw$status[raw$step == step] <- "Cancelled, Deleted"
      } else if (all(hit$status %in% c("Completed, Deleted", "Optional, not run")) && any(hit$status == "Completed, Deleted")) {
        raw$status[raw$step == step] <- "Completed, Deleted"
      } else if (any(hit$status == "Cancelled")) {
        raw$status[raw$step == step] <- "Cancelled"
      } else if (all(hit$status %in% c("Completed", "Optional, not run")) && any(hit$status == "Completed")) {
        raw$status[raw$step == step] <- "Complete"
      } else if (any(hit$status %in% c("Running", "Running, no growth yet", "Waiting"))) {
        raw$status[raw$step == step] <- "Active"
      } else if (any(hit$status == "Likely failed")) {
        raw$status[raw$step == step] <- "Likely failed"
      } else {
        raw$status[raw$step == step] <- "Not started"
      }
    }
  }
  active <- names(active_states)
  raw$status[raw$step %in% active] <- "Active"
  if (!feature_matrix_exists && feature_count_files > 0) raw$status[raw$step == "featureCounts"] <- "Active"
  raw$status <- normalize_pipeline_status(raw$status)
  raw
}

status_rank <- function(status) {
  match(status, c("Active", "Likely failed", "Cancelled", "Likely failed, Deleted", "Cancelled, Deleted", "Completed, Deleted", "Complete", "Not started"), nomatch = 99)
}

rna_pipeline_order <- function() {
  c("Design matrix", "Cutadapt", "FastQC", "STAR", "featureCounts", "DESeq2", "GSEA", "RSEM (optional)", "Kallisto (optional)")
}

cutrun_pipeline_order <- function() {
  c("Design matrix", "Cutadapt", "FastQC", "Bowtie2", "SEACR", "Peak QC", "Differential Peaks", "MACS2 (optional)")
}

atac_pipeline_order <- function() {
  c("Design matrix", "Cutadapt", "FastQC", "Bowtie2", "MACS2 Peaks", "Differential Peaks")
}

all_pipeline_steps <- function() {
  unique(c(rna_pipeline_order(), cutrun_pipeline_order(), atac_pipeline_order()))
}

pipeline_order <- function(project = NULL) {
  if (!is.null(project) && is_cutrun_project(project)) return(cutrun_pipeline_order())
  if (!is.null(project) && is_atac_project(project)) return(atac_pipeline_order())
  rna_pipeline_order()
}

step_order <- function(step) {
  match(step, all_pipeline_steps(), nomatch = length(all_pipeline_steps()) + seq_along(step))
}

status_label <- function(status) {
  ifelse(identical(status, "Active"), "In progress", status)
}

status_css_key <- function(status) {
  switch(as.character(status %||% ""),
    "Active" = "active",
    "Complete" = "complete",
    "Cancelled" = "cancelled",
    "Likely failed" = "failed",
    "Completed, Deleted" = "deleted-complete",
    "Likely failed, Deleted" = "deleted-failed",
    "Cancelled, Deleted" = "deleted-cancelled",
    "not-started"
  )
}

status_pill <- function(status) {
  cls <- status_css_key(status)
  tags$span(class = paste("status-pill", cls), status_label(status))
}

status_cards <- function(df) {
  if (!NROW(df)) return(div(class = "empty-box", "No steps available."))
  df <- df[order(step_order(df$step)), , drop = FALSE]
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

sample_output_target <- function(project, sample, step) {
  targets <- sample_step_targets(project, sample, step)
  if (length(targets)) targets[[1]] else ""
}

fastqc_expected_targets <- function(reads, outdir) {
  reads <- unique(unlist(lapply(as.character(reads), split_fastq_path_list), use.names = FALSE))
  reads <- reads[nzchar(reads)]
  if (!length(reads)) return(character(0))
  stems <- sub(fastq_suffix_regex, "", basename(reads), ignore.case = TRUE)
  unique(c(
    file.path(outdir, paste0(stems, "_fastqc.html")),
    file.path(outdir, paste0(stems, "_screen.html"))
  ))
}

sample_step_targets <- function(project, sample, step, raw_pairs = NULL, trimmed_pairs = NULL) {
  data_dir <- project$data_dir
  if (identical(step, "FastQC")) {
    expected_for <- function(trimmed) {
      pairs <- if (trimmed) trimmed_pairs else raw_pairs
      if (is.null(pairs)) pairs <- sample_fastq_pairs(project, trimmed)
      hit <- pairs[pairs$sample == sample, , drop = FALSE]
      if (!NROW(hit)) return(character(0))
      reads <- c(hit$r1[1], if (project$paired_end) hit$r2[1] else character(0))
      outdir <- file.path(data_dir, if (trimmed) "fastqc_cutadapt" else "fastqc")
      fastqc_expected_targets(reads, outdir)
    }
    raw <- expected_for(FALSE)
    trimmed <- expected_for(TRUE)
    if (length(raw) && all(file.exists(raw))) return(raw)
    if (length(trimmed) && all(file.exists(trimmed))) return(trimmed)
    return(if (length(raw)) raw else trimmed)
  }
  if (is_cutrun_project(project)) {
    return(switch(step,
      "Cutadapt" = {
        cutadapt_dir <- file.path(data_dir, "cutadapt")
        pairs <- raw_pairs
        if (is.null(pairs)) pairs <- sample_fastq_pairs(project, FALSE)
        hit <- pairs[pairs$sample == sample, , drop = FALSE]
        expected <- character(0)
        if (NROW(hit)) {
          reads <- unique(c(hit$r1[1], if (project$paired_end) hit$r2[1] else character(0)))
          expected <- file.path(cutadapt_dir, basename(reads))
          if (length(expected) && all(file.exists(expected))) return(expected)
        }
        hits <- if (dir.exists(cutadapt_dir)) {
          list.files(cutadapt_dir, pattern = paste0("^", sample, ".*", fastq_suffix_regex), full.names = TRUE, ignore.case = TRUE)
        } else character(0)
        needed <- if (isTRUE(project$paired_end)) 2 else 1
        if (length(hits) >= needed) return(sort(hits))
        if (length(expected)) expected else file.path(cutadapt_dir, paste0(sample, ".fastq.gz"))
      },
      "Bowtie2" = file.path(data_dir, "bowtie2", sample, paste0(sample, "_alignment_summary.txt")),
      "SEACR" = {
        root <- file.path(data_dir, "seacr")
        pattern <- paste0("^", gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", sample, perl = TRUE), "\\.(stringent|relaxed)\\.bed$")
        hits <- if (dir.exists(root)) list.files(root, pattern = pattern, recursive = TRUE, full.names = TRUE) else character(0)
        if (length(hits)) hits else file.path(root, sample, paste0(sample, ".stringent.bed"))
      },
      "MACS2 (optional)" = {
        file.path(data_dir, "macs2", sample, paste0(sample, "_macs2_complete.txt"))
      },
      character(0)
    ))
  }
  if (is_atac_project(project)) {
    return(switch(step,
      "Cutadapt" = {
        cutadapt_dir <- file.path(data_dir, "cutadapt")
        pairs <- raw_pairs
        if (is.null(pairs)) pairs <- sample_fastq_pairs(project, FALSE)
        hit <- pairs[pairs$sample == sample, , drop = FALSE]
        expected <- character(0)
        if (NROW(hit)) {
          reads <- unique(c(hit$r1[1], if (project$paired_end) hit$r2[1] else character(0)))
          expected <- file.path(cutadapt_dir, basename(reads))
          if (length(expected) && all(file.exists(expected))) return(expected)
        }
        hits <- if (dir.exists(cutadapt_dir)) list.files(cutadapt_dir, pattern = paste0("^", sample, ".*", fastq_suffix_regex), full.names = TRUE, ignore.case = TRUE) else character(0)
        needed <- if (isTRUE(project$paired_end)) 2 else 1
        if (length(hits) >= needed) return(sort(hits))
        if (length(expected)) expected else file.path(cutadapt_dir, paste0(sample, ".fastq.gz"))
      },
      "Bowtie2" = file.path(data_dir, "bowtie2", sample, paste0(sample, "_alignment_summary.txt")),
      "MACS2 Peaks" = file.path(data_dir, "macs2", sample, paste0(sample, "_peaks.narrowPeak")),
      character(0)
    ))
  }
  switch(step,
    "Cutadapt" = {
      cutadapt_dir <- file.path(data_dir, "cutadapt")
      pairs <- raw_pairs
      if (is.null(pairs)) pairs <- sample_fastq_pairs(project, FALSE)
      hit <- pairs[pairs$sample == sample, , drop = FALSE]
      expected <- character(0)
      if (NROW(hit)) {
        reads <- unique(c(hit$r1[1], if (project$paired_end) hit$r2[1] else character(0)))
        expected <- file.path(cutadapt_dir, basename(reads))
        if (length(expected) && all(file.exists(expected))) return(expected)
      }
      hits <- if (dir.exists(cutadapt_dir)) {
        list.files(cutadapt_dir, pattern = paste0("^", sample, ".*", fastq_suffix_regex), full.names = TRUE, ignore.case = TRUE)
      } else character(0)
      needed <- if (isTRUE(project$paired_end)) 2 else 1
      if (length(hits) >= needed) return(sort(hits))
      if (length(expected)) expected else file.path(cutadapt_dir, paste0(sample, ".fastq.gz"))
    },
    "STAR" = file.path(data_dir, "star", sample, paste0(sample, "Aligned.sortedByCoord.out.bam")),
    "RSEM (optional)" = file.path(data_dir, "rsem", sample, paste0(sample, ".genes.results")),
    "Kallisto (optional)" = file.path(data_dir, "kallisto", sample, "abundance.tsv"),
    "featureCounts" = file.path(data_dir, "featurecounts", sample, paste0(sample, "_counts.txt")),
    character(0)
  )
}

minimum_expected_bytes <- function(step) {
  switch(step,
    "FastQC" = 1000,
    "Cutadapt" = 100,
    "STAR" = 1000,
    "Bowtie2" = 100,
    "SEACR" = 10,
    "MACS2 Peaks" = 10,
    "MACS2 (optional)" = 10,
    "RSEM (optional)" = 100,
    "Kallisto (optional)" = 100,
    "featureCounts" = 100,
    1
  )
}

file_size_for <- function(path) {
  if (!nzchar(path %||% "") || !file.exists(path)) return(0)
  info <- file.info(path)
  as.numeric(info$size %||% 0)
}

previous_size_for <- function(cache, path) {
  if (!NROW(cache) || !"path" %in% names(cache) || !"size" %in% names(cache)) return(NA_real_)
  hit <- cache[cache$path == path, , drop = FALSE]
  if (!NROW(hit)) return(NA_real_)
  as.numeric(tail(hit$size, 1))
}

read_metric_lines <- function(path) {
  if (!nzchar(path %||% "") || !file.exists(path)) return(character(0))
  info <- file.info(path)
  signature <- paste(info$size[[1]], as.numeric(info$mtime[[1]]), sep = ":")
  if (exists(path, envir = METRIC_LINES_CACHE, inherits = FALSE)) {
    cached <- get(path, envir = METRIC_LINES_CACHE, inherits = FALSE)
    if (identical(cached$signature, signature)) return(cached$value)
  }
  value <- tryCatch(readLines(path, warn = FALSE), error = function(e) character(0))
  assign(path, list(signature = signature, value = value), envir = METRIC_LINES_CACHE)
  value
}

metric_file_to_named_list <- function(path) {
  rows <- read_key_value_table(path)
  if (!NROW(rows) || NCOL(rows) < 2) return(list())
  vals <- as.list(as.character(rows[[2]]))
  names(vals) <- as.character(rows[[1]])
  vals
}

cutrun_alignment_summary_table <- function(project) {
  summary_path <- file.path(project$data_dir, "bowtie2_summary", "cutrun_alignment_summary.txt")
  saved <- safe_read_table(summary_path, 5000)
  files <- if (dir.exists(file.path(project$data_dir, "bowtie2"))) {
    list.files(file.path(project$data_dir, "bowtie2"), pattern = "_alignment_summary\\.txt$", recursive = TRUE, full.names = TRUE)
  } else character(0)
  if (!length(files)) return(saved)
  rows <- lapply(sort(files), function(path) {
    vals <- metric_file_to_named_list(path)
    if (!length(vals)) return(NULL)
    sample <- vals[["sample"]] %||% basename(dirname(path))
    cols <- c(
      sample = sample,
      mapped_reads = vals[["mapped_reads"]] %||% "",
      deduplicated_reads = vals[["deduplicated_reads"]] %||% "",
      fragments_used_for_signal = vals[["fragments_used_for_signal"]] %||% "",
      duplicate_fraction = vals[["duplicate_fraction"]] %||% "",
      normalization_mode = vals[["normalization_mode"]] %||% "",
      spikein_mapped_reads = vals[["spikein_mapped_reads"]] %||% "",
      spikein_scale_factor = vals[["spikein_scale_factor"]] %||% "",
      normalized_bedgraph = vals[["normalized_bedgraph"]] %||% ""
    )
    as.data.frame(as.list(cols), stringsAsFactors = FALSE, check.names = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(saved)
  do.call(rbind, rows)
}

cutrun_seacr_frip_table <- function(project) {
  qc_root <- file.path(project$data_dir, "cutrun_peak_qc")
  saved_paths <- if (dir.exists(qc_root)) list.files(qc_root, pattern = "^seacr_frip_summary\\.tsv$", recursive = TRUE, full.names = TRUE) else character(0)
  saved <- if (length(saved_paths)) safe_read_table(saved_paths[[which.max(file.info(saved_paths)$mtime)]], 5000) else data.frame()
  files <- if (dir.exists(file.path(project$data_dir, "seacr"))) {
    list.files(file.path(project$data_dir, "seacr"), pattern = "_seacr_summary\\.txt$", recursive = TRUE, full.names = TRUE)
  } else character(0)
  if (!length(files)) return(saved)
  rows <- lapply(sort(files), function(path) {
    vals <- metric_file_to_named_list(path)
    if (!length(vals)) return(NULL)
    sample <- basename(dirname(path))
    parent <- basename(dirname(dirname(path)))
    combo <- if (grepl("^(norm|non)_(stringent|relaxed)$", parent)) parent else "legacy"
    cols <- c(
      sample = sample,
      seacr_run = combo,
      frip = vals[["frip"]] %||% "",
      fragments_in_peaks = vals[["fragments_in_peaks"]] %||% "",
      total_fragments = vals[["total_fragments"]] %||% "",
      normalization = vals[["normalization"]] %||% "",
      stringency = vals[["stringency"]] %||% "",
      target_bedgraph = vals[["target_bedgraph"]] %||% "",
      control_bedgraph = vals[["control_bedgraph"]] %||% ""
    )
    as.data.frame(as.list(cols), stringsAsFactors = FALSE, check.names = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(saved)
  do.call(rbind, rows)
}

cutrun_peak_qc_summary_table <- function(project) {
  root <- file.path(project$data_dir, "cutrun_peak_qc")
  paths <- if (dir.exists(root)) list.files(root, pattern = "^cutrun_peak_qc_summary\\.txt$", recursive = TRUE, full.names = TRUE) else character(0)
  if (!length(paths)) return(data.frame())
  read_key_value_table(paths[[which.max(file.info(paths)$mtime)]])
}

cutrun_diffbind_summary_table <- function(project) {
  root <- file.path(project$data_dir, "cutrun_diffbind")
  legacy <- file.path(root, "cutrun_diffbind_summary.tsv")
  paths <- if (dir.exists(root)) list.files(root, pattern = "^cutrun_diffbind_summary\\.tsv$", recursive = TRUE, full.names = TRUE) else character(0)
  paths <- unique(c(if (file.exists(legacy)) legacy else character(0), paths))
  rows <- lapply(paths, safe_read_table, n = 5000)
  rows <- Filter(NROW, rows)
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

cutrun_diffbind_result_dirs <- function(project) {
  root <- file.path(project$data_dir, "cutrun_diffbind")
  if (!dir.exists(root)) return(character(0))
  dirs <- list.dirs(root, recursive = FALSE, full.names = TRUE)
  dirs[file.exists(file.path(dirs, "all_differential_peaks.tsv"))]
}

cutrun_fragment_plot_files <- function(project) {
  root <- file.path(project$data_dir, "bowtie2")
  if (!dir.exists(root)) return(character(0))
  sort(list.files(root, pattern = "_insert_size_histogram\\.(jpg|jpeg|png|pdf)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE))
}

human_file_size <- function(path) {
  bytes <- file_size_for(path)
  if (!is.finite(bytes) || bytes <= 0) return("0 B")
  units <- c("B", "KB", "MB", "GB", "TB")
  power <- min(floor(log(bytes, 1024)), length(units) - 1L)
  paste0(format(round(bytes / (1024^power), if (power == 0) 0 else 1), nsmall = if (power == 0) 0 else 1, trim = TRUE), " ", units[[power + 1L]])
}

cutrun_signal_track_table <- function(project) {
  root <- file.path(project$data_dir, "bowtie2")
  files <- if (dir.exists(root)) list.files(root, pattern = "\\.(bw|bedgraph)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE) else character(0)
  if (!length(files)) return(data.frame())
  rows <- lapply(sort(files), function(path) {
    name <- basename(path)
    normalization <- if (grepl("spikein", name, ignore.case = TRUE)) "E. coli spike-in" else if (grepl("cpm", name, ignore.case = TRUE)) "CPM" else "Raw / none"
    data.frame(
      sample = basename(dirname(path)),
      format = if (tolower(tools::file_ext(path)) == "bw") "bigWig" else "bedGraph",
      normalization = normalization,
      size = human_file_size(path),
      file = path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

cutrun_files_by_category <- function(project, category = "all") {
  category <- category %||% "all"
  files <- switch(category,
    qc = unlist(lapply(file.path(project$data_dir, c("fastqc", "fastqc_cutadapt")), function(path) if (dir.exists(path)) list.files(path, pattern = "\\.(html|zip)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE) else character(0)), use.names = FALSE),
    alignment = result_file_choices(project, "bowtie2", "\\.(bam|bai|txt|jpg|jpeg|png|pdf)$"),
    signal = result_file_choices(project, "bowtie2", "\\.(bw|bedgraph)$"),
    peaks = result_file_choices(project, c("seacr", "cutrun_dedup_sensitivity", "macs2", "cutrun_peak_qc"), "\\.(bed|narrowPeak|broadPeak|xls|txt|tsv)$"),
    differential = result_file_choices(project, "cutrun_diffbind", "\\.(bed|txt|tsv|csv|png|pdf|rds)$"),
    result_file_choices(project, c("fastqc", "fastqc_cutadapt", "bowtie2", "seacr", "cutrun_dedup_sensitivity", "macs2", "cutrun_peak_qc", "cutrun_diffbind"), "\\.(html|zip|bam|bai|bw|bedgraph|bed|narrowPeak|broadPeak|xls|txt|tsv|csv|png|jpg|jpeg|pdf|rds)$")
  )
  files <- unname(files)
  files <- sort(unique(files[file.exists(files)]))
  stats::setNames(files, relative_result_labels(project, files))
}

atac_alignment_summary_table <- function(project) {
  root <- file.path(project$data_dir, "bowtie2")
  files <- if (dir.exists(root)) list.files(root, pattern = "_alignment_summary\\.txt$", recursive = TRUE, full.names = TRUE) else character(0)
  rows <- lapply(sort(files), function(path) {
    x <- metric_file_to_named_list(path); if (!length(x)) return(NULL)
    as.data.frame(as.list(c(sample = x$sample %||% basename(dirname(path)), mapped_reads = x$mapped_reads %||% "", deduplicated_reads = x$deduplicated_reads %||% "", bigwig_normalization = x$bigwig_normalization %||% "", bigwig = x$bigwig %||% "")), stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows); if (length(rows)) do.call(rbind, rows) else data.frame()
}

atac_postprocess_status_table <- function(project) {
  design <- project_design_df(project)
  samples <- if (NROW(design) && "sample" %in% names(design)) as.character(design$sample) else character(0)
  bowtie_root <- file.path(project$data_dir, "bowtie2")
  if (dir.exists(bowtie_root)) samples <- unique(c(samples, basename(list.dirs(bowtie_root, recursive = FALSE, full.names = TRUE))))
  samples <- sort(unique(samples[nzchar(samples)]))
  if (!length(samples)) return(data.frame())
  rows <- lapply(samples, function(sample) {
    prefix <- file.path(bowtie_root, sample, sample)
    input_bam <- paste0(prefix, "Aligned.sortedByCoord.out.bam")
    expected <- c(
      `deduplicated BAM` = paste0(prefix, "Aligned.sortedByCoord_removeDup.out.bam"),
      `deduplicated BAM index` = paste0(prefix, "Aligned.sortedByCoord_removeDup.out.bam.bai"),
      `deduplicated BED` = paste0(prefix, "Aligned.sortedByCoord_removeDup.out.bed"),
      `Picard duplicate metrics` = paste0(prefix, "_markedDup_metrics.txt"),
      `insert-size metrics` = paste0(prefix, "_insert_size_metrics.txt"),
      `insert-size plot` = paste0(prefix, "_insert_size_histogram.jpg"),
      bigWig = paste0(prefix, "Aligned.sortedByCoord_removeDup.out.bw"),
      `alignment summary` = paste0(prefix, "_alignment_summary.txt")
    )
    input_ok <- file.exists(input_bam) && file_size_for(input_bam) > 0
    input_mtime <- if (input_ok) file.info(input_bam)$mtime else as.POSIXct(NA)
    valid <- vapply(expected, function(path) {
      if (!file.exists(path) || file_size_for(path) <= 0) return(FALSE)
      if (is.na(input_mtime)) return(TRUE)
      isTRUE(file.info(path)$mtime >= input_mtime)
    }, logical(1))
    issues <- names(expected)[!valid]
    summary_path <- unname(expected[["alignment summary"]])
    if (file.exists(summary_path) && file_size_for(summary_path) > 0) {
      summary_values <- metric_file_to_named_list(summary_path)
      if (!identical(toupper(summary_values$bigwig_normalization %||% ""), "CPM")) issues <- unique(c(issues, "CPM bigWig normalization"))
    }
    status <- if (!input_ok) "Full Bowtie2 required" else if (length(issues)) "Repair available" else "Complete"
    data.frame(sample = sample, status = status, issues = if (length(issues)) paste(issues, collapse = ", ") else "None", input_bam = input_bam, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

atac_results_explorer_ui <- function() {
  div(class = "native-results-host cutrun-results-host", div(class = "app-shell cutrun-results-shell",
    div(class = "hero cutrun-results-hero", div(class = "hero-copy", h1(class = "hero-title", "ATAC-seq Results Explorer"), div(class = "hero-kicker", "GRCm39/GENCODE M39 accessibility analysis"))),
    div(class = "main-tabs", tabsetPanel(id = "atac_results_tabs",
      tabPanel("Overview", br(), actionButton("refresh_atac_results", "Refresh results", class = "btn-primary"), tags$h4("Pipeline status"), table_output("results_overview"), tags$h4("Design matrix"), table_output("design_table")),
      tabPanel("QC", br(), tabsetPanel(id = "atac_qc_tabs",
        tabPanel("Initial QC", br(), sidebarLayout(
          sidebarPanel(width = 2, uiOutput("atac_qc_sample_control"), uiOutput("atac_qc_mode_control"), tags$hr(), helpText("FastQC and FastQ Screen reports for raw or cutadapt-trimmed reads.")),
          mainPanel(width = 10, uiOutput("atac_qc_status_ui"), tabsetPanel(
            tabPanel("R1 FastQC", uiOutput("atac_r1_fastqc_ui")),
            tabPanel("R1 Screen", uiOutput("atac_r1_screen_ui")),
            tabPanel("R2 FastQC", uiOutput("atac_r2_fastqc_ui")),
            tabPanel("R2 Screen", uiOutput("atac_r2_screen_ui"))
          ))
        )),
        tabPanel("Alignment", br(), tags$h4("Alignment summary across samples"), table_output("atac_alignment_summary"), tags$hr(), tags$h4("Post-alignment output checks"), table_output("atac_postprocess_status")),
        tabPanel("Fragment Size", br(), tags$p(class = "muted small-note", "Inspect the paired-end insert-size distribution produced after duplicate removal."), uiOutput("atac_insert_size_ui"))
      )),
      tabPanel("Peaks", br(), sidebarLayout(
        sidebarPanel(width = 2, uiOutput("atac_peak_file_ui"), tags$hr(), helpText("Inspect MACS2 ATAC peak calls and the corresponding TSS-centered signal heatmap.")),
        mainPanel(width = 10, tabsetPanel(
          tabPanel("Peak Table", br(), table_output("atac_peak_table")),
          tabPanel("TSS Heatmap", br(), uiOutput("atac_peak_heatmap_ui"))
        ))
      )),
      tabPanel("Differential Accessibility", br(), sidebarLayout(
        sidebarPanel(width = 2, uiOutput("atac_diffbind_dir_ui"), tags$hr(), helpText("Each comparison is stored and displayed independently.")),
        mainPanel(width = 10, tabsetPanel(
          tabPanel("Results", br(), table_output("atac_diffbind_table")),
          tabPanel("PCA", br(), uiOutput("atac_diffbind_pca_ui")),
          tabPanel("Volcano", br(), uiOutput("atac_diffbind_volcano_ui"))
        ))
      )),
      tabPanel("Files", br(), uiOutput("atac_file_ui"), uiOutput("atac_file_view"))
    ))
  ))
}

cutrun_metric_card <- function(label, value, note = "", tone = "blue") {
  div(
    class = paste("cutrun-metric-card", paste0("tone-", tone)),
    div(class = "cutrun-metric-label", label),
    div(class = "cutrun-metric-value", value %||% "—"),
    if (nzchar(note %||% "")) div(class = "cutrun-metric-note", note)
  )
}

cutrun_summary_cards_ui <- function(project) {
  design <- project_design_df(project)
  alignment <- cutrun_alignment_summary_table(project)
  frip <- cutrun_seacr_frip_table(project)
  mapped <- if (NROW(alignment) && "mapped_reads" %in% names(alignment)) clean_metric_number(alignment$mapped_reads) else numeric(0)
  spikein <- if (NROW(alignment) && "spikein_mapped_reads" %in% names(alignment)) clean_metric_number(alignment$spikein_mapped_reads) else numeric(0)
  frip_values <- if (NROW(frip) && "frip" %in% names(frip)) clean_metric_number(frip$frip) else numeric(0)
  mapped <- mapped[is.finite(mapped)]
  spikein <- spikein[is.finite(spikein)]
  frip_values <- frip_values[is.finite(frip_values)]
  frip_pct <- if (length(frip_values)) ifelse(frip_values <= 1, frip_values * 100, frip_values) else numeric(0)
  modes <- if (NROW(alignment) && "normalization_mode" %in% names(alignment)) unique(toupper(trimws(as.character(alignment$normalization_mode)))) else character(0)
  modes <- modes[nzchar(modes)]
  trimmed_fastqc <- count_files(file.path(project$data_dir, "fastqc_cutadapt"), "\\.html$")
  div(
    class = "cutrun-metric-grid",
    cutrun_metric_card("Samples", format_metric_value(NROW(design)), "Included in the saved design matrix", "blue"),
    cutrun_metric_card("Median mapped reads", if (length(mapped)) format_metric_value(stats::median(mapped)) else "—", paste(length(mapped), "alignment summaries"), "green"),
    cutrun_metric_card("Median E. coli reads", if (length(spikein)) format_metric_value(stats::median(spikein)) else "—", if (length(spikein)) "Spike-in alignment" else "No spike-in summary yet", "gold"),
    cutrun_metric_card("Median FRiP", if (length(frip_pct)) format_metric_value(stats::median(frip_pct), "%") else "—", "SEACR peaks", "purple"),
    cutrun_metric_card("Normalization", if (length(modes)) paste(modes, collapse = ", ") else "—", "From completed Bowtie2 jobs", "blue"),
    cutrun_metric_card("Trimmed FastQC", format_metric_value(trimmed_fastqc), "Reports from Cutadapt reads", "green")
  )
}

cutrun_qc_samples <- function(project) {
  design <- project_design_df(project)
  samples <- if (NROW(design) && "sample" %in% names(design)) trimws(as.character(design$sample)) else character(0)
  samples <- samples[nzchar(samples)]
  if (length(samples)) return(unique(samples))
  dirs <- file.path(project$data_dir, c("fastqc_cutadapt", "fastqc"))
  reports <- unlist(lapply(dirs[dir.exists(dirs)], function(path) {
    list.files(path, pattern = "_(fastqc|screen)\\.html$", full.names = FALSE, ignore.case = TRUE)
  }), use.names = FALSE)
  stems <- sub("_(fastqc|screen)\\.html$", "", reports, ignore.case = TRUE)
  stems <- sub("([._-]R)[12]([._-]?[0-9]*)$", "", stems, ignore.case = TRUE)
  sort(unique(stems[nzchar(stems)]))
}

cutrun_qc_report_path <- function(project, sample, read = c("R1", "R2"), report = c("fastqc", "screen"), trimmed = TRUE) {
  read <- match.arg(read)
  report <- match.arg(report)
  base_dir <- file.path(project$data_dir, if (isTRUE(trimmed)) "fastqc_cutadapt" else "fastqc")
  design <- project_design_df(project)
  filenames <- character(0)
  if (NROW(design) && all(c("sample", "filename") %in% names(design))) {
    hit <- design[trimws(as.character(design$sample)) == sample, , drop = FALSE]
    if (NROW(hit)) filenames <- trimws(unlist(strsplit(as.character(hit$filename[[1]]), ",", fixed = TRUE), use.names = FALSE))
  }
  read_index <- if (identical(read, "R2")) 2L else 1L
  read_file <- if (length(filenames) >= read_index) filenames[[read_index]] else ""
  candidates <- character(0)
  if (nzchar(read_file)) {
    stem <- sub(fastq_suffix_regex, "", basename(read_file), ignore.case = TRUE)
    candidates <- file.path(base_dir, paste0(stem, "_", report, ".html"))
  }
  candidates <- unique(c(
    candidates,
    file.path(base_dir, paste0(sample, "_", read, "_001_", report, ".html")),
    file.path(base_dir, paste0(sample, "_", read, "_", report, ".html"))
  ))
  hit <- candidates[file.exists(candidates)]
  if (length(hit)) return(hit[[1]])
  if (dir.exists(base_dir)) {
    files <- list.files(base_dir, pattern = paste0("_", report, "\\.html$"), full.names = TRUE, ignore.case = TRUE)
    sample_key <- gsub("[^a-z0-9]+", "", tolower(sample))
    file_key <- gsub("[^a-z0-9]+", "", tolower(basename(files)))
    read_key <- tolower(read)
    fallback <- files[grepl(sample_key, file_key, fixed = TRUE) & grepl(read_key, file_key, fixed = TRUE)]
    if (length(fallback)) return(fallback[[1]])
  }
  candidates[[1]]
}

cutrun_qc_report_ui <- function(project, sample, read, report, trimmed) {
  if (!nzchar(sample %||% "")) return(div(class = "empty-box", "No sample selected."))
  path <- cutrun_qc_report_path(project, sample, read, report, trimmed)
  if (!file.exists(path)) {
    mode <- if (isTRUE(trimmed)) "trimmed" else "raw"
    label <- if (identical(report, "fastqc")) "FastQC" else "FastQ Screen"
    return(div(class = "empty-box", sprintf("%s has not been generated for %s %s (%s reads).", label, sample, read, mode)))
  }
  image_or_file_ui(path, "calc(100vh - 210px)")
}

cutrun_results_explorer_ui <- function() {
  div(
    class = "native-results-host cutrun-results-host",
    div(
      class = "app-shell cutrun-results-shell",
      div(
        class = "hero cutrun-results-hero",
        div(
          class = "hero-topbar",
          div(
            class = "hero-copy",
            h1(class = "hero-title", "CUT&RUN Results Explorer"),
            div(class = "hero-kicker", "Developed by CSHL's Bioinformatics Shared Resource")
          ),
          div(
            class = "hero-logos",
            if (file.exists(LOGO_CSL_PATH)) tags$img(class = "hero-logo", src = file.path("csl_logo", basename(LOGO_CSL_PATH))),
            if (file.exists(LOGO_PATH)) tags$img(class = "hero-logo", src = file.path("codespring_logo", basename(LOGO_PATH)))
          )
        )
      ),
      div(
        class = "main-tabs",
        tabsetPanel(
          id = "cutrun_results_tabs",
          tabPanel("Overview",
            br(),
            div(
              class = "cutrun-results-actions",
              span(class = "cutrun-updated-note", "Live summary of saved pipeline outputs"),
              actionButton("refresh_cutrun_results", "Refresh results", class = "btn-primary")
            ),
            uiOutput("cutrun_summary_cards"),
            div(class = "cutrun-section-heading", tags$h4("Pipeline status"), tags$p("Completion state for every CUT&RUN analysis stage.")),
            table_output("results_overview"),
            br(),
            div(class = "cutrun-section-heading", tags$h4("Experimental design"), tags$p("Samples, marks, conditions, replicates, and matched controls used by the analysis.")),
            table_output("design_table")
          ),
          tabPanel("QC",
            br(),
            tabsetPanel(
              id = "cutrun_qc_tabs",
              tabPanel("Initial QC",
                br(),
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                    uiOutput("cutrun_qc_sample_control"),
                    uiOutput("cutrun_qc_mode_control"),
                    tags$hr(),
                    helpText("FastQC and FastQ Screen reports for raw or cutadapt-trimmed R1/R2 reads.")
                  ),
                  mainPanel(
                    width = 9,
                    uiOutput("cutrun_qc_status_ui"),
                    tabsetPanel(
                      tabPanel("R1 FastQC", uiOutput("cutrun_r1_fastqc_ui")),
                      tabPanel("R1 Screen", uiOutput("cutrun_r1_screen_ui")),
                      tabPanel("R2 FastQC", uiOutput("cutrun_r2_fastqc_ui")),
                      tabPanel("R2 Screen", uiOutput("cutrun_r2_screen_ui"))
                    )
                  )
                )
              ),
              tabPanel("Alignment",
                br(),
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                    uiOutput("cutrun_alignment_sample_control"),
                    tags$hr(),
                    helpText("Bowtie2 mapping, duplication, fragment, normalization, and E. coli spike-in metrics.")
                  ),
                  mainPanel(
                    width = 9,
                    uiOutput("cutrun_alignment_status_ui"),
                    div(class = "cutrun-chart-card", plotOutput("cutrun_alignment_plot", height = "360px")),
                    div(class = "cutrun-section-heading", tags$h4("Alignment summary across samples"), downloadButton("download_cutrun_alignment", "Download summary")),
                    table_output("cutrun_alignment_summary"),
                    tags$hr(),
                    tags$h4("Selected sample"),
                    table_output("cutrun_alignment_sample_table")
                  )
                )
              ),
              tabPanel("Fragment Size",
                br(),
                sidebarLayout(
                  sidebarPanel(width = 3, uiOutput("cutrun_fragment_sample_ui"), tags$hr(), helpText("Picard insert-size distribution for the selected paired-end library.")),
                  mainPanel(width = 9, uiOutput("cutrun_fragment_size_ui"))
                )
              ),
              tabPanel("Peak QC",
                br(),
                uiOutput("cutrun_peak_qc_cards"),
                div(class = "cutrun-chart-card", plotOutput("cutrun_frip_plot", height = "340px")),
                div(class = "cutrun-section-heading", tags$h4("SEACR FRiP summary"), downloadButton("download_cutrun_frip", "Download FRiP")),
                table_output("cutrun_frip_summary"),
                br(),
                div(class = "cutrun-section-heading", tags$h4("Project peak-union QC"), tags$p("Differential binding uses independent replicate-supported consensus sets for each cell type and mark.")),
                table_output("cutrun_peak_qc_summary")
              )
            )
          ),
          tabPanel("Signal & Peaks",
            br(),
            tabsetPanel(
              id = "cutrun_peak_tabs",
              tabPanel("SEACR Peaks",
                br(),
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                    uiOutput("cutrun_seacr_peak_ui"),
                    tags$hr(),
                    helpText("Inspect native-width SEACR stringent or relaxed peaks for one target sample.")
                  ),
                  mainPanel(
                    width = 9,
                    uiOutput("cutrun_seacr_peak_cards"),
                    div(class = "cutrun-section-heading", tags$h4("Selected SEACR peaks"), downloadButton("download_cutrun_seacr", "Download BED")),
                    table_output("cutrun_seacr_peak_table")
                  )
                )
              ),
              tabPanel("Signal Tracks",
                br(),
                div(class = "cutrun-section-heading", tags$h4("Genome-browser tracks"), tags$p("BigWig and bedGraph signals, including spike-in, CPM, or raw normalization.")),
                table_output("cutrun_signal_tracks")
              ),
              tabPanel("Peak Counts",
                br(),
                tags$p(class = "muted small-note", "Project-wide union counts are intended for QC. Use Differential Binding for mark-specific statistical comparisons."),
                table_output("cutrun_peak_counts")
              ),
              tabPanel("MACS2 (optional)",
                br(),
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                    uiOutput("cutrun_macs2_peak_ui"),
                    tags$hr(),
                    helpText("Optional comparison or broad-mark peak calls.")
                  ),
                  mainPanel(width = 9, table_output("cutrun_macs2_peak_table"))
                )
              )
            )
          ),
          tabPanel("Differential Binding",
            br(),
            sidebarLayout(
              sidebarPanel(
                width = 3,
                uiOutput("cutrun_diffbind_comparison_ui"),
                numericInput("cutrun_diffbind_fdr", "FDR cutoff", value = 0.05, min = 0, max = 1, step = 0.001),
                numericInput("cutrun_diffbind_fold", "Absolute log2 fold cutoff", value = 0, min = 0, step = 0.1),
                tags$hr(),
                helpText("Each cell type and mark is analyzed independently from raw BAM fragment counts with spike-in or genomic-background normalization.")
              ),
              mainPanel(
                width = 9,
                uiOutput("cutrun_diffbind_cards"),
                div(class = "cutrun-section-heading", tags$h4("Analysis summary"), tags$p("Completed, skipped, and failed comparisons.")),
                table_output("cutrun_diffbind_summary"),
                tags$hr(),
                tabsetPanel(
                  id = "cutrun_diffbind_tabs",
                  tabPanel("Results", br(), downloadButton("download_cutrun_diffbind_results", "Download filtered results"), br(), br(), table_output("cutrun_diffbind_results")),
                  tabPanel("Significant Peaks", br(), downloadButton("download_cutrun_diffbind_significant", "Download significant peaks"), br(), br(), table_output("cutrun_diffbind_significant")),
                  tabPanel("PCA", br(), uiOutput("cutrun_diffbind_pca_ui")),
                  tabPanel("Volcano", br(), uiOutput("cutrun_diffbind_volcano_ui")),
                  tabPanel("MA Plot", br(), uiOutput("cutrun_diffbind_ma_ui")),
                  tabPanel("Heatmap", br(), uiOutput("cutrun_diffbind_heatmap_ui")),
                  tabPanel("Normalization", br(), table_output("cutrun_diffbind_normalization")),
                  tabPanel("Consensus Counts", br(), table_output("cutrun_diffbind_consensus"))
                )
              )
            )
          ),
          tabPanel("Files",
            br(),
            sidebarLayout(
              sidebarPanel(
                width = 3,
                selectInput("cutrun_file_category", "Category", choices = c("QC reports" = "qc", "Alignment" = "alignment", "Signal tracks" = "signal", "Peaks" = "peaks", "Differential binding" = "differential", "All files" = "all"), selected = "qc", selectize = FALSE),
                uiOutput("cutrun_file_ui"),
                tags$hr(),
                uiOutput("cutrun_file_metadata_ui")
              ),
              mainPanel(width = 9, uiOutput("cutrun_file_view"))
            )
          )
        )
      )
    )
  )
}

clean_metric_number <- function(x) {
  x <- gsub(",", "", as.character(x %||% ""))
  x <- gsub("%", "", x, fixed = TRUE)
  suppressWarnings(as.numeric(trimws(x)))
}

format_metric_value <- function(x, suffix = "", digits = 1) {
  val <- suppressWarnings(as.numeric(x))
  if (!is.finite(val)) return("")
  if (identical(suffix, "%")) return(paste0(format(round(val, digits), nsmall = digits, trim = TRUE), "%"))
  if (abs(val - round(val)) < .Machine$double.eps^0.5) return(format(round(val), big.mark = ",", scientific = FALSE, trim = TRUE))
  format(round(val, digits), big.mark = ",", scientific = FALSE, trim = TRUE)
}

latest_job_for_sample <- function(jobs, step, sample) {
  if (!NROW(jobs) || !"step" %in% names(jobs)) return(data.frame())
  hit <- jobs[canonical_job_step(jobs$step) == canonical_job_step(step), , drop = FALSE]
  if ("sample" %in% names(hit) && NROW(hit)) {
    sample_hit <- hit[nzchar(hit$sample) & hit$sample == sample, , drop = FALSE]
    if (NROW(sample_hit)) hit <- sample_hit
  }
  if (NROW(hit)) tail(hit, 1) else data.frame()
}

extract_cutadapt_metrics <- function(project, sample, jobs) {
  hit <- latest_job_for_sample(jobs, "Cutadapt", sample)
  stdout <- if (NROW(hit) && "stdout" %in% names(hit)) hit$stdout[1] else ""
  lines <- read_metric_lines(stdout)
  text <- paste(lines, collapse = "\n")
  before <- NA_real_
  after <- NA_real_
  m <- regmatches(text, regexec("Total read pairs processed:[[:space:]]*([0-9,]+)", text))[[1]]
  if (length(m) >= 2) before <- clean_metric_number(m[2])
  m <- regmatches(text, regexec("Pairs written \\(passing filters\\):[[:space:]]*([0-9,]+)", text))[[1]]
  if (length(m) >= 2) after <- clean_metric_number(m[2])
  if (!is.finite(before)) {
    m <- regmatches(text, regexec("Total reads processed:[[:space:]]*([0-9,]+)", text))[[1]]
    if (length(m) >= 2) before <- clean_metric_number(m[2])
  }
  if (!is.finite(after)) {
    m <- regmatches(text, regexec("Reads written \\(passing filters\\):[[:space:]]*([0-9,]+)", text))[[1]]
    if (length(m) >= 2) after <- clean_metric_number(m[2])
  }
  c(
    `Reads before` = format_metric_value(before),
    `Reads after` = format_metric_value(after)
  )
}

extract_star_metrics <- function(project, sample) {
  log_path <- file.path(project$data_dir, "star", sample, paste0(sample, "Log.final.out"))
  lines <- read_metric_lines(log_path)
  if (!length(lines)) return(c(`Input reads` = "", `Uniquely mapped %` = ""))
  metric_value <- function(pattern) {
    hit <- grep(pattern, lines, value = TRUE, fixed = TRUE)
    if (!length(hit)) return(NA_real_)
    parts <- strsplit(hit[[1]], "\\|", fixed = FALSE)[[1]]
    clean_metric_number(tail(parts, 1))
  }
  c(
    `Input reads` = format_metric_value(metric_value("Number of input reads")),
    `Uniquely mapped %` = format_metric_value(metric_value("Uniquely mapped reads %"), "%")
  )
}

extract_featurecounts_metrics <- function(project, sample) {
  summary_path <- file.path(project$data_dir, "featurecounts", sample, paste0(sample, "_counts.txt.summary"))
  if (!file.exists(summary_path)) {
    return(c(Assigned = "", `Assigned %` = ""))
  }
  x <- tryCatch(utils::read.table(summary_path, sep = "\t", header = TRUE, quote = "\"", comment.char = "", check.names = FALSE), error = function(e) NULL)
  if (is.null(x) || !NROW(x) || NCOL(x) < 2) return(c(Assigned = "", `Assigned %` = ""))
  names(x)[1] <- "Status"
  val_col <- setdiff(names(x), "Status")[1]
  vals <- suppressWarnings(as.numeric(x[[val_col]]))
  assigned <- vals[match("Assigned", x$Status)]
  total <- sum(vals, na.rm = TRUE)
  pct <- if (is.finite(assigned) && total > 0) assigned * 100 / total else NA_real_
  c(
    Assigned = format_metric_value(assigned),
    `Assigned %` = format_metric_value(pct, "%")
  )
}

sample_step_metrics <- function(project, sample, step, jobs) {
  switch(step,
    "Cutadapt" = extract_cutadapt_metrics(project, sample, jobs),
    "STAR" = extract_star_metrics(project, sample),
    "Bowtie2" = extract_bowtie2_metrics(project, sample),
    "featureCounts" = extract_featurecounts_metrics(project, sample),
    c()
  )
}

sample_progress <- function(project, active_states = active_job_state_map(project), previous_cache = data.frame(), jobs = NULL) {
  design <- included_design_table(project)
  if (!NROW(design) || !"sample" %in% names(design)) return(list(table = data.frame(), cache = previous_cache))
  sample_steps <- sample_level_steps_for_project(project)
  if (is.null(jobs)) jobs <- job_history(project)
  deleted_records <- deleted_step_records(project)
  raw_pairs <- if (any(sample_steps %in% c("Cutadapt", "FastQC"))) sample_fastq_pairs(project, FALSE) else NULL
  trimmed_pairs <- if ("FastQC" %in% sample_steps) sample_fastq_pairs(project, TRUE) else NULL
  active_job_states <- c("PENDING", "CONFIGURING", "COMPLETING", "RUNNING", "SUSPENDED", "Submitted")
  completed_job_states <- c("COMPLETED", "COMPLETED+", "CD")
  cancelled_job_states <- c("CANCELLED", "CANCELLED+", "CA")
  failed_job_states <- c("TIMEOUT", "FAILED", "NODE_FAIL", "PREEMPTED", "OUT_OF_MEMORY", "BOOT_FAIL")
  active_jobs <- if (NROW(jobs) && "slurm_state" %in% names(jobs)) jobs[jobs$slurm_state %in% active_job_states, , drop = FALSE] else data.frame()
  rows <- list()
  cache_rows <- list()
  target_design <- if (is_cutrun_project(project)) cutrun_design(project) else design
  target_by_sample <- if ("target" %in% names(target_design)) stats::setNames(as.character(target_design$target), as.character(target_design$sample)) else setNames(rep("", NROW(target_design)), as.character(target_design$sample))
  for (sample in as.character(design$sample)) {
    for (step in sample_steps) {
      if (is_cutrun_project(project) && step %in% c("SEACR", "MACS2 (optional)") && cutrun_control_like(target_by_sample[[sample]] %||% "")) next
      targets <- sample_step_targets(project, sample, step, raw_pairs = raw_pairs, trimmed_pairs = trimmed_pairs)
      target <- paste(targets, collapse = "; ")
      sizes <- vapply(targets, file_size_for, numeric(1))
      size <- sum(sizes, na.rm = TRUE)
      previous_sizes <- vapply(targets, function(path) previous_size_for(previous_cache, path), numeric(1))
      previous_size <- sum(previous_sizes[is.finite(previous_sizes)], na.rm = TRUE)
      has_previous <- any(is.finite(previous_sizes))
      active_hit <- if (NROW(active_jobs)) {
        step_hits <- active_jobs[active_jobs$step == step, , drop = FALSE]
        if ("sample" %in% names(step_hits) && NROW(step_hits)) {
          step_hits[nzchar(step_hits$sample) & step_hits$sample == sample, , drop = FALSE]
        } else step_hits
      } else data.frame()
      all_step_hits <- if (NROW(jobs)) {
        step_hits <- jobs[jobs$step == step, , drop = FALSE]
        if ("sample" %in% names(step_hits) && NROW(step_hits)) {
          sample_hits <- step_hits[nzchar(step_hits$sample) & step_hits$sample == sample, , drop = FALSE]
          has_sample_tracking <- any(nzchar(step_hits$sample))
          if (NROW(sample_hits)) sample_hits else if (has_sample_tracking) data.frame() else step_hits
        } else step_hits
      } else data.frame()
      step_jobs <- if (NROW(jobs)) jobs[jobs$step == step, , drop = FALSE] else data.frame()
      has_sample_tracking <- NROW(step_jobs) && "sample" %in% names(step_jobs) && any(nzchar(step_jobs$sample))
      fallback_active <- !has_sample_tracking && step %in% names(active_states)
      active <- NROW(active_hit) > 0 || fallback_active
      latest_hit <- if (NROW(active_hit)) tail(active_hit, 1) else if (NROW(all_step_hits)) tail(all_step_hits, 1) else data.frame()
      slurm_state <- if (NROW(latest_hit) && "slurm_state" %in% names(latest_hit)) latest_hit$slurm_state[1] else if (fallback_active) active_states[[step]] else ""
      elapsed <- if (NROW(latest_hit) && "elapsed" %in% names(latest_hit)) latest_hit$elapsed[1] else ""
      min_size <- minimum_expected_bytes(step)
      complete_outputs <- length(sizes) > 0 && all(sizes >= min_size)
      deleted_record <- latest_deleted_record_from_records(deleted_records, step, sample)
      deleted_status <- if (NROW(deleted_record) && "deleted_status" %in% names(deleted_record)) as.character(deleted_record$deleted_status[1]) else ""
      if (nzchar(deleted_status) && NROW(latest_hit) && "time" %in% names(latest_hit) && "time" %in% names(deleted_record)) {
        latest_job_time <- suppressWarnings(as.POSIXct(latest_hit$time[1], tz = "UTC"))
        deleted_time <- suppressWarnings(as.POSIXct(deleted_record$time[1], tz = "UTC"))
        if (!is.na(latest_job_time) && !is.na(deleted_time) && latest_job_time > deleted_time) deleted_status <- ""
      }
      deleted_outputs <- nzchar(deleted_status) && size == 0 && !active
      error_signal <- job_error_signal(jobs, step, sample)
      fatal_error_signal <- cutrun_macs_fatal_error_signal(project, jobs, step, sample)
      growing <- active && has_previous && size > previous_size
      optional <- step %in% c("RSEM (optional)", "Kallisto (optional)")
      slurm_running <- active && slurm_state %in% c("RUNNING", "COMPLETING")
      slurm_waiting <- active && slurm_state %in% c("PENDING", "CONFIGURING", "Submitted")
      slurm_complete <- slurm_state %in% completed_job_states
      slurm_cancelled <- slurm_state %in% cancelled_job_states
      slurm_failed <- slurm_state %in% failed_job_states
      status <- if (deleted_outputs) {
        deleted_status
      } else if (slurm_cancelled) {
        "Cancelled"
      } else if (slurm_failed) {
        "Likely failed"
      } else if (slurm_running) {
        "Running"
      } else if (slurm_waiting) {
        "Waiting"
      } else if (active && growing) {
        "Running"
      } else if (active) {
        if (slurm_running) "Running" else "Waiting"
      } else if (fatal_error_signal) {
        "Likely failed"
      } else if (error_signal && !complete_outputs) {
        "Likely failed"
      } else if (complete_outputs || (slurm_complete && size > 0)) {
        "Completed"
      } else if (slurm_complete) {
        "Likely failed"
      } else if (size > 0 && size < min_size) {
        "Likely failed"
      } else if (optional) {
        "Optional, not run"
      } else {
        "Not started"
      }
      note <- if (identical(status, "Likely failed") && fatal_error_signal) {
        "MACS reported an internal peak-calling exception; partial peak files are not accepted as complete."
      } else if (identical(status, "Likely failed") && slurm_complete && !complete_outputs) {
        "SLURM completed, but the expected output is missing or too small."
      } else if (status == "Likely failed") {
        if (error_signal) "A failed SLURM state or non-empty error log was detected for this sample/step." else paste0("Output exists but is smaller than expected (<", min_size, " bytes).")
      } else if (grepl(", Deleted$", status)) {
        "Data outputs for this step were deleted after the recorded status."
      } else if (identical(status, "Cancelled")) {
        "SLURM reports this job was cancelled."
      } else if (identical(status, "Running") && growing) {
        "Output file size increased since the last refresh."
      } else if (identical(status, "Running") && size == 0) {
        "SLURM reports this sample is running; output has not been written yet."
      } else {
        ""
      }
      display_status <- status
      metrics <- sample_step_metrics(project, sample, step, jobs)
      rows[[length(rows) + 1]] <- data.frame(
        sample = sample,
        step = step,
        status = status,
        display_status = display_status,
        slurm_state = slurm_state,
        time_running = elapsed,
        output_bytes = size,
        target = target,
        note = note,
        metric_1_name = if (length(metrics) >= 1) names(metrics)[1] else "",
        metric_1_value = if (length(metrics) >= 1) unname(metrics[1]) else "",
        metric_2_name = if (length(metrics) >= 2) names(metrics)[2] else "",
        metric_2_value = if (length(metrics) >= 2) unname(metrics[2]) else "",
        stringsAsFactors = FALSE
      )
      if (length(targets)) {
        for (j in seq_along(targets)) {
          cache_rows[[length(cache_rows) + 1]] <- data.frame(path = targets[[j]], size = sizes[[j]], checked = as.character(Sys.time()), stringsAsFactors = FALSE)
        }
      }
    }
  }
  out <- do.call(rbind, rows)
  out <- out[order(out$sample, step_order(out$step)), , drop = FALSE]
  cache <- if (length(cache_rows)) do.call(rbind, cache_rows) else data.frame(path = character(), size = numeric(), checked = character(), stringsAsFactors = FALSE)
  list(table = out, cache = cache)
}


status_class <- function(status) {
  key <- tolower(gsub("[^A-Za-z0-9]+", "-", status %||% ""))
  paste("sample-status", key)
}

sample_progress_matrix_ui <- function(progress_df) {
  if (!NROW(progress_df)) return(div(class = "empty-box", "No sample progress available yet."))
  steps <- unique(progress_df$step[order(step_order(progress_df$step))])
  samples <- unique(progress_df$sample)
  div(
    class = "sample-matrix-wrap",
    tags$table(
      class = "sample-matrix",
      tags$thead(
        tags$tr(c(
          list(tags$th("Sample")),
          lapply(steps, tags$th)
        ))
      ),
      tags$tbody(lapply(samples, function(sample) {
        tags$tr(c(
          list(tags$td(class = "sample-name", sample)),
          lapply(steps, function(step) {
            hit <- progress_df[progress_df$sample == sample & progress_df$step == step, , drop = FALSE]
            if (!NROW(hit)) return(tags$td(""))
            title <- paste0(
              "Status: ", hit$status[1],
              "\nBytes: ", hit$output_bytes[1],
              "\nPath: ", hit$target[1],
              if ("metric_1_name" %in% names(hit) && nzchar(hit$metric_1_name[1])) paste0("\n", hit$metric_1_name[1], ": ", hit$metric_1_value[1]) else "",
              if ("metric_2_name" %in% names(hit) && nzchar(hit$metric_2_name[1])) paste0("\n", hit$metric_2_name[1], ": ", hit$metric_2_value[1]) else "",
              if (nzchar(hit$note[1])) paste0("\nNote: ", hit$note[1]) else ""
            )
            tags$td(
              tags$span(
                class = status_class(hit$status[1]),
                title = title,
                hit$display_status[1]
              )
            )
          })
        ))
      }))
    )
  )
}

sample_progress_detail_table <- function(progress_df) {
  if (!NROW(progress_df)) return(data.frame())
  time_running <- if ("time_running" %in% names(progress_df)) as.character(progress_df$time_running) else rep("", NROW(progress_df))
  out <- data.frame(
    Sample = progress_df$sample,
    Step = progress_df$step,
    Status = progress_df$display_status,
    `Time running` = ifelse(nzchar(time_running), time_running, "-"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (all(c("metric_1_name", "metric_1_value", "metric_2_name", "metric_2_value") %in% names(progress_df))) {
    metric_1_names <- unique(progress_df$metric_1_name[nzchar(progress_df$metric_1_name)])
    metric_2_names <- unique(progress_df$metric_2_name[nzchar(progress_df$metric_2_name)])
    if (length(metric_1_names) == 1) out[[metric_1_names[[1]]]] <- progress_df$metric_1_value
    if (length(metric_2_names) == 1) out[[metric_2_names[[1]]]] <- progress_df$metric_2_value
  }
  out[order(out$Sample, step_order(out$Step)), , drop = FALSE]
}

tool_progress_output_id <- function(step) {
  paste0("tool_progress_", tolower(gsub("[^A-Za-z0-9]+", "_", step)))
}

tool_progress_ui_output_id <- function(step) {
  paste0("tool_progress_ui_", tolower(gsub("[^A-Za-z0-9]+", "_", step)))
}

tool_retry_ui_output_id <- function(step) {
  paste0("tool_retry_ui_", tolower(gsub("[^A-Za-z0-9]+", "_", step)))
}

tool_retry_button_id <- function(step) {
  paste0("retry_incomplete_", tolower(gsub("[^A-Za-z0-9]+", "_", step)))
}

tool_message_output_id <- function(step) {
  paste0("tool_message_", tolower(gsub("[^A-Za-z0-9]+", "_", step)))
}

tool_cancel_button_id <- function(step) {
  paste0("cancel_jobs_", tolower(gsub("[^A-Za-z0-9]+", "_", step)))
}

tool_cancel_confirm_id <- function(step) {
  paste0("confirm_cancel_", tolower(gsub("[^A-Za-z0-9]+", "_", step)))
}

tool_cancel_samples_id <- function(step) {
  paste0("cancel_samples_", tolower(gsub("[^A-Za-z0-9]+", "_", step)))
}

tool_delete_data_button_id <- function(step) {
  paste0("delete_data_", tolower(gsub("[^A-Za-z0-9]+", "_", step)))
}

tool_delete_data_confirm_id <- function(step) {
  paste0("confirm_delete_data_", tolower(gsub("[^A-Za-z0-9]+", "_", step)))
}

tool_delete_data_samples_id <- function(step) {
  paste0("delete_data_samples_", tolower(gsub("[^A-Za-z0-9]+", "_", step)))
}

sample_level_pipeline_steps <- function() {
  unique(c("Cutadapt", "FastQC", "STAR", "featureCounts", "RSEM (optional)", "Kallisto (optional)", "Bowtie2", "SEACR", "MACS2 (optional)", "MACS2 Peaks"))
}

sample_level_steps_for_project <- function(project) {
  if (is_cutrun_project(project)) c("Cutadapt", "FastQC", "Bowtie2", "SEACR", "MACS2 (optional)")
  else if (is_atac_project(project)) c("Cutadapt", "FastQC", "Bowtie2", "MACS2 Peaks")
  else c("Cutadapt", "FastQC", "STAR", "featureCounts", "RSEM (optional)", "Kallisto (optional)")
}

primary_run_button_id <- function(project, step) {
  if (identical(step, "Cutadapt")) return("run_cutadapt")
  if (identical(step, "FastQC")) return("run_fastqc")
  if (identical(step, "Bowtie2")) return(if (is_cutrun_project(project)) "run_cutrun_bowtie2" else "run_atac_bowtie2")
  if (identical(step, "SEACR")) return("run_cutrun_seacr")
  if (identical(step, "MACS2 (optional)")) return("run_cutrun_macs2")
  if (identical(step, "MACS2 Peaks")) return("run_atac_macs2")
  if (identical(step, "STAR")) return("run_star")
  if (identical(step, "featureCounts")) return("run_featurecounts")
  if (identical(step, "RSEM (optional)")) return("run_rsem")
  if (identical(step, "Kallisto (optional)")) return("run_kallisto")
  ""
}

sample_retry_candidates <- function(progress_df, step) {
  if (!NROW(progress_df) || !all(c("sample", "step", "status") %in% names(progress_df))) return(character(0))
  hit <- progress_df[progress_df$step == step, , drop = FALSE]
  if (!NROW(hit)) return(character(0))
  retry_status <- hit$status %in% c("Not started", "Likely failed", "Cancelled", "Possibly incomplete") |
    grepl(", Deleted$", hit$status)
  sort(unique(as.character(hit$sample[retry_status & nzchar(as.character(hit$sample))])))
}

sample_retry_ui <- function(project, progress_df, step) {
  if (!NROW(progress_df) || !"step" %in% names(progress_df) || !any(progress_df$step == step)) return(NULL)
  samples <- sample_retry_candidates(progress_df, step)
  if (!length(samples)) {
    return(div(class = "sample-retry-zone complete", tags$strong("Incomplete/failed samples"), tags$span("None detected.")))
  }
  primary_id <- primary_run_button_id(project, step)
  if (!nzchar(primary_id)) return(NULL)
  onclick <- sprintf(
    "var button=document.getElementById('%s'); if(button){button.click();}",
    primary_id
  )
  div(
    class = "sample-retry-zone",
    div(class = "sample-retry-heading",
        tags$strong(sprintf("Incomplete/failed samples (%d)", length(samples))),
        tags$span("Only these samples will be submitted; completed and active samples are skipped.")),
    div(class = "sample-retry-chip-wrap", lapply(samples, function(sample) tags$span(class = "sample-retry-chip", sample))),
    actionButton(tool_retry_button_id(step), "Run incomplete/failed samples only", class = "btn-warning", onclick = onclick)
  )
}

runnable_pipeline_steps <- function(project = NULL) {
  if (is.null(project)) return(setdiff(all_pipeline_steps(), "Design matrix"))
  setdiff(pipeline_order(project), "Design matrix")
}

selected_choice <- function(value, choices, default = NULL) {
  choices_vec <- unname(as.character(choices))
  choices_vec <- choices_vec[nzchar(choices_vec)]
  if (!length(choices_vec)) return(character(0))
  value <- as.character(value %||% "")
  if (length(value) && nzchar(value[[1]]) && value[[1]] %in% choices_vec) return(value[[1]])
  default <- as.character(default %||% "")
  if (length(default) && nzchar(default[[1]]) && default[[1]] %in% choices_vec) return(default[[1]])
  choices_vec[[1]]
}

resolve_comparison_inputs <- function(project, compare_col = NULL, reference = NULL, comparison = NULL) {
  cols <- design_compare_columns(project)
  if (!length(cols)) stop("No comparison columns found between sample and filename in design_matrix.txt.")
  default_col <- if ("treatment" %in% cols) "treatment" else cols[[1]]
  compare_col <- selected_choice(compare_col, cols, default_col)
  vals <- design_compare_values(project, compare_col)
  if (length(vals) < 2) stop("Selected comparison column must contain at least two groups: ", compare_col)
  reference <- selected_choice(reference, vals, vals[[1]])
  alternatives <- vals[vals != reference]
  default_comparison <- if (length(alternatives)) alternatives[[1]] else vals[[2]]
  comparison <- selected_choice(comparison, vals, default_comparison)
  if (identical(reference, comparison)) {
    if (length(alternatives)) comparison <- alternatives[[1]]
  }
  list(compare_col = compare_col, reference = reference, comparison = comparison, values = vals)
}

sample_progress_step_table <- function(progress_df, step) {
  if (!NROW(progress_df) || !"step" %in% names(progress_df)) return(NULL)
  hit <- progress_df[progress_df$step == step, , drop = FALSE]
  if (!NROW(hit)) return(NULL)
  hit <- hit[order(hit$sample), , drop = FALSE]
  time_running <- if ("time_running" %in% names(hit)) as.character(hit$time_running) else rep("", NROW(hit))
  out <- data.frame(
    Sample = hit$sample,
    Status = hit$display_status,
    `Time running` = ifelse(nzchar(time_running), time_running, "-"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  metric_1_names <- if ("metric_1_name" %in% names(hit)) unique(hit$metric_1_name[nzchar(hit$metric_1_name)]) else character(0)
  metric_2_names <- if ("metric_2_name" %in% names(hit)) unique(hit$metric_2_name[nzchar(hit$metric_2_name)]) else character(0)
  if (length(metric_1_names) == 1) out[[metric_1_names[[1]]]] <- hit$metric_1_value
  if (length(metric_2_names) == 1) out[[metric_2_names[[1]]]] <- hit$metric_2_value
  out
}

sample_progress_step_ui <- function(progress_df, step) {
  table <- sample_progress_step_table(progress_df, step)
  if (!NROW(table)) return(NULL)
  hit <- progress_df[progress_df$step == step, , drop = FALSE]
  hit <- hit[order(hit$sample), , drop = FALSE]
  div(
    class = "tool-progress-wrap",
    div(class = "tool-progress-title", paste("Sample progress —", NROW(table), "samples")),
    tags$table(
      class = "tool-progress-table",
      tags$thead(tags$tr(lapply(colnames(table), tags$th))),
      tags$tbody(lapply(seq_len(NROW(hit)), function(i) {
        title <- paste0(
          "Status: ", hit$status[i],
          "\nSLURM: ", if (nzchar(hit$slurm_state[i])) hit$slurm_state[i] else "-",
          "\nBytes: ", hit$output_bytes[i],
          "\nPath: ", hit$target[i],
          if ("metric_1_name" %in% names(hit) && nzchar(hit$metric_1_name[i])) paste0("\n", hit$metric_1_name[i], ": ", hit$metric_1_value[i]) else "",
          if ("metric_2_name" %in% names(hit) && nzchar(hit$metric_2_name[i])) paste0("\n", hit$metric_2_name[i], ": ", hit$metric_2_value[i]) else "",
          if (nzchar(hit$note[i])) paste0("\nNote: ", hit$note[i]) else ""
        )
        row_values <- as.list(table[i, , drop = FALSE])
        tags$tr(lapply(seq_along(row_values), function(j) {
          nm <- names(row_values)[j]
          value <- as.character(row_values[[j]])
          if (identical(nm, "Sample")) return(tags$td(class = "sample-name", value))
          if (identical(nm, "Status")) return(tags$td(tags$span(class = status_class(hit$status[i]), title = title, value)))
          tags$td(value)
        }))
      }))
    )
  )
}

optimistic_step_progress <- function(project, step, input_mode = "", samples = NULL) {
  design <- safe_read_table(project$design_matrix_path)
  if (!NROW(design) || !"sample" %in% names(design)) return(data.frame())
  design_samples <- as.character(design$sample)
  design_samples <- design_samples[nzchar(design_samples)]
  requested_samples <- unique(as.character(samples %||% character(0)))
  requested_samples <- requested_samples[nzchar(requested_samples)]
  samples <- if (length(requested_samples)) intersect(design_samples, requested_samples) else design_samples
  if (!length(samples)) return(data.frame())
  rows <- lapply(samples, function(sample) {
    target <- sample_output_target(project, sample, step)
    data.frame(
      sample = sample,
      step = step,
      status = "Waiting",
      display_status = "Waiting",
      slurm_state = "Submitted",
      time_running = "-",
      output_bytes = 0,
      target = target,
      note = "Submitted; waiting for scheduler status refresh.",
      metric_1_name = "",
      metric_1_value = "",
      metric_2_name = "",
      metric_2_value = "",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

optimistic_status <- function(current_status, step, input_mode = "") {
  if (!NROW(current_status)) {
    current_status <- data.frame(
      step = all_pipeline_steps(),
      status = "Not started",
      path = "",
      input = "",
      stringsAsFactors = FALSE
    )
  }
  if (!step %in% current_status$step) {
    current_status <- rbind(current_status, data.frame(step = step, status = "Not started", path = "", input = "", stringsAsFactors = FALSE))
  }
  current_status$status[current_status$step == step] <- "Active"
  if (!"input" %in% names(current_status)) current_status$input <- ""
  current_status$input[current_status$step == step] <- input_mode
  current_status[order(step_order(current_status$step)), , drop = FALSE]
}

save_job <- function(project, step, command, output = "") {
  row <- data.frame(
    time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    project_id = project$id %||% paste(project$analysis_key %||% "analysis", project$name, sep = "/"),
    project = project$name,
    analysis = project$analysis,
    data_dir = project$data_dir %||% "",
    step = step,
    command = gsub("[\t\r\n]+", " ", paste(command, collapse = " ")),
    output = gsub("\n", "\\n", as.character(output %||% ""), fixed = TRUE),
    stringsAsFactors = FALSE
  )
  existing <- if (file.exists(JOBS_PATH)) {
    tryCatch(utils::read.delim(JOBS_PATH, check.names = FALSE, stringsAsFactors = FALSE, fill = TRUE), error = function(e) data.frame())
  } else data.frame()
  all_cols <- unique(c(names(existing), names(row)))
  for (col in setdiff(all_cols, names(existing))) existing[[col]] <- character(NROW(existing))
  for (col in setdiff(all_cols, names(row))) row[[col]] <- ""
  out <- rbind(existing[, all_cols, drop = FALSE], row[, all_cols, drop = FALSE])
  utils::write.table(out, JOBS_PATH, sep = "\t", row.names = FALSE, quote = TRUE, append = FALSE, col.names = TRUE)
}

append_run_manifest <- function(project, step, sample = "", command = character(0), output_target = "", input_mode = "", reference = "", job_id = "") {
  log_dir <- file.path(dirname(project$data_dir), "log")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  manifest <- file.path(log_dir, "codespringweb_run_manifest.tsv")
  row <- data.frame(
    time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    project = project$name,
    analysis = project$analysis,
    step = step,
    sample = sample %||% "",
    job_id = job_id %||% "",
    input_mode = input_mode %||% "",
    genome = project$genome %||% "",
    reference = reference %||% gencode_label(project),
    output_target = output_target %||% "",
    command = paste(command, collapse = " "),
    stringsAsFactors = FALSE
  )
  utils::write.table(row, manifest, sep = "\t", row.names = FALSE, quote = TRUE, append = file.exists(manifest), col.names = !file.exists(manifest))
  manifest
}

extract_output_field <- function(x, key) {
  text <- gsub("\\\\+n", "\n", as.character(x %||% ""), perl = TRUE)
  lines <- trimws(unlist(strsplit(text, "\n", fixed = TRUE)))
  prefix <- paste0(key, ":")
  hit <- lines[startsWith(lines, prefix)]
  if (length(hit)) return(trimws(sub(paste0("^", key, ":[[:space:]]*"), "", hit[[1]])))
  pat <- paste0(key, ":[[:space:]]*")
  m <- regexpr(pat, text)
  if (m < 0) return("")
  rest <- substr(text, m + attr(m, "match.length"), nchar(text))
  rest <- sub("[[:space:]]+(job_id|sample|target|input_mode|stdout|stderr|manifest):.*$", "", rest)
  trimws(rest)
}

job_name_for <- function(project, step, sample = "") {
  raw <- paste("csl", project$name, step, sample, sep = "_")
  raw <- gsub("[^A-Za-z0-9_]+", "_", raw)
  substr(gsub("_+", "_", raw), 1, 80)
}

project_job_name_prefix <- function(project) {
  raw <- paste("csl", project$name, sep = "_")
  raw <- gsub("[^A-Za-z0-9_]+", "_", raw)
  paste0(gsub("_+", "_", raw), "_")
}

rna_workdir <- function(project) {
  normalizePath(file.path(CSL_ROOT, analysis_notebook_dir(project$analysis_key)), winslash = "/", mustWork = FALSE)
}

genome_reference_catalog <- function() {
  list(
    human = list(
      human_gencode50 = list(
        label = "Human GRCh38 / GENCODE v50",
        star_index = "/grid/bsr/data/data/utama/genome/human_gencode50/STAR_index",
        kallisto_index = "/grid/bsr/data/data/utama/genome/human_gencode50/gencode.v50.transcripts.idx",
        rsem_index = "/grid/bsr/data/data/utama/genome/human_gencode50/rsem_index/rsem",
        gtf = "/grid/bsr/data/data/utama/genome/human_gencode50/gencode.v50.primary_assembly.annotation.gtf",
        strand_bed = "/grid/bsr/data/data/utama/genome/human_gencode50/gencode.v50.annotation_forStrandDetect_geneID.bed"
      ),
      human_gencode42 = list(
        label = "Human hg38 / GENCODE v42 legacy",
        star_index = "/grid/bsr/data/data/utama/genome/hg38_p13_gencode/hg38_p13_gencode_rel42_all_starindex",
        kallisto_index = "/grid/bsr/data/data/utama/genome/hg38_p13_gencode/gencode.v45.transcripts.idx",
        rsem_index = "/grid/bsr/data/data/utama/genome/human_rsem_index_star_gencode_hg38_p13_rel42_v2.7.2b/human_gencode",
        gtf = "/grid/bsr/data/data/utama/genome/hg38_p13_gencode/gencode.v42.chr_patch_hapl_scaff.annotation.gtf",
        strand_bed = "/grid/bsr/data/data/utama/genome/hg38_p13_gencode/gencode.v42.chr_patch_hapl_scaff.annotation_forStrandDetect_geneID.bed"
      )
    ),
    mouse = list(
      mouse_gencodeM39 = list(
        label = "Mouse GRCm39 / GENCODE M39",
        star_index = "/grid/bsr/data/data/utama/genome/mouse_gencodeM39/STAR_index",
        kallisto_index = "/grid/bsr/data/data/utama/genome/mouse_gencodeM39/gencode.vM39.transcripts.idx",
        rsem_index = "/grid/bsr/data/data/utama/genome/mouse_gencodeM39/rsem_index/rsem",
        gtf = "/grid/bsr/data/data/utama/genome/mouse_gencodeM39/gencode.vM39.primary_assembly.annotation.gtf",
        strand_bed = "/grid/bsr/data/data/utama/genome/mouse_gencodeM39/gencode.vM39.annotation_forStrandDetect_geneID.bed"
      ),
      mouse_gencodeM29 = list(
        label = "Mouse GRCm39 / GENCODE M29 legacy",
        star_index = "/grid/bsr/data/data/utama/genome/GRCm39_M29_gencode/GRCm39_M29_gencode_starindex",
        kallisto_index = "/grid/bsr/data/data/utama/genome/GRCm39_M29_gencode/gencode.vM29.transcripts.idx",
        rsem_index = "/grid/bsr/data/data/utama/genome/mouse_rsem_index_star_gencode_GRCm39_M29_v2.7.10a/mouse_gencode",
        gtf = "/grid/bsr/data/data/utama/genome/GRCm39_M29_gencode/gencode.vM29.annotation.gtf",
        strand_bed = "/grid/bsr/data/data/utama/genome/GRCm39_M29_gencode/gencode.vM29.annotation_forStrandDetect_geneID.bed"
      )
    )
  )
}

genome_species <- function(project) {
  genome <- tolower(project$genome %||% "mouse")
  if (genome %in% c("human", "mouse")) return(genome)
  if (grepl("^human", genome)) return("human")
  if (grepl("^mouse", genome)) return("mouse")
  "mouse"
}

genome_reference_key <- function(project) {
  catalog <- genome_reference_catalog()
  species <- genome_species(project)
  if (is_atac_project(project) || is_cutrun_project(project)) {
    return(if (identical(species, "human")) "human_gencode50" else "mouse_gencodeM39")
  }
  genome <- tolower(project$genome %||% "")
  ref <- project$genome_version %||% project$reference_genome %||% ""
  ref <- trimws(as.character(ref))
  if (nzchar(ref) && ref %in% names(catalog[[species]])) return(ref)
  if (nzchar(genome) && genome %in% names(catalog[[species]])) return(genome)
  if (identical(species, "human")) return("human_gencode42")
  "mouse_gencodeM29"
}

genome_reference_choices <- function(species) {
  catalog <- genome_reference_catalog()
  species <- tolower(species %||% "mouse")
  if (!species %in% names(catalog)) species <- "mouse"
  refs <- catalog[[species]]
  stats::setNames(names(refs), vapply(refs, `[[`, character(1), "label"))
}

genome_resources <- function(project) {
  species <- genome_species(project)
  ref <- genome_reference_key(project)
  genome_reference_catalog()[[species]][[ref]]
}

cutrun_reference_resources <- function(project) {
  species <- genome_species(project)
  if (identical(species, "human")) {
    list(
      label = "Human GRCh38 / GENCODE v50 Bowtie2 CUT&RUN reference",
      bowtie2_index = "/grid/bsr/data/data/utama/genome/human_gencode50/bowtie2_index/GRCh38_gencode50",
      chrom_sizes = "/grid/bsr/data/data/utama/genome/human_gencode50/GRCh38.chrom.sizes",
      macs2_genome = "hs",
      blacklist = ""
    )
  } else {
    list(
      label = "Mouse GRCm39 / GENCODE M39 Bowtie2 CUT&RUN reference",
      bowtie2_index = "/grid/bsr/data/data/utama/genome/mouse_gencodeM39/bowtie2_index/GRCm39_gencodeM39",
      chrom_sizes = "/grid/bsr/data/data/utama/genome/mouse_gencodeM39/GRCm39.chrom.sizes",
      macs2_genome = "mm",
      blacklist = file.path(SCRIPTS_DIR, "test", "manifest_atac", "mm39-blacklist.bed")
    )
  }
}

atac_reference_resources <- function(project) {
  base <- cutrun_reference_resources(project)
  if (identical(genome_species(project), "human")) {
    base$macs2_genome <- "2.7e+9"
    c(base, list(
      effective_genome_size = "2913022398", homer_genome = "hg38",
      tss_bed = "/grid/bsr/data/data/utama/genome/human_gencode50/gencode.v50.annotation_onlyChrNoMito.bed"
    ))
  } else {
    base$macs2_genome <- "1.87e+9"
    c(base, list(
      effective_genome_size = "2654621783", homer_genome = "mm39",
      tss_bed = "/grid/bsr/data/data/utama/genome/mouse_gencodeM39/gencode.vM39.annotation_onlyChrNoMito.bed"
    ))
  }
}

project_reference_label <- function(project) {
  if (is_cutrun_project(project)) cutrun_reference_resources(project)$label else if (is_atac_project(project)) atac_reference_resources(project)$label else gencode_label(project)
}

adapter_choices_r1 <- function() {
  c(
    "Illumina Universal TruSeq RNA" = "AGATCGGAAGAGCACACGTCTGAACTCCAGTCA",
    "Nextera Transposase ATAC" = "CTGTCTCTTATACACATCTCCGAGCCCACGAGAC",
    "Illumina Small RNA 3 prime" = "TGGAATTCTCGG",
    "Illumina Small RNA 5 prime" = "GATCGTCGGACT",
    "Custom adapter" = "__custom__"
  )
}

adapter_choices_r2 <- function() {
  c(
    "Illumina Universal TruSeq RNA" = "AGATCGGAAGAGCGTCGTGTAGGGAAAGAGTGT",
    "Nextera Transposase ATAC" = "CTGTCTCTTATACACATCTGACGCTGCCGACGA",
    "Illumina Small RNA 3 prime" = "TGGAATTCTCGG",
    "Illumina Small RNA 5 prime" = "GATCGTCGGACT",
    "Custom adapter" = "__custom__"
  )
}

selected_adapter_value <- function(selected, custom) {
  selected <- selected %||% ""
  if (identical(selected, "__custom__")) return(trimws(custom %||% ""))
  selected
}

gencode_label <- function(project) {
  genome_resources(project)$label
}

resolve_read_path <- function(base, value, allow_absolute = TRUE) {
  value <- trimws(as.character(value %||% ""))
  if (!nzchar(value)) return("")
  if (isTRUE(allow_absolute) && startsWith(path.expand(value), "/")) return(path.expand(value))
  file.path(base, basename(value))
}

sample_fastq_pairs <- function(project, trimmed = FALSE) {
  design <- included_design_table(project)
  if (!NROW(design) || !"sample" %in% names(design) || !"filename" %in% names(design)) return(data.frame())
  base <- if (trimmed) file.path(project$data_dir, "cutadapt") else project$fastq_dir
  rows <- lapply(seq_len(NROW(design)), function(i) {
    sample <- as.character(design$sample[i])
    lanes <- trimws(unlist(strsplit(as.character(design$filename[i]), ";", fixed = TRUE)))
    lanes <- lanes[nzchar(lanes)]
    lane_parts <- lapply(lanes, function(lane) {
      parts <- trimws(unlist(strsplit(lane, ",", fixed = TRUE)))
      parts[nzchar(parts)]
    })
    lane_parts <- Filter(length, lane_parts)
    if (!length(lane_parts)) return(NULL)
    if (isTRUE(project$paired_end) && any(lengths(lane_parts) < 2L)) {
      stop("Every paired-end lane in filename must contain R1,R2. Separate pooled lanes with semicolons. Sample: ", sample)
    }
    lane_count <- length(lane_parts)
    if (isTRUE(trimmed) && lane_count > 1L) {
      r1 <- file.path(base, paste0(sample, "_R1.fastq.gz"))
      r2 <- if (project$paired_end) file.path(base, paste0(sample, "_R2.fastq.gz")) else r1
    } else if (isTRUE(trimmed)) {
      r1 <- resolve_read_path(base, lane_parts[[1]][1], allow_absolute = FALSE)
      r2 <- if (project$paired_end) resolve_read_path(base, lane_parts[[1]][2], allow_absolute = FALSE) else r1
    } else {
      r1 <- paste(vapply(lane_parts, function(parts) resolve_read_path(base, parts[1], allow_absolute = TRUE), character(1)), collapse = ",")
      r2 <- if (project$paired_end) paste(vapply(lane_parts, function(parts) resolve_read_path(base, parts[2], allow_absolute = TRUE), character(1)), collapse = ",") else r1
    }
    trimmed_r1 <- if (lane_count > 1L) file.path(project$data_dir, "cutadapt", paste0(sample, "_R1.fastq.gz")) else file.path(project$data_dir, "cutadapt", basename(lane_parts[[1]][1]))
    trimmed_r2 <- if (project$paired_end) {
      if (lane_count > 1L) file.path(project$data_dir, "cutadapt", paste0(sample, "_R2.fastq.gz")) else file.path(project$data_dir, "cutadapt", basename(lane_parts[[1]][2]))
    } else trimmed_r1
    data.frame(sample = sample, r1 = r1, r2 = r2, lane_count = lane_count, trimmed_r1 = trimmed_r1, trimmed_r2 = trimmed_r2, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(out)) data.frame() else out
}

split_fastq_path_list <- function(value) {
  paths <- trimws(unlist(strsplit(as.character(value %||% ""), ",", fixed = TRUE)))
  paths[nzchar(paths)]
}

parse_sbatch_job_id <- function(output) {
  m <- regexpr("Submitted batch job[[:space:]]+[0-9]+", output)
  if (m >= 0) return(sub(".*Submitted batch job[[:space:]]+", "", regmatches(output, m)))
  m <- regexpr("Job ID:[[:space:]]*[0-9]+", output)
  if (m >= 0) return(sub("Job ID:[[:space:]]*", "", regmatches(output, m)))
  m <- regexpr("job_id:[[:space:]]*[0-9]+", output)
  if (m >= 0) return(sub("job_id:[[:space:]]*", "", regmatches(output, m)))
  ""
}

submit_screen_message <- function(step, sample = "", job_id = "", input_mode = "", dependency_ids = character()) {
  label <- step
  if (nzchar(sample %||% "")) label <- paste(label, "-", sample)
  lines <- paste("Submitted", label)
  if (nzchar(job_id %||% "")) lines <- c(lines, paste("Job ID:", job_id))
  deps <- dependency_ids[nzchar(dependency_ids)]
  if (length(deps)) lines <- c(lines, paste("After jobs:", paste(deps, collapse = ", ")))
  lines <- c(lines, "Logs are available in the Logs tab.")
  paste(lines, collapse = "\n")
}

submit_sbatch <- function(project, step, script, args, log_name, input_mode = "", sample = "", target = "", reference = "", dependency_ids = character(0)) {
  log_dir <- file.path(dirname(project$data_dir), "log")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  tool_slug <- clean_name(log_name, clean_name(step, "job"))
  scope <- sample %||% ""
  if (!nzchar(scope)) scope <- input_mode %||% ""
  if (!nzchar(scope) && nzchar(target %||% "")) scope <- basename(target)
  if (!nzchar(scope)) scope <- "project"
  scope_slug <- clean_name(scope, "project")
  stdout <- file.path(log_dir, paste0("output_", tool_slug, "_", scope_slug, ".txt"))
  stderr <- file.path(log_dir, paste0("error_", tool_slug, "_", scope_slug, ".txt"))
  submit_log <- file.path(log_dir, paste0("submit_", tool_slug, "_", scope_slug, ".txt"))
  cat("", file = stdout)
  cat("", file = stderr)
  job_name <- job_name_for(project, step, sample)
  dep <- dependency_ids[nzchar(dependency_ids)]
  cmd <- c("sbatch", "--open-mode=append", "-J", job_name, "-e", stderr, "-o", stdout)
  if (length(dep)) cmd <- c(cmd, paste0("--dependency=afterok:", paste(dep, collapse = ":")))
  cmd <- c(cmd, script, args)
  writeLines(c(
    paste("time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste("project:", project$name),
    paste("step:", step),
    paste("sample:", sample %||% ""),
    paste("log_scope:", scope),
    paste("target:", target %||% ""),
    paste("dependencies:", paste(dep, collapse = ",")),
    paste("stdout:", stdout),
    paste("stderr:", stderr),
    paste("command:", paste(shQuote(cmd), collapse = " "))
  ), submit_log)
  if (Sys.which("sbatch") == "") {
    msg <- "ERROR: sbatch was not found. Run on the server to submit jobs."
    write(msg, submit_log, append = TRUE)
    save_job(project, step, cmd, paste(c(msg, if (nzchar(sample)) paste("sample:", sample), if (nzchar(target)) paste("target:", target), paste("stdout:", stdout), paste("stderr:", stderr), paste("submit_log:", submit_log)), collapse = "\n"))
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
  writeLines(c("", "sbatch response:", out_text, paste("job_id:", job_id %||% "")), submit_log, sep = "\n", useBytes = TRUE)
  manifest <- append_run_manifest(project, step, sample, cmd, target, input_mode, reference, job_id)
  save_job(project, step, cmd, paste(c(out_text, if (nzchar(job_id)) paste("job_id:", job_id), if (nzchar(sample)) paste("sample:", sample), if (nzchar(target)) paste("target:", target), if (nzchar(input_mode)) paste("input_mode:", input_mode), paste("stdout:", stdout), paste("stderr:", stderr), paste("submit_log:", submit_log), paste("manifest:", manifest)), collapse = "\n"))
  if (!nzchar(job_id)) {
    return(paste("ERROR: sbatch did not return a job ID for", step, "\nSubmit log:", submit_log, "\nsbatch response:\n", out_text))
  }
  submit_screen_message(step, sample, job_id, input_mode, dep)
}

submit_sbatch_wrap <- function(project, step, shell_command, log_name, input_mode = "", sample = "", target = "", reference = "", dependency_ids = character(0)) {
  log_dir <- file.path(dirname(project$data_dir), "log")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  tool_slug <- clean_name(log_name, clean_name(step, "job"))
  scope <- sample %||% ""
  if (!nzchar(scope)) scope <- input_mode %||% ""
  if (!nzchar(scope) && nzchar(target %||% "")) scope <- basename(target)
  if (!nzchar(scope)) scope <- "project"
  scope_slug <- clean_name(scope, "project")
  stdout <- file.path(log_dir, paste0("output_", tool_slug, "_", scope_slug, ".txt"))
  stderr <- file.path(log_dir, paste0("error_", tool_slug, "_", scope_slug, ".txt"))
  submit_log <- file.path(log_dir, paste0("submit_", tool_slug, "_", scope_slug, ".txt"))
  wrap_script <- file.path(log_dir, paste0("sbatch_", tool_slug, "_", scope_slug, ".sh"))
  cat("", file = stdout)
  cat("", file = stderr)
  job_name <- job_name_for(project, step, sample)
  dep <- dependency_ids[nzchar(dependency_ids)]
  cmd <- c("sbatch", "--open-mode=append", "-J", job_name, "-e", stderr, "-o", stdout)
  if (length(dep)) cmd <- c(cmd, paste0("--dependency=afterok:", paste(dep, collapse = ":")))
  writeLines(c(
    "#!/usr/bin/env bash",
    "set -euo pipefail",
    shell_command
  ), wrap_script)
  Sys.chmod(wrap_script, mode = "0755")
  cmd <- c(cmd, wrap_script)
  writeLines(c(
    paste("time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste("project:", project$name),
    paste("step:", step),
    paste("sample:", sample %||% ""),
    paste("log_scope:", scope),
    paste("target:", target %||% ""),
    paste("dependencies:", paste(dep, collapse = ",")),
    paste("stdout:", stdout),
    paste("stderr:", stderr),
    paste("sbatch_script:", wrap_script),
    paste("wrapped_command:", shell_command),
    paste("command:", paste(shQuote(cmd), collapse = " "))
  ), submit_log)
  if (Sys.which("sbatch") == "") {
    msg <- "ERROR: sbatch was not found. Run on the server to submit jobs."
    write(msg, submit_log, append = TRUE)
    save_job(project, step, cmd, paste(c(msg, if (nzchar(target)) paste("target:", target), paste("stdout:", stdout), paste("stderr:", stderr), paste("submit_log:", submit_log)), collapse = "\n"))
    return(msg)
  }
  out <- tryCatch(system2(cmd[1], cmd[-1], stdout = TRUE, stderr = TRUE), error = function(e) conditionMessage(e))
  out_text <- paste(out, collapse = "\n")
  job_id <- parse_sbatch_job_id(out_text)
  writeLines(c("", "sbatch response:", out_text, paste("job_id:", job_id %||% "")), submit_log, sep = "\n", useBytes = TRUE)
  manifest <- append_run_manifest(project, step, sample, cmd, target, input_mode, reference, job_id)
  save_job(project, step, cmd, paste(c(out_text, if (nzchar(job_id)) paste("job_id:", job_id), if (nzchar(sample)) paste("sample:", sample), if (nzchar(target)) paste("target:", target), if (nzchar(input_mode)) paste("input_mode:", input_mode), paste("stdout:", stdout), paste("stderr:", stderr), paste("submit_log:", submit_log), paste("manifest:", manifest)), collapse = "\n"))
  if (!nzchar(job_id)) {
    return(paste("ERROR: sbatch did not return a job ID for", step, "\nSubmit log:", submit_log, "\nsbatch response:\n", out_text))
  }
  submit_screen_message(step, sample, job_id, input_mode, dep)
}

write_featurecounts_matrix_script <- function(project) {
  log_dir <- file.path(dirname(project$data_dir), "log")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  script <- file.path(log_dir, "build_featurecounts_count_matrix.R")
  lines <- c(
    "args <- commandArgs(TRUE)",
    "feature_dir <- args[[1]]",
    "counts_dir <- args[[2]]",
    "dir.create(counts_dir, recursive = TRUE, showWarnings = FALSE)",
    "files <- list.files(feature_dir, pattern = '_counts\\\\.txt$', recursive = TRUE, full.names = TRUE)",
    "if (!length(files)) stop('No featureCounts *_counts.txt files found in ', feature_dir)",
    "read_one <- function(path) {",
    "  x <- read.table(path, sep='\\t', header=TRUE, quote='\"', comment.char='#', check.names=FALSE)",
    "  if (!nrow(x)) return(NULL)",
    "  gene_col <- intersect(c('Geneid','gene_id','gene_name','GeneID'), names(x))[1]",
    "  if (is.na(gene_col)) gene_col <- names(x)[1]",
    "  count_col <- tail(names(x), 1)",
    "  sample <- sub('_counts\\\\.txt$', '', basename(path))",
    "  data.frame(gene=x[[gene_col]], value=suppressWarnings(as.numeric(x[[count_col]])), sample=sample, stringsAsFactors=FALSE)",
    "}",
    "parts <- Filter(Negate(is.null), lapply(files, read_one))",
    "if (!length(parts)) stop('featureCounts files were empty or unreadable.')",
    "samples <- vapply(parts, function(x) unique(x$sample)[1], character(1))",
    "mat <- Reduce(function(a,b) merge(a,b, by='gene', all=TRUE), lapply(parts, function(x) { out <- x[, c('gene','value')]; names(out)[2] <- unique(x$sample)[1]; out }))",
    "mat[is.na(mat)] <- 0",
    "names(mat)[1] <- 'Geneid'",
    "write.table(mat, file=file.path(counts_dir, 'count_matrix.txt'), sep='\\t', row.names=FALSE, quote=FALSE)",
    "read_summary <- function(count_path) {",
    "  summary_path <- paste0(count_path, '.summary')",
    "  sample <- sub('_counts\\\\.txt$', '', basename(count_path))",
    "  if (!file.exists(summary_path)) {",
    "    return(data.frame(Status='Assigned', value=sum(read_one(count_path)$value, na.rm=TRUE), sample=sample, stringsAsFactors=FALSE))",
    "  }",
    "  s <- read.table(summary_path, sep='\\t', header=TRUE, quote='\"', comment.char='', check.names=FALSE)",
    "  if (!nrow(s)) return(NULL)",
    "  names(s)[1] <- 'Status'",
    "  value_col <- setdiff(names(s), 'Status')[1]",
    "  data.frame(Status=s$Status, value=suppressWarnings(as.numeric(s[[value_col]])), sample=sample, stringsAsFactors=FALSE)",
    "}",
    "summary_parts <- Filter(Negate(is.null), lapply(files, read_summary))",
    "if (length(summary_parts)) {",
    "  summary <- Reduce(function(a,b) merge(a,b, by='Status', all=TRUE), lapply(summary_parts, function(x) { out <- x[, c('Status','value')]; names(out)[2] <- unique(x$sample)[1]; out }))",
    "  summary[is.na(summary)] <- 0",
    "  write.table(summary, file=file.path(counts_dir, 'featurecounts_summary.txt'), sep='\\t', row.names=FALSE, quote=FALSE)",
    "}"
  )
  writeLines(lines, script)
  script
}

build_featurecounts_matrix_now <- function(project) {
  feature_dir <- file.path(project$data_dir, "featurecounts")
  counts_dir <- file.path(project$data_dir, "counts")
  dir.create(counts_dir, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(feature_dir, pattern = "_counts\\.txt$", recursive = TRUE, full.names = TRUE)
  if (!length(files)) stop("No featureCounts *_counts.txt files found in ", feature_dir)
  read_one <- function(path) {
    x <- utils::read.table(path, sep = "\t", header = TRUE, quote = "\"", comment.char = "#", check.names = FALSE)
    if (!NROW(x)) return(NULL)
    gene_col <- intersect(c("Geneid", "gene_id", "gene_name", "GeneID"), names(x))[1]
    if (is.na(gene_col)) gene_col <- names(x)[1]
    count_col <- tail(names(x), 1)
    sample <- sub("_counts\\.txt$", "", basename(path))
    data.frame(gene = x[[gene_col]], value = suppressWarnings(as.numeric(x[[count_col]])), sample = sample, stringsAsFactors = FALSE)
  }
  parts <- Filter(Negate(is.null), lapply(files, read_one))
  if (!length(parts)) stop("featureCounts files were empty or unreadable.")
  samples <- vapply(parts, function(x) unique(x$sample)[1], character(1))
  mat <- Reduce(function(a, b) merge(a, b, by = "gene", all = TRUE), lapply(parts, function(x) {
    out <- x[, c("gene", "value")]
    names(out)[2] <- unique(x$sample)[1]
    out
  }))
  mat[is.na(mat)] <- 0
  names(mat)[1] <- "Geneid"
  utils::write.table(mat, file = file.path(counts_dir, "count_matrix.txt"), sep = "\t", row.names = FALSE, quote = FALSE)
  read_summary <- function(count_path) {
    summary_path <- paste0(count_path, ".summary")
    sample <- sub("_counts\\.txt$", "", basename(count_path))
    if (!file.exists(summary_path)) {
      return(data.frame(Status = "Assigned", value = sum(read_one(count_path)$value, na.rm = TRUE), sample = sample, stringsAsFactors = FALSE))
    }
    s <- utils::read.table(summary_path, sep = "\t", header = TRUE, quote = "\"", comment.char = "", check.names = FALSE)
    if (!NROW(s)) return(NULL)
    names(s)[1] <- "Status"
    value_col <- setdiff(names(s), "Status")[1]
    data.frame(Status = s$Status, value = suppressWarnings(as.numeric(s[[value_col]])), sample = sample, stringsAsFactors = FALSE)
  }
  summary_parts <- Filter(Negate(is.null), lapply(files, read_summary))
  if (length(summary_parts)) {
    summary <- Reduce(function(a, b) merge(a, b, by = "Status", all = TRUE), lapply(summary_parts, function(x) {
      out <- x[, c("Status", "value")]
      names(out)[2] <- unique(x$sample)[1]
      out
    }))
    summary[is.na(summary)] <- 0
    utils::write.table(summary, file = file.path(counts_dir, "featurecounts_summary.txt"), sep = "\t", row.names = FALSE, quote = FALSE)
  }
  file.path(counts_dir, "count_matrix.txt")
}

write_gene_name_count_matrix_script <- function(project) {
  log_dir <- file.path(dirname(project$data_dir), "log")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  script <- file.path(log_dir, "build_gene_name_count_matrix.R")
  lines <- c(
    "args <- commandArgs(TRUE)",
    "count_matrix <- args[[1]]",
    "gtf <- args[[2]]",
    "out_file <- args[[3]]",
    "if (!file.exists(count_matrix)) stop('Count matrix not found: ', count_matrix)",
    "if (!file.exists(gtf)) stop('GTF not found: ', gtf)",
    "x <- read.table(count_matrix, sep='\\t', header=TRUE, quote='\"', comment.char='', check.names=FALSE)",
    "gene_col <- intersect(c('Geneid','gene_id','GeneID'), names(x))[1]",
    "if (is.na(gene_col)) gene_col <- names(x)[1]",
    "ids <- sub('\\\\..*$', '', as.character(x[[gene_col]]))",
    "map <- new.env(parent=emptyenv())",
    "con <- file(gtf, open='r')",
    "on.exit(close(con), add=TRUE)",
    "repeat {",
    "  lines <- readLines(con, n=100000, warn=FALSE)",
    "  if (!length(lines)) break",
    "  lines <- lines[grepl('\\\\tgene\\\\t', lines, fixed=FALSE)]",
    "  if (!length(lines)) next",
    "  gid <- sub('.*gene_id \"([^\"]+)\".*', '\\\\1', lines)",
    "  gname <- sub('.*gene_name \"([^\"]+)\".*', '\\\\1', lines)",
    "  keep <- gid != lines & gname != lines & nzchar(gid) & nzchar(gname)",
    "  if (any(keep)) {",
    "    gid <- sub('\\\\..*$', '', gid[keep])",
    "    gname <- gname[keep]",
    "    for (i in seq_along(gid)) if (!exists(gid[[i]], envir=map, inherits=FALSE)) assign(gid[[i]], gname[[i]], envir=map)",
    "  }",
    "}",
    "gene_name <- vapply(ids, function(id) if (exists(id, envir=map, inherits=FALSE)) get(id, envir=map, inherits=FALSE) else id, character(1))",
    "sample_cols <- setdiff(names(x), gene_col)",
    "for (col in sample_cols) x[[col]] <- suppressWarnings(as.numeric(x[[col]]))",
    "agg <- aggregate(x[, sample_cols, drop=FALSE], by=list(gene_name), FUN=sum, na.rm=TRUE)",
    "names(agg)[1] <- 'Geneid'",
    "agg <- agg[order(agg$Geneid), , drop=FALSE]",
    "dir.create(dirname(out_file), recursive=TRUE, showWarnings=FALSE)",
    "write.table(agg, out_file, sep='\\t', quote=FALSE, row.names=FALSE)"
  )
  writeLines(lines, script)
  script
}

write_quant_matrix_script <- function(project, tool = c("rsem", "kallisto")) {
  tool <- match.arg(tool)
  log_dir <- file.path(dirname(project$data_dir), "log")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  script <- file.path(log_dir, paste0("build_", tool, "_matrices.R"))
  if (identical(tool, "rsem")) {
    lines <- c(
      "args <- commandArgs(TRUE)",
      "quant_dir <- args[[1]]",
      "counts_dir <- args[[2]]",
      "dir.create(counts_dir, recursive = TRUE, showWarnings = FALSE)",
      "gene_files <- list.files(quant_dir, pattern = '\\\\.genes\\\\.results$', recursive = TRUE, full.names = TRUE)",
      "isoform_files <- list.files(quant_dir, pattern = '\\\\.isoforms\\\\.results$', recursive = TRUE, full.names = TRUE)",
      "if (!length(gene_files) && !length(isoform_files)) stop('No RSEM *.genes.results or *.isoforms.results files found in ', quant_dir)",
      "read_metric <- function(path, metric, key_cols) {",
      "  x <- read.table(path, sep='\\t', header=TRUE, quote='\"', comment.char='', check.names=FALSE)",
      "  sample <- sub('\\\\.(genes|isoforms)\\\\.results$', '', basename(path))",
      "  key_cols <- key_cols[key_cols %in% names(x)]",
      "  if (!length(key_cols)) key_cols <- names(x)[1]",
      "  if (!metric %in% names(x)) return(NULL)",
      "  out <- x[, key_cols, drop=FALSE]",
      "  out[[sample]] <- suppressWarnings(as.numeric(x[[metric]]))",
      "  out",
      "}",
      "write_metric <- function(files, metric, outfile, key_cols) {",
      "  if (!length(files)) return(invisible(FALSE))",
      "  parts <- Filter(Negate(is.null), lapply(files, read_metric, metric=metric, key_cols=key_cols))",
      "  if (!length(parts)) return(invisible(FALSE))",
      "  merge_by <- intersect(key_cols, names(parts[[1]]))",
      "  if (!length(merge_by)) merge_by <- names(parts[[1]])[1]",
      "  mat <- Reduce(function(a,b) merge(a,b, by=merge_by, all=TRUE), parts)",
      "  mat[is.na(mat)] <- 0",
      "  write.table(mat, file=file.path(counts_dir, outfile), sep='\\t', row.names=FALSE, quote=FALSE)",
      "}",
      "write_metric(gene_files, 'expected_count', 'rsem_expected_count_matrix.txt', c('gene_id'))",
      "write_metric(gene_files, 'TPM', 'rsem_tpm_matrix.txt', c('gene_id'))",
      "write_metric(gene_files, 'FPKM', 'rsem_fpkm_matrix.txt', c('gene_id'))",
      "write_metric(isoform_files, 'expected_count', 'rsem_isoform_expected_count_matrix.txt', c('transcript_id','gene_id'))",
      "write_metric(isoform_files, 'TPM', 'rsem_isoform_tpm_matrix.txt', c('transcript_id','gene_id'))",
      "write_metric(isoform_files, 'FPKM', 'rsem_isoform_fpkm_matrix.txt', c('transcript_id','gene_id'))"
    )
  } else {
    lines <- c(
      "args <- commandArgs(TRUE)",
      "quant_dir <- args[[1]]",
      "counts_dir <- args[[2]]",
      "dir.create(counts_dir, recursive = TRUE, showWarnings = FALSE)",
      "files <- list.files(quant_dir, pattern = 'abundance\\\\.tsv$', recursive = TRUE, full.names = TRUE)",
      "if (!length(files)) stop('No Kallisto abundance.tsv files found in ', quant_dir)",
      "read_metric <- function(path, metric) {",
      "  x <- read.table(path, sep='\\t', header=TRUE, quote='\"', comment.char='', check.names=FALSE)",
      "  sample <- basename(dirname(path))",
      "  id_col <- intersect(c('target_id','transcript_id','gene_id'), names(x))[1]",
      "  if (is.na(id_col)) id_col <- names(x)[1]",
      "  if (!metric %in% names(x)) return(NULL)",
      "  out <- data.frame(gene=x[[id_col]], value=suppressWarnings(as.numeric(x[[metric]])), stringsAsFactors=FALSE)",
      "  names(out)[2] <- sample",
      "  out",
      "}",
      "write_metric <- function(metric, outfile) {",
      "  parts <- Filter(Negate(is.null), lapply(files, read_metric, metric=metric))",
      "  if (!length(parts)) return(invisible(FALSE))",
      "  mat <- Reduce(function(a,b) merge(a,b, by='gene', all=TRUE), parts)",
      "  mat[is.na(mat)] <- 0",
      "  names(mat)[1] <- 'target_id'",
      "  write.table(mat, file=file.path(counts_dir, outfile), sep='\\t', row.names=FALSE, quote=FALSE)",
      "}",
      "write_metric('est_counts', 'kallisto_est_counts_matrix.txt')",
      "write_metric('tpm', 'kallisto_tpm_matrix.txt')"
    )
  }
  writeLines(lines, script)
  script
}

missing_read_message <- function(project, pairs, trimmed = FALSE) {
  if (!file.exists(project$design_matrix_path)) {
    return(paste(
      "No design_matrix.txt was found for this project.",
      paste("Expected:", project$design_matrix_path),
      "Create or save the design matrix in the Design Matrix tab before running this step.",
      sep = "\n"
    ))
  }
  if (isTRUE(trimmed)) {
    active_msg <- active_upstream_message(project, "Cutadapt", "steps that use trimmed reads")
    if (nzchar(active_msg)) return(active_msg)
  }
  read_bases <- if (isTRUE(trimmed)) file.path(project$data_dir, "cutadapt") else project_fastq_dirs(project)
  missing_bases <- read_bases[!dir.exists(read_bases)]
  if (!length(read_bases) || length(missing_bases)) {
    return(paste(
      if (isTRUE(trimmed)) "Run cutadapt successfully before using trimmed reads. The trimmed FASTQ folder is missing or does not exist." else "The raw FASTQ folder is missing or does not exist.",
      paste("FASTQ folder(s):", paste(if (length(missing_bases)) missing_bases else read_bases, collapse = ", ")),
      "Choose the correct raw FASTQ folder(s) in project setup, then save/create the project again.",
      sep = "\n"
    ))
  }
  if (!NROW(pairs)) return("No samples/read files found in design_matrix.txt.")
  reads <- unique(unlist(lapply(c(pairs$r1, if (isTRUE(project$paired_end)) pairs$r2 else character(0)), split_fastq_path_list), use.names = FALSE))
  missing <- reads[nzchar(reads) & !file.exists(reads)]
  if (length(missing)) {
    return(paste(c("These read files do not exist. Check the FASTQ folder and design_matrix.txt filenames:", missing), collapse = "\n"))
  }
  ""
}

sample_submission_plan <- function(project, step, target_list) {
  target_list <- target_list[intersect(names(target_list), project_samples(project))]
  samples <- names(target_list)
  if (!length(samples)) {
    return(list(samples = character(0), active = character(0), complete = character(0), message = "No samples were available for this step."))
  }
  jobs <- job_history(project)
  active_jobs <- if (NROW(jobs) && all(c("step", "slurm_state") %in% names(jobs))) {
    jobs[canonical_job_step(jobs$step) == canonical_job_step(step) & jobs$slurm_state %in% active_slurm_states(), , drop = FALSE]
  } else data.frame()
  active <- character(0)
  complete <- character(0)
  retry <- character(0)
  submit <- character(0)
  min_size <- minimum_expected_bytes(canonical_job_step(step))
  retry_states <- c("CANCELLED", "CANCELLED+", "CA", "FAILED", "TIMEOUT", "NODE_FAIL", "OUT_OF_MEMORY", "PREEMPTED", "BOOT_FAIL")
  for (sample in samples) {
    sample_active <- FALSE
    if (NROW(active_jobs)) {
      if ("sample" %in% names(active_jobs) && any(nzchar(active_jobs$sample))) {
        sample_active <- any(active_jobs$sample == sample)
      } else {
        sample_active <- TRUE
      }
    }
    targets <- target_list[[sample]]
    targets <- targets[nzchar(targets)]
    sample_complete <- length(targets) > 0 && all(file.exists(targets)) && all(vapply(targets, file_size_for, numeric(1)) >= min_size)
    latest <- latest_job_for_sample(jobs, step, sample)
    latest_state <- if (NROW(latest) && "slurm_state" %in% names(latest)) latest$slurm_state[1] else ""
    latest_time <- if (NROW(latest) && "time" %in% names(latest)) {
      suppressWarnings(as.POSIXct(latest$time[1]))
    } else {
      as.POSIXct(NA)
    }
    target_mtimes <- if (sample_complete) file.info(targets)$mtime else as.POSIXct(character(0))
    completion_is_current <- sample_complete && (
      is.na(latest_time) ||
        (length(target_mtimes) > 0 && all(!is.na(target_mtimes)) && all(target_mtimes >= latest_time))
    )
    deleted_status <- latest_deleted_status(project, step, sample)
    should_retry <- latest_state %in% retry_states || grepl(", Deleted$", deleted_status)
    if (sample_active) active <- c(active, sample)
    else if (completion_is_current) complete <- c(complete, sample)
    else if (should_retry) {
      retry <- c(retry, sample)
      submit <- c(submit, sample)
    }
    else if (sample_complete) complete <- c(complete, sample)
    else submit <- c(submit, sample)
  }
  notes <- character(0)
  if (length(active)) notes <- c(notes, paste("Skipped active samples:", paste(active, collapse = ", ")))
  if (length(complete)) notes <- c(notes, paste("Skipped completed samples:", paste(complete, collapse = ", ")))
  if (length(retry)) notes <- c(notes, paste("Resubmitting failed/cancelled/deleted samples:", paste(unique(retry), collapse = ", ")))
  if (!length(submit) && !length(active)) notes <- c(notes, paste("All samples are already complete for", step, ". Delete selected sample data first if you want to force a rerun."))
  list(samples = unique(submit), active = unique(active), complete = unique(complete), retry = unique(retry), message = paste(notes, collapse = "\n"))
}

append_plan_message <- function(messages, plan) {
  notes <- trimws(plan$message %||% "")
  if (nzchar(notes)) c(messages, notes) else messages
}

submit_fastqc_jobs <- function(project, trimmed = FALSE) {
  outdir <- file.path(project$data_dir, if (trimmed) "fastqc_cutadapt" else "fastqc")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  pairs <- sample_fastq_pairs(project, trimmed)
  msg <- missing_read_message(project, pairs, trimmed)
  if (nzchar(msg)) return(record_preflight_failure(project, "FastQC", msg, "fastQC"))
  targets <- split(seq_len(NROW(pairs)), pairs$sample)
  target_list <- lapply(targets, function(idx) {
    row <- pairs[idx[[1]], , drop = FALSE]
    reads <- unique(unlist(lapply(c(row$r1[1], if (project$paired_end) row$r2[1] else character(0)), split_fastq_path_list), use.names = FALSE))
    fastqc_expected_targets(reads, outdir)
  })
  plan <- sample_submission_plan(project, "FastQC", target_list)
  if (!length(plan$samples)) return(plan$message)
  pairs <- pairs[pairs$sample %in% plan$samples, , drop = FALSE]
  script <- file.path(SCRIPTS_DIR, "FastQC", "qsub_fastqc.sh")
  runner <- file.path(SCRIPTS_DIR, "FastQC", "fastqc.sh")
  missing_scripts <- c(script, runner)
  missing_scripts <- missing_scripts[!file.exists(missing_scripts)]
  if (length(missing_scripts)) return(record_preflight_failure(project, "FastQC", paste("Required FastQC scripts are missing:", paste(missing_scripts, collapse = ", ")), "fastQC"))
  input_mode <- if (trimmed) "trimmed reads" else "raw reads"
  commands <- vapply(seq_len(NROW(pairs)), function(i) {
    reads <- unique(unlist(lapply(c(pairs$r1[i], if (project$paired_end) pairs$r2[i] else character(0)), split_fastq_path_list), use.names = FALSE))
    reads <- reads[nzchar(reads)]
    target <- file.path(outdir, sub(fastq_suffix_regex, "_fastqc.html", basename(reads[[1]]), ignore.case = TRUE))
    submit_sbatch(project, "FastQC", script, c(paste(reads, collapse = ","), outdir, project$name, runner), "fastQC", input_mode, sample = pairs$sample[i], target = target)
  }, character(1))
  paste(append_plan_message(commands, plan), collapse = "\n")
}

submit_cutadapt_jobs <- function(project, adapter1, adapter2, min_length) {
  outdir <- file.path(project$data_dir, "cutadapt")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  pairs <- sample_fastq_pairs(project, FALSE)
  msg <- missing_read_message(project, pairs, FALSE)
  if (nzchar(msg)) return(record_preflight_failure(project, "Cutadapt", msg, "cutadapt"))
  target_list <- lapply(split(seq_len(NROW(pairs)), pairs$sample), function(idx) {
    row <- pairs[idx[[1]], , drop = FALSE]
    unique(c(row$trimmed_r1[1], if (project$paired_end) row$trimmed_r2[1] else character(0)))
  })
  plan <- sample_submission_plan(project, "Cutadapt", target_list)
  if (!length(plan$samples)) return(plan$message)
  pairs <- pairs[pairs$sample %in% plan$samples, , drop = FALSE]
  script <- file.path(SCRIPTS_DIR, if (project$paired_end) "cutadapt_PE/qsub_cutadapt_PE.sh" else "cutadapt_SE/qsub_cutadapt_SE.sh")
  runner <- if (project$paired_end) file.path(SCRIPTS_DIR, "cutadapt_PE/cutadapt_PE.sh") else ""
  missing_scripts <- c(script, if (project$paired_end) runner else character(0))
  missing_scripts <- missing_scripts[!file.exists(missing_scripts)]
  if (length(missing_scripts)) return(record_preflight_failure(project, "Cutadapt", paste("Required Cutadapt scripts are missing:", paste(missing_scripts, collapse = ", ")), "cutadapt"))
  input_mode <- "raw reads"
  messages <- apply(pairs, 1, function(row) {
    trimmed1 <- row[["trimmed_r1"]]
    trimmed2 <- if (project$paired_end) row[["trimmed_r2"]] else trimmed1
    read2 <- if (project$paired_end) row[["r2"]] else row[["r1"]]
    args <- c(min_length, adapter1, adapter2, trimmed1, trimmed2, row[["r1"]], read2, project$name)
    if (project$paired_end) args <- c(args, runner)
    submit_sbatch(project, "Cutadapt", script, args, "cutadapt", input_mode, sample = row[["sample"]], target = trimmed1)
  })
  paste(append_plan_message(messages, plan), collapse = "\n")
}

cutrun_design <- function(project) {
  design <- safe_read_table(project$design_matrix_path)
  if (!NROW(design)) return(design)
  if (!"sample" %in% names(design)) names(design)[1] <- "sample"
  if ("include" %in% names(design)) {
    design <- design[vapply(design$include, as_design_bool, logical(1)), , drop = FALSE]
  }
  for (col in c("cell_type", "mark", "target", "target_class", "seacr_stringency", "condition", "replicate", "control_sample")) {
    if (!col %in% names(design)) design[[col]] <- ""
  }
  design <- infer_cutrun_metadata(design)
  missing_mark <- !nzchar(trimws(as.character(design$mark)))
  design$mark[missing_mark] <- as.character(design$target[missing_mark])
  missing_target <- !nzchar(trimws(as.character(design$target)))
  design$target[missing_target] <- as.character(design$mark[missing_target])
  design$cell_type[!nzchar(trimws(as.character(design$cell_type)))] <- "all"
  design
}

cutrun_seacr_stringency_for <- function(project, sample, default = "stringent") {
  default <- tolower(trimws(as.character(default %||% "stringent")))
  if (!default %in% c("stringent", "relaxed")) default <- "stringent"
  design <- cutrun_design(project)
  if (!NROW(design) || !sample %in% design$sample) return(default)
  value <- tolower(trimws(as.character(design$seacr_stringency[match(sample, design$sample)] %||% "auto")))
  if (value %in% c("stringent", "relaxed")) value else default
}

cutrun_seacr_combo_key <- function(norm = "non", stringency = "stringent") {
  norm <- selected_choice(tolower(trimws(as.character(norm %||% "non"))), c("norm", "non"), "non")
  stringency <- selected_choice(tolower(trimws(as.character(stringency %||% "stringent"))), c("stringent", "relaxed"), "stringent")
  paste(norm, stringency, sep = "_")
}

cutrun_seacr_combo_dir <- function(project, norm = "non", stringency = "stringent", sensitivity = FALSE) {
  root <- if (isTRUE(sensitivity)) "cutrun_dedup_sensitivity" else "seacr"
  file.path(project$data_dir, root, cutrun_seacr_combo_key(norm, stringency))
}

cutrun_macs2_peak_type_for <- function(project, sample, default = "auto") {
  default <- tolower(trimws(as.character(default %||% "auto")))
  if (default %in% c("narrow", "broad")) return(default)
  design <- cutrun_design(project)
  if (!NROW(design) || !sample %in% design$sample) return("narrow")
  target_class <- tolower(trimws(as.character(design$target_class[match(sample, design$sample)] %||% "tf_or_other")))
  if (identical(target_class, "histone_broad")) "broad" else "narrow"
}

cutrun_control_like <- function(x) {
  grepl("igg|input|control", tolower(as.character(x %||% "")))
}

cutrun_target_design <- function(project, include_controls = FALSE) {
  design <- cutrun_design(project)
  if (!NROW(design)) return(design)
  if (isTRUE(include_controls)) return(design)
  design[!cutrun_control_like(design$target), , drop = FALSE]
}

cutrun_control_sample_for <- function(project, sample) {
  design <- cutrun_design(project)
  if (!NROW(design) || !sample %in% design$sample) return("")
  row <- design[design$sample == sample, , drop = FALSE][1, , drop = FALSE]
  explicit <- trimws(as.character(row$control_sample %||% ""))
  if (nzchar(explicit) && explicit %in% design$sample) return(explicit)
  controls <- design[cutrun_control_like(design$target), , drop = FALSE]
  if (!NROW(controls)) return("")
  cell <- trimws(as.character(row$cell_type %||% ""))
  condition <- trimws(as.character(row$condition %||% ""))
  control_cell <- trimws(as.character(controls$cell_type))
  control_condition <- trimws(as.character(controls$condition))
  exact <- controls[control_cell == cell & control_condition == condition, , drop = FALSE]
  if (NROW(exact) == 1L) return(as.character(exact$sample[1]))
  cell_default <- controls[control_cell == cell & !nzchar(control_condition), , drop = FALSE]
  if (NROW(cell_default) == 1L) return(as.character(cell_default$sample[1]))
  global_condition <- controls[control_cell %in% c("", "all") & control_condition == condition, , drop = FALSE]
  if (NROW(global_condition) == 1L) return(as.character(global_condition$sample[1]))
  global <- controls[control_cell %in% c("", "all") & !nzchar(control_condition), , drop = FALSE]
  if (NROW(global) == 1L) return(as.character(global$sample[1]))
  ""
}

cutrun_bowtie2_bam <- function(project, sample) {
  file.path(project$data_dir, "bowtie2", sample, paste0(sample, "Aligned.sortedByCoord.out.bam"))
}

cutrun_bowtie2_complete_marker <- function(project, sample) {
  file.path(project$data_dir, "bowtie2", sample, paste0(sample, "_alignment_summary.txt"))
}

cutrun_bowtie2_bedgraph <- function(project, sample) {
  sample_dir <- file.path(project$data_dir, "bowtie2", sample)
  summary_path <- file.path(sample_dir, paste0(sample, "_alignment_summary.txt"))
  if (file.exists(summary_path)) {
    lines <- read_metric_lines(summary_path)
    hit <- grep("^normalized_bedgraph\\t", lines, value = TRUE)
    if (length(hit)) {
      path <- strsplit(hit[[1]], "\t", fixed = TRUE)[[1]][2] %||% ""
      if (nzchar(path) && file.exists(path)) return(path)
    }
  }
  for (suffix in c("_fragments.spikein.bedgraph", "_fragments.CPM.bedgraph", "_fragments.raw.bedgraph")) {
    path <- file.path(sample_dir, paste0(sample, suffix))
    if (file.exists(path)) return(path)
  }
  file.path(sample_dir, paste0(sample, "_fragments.raw.bedgraph"))
}

cutrun_seacr_bedgraph <- function(project, sample, norm = "non") {
  sample_dir <- file.path(project$data_dir, "bowtie2", sample)
  norm <- selected_choice(tolower(trimws(as.character(norm %||% "non"))), c("norm", "non"), "non")
  if (identical(norm, "norm")) {
    raw <- file.path(sample_dir, paste0(sample, "_fragments.raw.bedgraph"))
    return(raw)
  }
  cutrun_bowtie2_bedgraph(project, sample)
}

cutrun_bowtie2_fragments <- function(project, sample) {
  file.path(project$data_dir, "bowtie2", sample, paste0(sample, "_fragments.bed"))
}

cutrun_postprocess_status_table <- function(project) {
  design <- cutrun_design(project)
  if (!NROW(design) || !"sample" %in% names(design)) return(data.frame())
  rows <- lapply(as.character(design$sample), function(sample) {
    prefix <- file.path(project$data_dir, "bowtie2", sample, sample)
    aligned_bam <- paste0(prefix, "Aligned.sortedByCoord.out.bam")
    dedup_bam <- paste0(prefix, "Aligned.sortedByCoord_removeDup.out.bam")
    fragments <- paste0(prefix, "_fragments.bed")
    summary <- paste0(prefix, "_alignment_summary.txt")
    sample_pattern <- gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", sample, perl = TRUE)
    signal_bedgraphs <- list.files(dirname(prefix), pattern = paste0("^", sample_pattern, "_fragments\\.(raw|CPM|spikein)\\.bedgraph$"), full.names = TRUE)
    signal_bigwigs <- list.files(dirname(prefix), pattern = paste0("^", sample_pattern, "_fragments\\.(raw|CPM|spikein)\\.bw$"), full.names = TRUE)
    bedgraph_ok <- length(signal_bedgraphs) > 0 && any(vapply(signal_bedgraphs, file_size_for, numeric(1)) > 0)
    bigwig_ok <- length(signal_bigwigs) > 0 && any(vapply(signal_bigwigs, file_size_for, numeric(1)) > 0)
    repairable <- file_size_for(aligned_bam) > 0 && file_size_for(dedup_bam) > 0
    complete <- file_size_for(fragments) > 0 && bedgraph_ok && bigwig_ok && file_size_for(summary) >= minimum_expected_bytes("Bowtie2")
    issues <- c(
      if (file_size_for(fragments) <= 0) "fragments BED",
      if (!bedgraph_ok) "signal bedGraph",
      if (!bigwig_ok) "signal bigWig",
      if (file_size_for(summary) < minimum_expected_bytes("Bowtie2")) "alignment summary"
    )
    data.frame(
      sample = sample,
      status = if (complete) "Complete" else if (repairable) "Repair available" else "Full Bowtie2 required",
      issues = if (length(issues)) paste(issues, collapse = ", ") else "None",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

cutrun_missing_bowtie2_message <- function(project, step = "this step") {
  active_msg <- active_upstream_message(project, "Bowtie2", step)
  if (nzchar(active_msg)) return(active_msg)
  design <- cutrun_design(project)
  if (!NROW(design)) return("No samples found in design_matrix.txt. Create or fix the design matrix before running CUT&RUN peak calling.")
  target_design <- cutrun_target_design(project, include_controls = FALSE)
  required_samples <- if (step %in% c("SEACR", "MACS2", "MACS2 (optional)")) {
    controls <- vapply(as.character(target_design$sample), function(sample) cutrun_control_sample_for(project, sample), character(1))
    unique(c(as.character(target_design$sample), controls[nzchar(controls)]))
  } else {
    as.character(design$sample)
  }
  if (!length(required_samples)) return(paste("No non-control CUT&RUN target samples were found for", step, ". Fill the target column and avoid labeling every row as IgG/input/control."))
  files <- if (step %in% c("MACS2", "MACS2 (optional)")) {
    vapply(required_samples, function(sample) cutrun_bowtie2_bam(project, sample), character(1))
  } else {
    vapply(required_samples, function(sample) cutrun_bowtie2_bedgraph(project, sample), character(1))
  }
  if (identical(step, "SEACR")) {
    target_fragments <- vapply(as.character(target_design$sample), function(sample) cutrun_bowtie2_fragments(project, sample), character(1))
    files <- c(files, target_fragments)
  }
  missing <- files[!file.exists(files) | vapply(files, file_size_for, numeric(1)) <= 0]
  if (length(missing)) {
    return(paste(c(
      paste("Run Bowtie2 successfully before", step, "so required target/control alignment files exist."),
      "Missing or empty files:",
      missing
    ), collapse = "\n"))
  }
  ""
}

extract_bowtie2_metrics <- function(project, sample) {
  summary_path <- file.path(project$data_dir, "bowtie2", sample, paste0(sample, "_alignment_summary.txt"))
  lines <- read_metric_lines(summary_path)
  if (!length(lines)) return(c(`Mapped reads` = "", `Spike-in reads` = "", `Duplicate %` = ""))
  one <- function(key) {
    hit <- grep(paste0("^", key, "\\t"), lines, value = TRUE)
    if (!length(hit)) return("")
    strsplit(hit[[1]], "\t", fixed = TRUE)[[1]][2] %||% ""
  }
  dup <- suppressWarnings(as.numeric(one("duplicate_fraction")))
  dup_text <- if (is.finite(dup)) sprintf("%.1f%%", dup * 100) else ""
  c(
    `Mapped reads` = one("mapped_reads"),
    `Spike-in reads` = one("spikein_mapped_reads"),
    `Duplicate %` = dup_text
  )
}

submit_atac_postprocess_jobs <- function(project, samples) {
  samples <- unique(trimws(as.character(samples %||% character(0))))
  samples <- samples[nzchar(samples)]
  if (!length(samples)) return(record_preflight_failure(project, "Bowtie2", "Select at least one repairable ATAC-seq sample.", "bowtie2_postprocess"))
  status <- atac_postprocess_status_table(project)
  eligible <- if (NROW(status)) status$sample[status$status != "Full Bowtie2 required"] else character(0)
  invalid <- setdiff(samples, eligible)
  if (length(invalid)) return(record_preflight_failure(project, "Bowtie2", paste("These samples are not currently repairable from an aligned BAM:", paste(invalid, collapse = ", ")), "bowtie2_postprocess"))
  res <- atac_reference_resources(project)
  full_qsub <- file.path(SCRIPTS_DIR, "bowtie2", "qsub_postprocess_atac_PE.sh")
  full_runner <- file.path(SCRIPTS_DIR, "bowtie2", "postprocess_atac_PE.sh")
  bigwig_qsub <- file.path(SCRIPTS_DIR, "bowtie2", "qsub_atac_bigwig.sh")
  bigwig_runner <- file.path(SCRIPTS_DIR, "bowtie2", "atac_bigwig.sh")
  required <- c(full_qsub, full_runner, bigwig_qsub, bigwig_runner, res$chrom_sizes)
  missing <- required[!file.exists(required)]
  if (length(missing)) return(record_preflight_failure(project, "Bowtie2", paste("ATAC post-alignment repair resources are missing:", paste(missing, collapse = ", ")), "bowtie2_postprocess"))
  messages <- vapply(samples, function(sample) {
    prefix <- file.path(project$data_dir, "bowtie2", sample, sample)
    target <- paste0(prefix, "_postprocess_summary.txt")
    sample_status <- status[status$sample == sample, , drop = FALSE][1, , drop = FALSE]
    issues <- if (NROW(sample_status) && !identical(sample_status$issues, "None")) strsplit(sample_status$issues, ", ", fixed = TRUE)[[1]] else character(0)
    bigwig_only_issues <- c("deduplicated BAM index", "bigWig", "alignment summary", "CPM bigWig normalization")
    dedup_bam <- paste0(prefix, "Aligned.sortedByCoord_removeDup.out.bam")
    bigwig_only <- identical(sample_status$status, "Repair available") && length(issues) > 0 && all(issues %in% bigwig_only_issues) && file.exists(dedup_bam) && file_size_for(dedup_bam) > 0
    qsub <- if (bigwig_only) bigwig_qsub else full_qsub
    runner <- if (bigwig_only) bigwig_runner else full_runner
    args <- if (bigwig_only) c(prefix, res$bowtie2_index, runner) else c(prefix, res$effective_genome_size, res$chrom_sizes, res$bowtie2_index, runner)
    mode <- if (bigwig_only) "regenerate BAM index and direct CPM bigWig only" else "repair deduplication, BED, insert-size, and CPM bigWig outputs"
    submit_sbatch(
      project, "Bowtie2", qsub,
      args,
      "bowtie2_postprocess", mode,
      sample = sample, target = target, reference = res$label
    )
  }, character(1))
  paste(messages, collapse = "\n")
}

submit_atac_bowtie2_jobs <- function(project, trimmed = TRUE) {
  if (!isTRUE(project$paired_end)) return(record_preflight_failure(project, "Bowtie2", "The established CodeSpringLab ATAC-seq workflow requires paired-end FASTQs.", "bowtie2"))
  res <- atac_reference_resources(project)
  index_exists <- file.exists(paste0(res$bowtie2_index, ".1.bt2")) || file.exists(paste0(res$bowtie2_index, ".1.bt2l"))
  missing_ref <- c(if (!index_exists) paste0(res$bowtie2_index, ".1.bt2[|l]"), if (!file.exists(res$chrom_sizes)) res$chrom_sizes)
  if (length(missing_ref)) return(record_preflight_failure(project, "Bowtie2", paste("ATAC-seq reference files are missing:", paste(missing_ref, collapse = ", ")), "bowtie2"))
  pairs <- sample_fastq_pairs(project, trimmed)
  msg <- missing_read_message(project, pairs, trimmed)
  if (nzchar(msg)) return(record_preflight_failure(project, "Bowtie2", msg, "bowtie2"))
  outdir <- file.path(project$data_dir, "bowtie2"); dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  targets <- stats::setNames(lapply(unique(pairs$sample), function(s) file.path(outdir, s, paste0(s, "_alignment_summary.txt"))), unique(pairs$sample))
  plan <- sample_submission_plan(project, "Bowtie2", targets)
  if (!length(plan$samples)) return(plan$message)
  pairs <- pairs[pairs$sample %in% plan$samples, , drop = FALSE]
  qsub <- file.path(SCRIPTS_DIR, "bowtie2", "qsub_bowtie2_PE.sh")
  runner <- file.path(SCRIPTS_DIR, "bowtie2", "bowtie2_PE.sh")
  missing_scripts <- c(qsub, runner)[!file.exists(c(qsub, runner))]
  if (length(missing_scripts)) return(record_preflight_failure(project, "Bowtie2", paste("CodeSpringLab ATAC-seq Bowtie2 scripts are missing:", paste(missing_scripts, collapse = ", ")), "bowtie2"))
  messages <- apply(pairs, 1, function(row) {
    sample <- row[["sample"]]; sample_dir <- file.path(outdir, sample); dir.create(sample_dir, recursive = TRUE, showWarnings = FALSE)
    prefix <- file.path(sample_dir, sample)
    submit_sbatch(project, "Bowtie2", qsub, c(prefix, res$bowtie2_index, row[["r1"]], row[["r2"]], res$effective_genome_size, res$chrom_sizes, project$name, runner),
                  "bowtie2_atac", paste(if (trimmed) "trimmed" else "raw", "reads; GRCm39/M39"), sample = sample,
                  target = paste0(prefix, "_alignment_summary.txt"), reference = res$label)
  })
  paste(append_plan_message(messages, plan), collapse = "\n")
}

submit_atac_macs2_jobs <- function(project, qvalue = "0.05") {
  res <- atac_reference_resources(project)
  design <- project_design_df(project)
  if (!NROW(design) || !"sample" %in% names(design)) return(record_preflight_failure(project, "MACS2 Peaks", "No samples found in design_matrix.txt.", "macs2"))
  outdir <- file.path(project$data_dir, "macs2"); dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  targets <- stats::setNames(lapply(design$sample, function(s) file.path(outdir, s, paste0(s, "_peaks.narrowPeak"))), design$sample)
  plan <- sample_submission_plan(project, "MACS2 Peaks", targets)
  if (!length(plan$samples)) return(plan$message)
  qsub <- file.path(SCRIPTS_DIR, "MACS2", "qsub_macs2_PE.sh"); runner <- file.path(SCRIPTS_DIR, "MACS2", "macs2_PE.sh")
  missing_resources <- c(qsub, runner)[!file.exists(c(qsub, runner))]
  if (length(missing_resources)) return(record_preflight_failure(project, "MACS2 Peaks", paste("Required ATAC-seq MACS2 resources are missing:", paste(missing_resources, collapse = ", ")), "macs2"))
  messages <- vapply(plan$samples, function(sample) {
    bed <- file.path(project$data_dir, "bowtie2", sample, paste0(sample, "Aligned.sortedByCoord_removeDup.out.bed"))
    bw_dir <- file.path(project$data_dir, "bowtie2")
    if (!file.exists(bed)) return(record_preflight_failure(project, "MACS2 Peaks", paste("Missing duplicate-removed Bowtie2 BED:", bed), "macs2"))
    sample_dir <- file.path(outdir, sample); dir.create(sample_dir, recursive = TRUE, showWarnings = FALSE)
    submit_sbatch(project, "MACS2 Peaks", qsub,
      c(sample, bed, res$macs2_genome, res$chrom_sizes, sample_dir, res$tss_bed, qvalue, project$name, res$homer_genome, bw_dir, runner),
      "macs2_atac", paste("ATAC shift -100/extsize 200; q", qvalue), sample = sample,
      target = file.path(sample_dir, paste0(sample, "_peaks.narrowPeak")), reference = res$label)
  }, character(1))
  paste(append_plan_message(messages, plan), collapse = "\n")
}

submit_atac_diffbind_job <- function(project, compare_col, reference, comparison, subset_col = "", subset_value = "") {
  design <- project_design_df(project)
  if (!NROW(design) || !nzchar(compare_col) || !compare_col %in% names(design)) return(record_preflight_failure(project, "Differential Peaks", "Select a valid ATAC-seq comparison variable from design_matrix.txt.", "diffbind"))
  subset_col <- trimws(as.character(subset_col %||% "")); subset_value <- trimws(as.character(subset_value %||% ""))
  if (nzchar(subset_col)) {
    if (!subset_col %in% names(design) || !nzchar(subset_value)) return(record_preflight_failure(project, "Differential Peaks", "Select a valid ATAC-seq subset before running DiffBind.", "diffbind"))
    design <- design[trimws(as.character(design[[subset_col]])) == subset_value, , drop = FALSE]
  }
  tab <- table(as.character(design[[compare_col]]))
  if (!all(c(reference, comparison) %in% names(tab)) || any(tab[c(reference, comparison)] < 2L)) return(record_preflight_failure(project, "Differential Peaks", "DiffBind requires at least two biological replicates in both selected conditions.", "diffbind"))
  res <- atac_reference_resources(project)
  qsub <- file.path(SCRIPTS_DIR, "DiffBind", "qsub_diffbind.sh"); runner <- file.path(SCRIPTS_DIR, "DiffBind", "diffbind.sh"); rscript <- file.path(SCRIPTS_DIR, "DiffBind", "DiffBind.R")
  required <- c(qsub, runner, rscript, if (genome_species(project) == "mouse") res$blacklist else character(0))
  missing_resources <- required[!file.exists(required)]
  if (length(missing_resources)) return(record_preflight_failure(project, "Differential Peaks", paste("Required ATAC-seq DiffBind resources are missing:", paste(missing_resources, collapse = ", ")), "diffbind"))
  comparison_design <- atac_diffbind_design(project, compare_col, subset_col, subset_value)
  design_dir <- dirname(comparison_design)
  if (genome_species(project) == "mouse") file.copy(res$blacklist, file.path(design_dir, "mm39-blacklist.bed"), overwrite = TRUE)
  display_reference <- if (nzchar(subset_value)) paste(subset_value, reference, sep = "_") else reference
  display_comparison <- if (nzchar(subset_value)) paste(subset_value, comparison, sep = "_") else comparison
  slug <- clean_name(paste0(display_comparison, "_vs_", display_reference), "comparison")
  outdir <- file.path(project$data_dir, "diffbind", slug); dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  target <- file.path(outdir, paste0("DifferentialPeaks_", comparison, "_vs_", reference, "_ref_annotated_with_stats.txt"))
  if (file.exists(target)) return("This ATAC-seq DiffBind comparison is already complete. Delete its data first to rerun.")
  submit_sbatch(project, "Differential Peaks", qsub,
    c(rscript, outdir, design_dir, file.path(project$data_dir, "macs2"), reference, comparison, genome_species(project), file.path(project$data_dir, "bowtie2"), runner),
    "diffbind_atac", paste(if (nzchar(subset_col)) paste0(subset_col, "=", subset_value, ";") else "all samples;", compare_col, comparison, "vs", reference, "GRCm39/M39"), target = target, reference = res$label)
}

submit_cutrun_bowtie2_jobs <- function(project, trimmed = TRUE, mapq = 30, max_fragment = 1000,
                                       dedup_target = FALSE, dedup_control = TRUE, remove_mito = TRUE,
                                       normalization_mode = "spikein", spikein_index_path = CUTRUN_DEFAULT_SPIKEIN_INDEX,
                                       spikein_name = CUTRUN_DEFAULT_SPIKEIN_NAME, spikein_min_reads = "1000") {
  res <- cutrun_reference_resources(project)
  outdir <- file.path(project$data_dir, "bowtie2")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  normalization_mode <- selected_choice(normalization_mode, c("CPM", "spikein", "none"), "spikein")
  spikein_index_path <- trimws(as.character(spikein_index_path %||% "none"))
  if (!nzchar(spikein_index_path) || identical(tolower(spikein_index_path), "none")) spikein_index_path <- CUTRUN_DEFAULT_SPIKEIN_INDEX
  if (identical(tolower(normalization_mode), "spikein") && !file.exists(paste0(spikein_index_path, ".1.bt2"))) {
    return(record_preflight_failure(project, "Bowtie2", paste(
      "Spike-in normalization was selected, but the spike-in Bowtie2 index was not found.",
      "Provide the Bowtie2 index prefix, not a .bt2 file. Example: /path/to/ecoli_index/ecoli"
    ), "bowtie2"))
  }
  pairs <- sample_fastq_pairs(project, trimmed)
  msg <- missing_read_message(project, pairs, trimmed)
  if (nzchar(msg)) return(record_preflight_failure(project, "Bowtie2", msg, "bowtie2"))
  target_list <- lapply(split(seq_len(NROW(pairs)), pairs$sample), function(idx) {
    sample <- pairs$sample[idx[[1]]]
    cutrun_bowtie2_complete_marker(project, sample)
  })
  plan <- sample_submission_plan(project, "Bowtie2", target_list)
  if (!length(plan$samples)) return(plan$message)
  pairs <- pairs[pairs$sample %in% plan$samples, , drop = FALSE]
  script <- file.path(SCRIPTS_DIR, "bowtie2", if (project$paired_end) "qsub_bowtie2_cutrun_PE.sh" else "qsub_bowtie2_cutrun_SE.sh")
  design <- cutrun_design(project)
  input_mode <- if (trimmed) "trimmed reads" else "raw reads"
  messages <- apply(pairs, 1, function(row) {
    sample <- row[["sample"]]
    sample_dir <- file.path(outdir, sample)
    dir.create(sample_dir, recursive = TRUE, showWarnings = FALSE)
    target <- cutrun_bowtie2_complete_marker(project, sample)
    target_row <- design[design$sample == sample, , drop = FALSE]
    is_control <- NROW(target_row) && cutrun_control_like(target_row$target[1])
    dedup_mode <- if ((is_control && isTRUE(dedup_control)) || (!is_control && isTRUE(dedup_target))) "dedup" else "keepdup"
    remove_mito_arg <- if (isTRUE(remove_mito)) "y" else "n"
    submit_sbatch(
      project, "Bowtie2", script,
      c(file.path(sample_dir, sample), res$bowtie2_index, row[["r1"]], row[["r2"]], res$chrom_sizes, project$name, mapq, max_fragment, dedup_mode, remove_mito_arg, normalization_mode, spikein_index_path, spikein_name, spikein_min_reads),
      "bowtie2", paste(input_mode, normalization_mode), sample = sample, target = target, reference = res$label
    )
  })
  paste(append_plan_message(messages, plan), collapse = "\n")
}

submit_cutrun_postprocess_jobs <- function(project, samples, trimmed = TRUE, mapq = 30, max_fragment = 1000,
                                           dedup_target = FALSE, dedup_control = TRUE, remove_mito = TRUE,
                                           normalization_mode = "spikein", spikein_index_path = CUTRUN_DEFAULT_SPIKEIN_INDEX,
                                           spikein_name = CUTRUN_DEFAULT_SPIKEIN_NAME, spikein_min_reads = "1000") {
  if (!isTRUE(project$paired_end)) return(record_preflight_failure(project, "Bowtie2", "Selective CUT&RUN post-alignment repair currently requires paired-end data.", "bowtie2_postprocess"))
  samples <- unique(trimws(as.character(samples %||% character(0))))
  samples <- samples[nzchar(samples)]
  if (!length(samples)) return(record_preflight_failure(project, "Bowtie2", "Select at least one repairable CUT&RUN sample.", "bowtie2_postprocess"))
  status <- cutrun_postprocess_status_table(project)
  eligible <- if (NROW(status)) as.character(status$sample[status$status == "Repair available"]) else character(0)
  invalid <- setdiff(samples, eligible)
  if (length(invalid)) return(record_preflight_failure(project, "Bowtie2", paste("These samples require full Bowtie2 or are already complete:", paste(invalid, collapse = ", ")), "bowtie2_postprocess"))

  pairs <- sample_fastq_pairs(project, trimmed)
  msg <- missing_read_message(project, pairs, trimmed)
  if (nzchar(msg)) return(record_preflight_failure(project, "Bowtie2", msg, "bowtie2_postprocess"))
  pairs <- pairs[pairs$sample %in% samples, , drop = FALSE]
  missing_samples <- setdiff(samples, as.character(pairs$sample))
  if (length(missing_samples)) return(record_preflight_failure(project, "Bowtie2", paste("FASTQ entries were not found for:", paste(missing_samples, collapse = ", ")), "bowtie2_postprocess"))

  res <- cutrun_reference_resources(project)
  script <- file.path(SCRIPTS_DIR, "bowtie2", "qsub_bowtie2_cutrun_PE.sh")
  runner <- file.path(SCRIPTS_DIR, "bowtie2", "bowtie2_cutrun_PE.sh")
  missing_resources <- c(script, runner, res$chrom_sizes)[!file.exists(c(script, runner, res$chrom_sizes))]
  if (length(missing_resources)) return(record_preflight_failure(project, "Bowtie2", paste("CUT&RUN repair resources are missing:", paste(missing_resources, collapse = ", ")), "bowtie2_postprocess"))

  normalization_mode <- selected_choice(normalization_mode, c("CPM", "spikein", "none"), "spikein")
  spikein_index_path <- trimws(as.character(spikein_index_path %||% "none"))
  if (!nzchar(spikein_index_path) || identical(tolower(spikein_index_path), "none")) spikein_index_path <- CUTRUN_DEFAULT_SPIKEIN_INDEX
  if (identical(tolower(normalization_mode), "spikein") && !file.exists(paste0(spikein_index_path, ".1.bt2"))) {
    return(record_preflight_failure(project, "Bowtie2", "Spike-in normalization was selected, but its Bowtie2 index was not found.", "bowtie2_postprocess"))
  }
  design <- cutrun_design(project)
  input_mode <- if (trimmed) "trimmed reads" else "raw reads"
  messages <- apply(pairs, 1, function(row) {
    sample <- row[["sample"]]
    sample_dir <- file.path(project$data_dir, "bowtie2", sample)
    target_row <- design[design$sample == sample, , drop = FALSE]
    is_control <- NROW(target_row) && cutrun_control_like(target_row$target[1])
    dedup_mode <- if ((is_control && isTRUE(dedup_control)) || (!is_control && isTRUE(dedup_target))) "dedup" else "keepdup"
    remove_mito_arg <- if (isTRUE(remove_mito)) "y" else "n"
    target <- cutrun_bowtie2_complete_marker(project, sample)
    submit_sbatch(
      project, "Bowtie2", script,
      c(file.path(sample_dir, sample), res$bowtie2_index, row[["r1"]], row[["r2"]], res$chrom_sizes, project$name, mapq, max_fragment, dedup_mode, remove_mito_arg, normalization_mode, spikein_index_path, spikein_name, spikein_min_reads, "repair"),
      "bowtie2_postprocess", paste(input_mode, normalization_mode, "post-alignment repair"), sample = sample, target = target, reference = res$label
    )
  })
  paste(messages, collapse = "\n")
}

submit_cutrun_seacr_jobs <- function(project, norm = "norm", stringency = "stringent") {
  msg <- cutrun_missing_bowtie2_message(project, "SEACR")
  if (nzchar(msg)) return(record_preflight_failure(project, "SEACR", msg, "seacr"))
  norm <- selected_choice(tolower(trimws(as.character(norm %||% "norm"))), c("norm", "non"), "norm")
  stringency <- selected_choice(tolower(trimws(as.character(stringency %||% "stringent"))), c("stringent", "relaxed"), "stringent")
  dir.create(file.path(project$data_dir, "seacr"), recursive = TRUE, showWarnings = FALSE)
  design <- cutrun_target_design(project, include_controls = FALSE)
  if (!NROW(design)) return(record_preflight_failure(project, "SEACR", "No non-control CUT&RUN target samples were found. Fill the target column and avoid labeling every row as IgG/input/control.", "seacr"))
  full_design <- cutrun_design(project)
  if (any(cutrun_control_like(full_design$target))) {
    resolved_controls <- vapply(as.character(design$sample), function(sample) cutrun_control_sample_for(project, sample), character(1))
    unresolved <- as.character(design$sample[!nzchar(resolved_controls)])
    if (length(unresolved)) {
      return(record_preflight_failure(project, "SEACR", paste(
        "Condition-matched IgG/input controls exist, but these targets do not resolve to exactly one compatible control:",
        paste(unresolved, collapse = ", "),
        "Set control_sample explicitly, or use matching cell_type and condition values in design_matrix.txt."
      ), "seacr"))
    }
  }
  controls <- vapply(as.character(design$sample), function(sample) cutrun_control_sample_for(project, sample), character(1))
  required_bedgraphs <- c(
    vapply(as.character(design$sample), function(sample) cutrun_seacr_bedgraph(project, sample, norm), character(1)),
    vapply(controls[nzchar(controls)], function(control) cutrun_seacr_bedgraph(project, control, norm), character(1))
  )
  missing_bedgraphs <- unique(required_bedgraphs[!file.exists(required_bedgraphs) | vapply(required_bedgraphs, file_size_for, numeric(1)) <= 0])
  if (length(missing_bedgraphs)) {
    return(record_preflight_failure(project, "SEACR", paste(c(
      if (identical(norm, "norm")) "SEACR norm requires raw target and control bedGraphs." else "SEACR non requires completed normalized target and control bedGraphs.",
      "Missing or empty files:", missing_bedgraphs
    ), collapse = "\n"), "seacr"))
  }
  target_list <- stats::setNames(lapply(as.character(design$sample), function(sample) {
    sample_stringency <- cutrun_seacr_stringency_for(project, sample, stringency)
    file.path(cutrun_seacr_combo_dir(project, norm, sample_stringency), sample, paste0(sample, ".", sample_stringency, ".bed"))
  }), as.character(design$sample))
  plan <- sample_submission_plan(project, "SEACR", target_list)
  if (!length(plan$samples)) return(plan$message)
  design <- design[design$sample %in% plan$samples, , drop = FALSE]
  script <- file.path(SCRIPTS_DIR, "SEACR", "qsub_seacr_cutrun.sh")
  messages <- vapply(as.character(design$sample), function(sample) {
    sample_stringency <- cutrun_seacr_stringency_for(project, sample, stringency)
    sample_dir <- file.path(cutrun_seacr_combo_dir(project, norm, sample_stringency), sample)
    dir.create(sample_dir, recursive = TRUE, showWarnings = FALSE)
    control <- cutrun_control_sample_for(project, sample)
    control_bdg <- if (nzchar(control)) cutrun_seacr_bedgraph(project, control, norm) else "none"
    target <- file.path(sample_dir, paste0(sample, ".", sample_stringency, ".bed"))
    submit_sbatch(
      project, "SEACR", script,
      c(cutrun_seacr_bedgraph(project, sample, norm), control_bdg, norm, sample_stringency, file.path(sample_dir, sample), project$name, cutrun_bowtie2_fragments(project, sample), file.path(SCRIPTS_DIR, "SEACR", "seacr_cutrun.sh")),
      "seacr", paste(norm, sample_stringency), sample = sample, target = target, reference = "SEACR local script"
    )
  }, character(1))
  paste(append_plan_message(messages, plan), collapse = "\n")
}

submit_cutrun_dedup_sensitivity_jobs <- function(project, samples = character(0), norm = "non", stringency = "stringent", max_fragment = "1000", remove_mito = TRUE) {
  design <- cutrun_target_design(project, include_controls = FALSE)
  if (!NROW(design)) return(record_preflight_failure(project, "SEACR sensitivity", "No non-control CUT&RUN target samples were found.", "cutrun_dedup_sensitivity"))
  requested_samples <- unique(trimws(as.character(samples %||% character(0))))
  requested_samples <- requested_samples[nzchar(requested_samples)]
  if (!length(requested_samples)) return(record_preflight_failure(project, "SEACR sensitivity", "Select at least one target sample for the deduplicated-target sensitivity analysis.", "cutrun_dedup_sensitivity"))
  available_samples <- as.character(design$sample)
  unknown_samples <- setdiff(requested_samples, available_samples)
  if (length(unknown_samples)) return(record_preflight_failure(project, "SEACR sensitivity", paste("These selected samples are not non-control CUT&RUN targets:", paste(unknown_samples, collapse = ", ")), "cutrun_dedup_sensitivity"))
  design <- design[match(requested_samples, available_samples), , drop = FALSE]
  res <- cutrun_reference_resources(project)
  qsub <- file.path(SCRIPTS_DIR, "CUTRUN", "qsub_cutrun_dedup_sensitivity.sh")
  runner <- file.path(SCRIPTS_DIR, "CUTRUN", "cutrun_dedup_sensitivity.sh")
  seacr_runner <- file.path(SCRIPTS_DIR, "SEACR", "seacr_cutrun.sh")
  required_scripts <- c(qsub, runner, seacr_runner, res$chrom_sizes)
  missing_scripts <- required_scripts[!file.exists(required_scripts)]
  if (length(missing_scripts)) return(record_preflight_failure(project, "SEACR sensitivity", paste("Required deduplicated-target sensitivity resources are missing:", paste(missing_scripts, collapse = ", ")), "cutrun_dedup_sensitivity"))
  norm <- selected_choice(norm, c("norm", "non"), "non")
  stringency <- selected_choice(stringency, c("stringent", "relaxed"), "stringent")
  max_fragment <- suppressWarnings(as.integer(max_fragment))
  if (!is.finite(max_fragment) || max_fragment <= 0) return(record_preflight_failure(project, "SEACR sensitivity", "Maximum fragment length must be a positive whole number.", "cutrun_dedup_sensitivity"))
  outdir <- cutrun_seacr_combo_dir(project, norm, stringency, sensitivity = TRUE)
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  full_design <- cutrun_design(project)
  if (any(cutrun_control_like(full_design$target))) {
    resolved <- vapply(as.character(design$sample), function(sample) cutrun_control_sample_for(project, sample), character(1))
    unresolved <- as.character(design$sample[!nzchar(resolved)])
    if (length(unresolved)) return(record_preflight_failure(project, "SEACR sensitivity", paste("These targets do not resolve to exactly one matched IgG/input control:", paste(unresolved, collapse = ", ")), "cutrun_dedup_sensitivity"))
  }
  missing_inputs <- character(0)
  for (sample in as.character(design$sample)) {
    control <- cutrun_control_sample_for(project, sample)
    inputs <- c(
      file.path(project$data_dir, "bowtie2", sample, paste0(sample, "Aligned.sortedByCoord_removeDup.out.bam")),
      cutrun_bowtie2_complete_marker(project, sample),
      if (nzchar(control)) cutrun_seacr_bedgraph(project, control, norm) else character(0)
    )
    missing_inputs <- c(missing_inputs, inputs[!file.exists(inputs) | vapply(inputs, file_size_for, numeric(1)) <= 0])
  }
  if (length(missing_inputs)) return(record_preflight_failure(project, "SEACR sensitivity", paste(c("Each target needs its duplicate-removed BAM, alignment summary, and matched control bedGraph.", "Missing or empty files:", unique(missing_inputs)), collapse = "\n"), "cutrun_dedup_sensitivity"))
  messages <- vapply(as.character(design$sample), function(sample) {
    control <- cutrun_control_sample_for(project, sample)
    sample_dir <- file.path(outdir, sample)
    dir.create(sample_dir, recursive = TRUE, showWarnings = FALSE)
    prefix <- file.path(sample_dir, sample)
    submit_sbatch(
      project, "SEACR sensitivity", qsub,
      c(file.path(project$data_dir, "bowtie2", sample, paste0(sample, "Aligned.sortedByCoord_removeDup.out.bam")), res$chrom_sizes, cutrun_bowtie2_complete_marker(project, sample), if (nzchar(control)) cutrun_seacr_bedgraph(project, control, norm) else "none", norm, stringency, prefix, project$name, max_fragment, if (isTRUE(remove_mito)) "y" else "n", seacr_runner, runner),
      "cutrun_dedup_sensitivity", paste("deduplicated target;", norm, stringency), sample = sample,
      target = file.path(sample_dir, paste0(sample, ".", stringency, ".bed")), reference = "deduplicated target BAM + matched control"
    )
  }, character(1))
  paste(messages, collapse = "\n")
}

submit_cutrun_peakqc_job <- function(project, norm = "non", stringency = "stringent") {
  data_dir <- project$data_dir
  norm <- selected_choice(tolower(trimws(as.character(norm %||% "non"))), c("norm", "non"), "non")
  stringency <- selected_choice(tolower(trimws(as.character(stringency %||% "stringent"))), c("stringent", "relaxed"), "stringent")
  seacr_dir <- cutrun_seacr_combo_dir(project, norm, stringency)
  bowtie2_dir <- file.path(data_dir, "bowtie2")
  if (count_files(seacr_dir, "\\.bed$") == 0) {
    return(record_preflight_failure(project, "Peak QC", "Run SEACR successfully before building consensus peaks and FRiP summaries.", "cutrun_peak_qc"))
  }
  outdir <- file.path(data_dir, "cutrun_peak_qc", cutrun_seacr_combo_key(norm, stringency))
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  target <- file.path(outdir, "seacr_consensus_peaks.bed")
  jobs <- job_history(project)
  active_jobs <- if (NROW(jobs) && all(c("step", "slurm_state") %in% names(jobs))) {
    jobs[canonical_job_step(jobs$step) == "Peak QC" & jobs$slurm_state %in% active_slurm_states(), , drop = FALSE]
  } else data.frame()
  if (NROW(active_jobs)) return("Peak QC is already active for this project.")
  if (file.exists(target) && file_size_for(target) >= minimum_expected_bytes("Peak QC")) {
    return("Peak QC is already complete. Delete Peak QC data first if you want to force a rerun.")
  }
  script <- file.path(SCRIPTS_DIR, "CUTRUN", "qsub_cutrun_peak_qc.sh")
  submit_sbatch(
    project, "Peak QC", script,
    c(seacr_dir, bowtie2_dir, outdir, project$name),
    "cutrun_peak_qc", paste("consensus peaks + FRiP", norm, stringency), sample = "", target = target, reference = "SEACR peaks and Bowtie2 BAMs"
  )
}

cutrun_alignment_values <- function(project, sample) {
  path <- cutrun_bowtie2_complete_marker(project, sample)
  lines <- read_metric_lines(path)
  value <- function(key, default = "") {
    hit <- grep(paste0("^", key, "\\t"), lines, value = TRUE)
    if (!length(hit)) return(default)
    parts <- strsplit(hit[[1]], "\t", fixed = TRUE)[[1]]
    trimws(parts[[2]] %||% default)
  }
  c(
    dedup_mode = value("dedup_mode", "keepdup"),
    normalization_mode = tolower(value("normalization_mode", "none")),
    spikein_name = value("spikein_name", CUTRUN_DEFAULT_SPIKEIN_NAME)
  )
}

cutrun_bowtie2_signal_bam <- function(project, sample) {
  metrics <- cutrun_alignment_values(project, sample)
  suffix <- if (identical(metrics[["dedup_mode"]], "dedup")) "Aligned.sortedByCoord_removeDup.out.bam" else "Aligned.sortedByCoord.out.bam"
  file.path(project$data_dir, "bowtie2", sample, paste0(sample, suffix))
}

cutrun_spikein_bam <- function(project, sample) {
  metrics <- cutrun_alignment_values(project, sample)
  if (!identical(metrics[["normalization_mode"]], "spikein")) return("")
  file.path(project$data_dir, "bowtie2", sample, paste0(sample, "_", metrics[["spikein_name"]], ".bam"))
}

cutrun_seacr_peak_path <- function(project, sample, norm = NULL, stringency = NULL) {
  root <- file.path(project$data_dir, "seacr")
  if (!is.null(norm) && !is.null(stringency)) {
    actual_stringency <- cutrun_seacr_stringency_for(project, sample, stringency)
    return(file.path(cutrun_seacr_combo_dir(project, norm, actual_stringency), sample, paste0(sample, ".", actual_stringency, ".bed")))
  }
  escaped <- gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", sample, perl = TRUE)
  hit <- if (dir.exists(root)) list.files(root, pattern = paste0("^", escaped, "\\.(stringent|relaxed)\\.bed$"), recursive = TRUE, full.names = TRUE) else character(0)
  hit <- hit[vapply(hit, file_size_for, numeric(1)) > 0]
  design <- cutrun_design(project)
  row <- if (NROW(design) && sample %in% design$sample) design[match(sample, design$sample), , drop = FALSE] else data.frame()
  requested <- if (NROW(row)) tolower(trimws(as.character(row$seacr_stringency[[1]] %||% "auto"))) else "auto"
  if (requested %in% c("stringent", "relaxed")) {
    preferred <- hit[grepl(paste0("\\.", requested, "\\.bed$"), hit)]
    if (length(preferred)) return(preferred[[which.max(file.info(preferred)$mtime)]])
  }
  if (length(hit)) return(hit[[which.max(file.info(hit)$mtime)]])
  file.path(root, sample, paste0(sample, ".stringent.bed"))
}

cutrun_diffbind_conditions <- function(project) {
  design <- cutrun_target_design(project, include_controls = FALSE)
  if (!NROW(design)) return(character(0))
  values <- unique(trimws(as.character(design$condition)))
  values <- values[nzchar(values)]
  preferred <- values[tolower(values) %in% c("veh", "vehicle", "control", "ctrl", "untreated")]
  c(preferred, setdiff(sort(values), preferred))
}

cutrun_diffbind_comparison_plan <- function(project, reference_condition, min_replicates = 1L) {
  design <- cutrun_target_design(project, include_controls = FALSE)
  reference_condition <- trimws(as.character(reference_condition %||% ""))
  required <- max(2L, suppressWarnings(as.integer(min_replicates)))
  if (!NROW(design) || !nzchar(reference_condition)) return(data.frame())
  design$cell_type <- trimws(as.character(design$cell_type)); design$cell_type[!nzchar(design$cell_type)] <- "all"
  design$mark <- trimws(as.character(design$mark))
  design$condition <- trimws(as.character(design$condition))
  groups <- split(seq_len(NROW(design)), paste(design$cell_type, design$mark, sep = "\r"))
  rows <- list()
  for (idx in groups) {
    group <- design[idx, , drop = FALSE]
    cell_type <- group$cell_type[[1]]; mark <- group$mark[[1]]
    tab <- table(group$condition)
    comparisons <- setdiff(names(tab), reference_condition)
    for (comparison in comparisons) {
      n_reference <- if (reference_condition %in% names(tab)) as.integer(tab[[reference_condition]]) else 0L
      n_comparison <- as.integer(tab[[comparison]])
      eligible <- n_reference >= required && n_comparison >= required
      id <- paste(cell_type, mark, comparison, sep = "|||")
      rows[[length(rows) + 1L]] <- data.frame(
        id = id,
        label = sprintf("%s — %s — %s vs %s (%s + %s reps)", cell_type, mark, comparison, reference_condition, n_comparison, n_reference),
        cell_type = cell_type, mark = mark, comparison = comparison, reference = reference_condition,
        comparison_replicates = n_comparison, reference_replicates = n_reference, eligible = eligible,
        reason = if (eligible) "Ready" else sprintf("Needs at least %s replicates in both conditions", required),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) return(data.frame())
  out <- do.call(rbind, rows)
  out[order(!out$eligible, out$cell_type, out$mark, out$comparison), , drop = FALSE]
}

cutrun_diffbind_sample_sheet <- function(project, reference_condition, min_replicates = 1L, cell_type = "", mark = "", comparison = "", seacr_norm = "non", seacr_stringency = "stringent") {
  design <- cutrun_target_design(project, include_controls = FALSE)
  if (!NROW(design)) stop("No non-control CUT&RUN samples were found in design_matrix.txt.")
  reference_condition <- trimws(as.character(reference_condition %||% ""))
  if (!nzchar(reference_condition)) stop("Choose a reference condition for differential peak analysis.")

  blank_mark <- !nzchar(trimws(as.character(design$mark)))
  blank_condition <- !nzchar(trimws(as.character(design$condition)))
  blank_replicate <- !nzchar(trimws(as.character(design$replicate)))
  if (any(blank_mark)) stop("Every non-control sample needs a mark value (for example pCreb, Creb, H3K27ac, or H3K4me3). Missing: ", paste(design$sample[blank_mark], collapse = ", "))
  if (any(blank_condition)) stop("Every non-control sample needs a condition value. Missing: ", paste(design$sample[blank_condition], collapse = ", "))
  if (any(blank_replicate)) stop("Every non-control sample needs a biological replicate value. Missing: ", paste(design$sample[blank_replicate], collapse = ", "))
  if (anyDuplicated(design$sample)) stop("Sample names must be unique before differential peak analysis.")

  if (nzchar(cell_type) || nzchar(mark) || nzchar(comparison)) {
    design_cell <- trimws(as.character(design$cell_type)); design_cell[!nzchar(design_cell)] <- "all"
    keep <- design_cell == cell_type & trimws(as.character(design$mark)) == mark & trimws(as.character(design$condition)) %in% c(reference_condition, comparison)
    design <- design[keep, , drop = FALSE]
    if (!NROW(design)) stop("The selected CUT&RUN comparison has no samples in design_matrix.txt.")
  }

  rows <- lapply(seq_len(NROW(design)), function(i) {
    sample <- as.character(design$sample[[i]])
    metrics <- cutrun_alignment_values(project, sample)
    use_spikein <- identical(tolower(trimws(as.character(seacr_norm))), "non") && identical(metrics[["normalization_mode"]], "spikein")
    data.frame(
      SampleID = sample,
      CellType = trimws(as.character(design$cell_type[[i]] %||% "all")),
      Mark = trimws(as.character(design$mark[[i]])),
      Condition = trimws(as.character(design$condition[[i]])),
      Replicate = trimws(as.character(design$replicate[[i]])),
      bamReads = cutrun_bowtie2_signal_bam(project, sample),
      Peaks = cutrun_seacr_peak_path(project, sample, seacr_norm, seacr_stringency),
      Spikein = if (use_spikein) cutrun_spikein_bam(project, sample) else "",
      normalization_mode = if (use_spikein) "spikein" else "none",
      stringsAsFactors = FALSE
    )
  })
  sheet <- do.call(rbind, rows)
  sheet$CellType[!nzchar(sheet$CellType)] <- "all"

  missing_inputs <- unique(c(
    sheet$bamReads[!file.exists(sheet$bamReads) | vapply(sheet$bamReads, file_size_for, numeric(1)) <= 0],
    sheet$Peaks[!file.exists(sheet$Peaks) | vapply(sheet$Peaks, file_size_for, numeric(1)) <= 0]
  ))
  spike_expected <- sheet$normalization_mode == "spikein"
  missing_spike <- sheet$Spikein[spike_expected & (!file.exists(sheet$Spikein) | vapply(sheet$Spikein, file_size_for, numeric(1)) <= 0)]
  missing_inputs <- unique(c(missing_inputs, missing_spike))
  missing_inputs <- missing_inputs[nzchar(missing_inputs)]
  if (length(missing_inputs)) stop(paste(c("Run Bowtie2 and SEACR successfully for every target sample first. Missing or empty inputs:", missing_inputs), collapse = "\n"))

  group_key <- paste(sheet$CellType, sheet$Mark, sep = "__")
  required_replicates <- max(2L, suppressWarnings(as.integer(min_replicates)))
  analyzable <- vapply(split(seq_len(NROW(sheet)), group_key), function(idx) {
    tab <- table(sheet$Condition[idx])
    length(tab) >= 2L && reference_condition %in% names(tab) && tab[[reference_condition]] >= required_replicates && any(tab[setdiff(names(tab), reference_condition)] >= required_replicates)
  }, logical(1))
  if (!any(analyzable)) {
    stop("No cell type/mark group has at least ", required_replicates, " biological replicates in the reference and comparison conditions. Under-replicated groups will not be tested.")
  }

  out_dir <- file.path(project$data_dir, "manifest", "cutrun_diffbind")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  sheet_name <- if (nzchar(cell_type) || nzchar(mark) || nzchar(comparison)) clean_name(paste(cutrun_seacr_combo_key(seacr_norm, seacr_stringency), cell_type, mark, comparison, "vs", reference_condition, sep = "_"), "comparison") else paste0(cutrun_seacr_combo_key(seacr_norm, seacr_stringency), "_resolved_samples")
  path <- file.path(out_dir, paste0(sheet_name, ".tsv"))
  utils::write.table(sheet, path, sep = "\t", row.names = FALSE, quote = FALSE)
  path
}

submit_cutrun_diffbind_jobs <- function(project, reference_condition, comparison_ids, min_replicates = 1L, seacr_norm = "non", seacr_stringency = "stringent") {
  min_replicates <- suppressWarnings(as.integer(min_replicates))
  if (!is.finite(min_replicates) || min_replicates < 1L) min_replicates <- 1L
  plan <- cutrun_diffbind_comparison_plan(project, reference_condition, min_replicates)
  comparison_ids <- unique(as.character(comparison_ids %||% character(0)))
  selected <- plan[plan$id %in% comparison_ids & plan$eligible, , drop = FALSE]
  if (!NROW(selected)) return(record_preflight_failure(project, "Differential Peaks", "Select at least one eligible cell type/mark comparison.", "cutrun_diffbind"))

  qsub <- file.path(SCRIPTS_DIR, "DiffBind", "qsub_cutrun_diffbind.sh")
  runner <- file.path(SCRIPTS_DIR, "DiffBind", "cutrun_diffbind.sh")
  r_script <- file.path(SCRIPTS_DIR, "DiffBind", "cutrun_diffbind.R")
  resources <- cutrun_reference_resources(project)
  blacklist <- trimws(as.character(resources$blacklist %||% ""))
  required <- c(qsub, runner, r_script, if (genome_species(project) == "mouse") blacklist else character(0))
  missing_scripts <- required[!file.exists(required)]
  if (length(missing_scripts)) {
    return(record_preflight_failure(project, "Differential Peaks", paste("CodeSpringLab CUT&RUN DiffBind scripts are missing:", paste(missing_scripts, collapse = ", ")), "cutrun_diffbind"))
  }
  root <- file.path(project$data_dir, "cutrun_diffbind"); dir.create(root, recursive = TRUE, showWarnings = FALSE)
  jobs <- job_history(project)
  messages <- character(0)
  for (i in seq_len(NROW(selected))) {
    spec <- selected[i, , drop = FALSE]
    run_slug <- paste(cutrun_seacr_combo_key(seacr_norm, seacr_stringency), clean_name(spec$cell_type), clean_name(spec$mark), paste0(clean_name(spec$comparison), "_vs_", clean_name(reference_condition)), sep = "__")
    outdir <- file.path(root, run_slug); dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
    target <- file.path(outdir, "_COMPLETE")
    if (file.exists(target)) {
      messages <- c(messages, paste(spec$label, "is already complete; skipped.")); next
    }
    active <- if (NROW(jobs) && all(c("step", "slurm_state", "sample") %in% names(jobs))) {
      jobs[canonical_job_step(jobs$step) == "Differential Peaks" & jobs$sample == run_slug & jobs$slurm_state %in% active_slurm_states(), , drop = FALSE]
    } else data.frame()
    if (NROW(active)) {
      messages <- c(messages, paste(spec$label, "is already active; skipped.")); next
    }
    sample_sheet <- tryCatch(cutrun_diffbind_sample_sheet(project, reference_condition, min_replicates, spec$cell_type, spec$mark, spec$comparison, seacr_norm, seacr_stringency), error = function(e) e)
    if (inherits(sample_sheet, "error")) {
      messages <- c(messages, record_preflight_failure(project, "Differential Peaks", paste(spec$label, conditionMessage(sample_sheet), sep = ": "), "cutrun_diffbind")); next
    }
    messages <- c(messages, submit_sbatch(
      project, "Differential Peaks", qsub,
      c(r_script, sample_sheet, outdir, reference_condition, min_replicates, genome_species(project), if (nzchar(blacklist)) blacklist else "none", spec$comparison, spec$cell_type, spec$mark, runner),
      "cutrun_diffbind", paste(spec$label, ";", cutrun_seacr_combo_key(seacr_norm, seacr_stringency), "; consensus support", min_replicates), sample = run_slug,
      target = target, reference = paste("SEACR + DiffBind/DESeq2;", genome_species(project))
    ))
  }
  paste(messages, collapse = "\n")
}

submit_cutrun_macs2_jobs <- function(project, qvalue = "0.01", peak_type = "auto") {
  msg <- cutrun_missing_bowtie2_message(project, "MACS2")
  if (nzchar(msg)) return(record_preflight_failure(project, "MACS2 (optional)", msg, "macs2"))
  res <- cutrun_reference_resources(project)
  outdir <- file.path(project$data_dir, "macs2")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  design <- cutrun_target_design(project, include_controls = FALSE)
  if (!NROW(design)) return(record_preflight_failure(project, "MACS2 (optional)", "No non-control CUT&RUN target samples were found. Fill the target column and avoid labeling every row as IgG/input/control.", "macs2"))
  target_list <- stats::setNames(lapply(as.character(design$sample), function(sample) {
    file.path(outdir, sample, paste0(sample, "_macs2_complete.txt"))
  }), as.character(design$sample))
  plan <- sample_submission_plan(project, "MACS2 (optional)", target_list)
  if (!length(plan$samples)) return(plan$message)
  design <- design[design$sample %in% plan$samples, , drop = FALSE]
  script <- file.path(SCRIPTS_DIR, "MACS2", "qsub_macs2_cutrun_PE.sh")
  runner <- file.path(SCRIPTS_DIR, "MACS2", "macs2_cutrun_PE.sh")
  missing_scripts <- c(script, runner)[!file.exists(c(script, runner))]
  if (length(missing_scripts)) {
    return(record_preflight_failure(project, "MACS2 (optional)", paste("Required CUT&RUN MACS scripts are missing:", paste(missing_scripts, collapse = ", ")), "macs2"))
  }
  messages <- vapply(as.character(design$sample), function(sample) {
    sample_dir <- file.path(outdir, sample)
    dir.create(sample_dir, recursive = TRUE, showWarnings = FALSE)
    sample_peak_type <- cutrun_macs2_peak_type_for(project, sample, peak_type)
    control <- cutrun_control_sample_for(project, sample)
    control_bam <- if (nzchar(control)) cutrun_bowtie2_bam(project, control) else "none"
    target <- file.path(sample_dir, paste0(sample, "_macs2_complete.txt"))
    submit_sbatch(
      project, "MACS2 (optional)", script,
      c(sample, cutrun_bowtie2_bam(project, sample), control_bam, res$macs2_genome, qvalue, sample_peak_type, sample_dir, project$name, runner),
      "macs2", paste(sample_peak_type, "q", qvalue), sample = sample, target = target, reference = res$macs2_genome
    )
  }, character(1))
  paste(append_plan_message(messages, plan), collapse = "\n")
}

submit_star_jobs <- function(project, trimmed = FALSE) {
  res <- genome_resources(project)
  outdir <- file.path(project$data_dir, "star")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  pairs <- sample_fastq_pairs(project, trimmed)
  msg <- missing_read_message(project, pairs, trimmed)
  if (nzchar(msg)) return(record_preflight_failure(project, "STAR", msg, "star"))
  target_list <- lapply(split(seq_len(NROW(pairs)), pairs$sample), function(idx) {
    sample <- pairs$sample[idx[[1]]]
    file.path(outdir, sample, paste0(sample, "Aligned.sortedByCoord.out.bam"))
  })
  plan <- sample_submission_plan(project, "STAR", target_list)
  if (!length(plan$samples)) return(plan$message)
  pairs <- pairs[pairs$sample %in% plan$samples, , drop = FALSE]
  script <- file.path(SCRIPTS_DIR, "STAR", if (project$paired_end) "qsub_star_PE.sh" else "qsub_star_SE.sh")
  messages <- apply(pairs, 1, function(row) {
    sample_dir <- file.path(outdir, row[["sample"]])
    dir.create(sample_dir, recursive = TRUE, showWarnings = FALSE)
    out_prefix <- file.path(sample_dir, row[["sample"]])
    input_mode <- if (trimmed) "trimmed reads" else "raw reads"
    target <- file.path(sample_dir, paste0(row[["sample"]], "Aligned.sortedByCoord.out.bam"))
    submit_sbatch(project, "STAR", script, c(out_prefix, res$star_index, row[["r1"]], row[["r2"]], project$name), "star", input_mode, sample = row[["sample"]], target = target, reference = res$label)
  })
  paste(append_plan_message(messages, plan), collapse = "\n")
}

submit_kallisto_jobs <- function(project, trimmed = FALSE) {
  res <- genome_resources(project)
  outdir <- file.path(project$data_dir, "kallisto")
  counts_dir <- file.path(project$data_dir, "counts")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  dir.create(counts_dir, recursive = TRUE, showWarnings = FALSE)
  pairs <- sample_fastq_pairs(project, trimmed)
  msg <- missing_read_message(project, pairs, trimmed)
  if (nzchar(msg)) return(record_preflight_failure(project, "Kallisto (optional)", msg, "kallisto"))
  target_list <- lapply(split(seq_len(NROW(pairs)), pairs$sample), function(idx) {
    sample <- pairs$sample[idx[[1]]]
    file.path(outdir, sample, "abundance.tsv")
  })
  plan <- sample_submission_plan(project, "Kallisto (optional)", target_list)
  if (!length(plan$samples)) return(plan$message)
  pairs <- pairs[pairs$sample %in% plan$samples, , drop = FALSE]
  script <- file.path(SCRIPTS_DIR, "Kallisto", if (project$paired_end) "qsub_kallisto_PE.sh" else "qsub_kallisto_SE.sh")
  messages <- apply(pairs, 1, function(row) {
    sample_dir <- file.path(outdir, row[["sample"]])
    dir.create(sample_dir, recursive = TRUE, showWarnings = FALSE)
    input_mode <- if (trimmed) "trimmed reads" else "raw reads"
    target <- file.path(sample_dir, "abundance.tsv")
    submit_sbatch(project, "Kallisto (optional)", script, c(sample_dir, res$kallisto_index, row[["r1"]], row[["r2"]], project$name), "kallisto", input_mode, sample = row[["sample"]], target = target, reference = res$kallisto_index)
  })
  ids <- vapply(messages, parse_sbatch_job_id, character(1))
  matrix_script <- write_quant_matrix_script(project, "kallisto")
  matrix_cmd <- paste(shQuote(Sys.which("Rscript") %||% "Rscript"), shQuote(matrix_script), shQuote(outdir), shQuote(counts_dir))
  matrix_msg <- submit_sbatch_wrap(project, "Kallisto (optional)", matrix_cmd, "kallisto_matrices", "Kallisto matrix build", target = file.path(counts_dir, "kallisto_tpm_matrix.txt"), reference = res$kallisto_index, dependency_ids = ids)
  paste(append_plan_message(c(messages, matrix_msg), plan), collapse = "\n")
}

submit_rsem_jobs <- function(project, feature = "gene_id") {
  res <- genome_resources(project)
  design <- safe_read_table(project$design_matrix_path)
  if (!NROW(design) || !"sample" %in% names(design)) return(record_preflight_failure(project, "RSEM (optional)", "No samples found in design_matrix.txt. Create or fix the design matrix before running RSEM.", "rsem"))
  msg <- missing_star_message(project, "RSEM", transcriptome = TRUE)
  if (nzchar(msg)) return(record_preflight_failure(project, "RSEM (optional)", msg, "rsem"))
  outdir <- file.path(project$data_dir, "rsem")
  counts_dir <- file.path(project$data_dir, "counts")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  dir.create(counts_dir, recursive = TRUE, showWarnings = FALSE)
  script <- file.path(SCRIPTS_DIR, "RSEM", if (project$paired_end) "qsub_RSEM_PE.sh" else "qsub_RSEM_SE.sh")
  target_list <- stats::setNames(lapply(as.character(design$sample), function(sample) {
    file.path(outdir, sample, paste0(sample, ".genes.results"))
  }), as.character(design$sample))
  plan <- sample_submission_plan(project, "RSEM (optional)", target_list)
  if (!length(plan$samples)) return(plan$message)
  samples_to_run <- as.character(design$sample[design$sample %in% plan$samples])
  messages <- vapply(samples_to_run, function(sample) {
    sample_dir <- file.path(outdir, sample)
    dir.create(sample_dir, recursive = TRUE, showWarnings = FALSE)
    bam <- file.path(project$data_dir, "star", sample, paste0(sample, "Aligned.sortedByCoord.out.bam"))
    bam_transcript <- file.path(project$data_dir, "star", sample, paste0(sample, "Aligned.toTranscriptome.out.bam"))
    count_prefix <- file.path(sample_dir, sample)
    target <- paste0(count_prefix, ".genes.results")
    submit_sbatch(project, "RSEM (optional)", script, c(bam, res$rsem_index, feature, count_prefix, res$strand_bed, bam_transcript, project$name), "rsem", paste("STAR BAM; feature", feature), sample = sample, target = target, reference = res$rsem_index)
  }, character(1))
  ids <- vapply(messages, parse_sbatch_job_id, character(1))
  matrix_script <- write_quant_matrix_script(project, "rsem")
  matrix_cmd <- paste(shQuote(Sys.which("Rscript") %||% "Rscript"), shQuote(matrix_script), shQuote(outdir), shQuote(counts_dir))
  matrix_msg <- submit_sbatch_wrap(project, "RSEM (optional)", matrix_cmd, "rsem_matrices", paste("RSEM matrix build; feature", feature), target = file.path(counts_dir, "rsem_tpm_matrix.txt"), reference = res$rsem_index, dependency_ids = ids)
  paste(append_plan_message(c(messages, matrix_msg), plan), collapse = "\n")
}

submit_featurecounts_jobs <- function(project, feature = "gene_name") {
  res <- genome_resources(project)
  design <- safe_read_table(project$design_matrix_path)
  if (!NROW(design) || !"sample" %in% names(design)) return(record_preflight_failure(project, "featureCounts", "No samples found in design_matrix.txt. Create or fix the design matrix before running featureCounts.", "featurecounts"))
  msg <- missing_star_message(project, "featureCounts", transcriptome = FALSE)
  if (nzchar(msg)) return(record_preflight_failure(project, "featureCounts", msg, "featurecounts"))
  outdir <- file.path(project$data_dir, "featurecounts")
  counts_dir <- file.path(project$data_dir, "counts")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  dir.create(counts_dir, recursive = TRUE, showWarnings = FALSE)
  script <- file.path(SCRIPTS_DIR, "featureCounts", if (project$paired_end) "qsub_featurecounts_PE.sh" else "qsub_featurecounts_SE.sh")
  target_list <- stats::setNames(lapply(as.character(design$sample), function(sample) {
    file.path(outdir, sample, paste0(sample, "_counts.txt"))
  }), as.character(design$sample))
  plan <- sample_submission_plan(project, "featureCounts", target_list)
  if (!length(plan$samples)) {
    matrix_path <- file.path(counts_dir, "count_matrix.txt")
    if (featurecounts_outputs_ready(project) && (!file.exists(matrix_path) || file_size_for(matrix_path) <= 0)) {
      matrix_msg <- submit_featurecounts_matrix_job(project, feature)
      return(paste(append_plan_message(matrix_msg, plan), collapse = "\n"))
    }
    return(plan$message)
  }
  samples_to_run <- as.character(design$sample[design$sample %in% plan$samples])
  messages <- vapply(samples_to_run, function(sample) {
    sample_dir <- file.path(outdir, sample)
    dir.create(sample_dir, recursive = TRUE, showWarnings = FALSE)
    bam <- file.path(project$data_dir, "star", sample, paste0(sample, "Aligned.sortedByCoord.out.bam"))
    count_prefix <- file.path(sample_dir, sample)
    target <- paste0(count_prefix, "_counts.txt")
    submit_sbatch(project, "featureCounts", script, c(bam, res$gtf, feature, count_prefix, res$strand_bed, project$name), "featurecounts", paste("STAR BAM; feature", feature), sample = sample, target = target, reference = res$gtf)
  }, character(1))
  ids <- vapply(messages, parse_sbatch_job_id, character(1))
  matrix_msg <- submit_featurecounts_matrix_job(project, feature, dependency_ids = ids)
  paste(append_plan_message(c(messages, matrix_msg), plan), collapse = "\n")
}

submit_featurecounts_matrix_job <- function(project, feature = "gene_name", dependency_ids = character(0)) {
  res <- genome_resources(project)
  outdir <- file.path(project$data_dir, "featurecounts")
  counts_dir <- file.path(project$data_dir, "counts")
  dir.create(counts_dir, recursive = TRUE, showWarnings = FALSE)
  matrix_script <- write_featurecounts_matrix_script(project)
  matrix_cmd <- paste(shQuote(Sys.which("Rscript") %||% "Rscript"), shQuote(matrix_script), shQuote(outdir), shQuote(counts_dir))
  submit_sbatch_wrap(project, "featureCounts", matrix_cmd, "featurecounts_count_matrix", paste("matrix build; feature", feature), target = file.path(counts_dir, "count_matrix.txt"), reference = res$gtf, dependency_ids = dependency_ids)
}

expected_featurecounts_files <- function(project) {
  design <- safe_read_table(project$design_matrix_path)
  if (!NROW(design) || !"sample" %in% names(design)) return(character(0))
  file.path(project$data_dir, "featurecounts", as.character(design$sample), paste0(as.character(design$sample), "_counts.txt"))
}

expected_star_bam_files <- function(project, transcriptome = FALSE) {
  design <- safe_read_table(project$design_matrix_path)
  if (!NROW(design) || !"sample" %in% names(design)) return(character(0))
  suffix <- if (isTRUE(transcriptome)) "Aligned.toTranscriptome.out.bam" else "Aligned.sortedByCoord.out.bam"
  file.path(project$data_dir, "star", as.character(design$sample), paste0(as.character(design$sample), suffix))
}

missing_star_message <- function(project, step = "this step", transcriptome = FALSE) {
  if (!file.exists(project$design_matrix_path)) {
    return(paste(
      "No design_matrix.txt was found for this project.",
      paste("Expected:", project$design_matrix_path),
      "Create or save the design matrix before running STAR-dependent steps.",
      sep = "\n"
    ))
  }
  active_msg <- active_upstream_message(project, "STAR", step)
  if (nzchar(active_msg)) return(active_msg)
  files <- expected_star_bam_files(project, transcriptome)
  if (!length(files)) return("No samples were found in design_matrix.txt.")
  missing <- files[!file.exists(files) | vapply(files, file_size_for, numeric(1)) <= 0]
  if (length(missing)) {
    required <- if (isTRUE(transcriptome)) "STAR genome and transcriptome BAM outputs" else "STAR BAM outputs"
    return(paste(c(
      paste("Run STAR successfully before", step, "so the required", required, "exist."),
      "Missing or empty files:",
      missing
    ), collapse = "\n"))
  }
  ""
}

featurecounts_outputs_ready <- function(project) {
  files <- expected_featurecounts_files(project)
  length(files) > 0 && all(file.exists(files)) && all(vapply(files, file_size_for, numeric(1)) > 0)
}

active_jobs_for_step <- function(project, step) {
  jobs <- job_history(project)
  if (!NROW(jobs) || !"step" %in% names(jobs) || !"slurm_state" %in% names(jobs)) return(data.frame())
  hit <- jobs[
    canonical_job_step(jobs$step) == canonical_job_step(step) &
      jobs$slurm_state %in% active_slurm_states(),
    ,
    drop = FALSE
  ]
  if ("job_id" %in% names(hit)) hit <- hit[nzchar(hit$job_id), , drop = FALSE]
  hit
}

active_upstream_message <- function(project, upstream_step, downstream_step) {
  active <- active_jobs_for_step(project, upstream_step)
  if (!NROW(active)) return("")
  sample_or_target <- if ("sample" %in% names(active)) as.character(active$sample) else character(NROW(active))
  if ("target" %in% names(active)) {
    empty_sample <- !nzchar(sample_or_target %||% "")
    sample_or_target[empty_sample] <- basename(as.character(active$target[empty_sample]))
  }
  sample_or_target <- sample_or_target[nzchar(sample_or_target)]
  job_ids <- if ("job_id" %in% names(active)) unique(as.character(active$job_id[nzchar(active$job_id)])) else character(0)
  paste(c(
    paste("Wait for", upstream_step, "to fully finish before running", downstream_step, "."),
    if (length(sample_or_target)) paste("Still active:", paste(unique(sample_or_target), collapse = ", ")),
    if (length(job_ids)) paste("Active SLURM job IDs:", paste(job_ids, collapse = ", "))
  ), collapse = "\n")
}

featurecounts_matrix_job_active <- function(jobs, matrix_path) {
  if (!NROW(jobs) || !"target" %in% names(jobs) || !"slurm_state" %in% names(jobs)) return(FALSE)
  active_states <- c("PENDING", "CONFIGURING", "COMPLETING", "RUNNING", "SUSPENDED", "Submitted")
  hit <- jobs[jobs$step == "featureCounts" & jobs$target == matrix_path, , drop = FALSE]
  NROW(hit) > 0 && any(hit$slurm_state %in% active_states)
}

submit_deseq2_job <- function(project, compare_col, reference, comparison, redundant = "NoRedundant", gene_name_counts = FALSE) {
  count_matrix <- file.path(project$data_dir, "counts", "count_matrix.txt")
  active_msg <- active_upstream_message(project, "featureCounts", "DESeq2")
  if (nzchar(active_msg)) {
    return(record_preflight_failure(project, "DESeq2", active_msg, "deseq2"))
  }
  if (!file.exists(count_matrix) || file_size_for(count_matrix) <= 0) {
    msg <- paste(
      "Run featureCounts successfully before DESeq2 so count_matrix.txt exists.",
      paste("Expected:", count_matrix),
      "If featureCounts sample outputs exist but this matrix is missing, refresh the Run Pipeline tab so the matrix builder can run.",
      sep = "\n"
    )
    return(record_preflight_failure(project, "DESeq2", msg, "deseq2"))
  }
  outdir <- file.path(project$data_dir, if (isTRUE(gene_name_counts)) "deseq2_gene_name" else "deseq2")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  script <- file.path(SCRIPTS_DIR, "DESeq2", "qsub_deseq2.sh")
  rscript <- file.path(SCRIPTS_DIR, "DESeq2", "DESeq2.R")
  input_mode <- format_comparison_label(compare_col, comparison, reference)
  dependency_ids <- character(0)
  if (isTRUE(gene_name_counts)) {
    res <- genome_resources(project)
    gene_matrix <- file.path(project$data_dir, "counts", "count_matrix_gene_name_aggregated.txt")
    build_script <- write_gene_name_count_matrix_script(project)
    build_cmd <- paste("Rscript", shQuote(build_script), shQuote(count_matrix), shQuote(res$gtf), shQuote(gene_matrix))
    build_msg <- submit_sbatch_wrap(project, "DESeq2", build_cmd, "deseq2_gene_name_count_matrix", "gene-name aggregated count matrix", target = gene_matrix, reference = res$gtf)
    build_id <- extract_job_id(build_msg)
    dependency_ids <- build_id[nzchar(build_id)]
    count_matrix <- gene_matrix
    input_mode <- paste(input_mode, "gene-name aggregated counts", sep = " - ")
  }
  design_matrix <- deseq_design_for_column(project, compare_col)
  submit_sbatch(project, "DESeq2", script, c(rscript, count_matrix, design_matrix, outdir, reference, comparison, redundant, project$name), "deseq2", input_mode, target = file.path(outdir, sprintf("DEG_%s_vs_%s(ref).txt", comparison, reference)), dependency_ids = dependency_ids)
}

write_gseapy_script <- function(project, run_slug = "pathway") {
  log_dir <- file.path(dirname(project$data_dir), "log")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  run_slug <- clean_name(run_slug, "pathway")
  script <- file.path(log_dir, paste0("run_gseapy_pathway_", run_slug, ".py"))
  lines <- c(
    "import os",
    "import sys",
    "import traceback",
    "",
    "script_dir = sys.argv[1]",
    "project_name = sys.argv[2]",
    "results_root = sys.argv[3]",
    "geneset = sys.argv[4]",
    "genome = sys.argv[5]",
    "feature = sys.argv[6]",
    "design_dir = sys.argv[7]",
    "deseq_dir = sys.argv[8]",
    "outpath_pathway = sys.argv[9]",
    "refcond = sys.argv[10]",
    "compared = sys.argv[11]",
    "",
    "if script_dir not in sys.path:",
    "    sys.path.insert(0, script_dir)",
    "import bulkRNAseq as csl",
    "",
    "if not results_root.endswith(os.sep):",
    "    results_root = results_root + os.sep",
    "os.makedirs(outpath_pathway, exist_ok=True)",
    "csl.project_name = project_name",
    "csl.res_dir = results_root",
    "",
    "print('CodeSpringLab script_dir:', script_dir, flush=True)",
    "print('CodeSpringLab bulkRNAseq:', getattr(csl, '__file__', 'unknown'), flush=True)",
    "print('Project:', project_name, flush=True)",
    "print('Design dir:', design_dir, flush=True)",
    "print('DESeq2 dir:', deseq_dir, flush=True)",
    "print('GSEA output:', outpath_pathway, flush=True)",
    "try:",
    "    gs, gs_res, pathways, terms, project_name_out = csl.gseapy_RunPathway(",
    "        geneset, genome, feature, design_dir, deseq_dir, outpath_pathway, refcond, compared",
    "    )",
    "    try:",
    "        csl.gseapy_DotPlot(outpath_pathway, pathways, geneset)",
    "    except Exception as exc:",
    "        print('WARNING: GSEA completed, but dot plot creation failed: {}'.format(exc), flush=True)",
    "    print('GSEA completed for {} vs {} using {}'.format(compared, refcond, geneset), flush=True)",
    "    print('Results:', outpath_pathway, flush=True)",
    "except Exception:",
    "    print('ERROR: CodeSpringLab GSEApy failed.', file=sys.stderr, flush=True)",
    "    traceback.print_exc()",
    "    raise"
  )
  writeLines(lines, script)
  script
}

write_gseapy_shell_script <- function(project, python_script, script_dir, project_name, results_root, geneset, genome,
                                      feature, design_dir, deseq_dir, outpath_pathway, reference, comparison,
                                      expected_report = "") {
  log_dir <- file.path(dirname(project$data_dir), "log")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  run_slug <- clean_name(paste(comparison, "vs", reference, geneset, sep = "_"), "pathway")
  script <- file.path(log_dir, paste0("run_gseapy_pathway_", run_slug, ".sh"))
  normalized_file <- file.path(deseq_dir, paste0("normalized_counts_", comparison, "_vs_", reference, "(ref).txt"))
  lines <- c(
    "#!/usr/bin/env bash",
    "set -eo pipefail",
    "trap 'rc=$?; echo \"ERROR: GSEApy job failed at line ${LINENO} with exit code ${rc}\" >&2; exit ${rc}' ERR",
    "echo '===== CodeSpringWeb GSEApy job ====='",
    "date",
    "echo \"Host: $(hostname)\"",
    "echo \"Working directory before setup: $(pwd)\"",
    "echo 'Bioinformatics Core kernel files:'",
    "if [ -f /usr/local/share/jupyter/kernels/bsr/kernel.json ]; then cat /usr/local/share/jupyter/kernels/bsr/kernel.json; fi",
    "if [ -f /usr/local/share/jupyter/kernels/bsr/start-kernel.sh ]; then cat /usr/local/share/jupyter/kernels/bsr/start-kernel.sh; fi",
    "trap - ERR",
    "set +eu",
    "export XDG_DATA_DIRS=\"${XDG_DATA_DIRS:-/usr/local/share:/usr/share}\"",
    "source /etc/profile",
    "PROFILE_RC=$?",
    "set +e",
    "module load BSR",
    "BSR_MODULE_RC=$?",
    "module load Python/3.7.4-GCCcore-8.3.0",
    "PYTHON_MODULE_RC=$?",
    "set -eu",
    "trap 'rc=$?; echo \"ERROR: GSEApy job failed at line ${LINENO} with exit code ${rc}\" >&2; exit ${rc}' ERR",
    "echo \"source /etc/profile exit code: $PROFILE_RC\"",
    "echo \"module load BSR exit code: $BSR_MODULE_RC\"",
    "echo \"module load Python/3.7.4-GCCcore-8.3.0 exit code: $PYTHON_MODULE_RC\"",
    "hash -r || true",
    "echo 'Loaded modules:'",
    "module list 2>&1 || true",
    "PYTHON_EXE=${CSL_PYTHON_BIN:-$(command -v python 2>/dev/null || true)}",
    "if [ -z \"${PYTHON_EXE:-}\" ] || ! \"$PYTHON_EXE\" -c 'import gseapy' >/dev/null 2>&1; then",
    "  for candidate in /grid/it/modules/bsr/software/Python/3.7.4-GCCcore-8.3.0/bin/python /grid/it/modules/bsr/software/Python/3.7.4-GCCcore-8.3.0/bin/python3; do",
    "    if [ -x \"$candidate\" ] && \"$candidate\" -c 'import gseapy' >/dev/null 2>&1; then PYTHON_EXE=\"$candidate\"; break; fi",
    "  done",
    "fi",
    "if [ -z \"${PYTHON_EXE:-}\" ]; then echo 'ERROR: python was not found after loading the Bioinformatics Core modules.' >&2; exit 1; fi",
    "echo \"Using Python: $PYTHON_EXE\"",
    "PYTHON_VERSION=$(\"$PYTHON_EXE\" -V 2>&1)",
    "echo \"Python executable reports: $PYTHON_VERSION\"",
    paste0("[ -s ", shQuote(file.path(design_dir, "design_matrix.txt")), " ] || { echo ", shQuote(paste0("ERROR: Missing design matrix: ", file.path(design_dir, "design_matrix.txt"))), " >&2; exit 1; }"),
    paste0("[ -s ", shQuote(normalized_file), " ] || { echo ", shQuote(paste0("ERROR: Missing normalized counts: ", normalized_file)), " >&2; exit 1; }"),
    paste0("[ -d ", shQuote(script_dir), " ] || { echo ", shQuote(paste0("ERROR: Missing CodeSpringLab scripts dir: ", script_dir)), " >&2; exit 1; }"),
    "\"$PYTHON_EXE\" - <<PY",
    "import sys",
    paste0("sys.path.insert(0, ", deparse(script_dir), ")"),
    "import pandas, matplotlib, gseapy",
    "import bulkRNAseq as csl",
    "print('Python version:', sys.version.replace('\\n', ' '))",
    "print('gseapy version:', getattr(gseapy, '__version__', 'unknown'))",
    "print('bulkRNAseq module:', getattr(csl, '__file__', 'unknown'))",
    "PY",
    paste(
      "\"$PYTHON_EXE\" -u",
      shQuote(python_script),
      shQuote(script_dir),
      shQuote(project_name),
      shQuote(results_root),
      shQuote(geneset),
      shQuote(genome),
      shQuote(feature),
      shQuote(design_dir),
      shQuote(deseq_dir),
      shQuote(outpath_pathway),
      shQuote(reference),
      shQuote(comparison)
    ),
    paste0("GENERIC_REPORT=", shQuote(file.path(outpath_pathway, "gseapy.gene_set.gsea.report.csv"))),
    paste0("EXPECTED_REPORT=", shQuote(expected_report %||% "")),
    "if [ -n \"${EXPECTED_REPORT:-}\" ] && [ ! -s \"$EXPECTED_REPORT\" ] && [ -s \"$GENERIC_REPORT\" ]; then cp \"$GENERIC_REPORT\" \"$EXPECTED_REPORT\"; fi",
    "if [ -n \"${EXPECTED_REPORT:-}\" ] && [ ! -s \"$EXPECTED_REPORT\" ]; then echo \"ERROR: Missing expected GSEA report: $EXPECTED_REPORT\" >&2; exit 1; fi"
  )
  writeLines(lines, script)
  Sys.chmod(script, mode = "0755")
  script
}

submit_gseapy_job <- function(project, compare_col, reference, comparison, geneset) {
  geneset <- trimws(geneset %||% "")
  if (!nzchar(geneset)) stop("Choose a GSEA gene-set database.")
  deseq_dir <- file.path(project$data_dir, "deseq2")
  normalized_file <- file.path(deseq_dir, paste0("normalized_counts_", comparison, "_vs_", reference, "(ref).txt"))
  active_msg <- active_upstream_message(project, "DESeq2", "GSEA")
  if (nzchar(active_msg)) {
    return(record_preflight_failure(project, "GSEA", active_msg, "gseapy"))
  }
  if (!file.exists(normalized_file)) {
    msg <- paste(
      "Run DESeq2 successfully for this exact comparison before GSEA.",
      paste("Expected normalized-counts file:", normalized_file),
      sep = "\n"
    )
    return(record_preflight_failure(project, "GSEA", msg, "gseapy"))
  }
  geneset_slug <- clean_name(geneset, "geneset")
  run_slug <- clean_name(paste(compare_col, comparison, "vs", reference, geneset, sep = "_"), "gseapy")
  outpath_pathway <- paste0(file.path(project$data_dir, "gseapy", paste0(comparison, "_vs_", reference), geneset_slug), "/")
  target <- file.path(outpath_pathway, paste0("report.gseapy.", geneset_slug, ".csv"))
  dir.create(outpath_pathway, recursive = TRUE, showWarnings = FALSE)
  design_matrix <- deseq_design_for_column(project, compare_col)
  design_dir <- dirname(design_matrix)
  results_root <- project$results_root %||% dirname(dirname(project$data_dir))
  python_script <- write_gseapy_script(project, run_slug)
  shell_script <- write_gseapy_shell_script(
    project = project,
    python_script = python_script,
    script_dir = SCRIPTS_DIR,
    project_name = project$name,
    results_root = results_root,
    geneset = geneset,
    genome = genome_species(project),
    design_dir = design_dir,
    deseq_dir = deseq_dir,
    outpath_pathway = outpath_pathway,
    reference = reference,
    comparison = comparison,
    feature = "auto",
    expected_report = target
  )
  cmd <- paste("bash", shQuote(shell_script))
  submit_sbatch_wrap(project, "GSEA", cmd, paste0("gseapy_", geneset_slug), format_comparison_label(compare_col, comparison, reference, geneset), target = target, reference = geneset)
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
  source_result <- tryCatch({
    sys.source(app_file, envir = env)
    NULL
  }, error = function(e) e)
  if (inherits(source_result, "error")) {
    return(list(
      id = paste(project$id, "native-error", normalizePath(cfg, winslash = "/", mustWork = FALSE), sep = "::"),
      ui = div(
        class = "empty-box",
        tags$h4("Results Explorer could not load."),
        tags$p("The embedded CodeSpringLab RNA-seq viewer raised an error while loading."),
        tags$pre(conditionMessage(source_result)),
        tags$p("Project data folder:"),
        tags$code(project$data_dir)
      ),
      server = function(input, output, session) NULL
    ))
  }
  if (!exists("ui", envir = env, inherits = FALSE) || !exists("server", envir = env, inherits = FALSE)) {
    return(list(
      id = paste(project$id, "native-missing-objects", normalizePath(cfg, winslash = "/", mustWork = FALSE), sep = "::"),
      ui = div(class = "empty-box", "Results Explorer loaded, but did not expose both ui and server objects."),
      server = function(input, output, session) NULL
    ))
  }
  list(
    id = paste(project$id, normalizePath(cfg, winslash = "/", mustWork = FALSE), sep = "::"),
    ui = div(class = "native-results-host", env$ui),
    server = env$server
  )
}

run_step_meta <- function(project = NULL) {
  steps <- pipeline_order(project)
  descriptions <- if (!is.null(project) && is_cutrun_project(project)) {
    c(
      "Create or load design_matrix.txt.",
      "Trim adapters and short reads.",
      "Generate per-read quality reports.",
      "Align fragments with Bowtie2 and create BAM/bedGraph/bigWig outputs.",
      "Call sparse CUT&RUN peaks with SEACR.",
      "Build consensus SEACR peaks, peak counts, and FRiP summaries.",
      "Build mark-specific consensus peaks and run DiffBind/DESeq2 differential binding.",
      "Optional MACS2 peak calling for comparison or broad histone marks."
    )
  } else if (!is.null(project) && is_atac_project(project)) {
    c(
      "Create or load design_matrix.txt.",
      "Trim Nextera/ATAC adapters and short reads.",
      "Generate per-read quality reports.",
      "Align paired-end fragments to the GRCm39/GENCODE M39 Bowtie2 index and create duplicate-removed CPM bigWigs.",
      "Call shifted ATAC-seq peaks with MACS2 and annotate them with Homer.",
      "Build a consensus peakset and test differential accessibility with DiffBind/DESeq2."
    )
  } else {
    c(
      "Create or load design_matrix.txt.",
      "Trim adapters and short reads.",
      "Generate per-read quality reports.",
      "Align reads and write BAM files.",
      "Create gene-level count files and count_matrix.txt.",
      "Run differential expression and normalized counts.",
      "Run pathway analysis.",
      "Optional RSEM quantification from STAR BAM/transcriptome outputs.",
      "Optional Kallisto transcript quantification from raw or trimmed reads."
    )
  }
  data.frame(
    order = seq_along(steps),
    step = steps,
    description = descriptions,
    stringsAsFactors = FALSE
  )
}

pipeline_stepper_ui <- function(project, status = NULL) {
  if (is.null(status) || !NROW(status)) status <- project_status(project)
  meta <- run_step_meta(project)
  div(class = "pipeline-stepper", lapply(seq_len(NROW(meta)), function(i) {
    hit <- match(meta$step[i], status$step)
    st <- status$status[hit] %||% "Not started"
    mode <- status$input[hit] %||% ""
    detail <- if ("detail" %in% names(status)) status$detail[hit] %||% "" else ""
    cls <- status_css_key(st)
    div(class = paste("pipeline-step", cls),
        div(class = "step-index", meta$order[i]),
        div(class = "step-main",
            tags$strong(meta$step[i]),
            tags$span(status_label(st)),
            if (nzchar(detail)) tags$em(detail) else if (nzchar(mode)) tags$em(mode) else NULL)
    )
  }))
}

tool_panel <- function(step, status, description, controls, button_id, button_label, progress_df = data.frame(), status_step = step, show_sample_progress = TRUE, show_job_actions = TRUE) {
  st <- status$status[match(status_step, status$step)] %||% "Not started"
  mode <- status$input[match(status_step, status$step)] %||% ""
  cls <- status_css_key(st)
  tags$details(
    class = paste("tool-panel", cls),
    open = if (identical(st, "Active") || identical(st, "Not started")) TRUE else NULL,
    tags$summary(
      div(class = "tool-summary",
          div(tags$strong(step), tags$span(description)),
          div(class = "tool-right", status_pill(st), if (nzchar(mode)) tags$small(mode) else NULL)
      )
    ),
    div(class = "tool-body",
        controls,
        actionButton(button_id, button_label, class = "btn-primary"),
        uiOutput(tool_message_output_id(step)),
        if (isTRUE(show_sample_progress) && status_step %in% sample_level_pipeline_steps()) uiOutput(tool_progress_ui_output_id(status_step)) else NULL,
        if (isTRUE(show_sample_progress) && status_step %in% sample_level_pipeline_steps()) uiOutput(tool_retry_ui_output_id(status_step)) else NULL,
        if (isTRUE(show_job_actions)) div(class = "tool-cancel-zone",
            actionButton(tool_cancel_button_id(step), "Cancel active jobs", class = "btn-danger btn-sm")
        ) else NULL,
        if (isTRUE(show_job_actions)) div(class = "tool-delete-zone",
            actionButton(tool_delete_data_button_id(step), "Delete step data", class = "btn-danger btn-sm")
        ) else NULL
    )
  )
}

active_slurm_states <- function() {
  c("PENDING", "CONFIGURING", "COMPLETING", "RUNNING", "SUSPENDED", "Submitted")
}

clean_run_label <- function(x, fallback = "") {
  x <- trimws(as.character(x %||% fallback))
  x <- gsub("[[:space:]]+", " ", x)
  ifelse(nzchar(x), x, fallback)
}

format_comparison_label <- function(compare_col = "", comparison = "", reference = "", suffix = "") {
  compare_col <- trimws(as.character(compare_col %||% ""))
  compare_col <- sub(":+$", "", compare_col)
  comparison <- trimws(as.character(comparison %||% ""))
  reference <- trimws(as.character(reference %||% ""))
  suffix <- trimws(as.character(suffix %||% ""))
  main <- if (nzchar(comparison) && nzchar(reference)) paste(comparison, "vs", reference) else trimws(paste(comparison, reference))
  if (nzchar(compare_col) && nzchar(main)) main <- paste0(compare_col, ": ", main)
  if (nzchar(suffix) && nzchar(main)) main <- paste(main, "-", suffix)
  clean_run_label(main, fallback = paste(compare_col, comparison, reference))
}

parse_comparison_label <- function(x) {
  x <- clean_run_label(x)
  if (!nzchar(x)) return("")
  x <- gsub("_vs_", " vs ", x, fixed = TRUE)
  x <- gsub("__vs__", " vs ", x, fixed = TRUE)
  suffix <- ""
  db_split <- strsplit(x, " - ", fixed = TRUE)[[1]]
  if (length(db_split) > 1) {
    suffix <- paste(db_split[-1], collapse = " - ")
    x <- db_split[1]
  }
  compare_col <- ""
  if (grepl(":", x, fixed = TRUE)) {
    colon_split <- strsplit(x, ":", fixed = TRUE)[[1]]
    compare_col <- sub(":+$", "", trimws(colon_split[1]))
    x <- sub("^:+", "", trimws(paste(colon_split[-1], collapse = ":")))
  }
  parts <- strsplit(x, " ", fixed = TRUE)[[1]]
  parts <- parts[nzchar(parts)]
  if (length(parts) >= 3 && identical(parts[2], "vs")) {
    return(format_comparison_label(compare_col, parts[1], parts[3], suffix))
  }
  if (length(parts) >= 4 && identical(parts[3], "vs")) {
    return(format_comparison_label(sub(":+$", "", parts[1]), parts[2], parts[4], suffix))
  }
  x
}

comparison_status_key <- function(x) {
  x <- vapply(as.character(x %||% ""), parse_comparison_label, character(1))
  x <- sub("^.*:[[:space:]]*", "", x)
  x <- sub("[[:space:]]+-[[:space:]].*$", "", x)
  gsub("[^a-z0-9]+", "", tolower(x))
}

drop_running_completed_labels <- function(running, complete) {
  if (!length(running) || !length(complete)) return(running)
  running[!comparison_status_key(running) %in% comparison_status_key(complete)]
}

comparison_label_from_file <- function(file, jobs = data.frame(), step = "DESeq2") {
  m <- regexec("^normalized_counts_(.*)_vs_(.*)\\(ref\\)\\.txt$", file)
  hit <- regmatches(file, m)[[1]]
  if (length(hit) != 3) return(file)
  comparison <- hit[[2]]
  reference <- hit[[3]]
  compare_col <- ""
  if (NROW(jobs) && all(c("step", "input_mode") %in% names(jobs))) {
    job_hit <- jobs[jobs$step == step & grepl(paste(comparison, "vs", reference), jobs$input_mode, fixed = TRUE), , drop = FALSE]
    if (NROW(job_hit)) {
      parsed <- parse_comparison_label(tail(job_hit$input_mode, 1))
      if (nzchar(parsed)) return(parsed)
    }
  }
  format_comparison_label(compare_col, comparison, reference)
}

completed_project_level_runs <- function(project, step, jobs = NULL) {
  data_dir <- project$data_dir
  if (is.null(jobs)) jobs <- job_history(project)
  if (identical(step, "DESeq2")) {
    files <- list.files(file.path(data_dir, "deseq2"), pattern = "^normalized_counts_.*\\(ref\\)\\.txt$", full.names = FALSE)
    labels <- vapply(files, comparison_label_from_file, character(1), jobs = jobs, step = step)
    return(sort(unique(labels[nzchar(labels)])))
  }
  if (identical(step, "GSEA")) {
    gsea_dir <- file.path(data_dir, "gseapy")
    files <- if (dir.exists(gsea_dir)) {
      list.files(gsea_dir, pattern = "gseapy\\.gene_set\\.gsea\\.report\\.csv$|^report\\.gseapy\\..*\\.csv$", recursive = TRUE, full.names = TRUE)
    } else character(0)
    database_files <- files[grepl("^report\\.gseapy\\..*\\.csv$", basename(files))]
    files <- database_files
    labels <- vapply(files, function(file) {
      root <- normalizePath(gsea_dir, winslash = "/", mustWork = FALSE)
      full <- normalizePath(file, winslash = "/", mustWork = FALSE)
      prefix <- paste0(sub("/+$", "", root), "/")
      rel <- if (startsWith(full, prefix)) substring(full, nchar(prefix) + 1) else basename(file)
      rel_dir <- dirname(rel)
      if (!nzchar(rel_dir) || identical(rel_dir, ".")) rel_dir <- basename(dirname(file))
      if (grepl("^report\\.gseapy\\..*\\.csv$", basename(file))) {
        db <- sub("^report\\.gseapy\\.", "", basename(file))
        db <- sub("\\.csv$", "", db)
        comparison_dir <- strsplit(rel_dir, "/", fixed = TRUE)[[1]][[1]]
        parse_comparison_label(paste(comparison_dir, "-", db))
      } else {
        parse_comparison_label(rel_dir)
      }
    }, character(1))
    return(sort(unique(labels[nzchar(labels)])))
  }
  character(0)
}

running_project_level_runs <- function(jobs, step) {
  if (!NROW(jobs) || !"step" %in% names(jobs) || !"slurm_state" %in% names(jobs)) return(character(0))
  active_states <- setdiff(active_slurm_states(), c("Submitted"))
  hit <- jobs[jobs$step == step & jobs$slurm_state %in% active_states, , drop = FALSE]
  if (!NROW(hit)) return(character(0))
  labels <- if ("input_mode" %in% names(hit)) as.character(hit$input_mode) else rep("", NROW(hit))
  fallback <- if ("job_id" %in% names(hit)) paste("Job", hit$job_id) else paste(step, seq_len(NROW(hit)))
  labels <- mapply(clean_run_label, labels, fallback, USE.NAMES = FALSE)
  labels <- vapply(labels, parse_comparison_label, character(1))
  sort(unique(labels[nzchar(labels)]))
}

cancelled_project_level_runs <- function(jobs, step) {
  if (!NROW(jobs) || !"step" %in% names(jobs) || !"slurm_state" %in% names(jobs)) return(character(0))
  cancelled_states <- c("CANCELLED", "CANCELLED+", "CA", "TIMEOUT", "FAILED", "NODE_FAIL", "PREEMPTED")
  hit <- jobs[jobs$step == step & jobs$slurm_state %in% cancelled_states, , drop = FALSE]
  if (!NROW(hit)) return(character(0))
  labels <- if ("input_mode" %in% names(hit)) as.character(hit$input_mode) else rep("", NROW(hit))
  fallback <- if ("job_id" %in% names(hit)) paste("Job", hit$job_id) else paste(step, seq_len(NROW(hit)))
  labels <- mapply(clean_run_label, labels, fallback, USE.NAMES = FALSE)
  labels <- vapply(labels, parse_comparison_label, character(1))
  sort(unique(labels[nzchar(labels)]))
}

project_level_step_summary_ui <- function(project, jobs, step) {
  complete <- completed_project_level_runs(project, step, jobs)
  running <- running_project_level_runs(jobs, step)
  cancelled <- cancelled_project_level_runs(jobs, step)
  if (length(complete) && length(running)) {
    if (identical(step, "GSEA")) {
      running <- setdiff(running, complete)
    } else {
      running <- drop_running_completed_labels(running, complete)
    }
  }
  if (!length(complete) && !length(running) && !length(cancelled)) return(NULL)
  title <- if (identical(step, "GSEA")) "GSEA status" else "Comparison status"
  row_ui <- function(label, values, cls) {
    if (!length(values)) return(NULL)
    div(class = "project-step-summary-row",
        div(class = "project-step-summary-label", label),
        div(class = "project-step-chip-wrap", lapply(values, function(value) span(class = paste("project-step-chip", cls), value)))
    )
  }
  div(class = "project-step-summary",
      div(class = "project-step-summary-title", title),
      row_ui("Running", running, "running"),
      row_ui("Cancelled", cancelled, "cancelled"),
      row_ui("Complete", complete, "complete")
  )
}

list_result_files <- function(project, pattern = "\\.(txt|csv|tsv|html|png|pdf)$") {
  if (!dir.exists(project$data_dir)) return(character(0))
  list.files(project$data_dir, pattern = pattern, recursive = TRUE, full.names = TRUE)
}

relative_result_labels <- function(project, files) {
  if (!length(files)) return(character(0))
  prefix <- paste0(normalizePath(project$data_dir, winslash = "/", mustWork = FALSE), "/")
  labels <- normalizePath(files, winslash = "/", mustWork = FALSE)
  labels[startsWith(labels, prefix)] <- substring(labels[startsWith(labels, prefix)], nchar(prefix) + 1)
  labels
}

result_file_choices <- function(project, subdirs = character(0), pattern = "\\.(txt|csv|tsv)$") {
  files <- list_result_files(project, pattern)
  if (length(subdirs)) {
    keep <- paste0("/(", paste(subdirs, collapse = "|"), ")/")
    files <- files[grepl(keep, files)]
  }
  files <- sort(files)
  stats::setNames(files, relative_result_labels(project, files))
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
.csl-header { background:linear-gradient(135deg,#0f2742 0%,#145f78 58%,#1f8f7a 100%); color:white; border:0; border-radius:8px; padding:34px 42px; margin-bottom:18px; min-height:168px; display:flex; align-items:center; justify-content:space-between; gap:34px; }
.brand-lockup { display:flex; align-items:center; gap:28px; }
.brand-lockup img { background:white; border-radius:8px; padding:10px; max-height:112px; max-width:285px; object-fit:contain; }
.csl-header h2 { margin:0 0 8px 0; font-weight:800; font-size:40px; color:white; }
.csl-header .muted { color:#dceaf4; }
.muted { color:#657084; }
.empty-box { background:white; border:1px solid #d8dde8; border-radius:8px; padding:18px; color:#657084; }
.cutrun-metric-grid { display:grid; grid-template-columns:repeat(auto-fit,minmax(180px,1fr)); gap:12px; margin:16px 0 20px 0; }
.cutrun-metric-grid.compact { grid-template-columns:repeat(auto-fit,minmax(160px,1fr)); }
.cutrun-metric-card { background:white; border:1px solid #d8dde8; border-top:4px solid #2f6fed; border-radius:10px; padding:15px 16px; min-height:116px; box-shadow:0 2px 8px rgba(15,39,66,0.05); }
.cutrun-metric-card.tone-green { border-top-color:#15936f; }
.cutrun-metric-card.tone-gold { border-top-color:#d39116; }
.cutrun-metric-card.tone-purple { border-top-color:#805ad5; }
.cutrun-metric-label { color:#657084; font-size:12px; font-weight:700; letter-spacing:.04em; text-transform:uppercase; }
.cutrun-metric-value { color:#143150; font-size:26px; line-height:1.15; font-weight:800; margin:7px 0 5px 0; overflow-wrap:anywhere; }
.cutrun-metric-note { color:#657084; font-size:12px; line-height:1.35; }
.cutrun-chart-card { background:white; border:1px solid #d8dde8; border-radius:10px; padding:14px 16px 8px 16px; margin:12px 0 18px 0; }
.read-source-note { background:#f5f9ff; border-left:4px solid #2f6fed; border-radius:6px; padding:10px 12px; margin:8px 0 12px 0; color:#48627d; }
.cutrun-results-host { overflow:visible !important; font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif; }
.cutrun-results-shell { background:rgba(255,255,255,.72); border:1px solid rgba(255,255,255,.9); border-radius:10px !important; box-shadow:none; overflow:hidden; }
.cutrun-results-hero { position:relative; background:linear-gradient(135deg,rgba(8,41,84,.96) 0%,rgba(12,69,132,.92) 54%,rgba(20,132,107,.86) 100%); color:white; }
.cutrun-results-hero:after { content:''; position:absolute; inset:0; background:radial-gradient(circle at 85% 18%,rgba(255,255,255,.16),transparent 20%),radial-gradient(circle at 18% 120%,rgba(255,255,255,.10),transparent 24%); pointer-events:none; }
.cutrun-results-hero .hero-topbar { display:flex; align-items:center; justify-content:space-between; gap:18px; flex-wrap:wrap; }
.cutrun-results-hero .hero-title { color:white; margin:0 0 4px 0; }
.cutrun-results-hero .hero-kicker { color:#dff7f0; }
.cutrun-results-host .main-tabs { padding:10px 12px 14px; }
.cutrun-results-host .main-tabs > .tabbable > .nav-tabs { border:0; display:flex; flex-wrap:wrap; gap:10px; margin-bottom:18px; }
.cutrun-results-host .main-tabs > .tabbable > .nav-tabs > li { margin:0; }
.cutrun-results-host .main-tabs > .tabbable > .nav-tabs > li > a { border:0 !important; border-radius:999px !important; background:rgba(255,255,255,.8); color:#27425f !important; font-weight:700; padding:11px 18px; box-shadow:0 6px 16px rgba(28,54,88,.08); }
.cutrun-results-host .main-tabs > .tabbable > .nav-tabs > li.active > a { background:linear-gradient(135deg,#0f62c6,#19a974) !important; color:white !important; box-shadow:0 10px 22px rgba(24,95,185,.28); }
.cutrun-results-host .tab-content .tabbable > .nav-tabs { border:1px solid rgba(209,223,239,.95); display:inline-flex; flex-wrap:wrap; gap:8px; margin:6px 0 18px; padding:8px; background:rgba(237,244,251,.95); border-radius:18px; }
.cutrun-results-host .tab-content .tabbable > .nav-tabs > li { margin:0; }
.cutrun-results-host .tab-content .tabbable > .nav-tabs > li > a { border:0 !important; border-radius:14px !important; background:transparent; color:#48627d !important; font-weight:700; padding:10px 14px; }
.cutrun-results-host .tab-content .tabbable > .nav-tabs > li.active > a { background:linear-gradient(135deg,#fff,#f4f9ff) !important; color:#143150 !important; box-shadow:0 8px 16px rgba(35,63,99,.10); }
.cutrun-results-host .row > .col-sm-2 > .well, .cutrun-results-host .row > .col-sm-3 > .well { background:rgba(248,251,255,.96); border:1px solid rgba(197,215,236,.9); border-radius:22px; padding:20px 18px; box-shadow:inset 0 1px 0 rgba(255,255,255,.9); }
.cutrun-results-host .row > .col-sm-10, .cutrun-results-host .row > .col-sm-9 { background:rgba(255,255,255,.86); border:1px solid rgba(214,225,238,.95); border-radius:22px; padding:20px 22px; box-shadow:0 10px 26px rgba(32,56,84,.08); }
.cutrun-results-host .form-control { border-radius:14px !important; border:1px solid #d7e0ea !important; box-shadow:none !important; min-height:46px; font-size:15px !important; }
.cutrun-results-host .control-label { font-size:13px; text-transform:uppercase; letter-spacing:.08em; color:#5a7088; margin-bottom:8px; }
.cutrun-results-host h4 { color:#18314e; font-weight:800; letter-spacing:-.02em; font-size:22px; }
.cutrun-results-host iframe { display:block; width:100%; min-height:680px; border:1px solid #d7e0ea !important; border-radius:18px; background:white; box-shadow:0 8px 22px rgba(32,56,84,.10); }
.cutrun-results-host .btn, .cutrun-results-host .btn-default, .cutrun-results-host .btn-primary { border:0 !important; border-radius:12px !important; background:linear-gradient(135deg,#0f62c6,#15936f) !important; color:white !important; font-weight:750; padding:9px 15px; box-shadow:0 8px 18px rgba(17,94,177,.18); }
.cutrun-results-host .btn:hover { transform:translateY(-1px); box-shadow:0 11px 22px rgba(17,94,177,.25); }
.cutrun-results-host table.dataTable thead th { background:#edf4fb !important; color:#304a66 !important; border-bottom:1px solid #aac2de !important; }
.cutrun-results-host table.dataTable tbody tr:nth-child(odd) { background:rgba(246,250,255,.9) !important; }
.cutrun-results-host .dataTables_wrapper { background:white; border:1px solid #dbe5f0; border-radius:14px; padding:10px; }
.cutrun-results-actions { display:flex; justify-content:space-between; align-items:center; gap:12px; margin-bottom:8px; }
.cutrun-updated-note { color:#657084; font-size:13px; font-weight:700; }
.cutrun-section-heading { display:flex; align-items:center; justify-content:space-between; gap:14px; flex-wrap:wrap; margin:16px 0 10px; }
.cutrun-section-heading h4 { margin:0; }
.cutrun-section-heading p { margin:0; color:#657084; font-size:13px; flex:1 1 320px; }
.cutrun-file-meta { display:flex; flex-direction:column; gap:9px; }
.cutrun-file-meta > div { display:flex; align-items:center; justify-content:space-between; gap:10px; padding-bottom:7px; border-bottom:1px solid #e6edf5; }
.cutrun-file-meta span { color:#657084; font-size:12px; font-weight:800; text-transform:uppercase; letter-spacing:.05em; }
.cutrun-file-meta code { display:block; white-space:normal; overflow-wrap:anywhere; word-break:break-word; padding:9px; border-radius:8px; background:#edf4fb; color:#304a66; }
@media (max-width:900px) {
  .cutrun-results-host .row > .col-sm-2, .cutrun-results-host .row > .col-sm-3, .cutrun-results-host .row > .col-sm-9, .cutrun-results-host .row > .col-sm-10 { width:100% !important; float:none !important; margin-bottom:12px; }
  .cutrun-results-actions { align-items:flex-start; }
  .cutrun-results-host iframe { min-height:560px; }
}
.btn-primary { background:#1f5eff; border-color:#1f5eff; }
.status-toolbar { display:flex; gap:12px; align-items:flex-end; flex-wrap:wrap; margin-bottom:16px; }
.status-grid { display:grid; grid-template-columns:repeat(auto-fit, minmax(250px, 1fr)); gap:12px; margin-bottom:18px; }
.status-card, .run-card, .tool-panel { background:white; border:1px solid #d8dde8; border-radius:8px; padding:14px; box-shadow:0 1px 2px rgba(15,23,36,0.04); }
.status-card-top, .run-card-top { display:flex; justify-content:space-between; align-items:center; gap:10px; margin-bottom:8px; }
.status-path { color:#657084; font-size:12px; white-space:normal; overflow-wrap:anywhere; word-break:break-word; }
.status-pill { display:inline-flex; align-items:center; border-radius:999px; padding:4px 9px; font-size:12px; font-weight:700; white-space:nowrap; }
.status-pill.active { color:#7c3d00; background:#fff4d6; border:1px solid #f0c36d; }
.status-pill.complete { color:#0b6b3a; background:#def7e8; border:1px solid #8fd8ad; }
.status-pill.cancelled { color:#8a2f24; background:#fff0ed; border:1px solid #e5a397; }
.status-pill.failed { color:#8a2f24; background:#fff0ed; border:1px solid #e5a397; }
.status-pill.deleted-complete { color:#315f4c; background:#eefaf3; border:1px solid #b7dfc7; }
.status-pill.deleted-failed, .status-pill.deleted-cancelled { color:#8a2f24; background:#fff0ed; border:1px solid #e5a397; }
.status-pill.not-started { color:#526070; background:#eef2f7; border:1px solid #cfd7e3; }
.run-grid { display:grid; grid-template-columns:1fr; gap:12px; margin-top:14px; }
.run-card p { min-height:38px; margin-bottom:12px; }
.tool-panel { padding:0; overflow:hidden; }
.tool-panel summary { cursor:pointer; list-style:none; padding:14px 16px; }
.tool-panel summary::-webkit-details-marker { display:none; }
.tool-panel.complete { border-left:5px solid #27ae60; }
.tool-panel.active { border-left:5px solid #d99a15; }
.tool-panel.cancelled { border-left:5px solid #d55745; }
.tool-panel.failed, .tool-panel.deleted-failed, .tool-panel.deleted-cancelled { border-left:5px solid #d55745; }
.tool-panel.deleted-complete { border-left:5px solid #7abf8d; }
.tool-panel.not-started { border-left:5px solid #d55745; }
.tool-summary { display:flex; justify-content:space-between; align-items:center; gap:16px; }
.tool-summary strong { display:block; font-size:16px; }
.tool-summary span { color:#657084; font-size:13px; }
.tool-right { display:flex; align-items:center; gap:8px; flex-wrap:wrap; justify-content:flex-end; }
.tool-right small { color:#657084; }
.tool-body { padding:0 16px 16px 16px; border-top:1px solid #edf1f6; }
.tool-body .form-group { margin-bottom:10px; }
.run-message-alert { margin:12px 0 16px 0; border-radius:8px; padding:13px 15px; border:1px solid #cfd7e3; background:white; color:#304a66; box-shadow:0 1px 2px rgba(15,23,36,0.04); white-space:pre-wrap; overflow-wrap:anywhere; }
.run-message-alert strong { display:block; margin-bottom:4px; color:#17202f; }
.run-message-alert.error { background:#fff0ed; border-color:#e5a397; color:#8a2f24; }
.run-message-alert.error strong { color:#8a2f24; }
.run-message-alert.success { background:#eefaf3; border-color:#b7dfc7; color:#315f4c; }
.run-message-alert.active { background:#fff4d6; border-color:#f0c36d; color:#7c3d00; }
.tool-message-alert { margin:12px 0; }
.tool-cancel-zone { margin-top:12px; padding-top:12px; border-top:1px dashed #d8dde8; display:flex; align-items:center; gap:12px; flex-wrap:wrap; }
.tool-delete-zone { margin-top:8px; padding:10px 12px; border:1px solid #f0c1ba; border-radius:8px; background:#fff7f5; display:flex; align-items:center; gap:12px; flex-wrap:wrap; }
.tool-progress-wrap { margin-top:16px; border:1px solid #d8dde8; border-radius:8px; overflow:hidden; background:#f8fafc; }
.tool-progress-title { padding:12px 14px; font-size:13px; font-weight:800; color:#304a66; text-transform:uppercase; letter-spacing:.04em; border-bottom:1px solid #d8dde8; background:#edf4fb; }
.tool-progress-table { width:100%; border-collapse:separate; border-spacing:0; table-layout:fixed; }
.tool-progress-table th { padding:11px 14px; font-size:12px; color:#657084; text-align:left; border-bottom:1px solid #e4eaf2; background:white; }
.tool-progress-table td { padding:11px 14px; font-size:13px; border-bottom:1px solid #edf1f6; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
.tool-progress-table tr:last-child td { border-bottom:0; }
.tool-progress-table .sample-status { min-width:118px; width:auto; max-width:100%; padding:6px 11px; font-size:12px; }
.tool-progress-table .sample-name { font-weight:800; color:#17202f; }
.sample-retry-zone { margin-top:12px; padding:12px 14px; border:1px solid #f0c36d; border-radius:8px; background:#fff8e6; }
.sample-retry-zone.complete { display:flex; align-items:center; gap:8px; border-color:#b7dfc7; background:#eefaf3; color:#315f4c; }
.sample-retry-heading { display:flex; align-items:baseline; gap:8px; flex-wrap:wrap; margin-bottom:9px; color:#7c3d00; }
.sample-retry-heading span { font-size:12px; color:#657084; }
.sample-retry-chip-wrap { display:flex; flex-wrap:wrap; gap:7px; margin-bottom:10px; max-height:180px; overflow:auto; }
.sample-retry-chip { border-radius:999px; border:1px solid #e3b34f; background:white; padding:5px 9px; font-size:12px; font-weight:800; color:#6f4200; overflow-wrap:anywhere; }
.project-step-summary { margin:12px 0; border:1px solid #d8dde8; border-radius:8px; background:#f8fafc; padding:12px; }
.project-step-summary-title { font-size:12px; font-weight:800; color:#304a66; text-transform:uppercase; letter-spacing:.04em; margin-bottom:8px; }
.project-step-summary-row { display:flex; align-items:flex-start; gap:10px; margin-top:8px; }
.project-step-summary-label { min-width:78px; font-size:12px; font-weight:800; color:#657084; padding-top:4px; }
.project-step-chip-wrap { display:flex; flex-wrap:wrap; gap:7px; flex:1; }
.project-step-chip { border-radius:999px; border:1px solid #cfd7e3; background:white; padding:5px 9px; font-size:12px; font-weight:800; color:#304a66; max-width:100%; overflow-wrap:anywhere; }
.project-step-chip.running { background:#fff4d6; color:#7c3d00; border-color:#f0c36d; }
.project-step-chip.cancelled { background:#fff0ed; color:#8a2f24; border-color:#e5a397; }
.project-step-chip.complete { background:#def7e8; color:#0b6b3a; border-color:#8fd8ad; }
.methods-table-wrap { background:white; border:1px solid #d8dde8; border-radius:8px; padding:12px; overflow:visible; }
.methods-table-wrap .dataTables_wrapper { width:100%; overflow:visible; }
.methods-table-wrap table.dataTable { width:100% !important; table-layout:fixed; }
.methods-table-wrap table.dataTable th,
.methods-table-wrap table.dataTable td { white-space:normal !important; overflow-wrap:anywhere; word-break:break-word; vertical-align:top; line-height:1.35; }
.methods-table-wrap table.dataTable td { padding-top:10px; padding-bottom:10px; }
.methods-table-wrap .dataTables_scrollBody { overflow-x:hidden !important; }
.adaptive-table-note { color:#657084; font-size:13px; font-weight:700; margin:0 0 10px 0; }
.resource-strip { display:grid; grid-template-columns:minmax(280px,.85fr) minmax(460px,1.45fr); gap:16px; align-items:stretch; margin:12px 0 18px 0; }
.resource-card { background:white; border:1px solid #d8dde8; border-radius:8px; padding:16px; min-width:0; }
.resource-card p { white-space:normal; overflow-wrap:anywhere; word-break:break-word; }
.resource-card.flowchart-card { display:flex; align-items:center; justify-content:center; min-height:360px; overflow:hidden; }
.resource-card img { width:100%; max-width:100%; max-height:380px; object-fit:contain; }
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
.pipeline-step.cancelled { background:#fff0ed; border-color:#e5a397; }
.pipeline-step.failed, .pipeline-step.deleted-failed, .pipeline-step.deleted-cancelled { background:#fff0ed; border-color:#e5a397; }
.pipeline-step.deleted-complete { background:#eefaf3; border-color:#b7dfc7; }
.step-index { width:28px; height:28px; border-radius:50%; background:white; display:flex; align-items:center; justify-content:center; font-weight:700; }
.step-main { display:flex; flex-direction:column; line-height:1.2; }
.step-main span, .step-main em { font-size:12px; color:#657084; margin-top:3px; font-style:normal; }
.log-viewer { max-height:620px; overflow:auto; background:#0d1623; color:#d9e8ff; border-radius:8px; border:1px solid #1f3857; padding:14px; }
.button-row { display:flex; gap:10px; align-items:center; flex-wrap:wrap; }
.setup-logo-panel { min-height:280px; background:white; border:1px solid #d8dde8; border-radius:8px; padding:24px; display:flex; flex-direction:column; align-items:center; justify-content:center; gap:24px; box-shadow:0 8px 20px rgba(15,23,36,.05); }
.setup-logo-panel img { max-width:100%; max-height:150px; object-fit:contain; }
.sample-matrix-wrap { width:100%; overflow:auto; background:white; border:1px solid #d8dde8; border-radius:8px; padding:10px; box-shadow:0 1px 2px rgba(15,23,36,.04); }
.sample-matrix { width:100%; min-width:860px; border-collapse:separate; border-spacing:0; }
.sample-matrix th { position:sticky; top:0; z-index:1; background:#edf4fb; color:#304a66; font-size:12px; text-transform:uppercase; letter-spacing:.04em; padding:10px 8px; border-bottom:1px solid #c7d6e8; text-align:center; }
.sample-matrix td { padding:9px 8px; border-bottom:1px solid #edf1f6; text-align:center; vertical-align:middle; }
.sample-matrix tr:nth-child(even) td { background:#f8fafc; }
.sample-matrix .sample-name { position:sticky; left:0; z-index:2; background:white; text-align:left; font-weight:800; color:#17202f; min-width:130px; }
.sample-matrix tr:nth-child(even) .sample-name { background:#f8fafc; }
.sample-status { display:inline-flex; width:100%; min-width:112px; justify-content:center; border-radius:999px; padding:5px 9px; font-size:12px; font-weight:800; border:1px solid #cfd7e3; background:#eef2f7; color:#526070; cursor:help; }
.sample-status.completed { background:#def7e8; color:#0b6b3a; border-color:#8fd8ad; }
.sample-status.running, .sample-status.running-no-growth-yet { background:#fff4d6; color:#7c3d00; border-color:#f0c36d; }
.sample-status.waiting { background:#fff4d6; color:#7c3d00; border-color:#f0c36d; }
.sample-status.cancelled { background:#fff0ed; color:#8a2f24; border-color:#e5a397; }
.sample-status.likely-failed, .sample-status.likely-failed-deleted, .sample-status.cancelled-deleted { background:#fff0ed; color:#9f2d20; border-color:#e5a397; }
.sample-status.completed-deleted { background:#eefaf3; color:#315f4c; border-color:#b7dfc7; }
.sample-status.optional-not-run { background:#f6f3fb; color:#5d4d79; border-color:#d9d0ea; }
.project-card { background:white; border:1px solid #d8dde8; border-radius:8px; padding:14px; box-shadow:0 8px 20px rgba(15,23,36,.06); }
.project-card-top { display:flex; justify-content:space-between; align-items:flex-start; gap:10px; margin-bottom:12px; }
.project-title-wrap h3 { margin:2px 0 0 0; font-size:19px; font-weight:800; color:#17202f; overflow-wrap:anywhere; }
.eyebrow { font-size:11px; text-transform:uppercase; letter-spacing:.04em; color:#657084; font-weight:800; }
.analysis-badge { border-radius:999px; padding:5px 9px; font-size:11px; font-weight:800; white-space:nowrap; background:#e8f2ff; color:#15549a; border:1px solid #b9d5f5; }
.analysis-badge.atac { background:#fff4d6; color:#7a4f00; border-color:#f0c36d; }
.analysis-badge.chip { background:#def7e8; color:#176a38; border-color:#8fd8ad; }
.project-meta-row { display:flex; flex-wrap:wrap; align-items:flex-start; gap:7px; margin-bottom:12px; min-width:0; }
.meta-chip { display:inline-block; min-width:0; max-width:100%; background:#eef3f8; border:1px solid #d8dde8; border-radius:999px; padding:5px 8px; font-size:12px; color:#273449; font-weight:700; white-space:normal; overflow-wrap:anywhere; word-break:break-word; line-height:1.25; }
.meta-chip.ref-chip { flex:1 1 100%; width:100%; border-radius:8px; white-space:normal !important; overflow-wrap:anywhere !important; word-break:break-word; }
#setup_table table { width:100%; table-layout:fixed; }
#setup_table th, #setup_table td { white-space:normal !important; overflow-wrap:anywhere !important; word-break:break-word; vertical-align:top; }
#setup_table th:first-child, #setup_table td:first-child { width:34%; }
.path-list { display:flex; flex-direction:column; gap:8px; }
.path-item { border-top:1px solid #edf1f6; padding-top:8px; }
.path-item span { display:block; font-size:11px; color:#657084; text-transform:uppercase; font-weight:800; margin-bottom:3px; }
.path-item code { display:block; white-space:normal; overflow-wrap:anywhere; background:#f8fafc; color:#17202f; border:1px solid #edf1f6; border-radius:6px; padding:7px; font-size:11px; }
.config-card { margin-top:14px; background:white; border:1px solid #d8dde8; border-radius:8px; padding:14px; box-shadow:0 8px 20px rgba(15,23,36,.05); }
.config-card code { display:block; white-space:normal; overflow-wrap:anywhere; margin-top:6px; background:#f8fafc; border:1px solid #edf1f6; border-radius:6px; padding:9px; color:#17202f; }
.native-results-host { margin: 0 !important; width:100% !important; max-width:100% !important; overflow:auto !important; }
.native-results-host > .container-fluid { max-width: none !important; width: 100% !important; margin: 0 !important; padding: 0 0 10px 0 !important; overflow-x:auto !important; }
.native-results-host .app-shell { border-radius: 10px !important; box-shadow: none !important; margin:0 !important; overflow:visible !important; }
.native-results-host .hero { padding: 8px 12px 8px 12px !important; }
.native-results-host .hero-title { font-size:24px !important; margin-bottom:4px !important; }
.native-results-host .hero-subtitle, .native-results-host .hero-kicker { display:none !important; }
.native-results-host .hero-logos { gap:10px !important; }
.native-results-host .hero-logo { height:44px !important; max-height:44px !important; width:auto !important; }
.native-results-host .main-tabs { padding: 6px 6px 10px 6px !important; overflow-x:auto !important; }
.native-results-host .tab-content, .native-results-host .tab-pane, .native-results-host .main-panel { max-width:100% !important; overflow-x:auto !important; padding-left:0 !important; padding-right:0 !important; }
.native-results-host img { max-width:100% !important; height:auto !important; object-fit:contain !important; }
.native-results-host .shiny-plot-output, .native-results-host .plot-output { max-width:100% !important; max-height:620px !important; }
.native-results-host img[src*='pca'], .native-results-host img[src*='volcano'] { max-height:620px !important; width:auto !important; }
.web-context-chip { display:inline-flex; align-items:center; gap:6px; border:1px solid #d8dde8; border-radius:999px; background:#ffffff; color:#304a66; font-size:12px; font-weight:800; padding:6px 10px; box-shadow:0 6px 16px rgba(20,38,64,.05); max-width:100%; }
.web-context-chip span { color:#657084; font-weight:700; }
.dataTables_wrapper { width:100%; max-width:100%; overflow-x:auto; }
.dataTables_scroll { width:100%; max-width:100%; overflow-x:auto; }
.dataTables_scrollBody { max-height:min(62vh, 560px) !important; overflow:auto !important; }
.native-results-host .dataTables_scrollBody { max-height:min(68vh, 650px) !important; overflow:auto !important; }
.native-results-host .table, .native-results-host table { max-width:100%; }
.native-results-host .shiny-html-output { max-width:100%; overflow-x:auto !important; }
.native-results-host .well {
  padding: 8px !important;
  margin-bottom: 8px !important;
}


/* Executive polish layer */
body {
  background: linear-gradient(180deg, #f7f9fc 0%, #edf2f7 100%);
  color: #132033;
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
}

body > .container-fluid > .row {
  display: flex;
  align-items: flex-start;
  gap: 12px;
  margin-left: 0;
  margin-right: 0;
}
body > .container-fluid > .row > .col-sm-2 {
  float: none;
  width: 280px;
  min-width: 280px;
  padding-left: 4px;
  padding-right: 4px;
}
body > .container-fluid > .row > .col-sm-10 {
  float: none;
  width: calc(100% - 292px);
  max-width: calc(100% - 292px);
  padding-left: 4px;
  padding-right: 0;
}
@media (max-width: 900px) {
  body > .container-fluid > .row {
    display: block;
  }
  body > .container-fluid > .row > .col-sm-2,
  body > .container-fluid > .row > .col-sm-10 {
    width: 100% !important;
    max-width: 100% !important;
    min-width: 0 !important;
  }
  .csl-header {
    flex-direction: column;
    align-items: flex-start;
    padding: 24px;
    gap: 18px;
    min-height: 0;
  }
  .brand-lockup {
    flex-direction: column;
    align-items: flex-start;
    gap: 16px;
    width: 100%;
  }
  .csl-header h2 {
    font-size: 34px;
    line-height: 1.05;
  }
  .brand-lockup img,
  .csl-header > img {
    max-width: 100%;
    max-height: 96px;
  }
}
.container-fluid { padding: 22px 28px 34px 28px; }
.csl-header {
  background: linear-gradient(135deg, #07111f 0%, #0b2b4a 58%, #0f6b68 100%);
  border: 1px solid rgba(255,255,255,.16);
  border-radius: 8px;
  box-shadow: 0 18px 42px rgba(7,17,31,.18);
  min-height: 190px;
}
.csl-header h2 { font-size: 44px; letter-spacing: 0; overflow-wrap: anywhere; word-break: break-word; }
.brand-lockup img, .csl-header > img {
  border: 1px solid rgba(255,255,255,.34);
  box-shadow: 0 12px 28px rgba(0,0,0,.16);
}
.brand-lockup { min-width: 0; }
.brand-lockup > div { min-width: 0; }
.csl-header .muted { overflow-wrap: anywhere; }
@media (max-width: 900px) {
  .csl-header h2 {
    font-size: 25px;
    line-height: 1.05;
  }
}
.well {
  background: rgba(255,255,255,.92);
  border: 1px solid #dce4ee;
  border-radius: 8px;
  box-shadow: 0 10px 24px rgba(20,38,64,.06);
}
.tabbable > .nav-tabs {
  display: flex;
  flex-wrap: wrap;
  gap: 8px;
  border-bottom: 0;
  margin-bottom: 16px;
}
.tabbable > .nav-tabs > li > a {
  border: 1px solid #dce4ee !important;
  border-radius: 8px !important;
  background: #fff;
  color: #24364d !important;
  font-weight: 800;
  padding: 11px 16px;
  box-shadow: 0 4px 12px rgba(20,38,64,.05);
}
.tabbable > .nav-tabs > li.active > a,
.tabbable > .nav-tabs > li.active > a:hover,
.tabbable > .nav-tabs > li.active > a:focus {
  background: #0f62c6 !important;
  border-color: #0f62c6 !important;
  color: #fff !important;
}
.tab-content {
  background: rgba(255,255,255,.82);
  border: 1px solid #dce4ee;
  border-radius: 8px;
  padding: 18px;
  box-shadow: 0 12px 30px rgba(20,38,64,.07);
}
.form-control, .selectize-input {
  border-radius: 8px !important;
  border-color: #cfd9e6 !important;
  box-shadow: none !important;
}
.btn, .action-button {
  border-radius: 8px !important;
  font-weight: 800;
}
.btn-primary {
  background: #0f62c6 !important;
  border-color: #0f62c6 !important;
  box-shadow: 0 8px 18px rgba(15,98,198,.22);
}
.status-card, .run-card, .tool-panel, .project-card, .config-card, .resource-card,
.setup-logo-panel, .sample-matrix-wrap, .empty-box {
  border-radius: 8px;
  border-color: #dce4ee;
  box-shadow: 0 10px 24px rgba(20,38,64,.06);
}
.tool-panel summary { padding: 16px 18px; }
.tool-summary strong { color: #132033; }
.pipeline-step {
  background: #fff;
  box-shadow: 0 6px 16px rgba(20,38,64,.05);
}
.pipeline-step.complete { background: #eefaf3; }
.pipeline-step.active { background: #fff8e6; }
.pipeline-step.cancelled { background: #fff0ed; }
.pipeline-step.failed, .pipeline-step.deleted-failed, .pipeline-step.deleted-cancelled { background: #fff0ed; }
.pipeline-step.deleted-complete { background: #eefaf3; }
.sample-matrix th {
  background: #10233a;
  color: #fff;
}
.sample-matrix td { background: #fff; }
.sample-matrix tr:nth-child(even) td { background: #f8fafc; }
.dataTables_wrapper {
  background: #fff;
  border: 1px solid #dce4ee;
  border-radius: 8px;
  padding: 10px;
}
.dataTables_wrapper .dataTables_filter input,
.dataTables_wrapper .dataTables_length select {
  border: 1px solid #cfd9e6;
  border-radius: 8px;
  padding: 5px 8px;
}
.dataTables_paginate .paginate_button {
  border-radius: 8px !important;
  border: 1px solid #dce4ee !important;
  background: #fff !important;
  color: #24364d !important;
}
.dataTables_paginate .paginate_button.current {
  background: #0f62c6 !important;
  color: #fff !important;
  border-color: #0f62c6 !important;
}
table.dataTable { table-layout: fixed; }
table.dataTable tbody td, table.dataTable thead th {
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  min-width: 118px;
  max-width: 118px;
}
table.dataTable tbody td:first-child, table.dataTable thead th:first-child {
  min-width: 150px;
  max-width: 190px;
}
.native-results-host .qc-report-frame {
  min-width: 0 !important;
  width: 112% !important;
  max-width: none !important;
  transform: scale(.9);
  transform-origin: top left;
}

/* Dropdown visibility hardening */
.selectize-control,
.selectize-control.single,
.selectize-control.multi {
  position: relative;
  z-index: 30;
  margin-bottom: 12px;
}
.form-group:has(.selectize-control.dropdown-active),
.selectize-control.dropdown-active {
  position: relative;
  z-index: 100000 !important;
}
.selectize-input,
.selectize-input input,
select.form-control,
.form-control option {
  color: #132033 !important;
  background: #ffffff !important;
}
select.form-control {
  color: #132033 !important;
  background-color: #ffffff !important;
  border: 1px solid #cfd9e6 !important;
  border-radius: 8px !important;
}
.selectize-dropdown {
  z-index: 100000 !important;
  background: #ffffff !important;
  color: #132033 !important;
  border: 1px solid #b9c9dc !important;
  box-shadow: 0 14px 30px rgba(20,38,64,.18) !important;
}
.selectize-dropdown-content {
  background: #ffffff !important;
  color: #132033 !important;
  max-height: 320px !important;
  overflow-y: auto !important;
}
.selectize-dropdown .option,
.selectize-dropdown .optgroup-header,
.selectize-dropdown [data-selectable] {
  color: #132033 !important;
  background: #ffffff !important;
  opacity: 1 !important;
  padding: 8px 12px !important;
  line-height: 1.3 !important;
}
.selectize-dropdown .option.active,
.selectize-dropdown [data-selectable].active {
  color: #07111f !important;
  background: #e8f2ff !important;
}
.selectize-dropdown .option:hover,
.selectize-dropdown [data-selectable]:hover {
  color: #07111f !important;
  background: #edf7f4 !important;
}
.tab-content,
.tab-pane,
.main-panel,
.sidebar-panel,
.well,
.form-group {
  overflow: visible;
}

.progress-header-row {
  display:flex;
  align-items:flex-start;
  justify-content:space-between;
  gap:16px;
  margin-bottom:14px;
}
.progress-header-row h3 { margin-top:0; margin-bottom:4px; }
.progress-header-row .muted { margin:0; }
.project-card {
  background: linear-gradient(180deg, #ffffff 0%, #f8fbff 100%);
}
.project-card-top {
  border-bottom: 1px solid #edf1f6;
  padding-bottom: 12px;
  display: block;
}
.project-title-wrap {
  width: 100%;
  min-width: 0;
}
.project-title-wrap h3 {
  width: 100%;
  overflow-wrap: anywhere;
  word-break: break-word;
}
.project-card-top .analysis-badge {
  display: inline-block;
  margin-top: 8px;
}
.compact-path-list .path-item {
  border-top: 0;
  background: #f7fbff;
  border: 1px solid #dce7f3;
  border-radius: 8px;
  padding: 10px;
}
.compact-path-list .path-item code {
  background: transparent;
  border: 0;
  padding: 0;
  font-size: 12px;
}
.project-manage {
  margin-top: 8px;
  background: #ffffff;
  border: 1px solid #d8dde8;
  border-radius: 8px;
  padding: 10px 12px;
}
.project-manage summary {
  cursor: pointer;
  font-weight: 800;
  color: #17202f;
}
.project-manage .checkbox {
  margin-top: 6px;
  margin-bottom: 6px;
}
.small-note {
  font-size: 12px;
  line-height: 1.35;
  margin: 8px 0;
}
.path-browser-actions {
  display:flex;
  gap:8px;
  flex-wrap:wrap;
  margin:8px 0 10px 0;
}
.path-browser-actions .btn {
  border-radius:6px;
}
.path-browser-current {
  background:#f8fafc;
  border:1px solid #d8dde8;
  border-radius:6px;
  padding:8px;
  font-size:12px;
  overflow-wrap:anywhere;
  margin-bottom:8px;
}
.path-browser-modal .form-group {
  margin-bottom:10px;
}
.path-browser-modal #browser_manual_path {
  font-family:Menlo, Monaco, Consolas, monospace;
  border-radius:6px;
  border-color:#cfd7e3;
  background:#fbfdff;
}
.path-browser-modal #browser_choice {
  min-height:300px;
  border-radius:8px;
  border-color:#c7d6e8;
  background:#fbfdff;
  font-size:13px;
}
.new-project-path-control {
  background:#f8fafc;
  border:1px solid #d8dde8;
  border-radius:8px;
  padding:10px;
  margin-bottom:10px;
}
.new-project-path-control .form-group {
  margin-bottom:8px;
}
.new-project-path-control .btn {
  width:100%;
  text-align:center;
  border-radius:6px;
  white-space:normal;
  min-height:34px;
  line-height:1.2;
}

"

ui <- fluidPage(
  tags$head(
    tags$title("CodeSpringApp"),
    tags$style(HTML(app_css)),
    tags$script(HTML("
      function cslFormatElapsed(total) {
        total = Math.max(0, Math.floor(total || 0));
        var days = Math.floor(total / 86400);
        var rest = total % 86400;
        var hours = Math.floor(rest / 3600);
        var minutes = Math.floor((rest % 3600) / 60);
        var seconds = rest % 60;
        function pad(x) { return String(x).padStart(2, '0'); }
        var hms = pad(hours) + ':' + pad(minutes) + ':' + pad(seconds);
        return days > 0 ? days + '-' + hms : hms;
      }
      function cslTickElapsed() {
        var now = Math.floor(Date.now() / 1000);
        $('.elapsed-live').each(function() {
          var base = parseInt($(this).attr('data-base') || '0', 10);
          var captured = parseInt($(this).attr('data-captured') || now, 10);
          $(this).text(cslFormatElapsed(base + Math.max(0, now - captured)));
        });
      }
      setInterval(cslTickElapsed, 1000);
      $(document).on('shiny:value.dt', cslTickElapsed);
      $(document).on('dblclick', '#browser_choice', function() {
        $('#browser_open_choice').trigger('click');
      });
      $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function(e) {
        var label = $(e.target).text().trim();
        if (label === 'Results Explorer') {
          window.setTimeout(function() {
            if (window.Shiny) {
              Shiny.setInputValue('native_results_tab_visible', Date.now(), {priority: 'event'});
            }
            $(window).trigger('resize');
          }, 100);
        }
      });
    "))
  ),
  div(class = "csl-header",
      div(class = "brand-lockup",
          if (file.exists(LOGO_PATH)) tags$img(src = file.path("codespring_logo", basename(LOGO_PATH))),
          div(h2("CodeSpringApp"), div(class = "muted", "Developed by James Rouse, Rad Utama and Alex Dobin (Bioinformatics Shared Resource)"))
      ),
      if (file.exists(LOGO_CSL_PATH)) tags$img(src = file.path("csl_logo", basename(LOGO_CSL_PATH)), style = "max-height:120px;max-width:300px;background:white;border-radius:8px;padding:10px;object-fit:contain;")
  ),
  sidebarLayout(
    sidebarPanel(
      class = "web-sidebar",
      width = 2,
      selectInput("analysis", "Analysis", choices = c("RNA-seq", "CUT&RUN", "ATAC-seq", "ChIP-seq"), selected = "RNA-seq", selectize = FALSE),
      uiOutput("project_ui"),
      tags$p(class = "muted small-note", "Saved projects are private to the Unix account running this app."),
      uiOutput("new_project_ui"),
      uiOutput("project_manage_ui"),
      tags$hr(),
      uiOutput("project_card")
    ),
    mainPanel(
      class = "web-main",
      width = 10,
      tabsetPanel(
        id = "web_main_tabs",
        tabPanel("Setup", br(), h3("Project Setup"),
                 fluidRow(
                   column(7, tableOutput("setup_table"), uiOutput("source_config_ui")),
                   column(5, div(class = "setup-logo-panel",
                                 if (file.exists(LOGO_PATH)) tags$img(src = file.path("codespring_logo", basename(LOGO_PATH))) else NULL,
                                 if (file.exists(LOGO_CSL_PATH)) tags$img(src = file.path("csl_logo", basename(LOGO_CSL_PATH))) else NULL))
                 )),
        tabPanel("Design Matrix", br(), h3("Design Matrix Builder"),
                 tags$p(class = "muted", "If no design_matrix.txt was provided during setup, build it here: scan the raw FASTQ folder, then edit include/sample/metadata cells directly. Filenames stay on the right so run steps know which reads belong to each sample."),
                 fluidRow(
                   column(7, textInput("metadata_cols", "Metadata columns", value = "treatment", placeholder = "cell_type, condition, replicate")),
                   column(5, br(),
                          div(class = "button-row",
                              actionButton("scan_fastqs", "Scan FASTQ folder", class = "btn-primary"),
                              actionButton("add_metadata_col", "Update metadata columns", class = "btn-default")
                          ))
                 ),
                 uiOutput("design_editor_ui"),
                 br(),
                 actionButton("save_design", "Save design_matrix.txt", class = "btn-primary"),
                 verbatimTextOutput("design_save_status")),
        tabPanel("Run Pipeline", br(), h3("Run Pipeline"),
                 tags$p(class = "muted", "Each tool has its own settings. Jobs are submitted with SLURM sbatch and keep running after this app or browser is closed. If a path or design matrix check fails before sbatch, the app writes a pre-submit error log instead of submitting an empty job."),
                 uiOutput("run_resource_strip"),
                 uiOutput("run_pipeline_stepper"),
                 uiOutput("run_step_cards"),
                 br(),
                 verbatimTextOutput("run_output")),
        tabPanel("Progress", br(),
                 div(class = "progress-header-row",
                     div(h3("Pipeline Progress"), tags$p(class = "muted", "Steps are shown in workflow order. The sample matrix below updates as outputs appear.")),
                     actionButton("refresh_progress", "Refresh now", class = "btn-primary")
                 ),
                 textOutput("progress_updated"),
                 uiOutput("pipeline_stepper"),
                 br(),
                 h4("Sample Progress"),
                 uiOutput("sample_progress_matrix_ui")),
        tabPanel("Results Explorer", uiOutput("native_results_ui")),
        tabPanel("Logs", br(), h3("Logs"), uiOutput("log_file_ui"), tags$pre(class = "log-viewer", textOutput("selected_log_text"))),
        tabPanel("Methods", br(),
                 h3("Methods Documentation"),
                 tags$p(class = "muted", "Project-level methods, reference genome, tool usage, and detected versions where available."),
                 h4("Tools and References"),
                 div(class = "methods-table-wrap", table_output("methods_tools_table")),
                 br(),
                 h4("Project and Reference"),
                 div(class = "methods-table-wrap", table_output("methods_project_table")),
                 br(),
                 div(class = "button-row",
                     downloadButton("download_methods_project", "Download project/reference"),
                     downloadButton("download_methods_tools", "Download tools/reference")))
      )
    )
  )
)

MAIN_UI <- ui
ui <- function(request) {
  query_string <- tryCatch(request$QUERY_STRING %||% "", error = function(e) "")
  if (access_token_valid(query_string)) return(MAIN_UI)
  fluidPage(
    tags$head(tags$title("CodeSpringApp access denied")),
    div(
      style = "max-width:720px;margin:80px auto;font-family:system-ui;padding:28px;border:1px solid #d8dee8;border-radius:10px;",
      h2("Access denied"),
      p("This port belongs to a different private CodeSpringApp launch, or the private URL token is missing."),
      p("Start CodeSpringApp from your own Unix account and open the exact private URL printed by your launcher.")
    )
  )
}

server <- function(input, output, session) {
  projects <- reactiveVal(discover_projects())
  design_state <- reactiveVal(data.frame())
  run_message <- reactiveVal("")
  tool_messages <- reactiveVal(list())
  progress_refresh <- reactiveVal(Sys.time())
  run_cards_refresh <- reactiveVal(Sys.time())
  native_registered_id <- reactiveVal("")
  native_results_refresh <- reactiveVal(0L)
  native_results_loaded_project <- reactiveVal("")
  job_history_state <- reactiveVal(data.frame())
  project_status_state <- reactiveVal(data.frame())
  featurecounts_matrix_autosubmitted <- reactiveVal(character(0))
  sample_size_cache <- reactiveVal(data.frame(path = character(), size = numeric(), checked = character(), stringsAsFactors = FALSE))
  sample_progress_state <- reactiveVal(data.frame())
  progress_refresh_busy <- reactiveVal(FALSE)
  cutrun_normalization_choice <- reactiveVal("spikein")
  path_browser <- reactiveValues(target = "", mode = "dir", path = CURRENT_HOME, message = "")
  project_selection <- reactiveValues(rna = "", cutrun = "", atac = "", chip = "")
  new_fastq_folders <- reactiveVal(character(0))

  new_project_input_values <- function() {
    values <- reactiveValuesToList(input)
    values$new_fastq_dirs <- paste(new_fastq_folders(), collapse = "\n")
    values
  }

  carry_forward_job_elapsed <- function(jobs, previous_jobs) {
    if (!NROW(jobs) || !NROW(previous_jobs) || !"job_id" %in% names(jobs) || !"job_id" %in% names(previous_jobs)) return(jobs)
    if (!"elapsed" %in% names(jobs) || !"elapsed" %in% names(previous_jobs)) return(jobs)
    previous <- previous_jobs[nzchar(previous_jobs$job_id) & nzchar(previous_jobs$elapsed), c("job_id", "elapsed"), drop = FALSE]
    if (!NROW(previous)) return(jobs)
    previous <- previous[!duplicated(previous$job_id, fromLast = TRUE), , drop = FALSE]
    hit <- match(jobs$job_id, previous$job_id)
    elapsed <- as.character(jobs$elapsed)
    fill <- !is.na(hit) & !nzchar(elapsed)
    jobs$elapsed[fill] <- previous$elapsed[hit[fill]]
    jobs
  }

  refresh_progress_now <- function() {
    if (!isTRUE(existing_project_selected())) {
      job_history_state(data.frame())
      sample_progress_state(data.frame())
      project_status_state(data.frame())
      progress_refresh(Sys.time())
      return(invisible(NULL))
    }
    p <- current_project()
    jobs <- carry_forward_job_elapsed(job_history(p, force_refresh = TRUE), isolate(job_history_state()))
    matrix_path <- file.path(p$data_dir, "counts", "count_matrix.txt")
    autosubmitted <- featurecounts_matrix_autosubmitted()
    if (
      identical(p$analysis_key, "rna") &&
      !file.exists(matrix_path) &&
      featurecounts_outputs_ready(p) &&
      !featurecounts_matrix_job_active(jobs, matrix_path)
    ) {
      built <- tryCatch({
        build_featurecounts_matrix_now(p)
        TRUE
      }, error = function(e) {
        FALSE
      })
      if (!built && !p$id %in% autosubmitted) {
        submit_featurecounts_matrix_job(p, "gene_name")
        featurecounts_matrix_autosubmitted(unique(c(autosubmitted, p$id)))
        jobs <- job_history(p)
      }
    }
    active_states <- active_job_state_map_from_jobs(jobs)
    res <- sample_progress(p, active_states, isolate(sample_size_cache()), jobs = jobs)
    status <- project_status(p, jobs = jobs, progress = res$table, active_states = active_states)
    old_status <- isolate(project_status_state())
    job_history_state(jobs)
    sample_size_cache(res$cache)
    sample_progress_state(res$table)
    project_status_state(status)
    if (!identical(status_signature(old_status), status_signature(status))) {
      run_cards_refresh(Sys.time())
    }
    progress_refresh(Sys.time())
  }

  safe_refresh_progress_now <- function(context = "refresh") {
    if (isTRUE(isolate(progress_refresh_busy()))) return(invisible(NULL))
    progress_refresh_busy(TRUE)
    on.exit(progress_refresh_busy(FALSE), add = TRUE)
    tryCatch(
      refresh_progress_now(),
      error = function(e) {
        run_message(paste("Progress", context, "failed:", conditionMessage(e)))
        progress_refresh(Sys.time())
      }
    )
  }

  finish_submit_refresh <- function() {
    session$onFlushed(function() {
      isolate(safe_refresh_progress_now("refresh"))
    }, once = TRUE)
  }

  submission_step <- function(label) {
    step <- switch(label,
      "RSEM" = "RSEM (optional)",
      "Kallisto" = "Kallisto (optional)",
      "MACS2" = "MACS2 (optional)",
      "GSEApy" = "GSEA",
      label
    )
    step
  }

  upstream_blocking_message <- function(msg) {
    msg <- trimws(as.character(msg %||% ""))
    startsWith(msg, "ERROR") &&
      grepl("Wait for .*fully finish|Run .*before|Run .*successfully before|before running|before using trimmed reads", msg)
  }

  set_tool_message <- function(step, msg) {
    msgs <- tool_messages()
    msgs[[canonical_job_step(step)]] <- trimws(msg %||% "")
    tool_messages(msgs)
  }

  tool_message_ui <- function(step) {
    msg <- tool_messages()[[canonical_job_step(step)]] %||% ""
    msg <- trimws(msg)
    if (!nzchar(msg)) return(NULL)
    div(class = "run-message-alert tool-message-alert error", tags$strong("Previous step required"), msg)
  }

  mark_submission_active <- function(label, input_mode = "", samples = NULL) {
    p <- current_project()
    step <- submission_step(label)
    sample_level_steps <- sample_level_steps_for_project(p)
    if (step %in% sample_level_steps) {
      optimistic <- optimistic_step_progress(p, step, input_mode, samples)
      if (NROW(optimistic)) {
        old <- isolate(sample_progress_state())
        if (NROW(old) && all(c("step", "sample") %in% names(old))) {
          replace <- old$step == step & old$sample %in% optimistic$sample
          old <- old[!replace, , drop = FALSE]
        }
        sample_progress_state(rbind(old, optimistic))
      }
    }
    project_status_state(optimistic_status(isolate(project_status_state()), step, input_mode))
    run_cards_refresh(Sys.time())
    progress_refresh(Sys.time())
  }

  run_submission <- function(label, expr, input_mode = "", samples = NULL) {
    step <- submission_step(label)
    run_message(paste("Submitting", label, "..."))
    set_tool_message(step, "")
    progress_refresh(Sys.time())
    msg <- tryCatch(force(expr), error = function(e) paste("ERROR submitting", label, ":", conditionMessage(e)))
    run_message(msg)
    set_tool_message(step, if (upstream_blocking_message(msg)) msg else "")
    if (!startsWith(msg, "ERROR")) {
      tryCatch(
        {
          mark_submission_active(label, input_mode, samples)
          jobs_now <- job_history(current_project(), force_refresh = TRUE)
          job_history_state(carry_forward_job_elapsed(jobs_now, isolate(job_history_state())))
          progress_refresh(Sys.time())
        },
        error = function(e) {
          combined <- paste(msg, "\nProgress display update failed:", conditionMessage(e))
          run_message(combined)
          set_tool_message(step, "")
        }
      )
      finish_submit_refresh()
    } else {
      progress_refresh(Sys.time())
    }
  }

  open_server_browser <- function(target, mode = "dir", current = "") {
    path_browser$target <- target
    path_browser$mode <- mode
    path_browser$path <- normalizePath(browser_start_path(current, mode), winslash = "/", mustWork = FALSE)
    path_browser$message <- ""
    title <- "Choose a server folder"
    showModal(modalDialog(
      title = title,
      div(class = "path-browser-modal",
          div(class = "path-browser-current", textOutput("browser_current_path_text")),
          textInput("browser_manual_path", "Type or paste a server folder", value = path_browser$path),
          div(class = "path-browser-actions",
              actionButton("browser_go_path", "Jump to typed path"),
              actionButton("browser_up", "Up one folder"),
              actionButton("browser_open_choice", "Open selected")
          ),
          uiOutput("browser_choices_ui")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("browser_use_current", "Use this folder", class = "btn-primary")
      ),
      easyClose = TRUE,
      size = "l"
    ))
  }

  output$browser_current_path_text <- renderText({
    paste(
      "Current folder:", normalizePath(path_browser$path, winslash = "/", mustWork = FALSE),
      "\nApp server user:", CURRENT_USER
    )
  })

  output$browser_choices_ui <- renderUI({
    listing <- server_browser_listing(path_browser$path, path_browser$mode)
    choices <- listing$choices
    message_box <- if (nzchar(path_browser$message %||% "")) div(class = "run-message-alert error", path_browser$message) else NULL
    if (identical(listing$status, "unreadable")) {
      return(tagList(message_box, div(
        class = "empty-box",
        tags$strong("This folder is not readable by the app process."),
        tags$br(),
        "The app is running as ", code(CURRENT_USER), ". Check folder permissions or launch CodeSpringApp from the intended Unix account."
      )))
    }
    if (identical(listing$status, "hidden_only")) {
      return(tagList(message_box, div(class = "empty-box", "This folder contains only hidden items. Hidden files and folders are not shown.")))
    }
    if (identical(listing$status, "empty")) return(tagList(message_box, div(class = "empty-box", "This folder is empty.")))
    flat_values <- unlist(choices, use.names = FALSE)
    folder_selector <- if (length(flat_values)) {
      selectInput("browser_choice", "Folders", choices = choices, selected = flat_values[[1]], selectize = FALSE, size = min(max(length(flat_values), 4), 18))
    } else {
      div(class = "empty-box", "This folder has no visible subfolders. You can still use the current folder.")
    }
    visible_files <- listing$files
    file_list <- if (length(visible_files)) {
      tagList(
        tags$h5(paste0("Files in this folder (", length(visible_files), ")")),
        div(class = "path-list compact-path-list", lapply(utils::head(visible_files, 100), function(path) {
          div(class = "path-item", span("File"), code(basename(path)))
        })),
        if (length(visible_files) > 100) tags$p(class = "muted", paste(length(visible_files) - 100, "additional files are not shown.")) else NULL
      )
    } else NULL
    tagList(message_box, folder_selector, file_list)
  })

  observeEvent(input$browse_new_fastq_dir, {
    open_server_browser("new_fastq_dir", "dir", input$new_fastq_dir %||% "")
  })

  observeEvent(input$browse_new_fastq_dirs, {
    existing <- new_fastq_folders()
    start <- if (length(existing)) tail(existing, 1) else ""
    open_server_browser("new_fastq_dir_add", "dir", input$new_fastq_dir_add %||% start)
  })

  observeEvent(input$add_new_fastq_dir, {
    value <- trimws(input$new_fastq_dir_add %||% "")
    if (!nzchar(value)) return()
    value <- normalizePath(path.expand(value), winslash = "/", mustWork = FALSE)
    new_fastq_folders(unique(c(new_fastq_folders(), value)))
    updateTextInput(session, "new_fastq_dir_add", value = "")
  })

  observeEvent(input$remove_new_fastq_dir, {
    remove <- input$new_fastq_dir_remove %||% ""
    if (!nzchar(remove)) return()
    new_fastq_folders(setdiff(new_fastq_folders(), remove))
  })

  output$new_fastq_folder_list_ui <- renderUI({
    folders <- new_fastq_folders()
    if (!length(folders)) {
      return(div(class = "empty-box", "No FASTQ folders added yet."))
    }
    tagList(
      div(class = "path-list compact-path-list",
          lapply(seq_along(folders), function(i) {
            div(class = "path-item", span(paste("Folder", i)), code(folders[[i]]))
          })
      ),
      div(class = "path-browser-actions",
          selectInput(
            "new_fastq_dir_remove", "Remove a folder",
            choices = stats::setNames(folders, paste("Folder", seq_along(folders))),
            selected = folders[[length(folders)]], selectize = FALSE
          ),
          actionButton("remove_new_fastq_dir", "Remove selected", class = "btn-default")
      )
    )
  })

  observeEvent(input$browse_new_results_root, {
    open_server_browser("new_results_root", "dir", input$new_results_root %||% "")
  })

  observeEvent(input$browse_new_design_matrix_path, {
    open_server_browser("new_design_matrix_path", "dir", input$new_design_matrix_path %||% "")
  })

  observeEvent(input$browser_go_path, {
    candidate <- path.expand(trimws(input$browser_manual_path %||% ""))
    if (!nzchar(candidate) || !dir.exists(candidate)) {
      path_browser$message <- paste("Folder does not exist:", candidate)
      return()
    }
    candidate <- normalizePath(candidate, winslash = "/", mustWork = FALSE)
    if (file.access(candidate, mode = 5) != 0) {
      path_browser$message <- paste("The app cannot read this folder:", candidate)
      return()
    }
    path_browser$message <- ""
    path_browser$path <- candidate
    updateTextInput(session, "browser_manual_path", value = path_browser$path)
  })

  observeEvent(input$browser_up, {
    path_browser$message <- ""
    path_browser$path <- normalizePath(dirname(path_browser$path), winslash = "/", mustWork = FALSE)
    updateTextInput(session, "browser_manual_path", value = path_browser$path)
  })

  observeEvent(input$browser_open_choice, {
    choice <- input$browser_choice %||% ""
    if (!nzchar(choice)) return()
    if (dir.exists(choice)) {
      path_browser$message <- ""
      path_browser$path <- normalizePath(choice, winslash = "/", mustWork = FALSE)
      updateTextInput(session, "browser_manual_path", value = path_browser$path)
    }
  })

  observeEvent(input$browser_use_current, {
    value <- normalizePath(path_browser$path, winslash = "/", mustWork = FALSE)
    if (!dir.exists(value) || file.access(value, mode = 5) != 0) {
      path_browser$message <- paste("The app cannot use this folder:", value)
      return()
    }
    updateTextInput(session, path_browser$target, value = value)
    removeModal()
  })

  filtered_projects <- reactive({
    p <- projects()
    analysis <- input$analysis
    if (!length(analysis) || is.null(analysis) || !nzchar(analysis)) return(p)
    p[vapply(p, function(x) identical(x$analysis, analysis), logical(1))]
  })

  output$project_ui <- renderUI({
    p <- filtered_projects()
    choices <- project_select_choices(p, input$analysis %||% "RNA-seq")
    key <- analysis_key(input$analysis %||% "RNA-seq")
    selected <- isolate(input$project_id)
    remembered <- isolate(project_selection[[key]]) %||% ""
    last <- read_last_project_id()
    if (is.null(selected) || !selected %in% unname(choices)) {
      selected <- if (remembered %in% unname(choices)) remembered else if (last %in% unname(choices)) last else "__new__"
    }
    selectInput("project_id", "Project Name", choices = choices, selected = selected, selectize = FALSE)
  })

  output$new_project_ui <- renderUI({
    if (!identical(input$project_id, "__new__")) return(NULL)
    new_analysis_key <- analysis_key(input$new_project_analysis %||% input$analysis %||% "RNA-seq")
    example <- example_dataset_paths(new_analysis_key)
    default_fastq_dir <- example$fastq_dir %||% ""
    default_design_dir <- example$design_dir %||% ""
    tagList(
      tags$hr(),
      h4("New Project"),
      if (!is.null(example)) div(
        class = "read-source-note",
        tags$strong(paste("Bundled", analysis_label(new_analysis_key), "example")),
        tags$p("Use the small example FASTQs and design matrix included with CodeSpringLab. Results are written only to your own results folder."),
        actionButton("use_example_dataset", "Use Example Dataset", class = "btn-default")
      ) else NULL,
      textInput("new_project_name", "Project name", value = "", placeholder = "e.g. my_project"),
      selectInput("new_project_analysis", "Analysis type", choices = c("RNA-seq", "CUT&RUN", "ATAC-seq", "ChIP-seq"), selected = input$analysis, selectize = FALSE),
      selectInput("new_species", "Species", choices = c("Mouse" = "mouse", "Human" = "human"), selected = "mouse", selectize = FALSE),
      uiOutput("new_genome_version_ui"),
      radioButtons("new_paired_end", "Reads", choices = c("Paired-end" = "paired", "Single-end" = "single"), selected = "paired"),
      radioButtons(
        "new_fastq_location_mode", "Where are the raw FASTQs?",
        choices = c("One folder" = "one", "Multiple folders (treat as one input pool)" = "multiple"),
        selected = "one"
      ),
      conditionalPanel("input.new_fastq_location_mode == 'one'", div(class = "new-project-path-control",
          textInput("new_fastq_dir", "Raw FASTQ folder", value = default_fastq_dir, placeholder = "Choose with Browse or paste a server path"),
          actionButton("browse_new_fastq_dir", "Browse server", class = "btn-default"),
          tags$p(class = "muted", "This folder must contain the FASTQ files named in design_matrix.txt. If this path is wrong, jobs are not submitted and a pre-submit error is written in the Logs tab.")
      )),
      conditionalPanel("input.new_fastq_location_mode == 'multiple'", div(class = "new-project-path-control",
          textInput("new_fastq_dir_add", "Add one raw FASTQ folder", value = "", placeholder = "Paste one server path, then click Add folder"),
          div(class = "path-browser-actions",
              actionButton("browse_new_fastq_dirs", "Browse server", class = "btn-default"),
              actionButton("add_new_fastq_dir", "Add folder", class = "btn-primary")
          ),
          uiOutput("new_fastq_folder_list_ui"),
          tags$p(class = "muted", "Add each sequencing-run folder separately. The saved folders are scanned as one input pool. Source FASTQs are read-only inputs; Cutadapt, Bowtie2, and all later steps write only beneath the project results folder.")
      )),
      div(class = "new-project-path-control",
          textInput("new_results_root", "Results root", value = DEFAULT_RESULTS_ROOT, placeholder = "Where CodeSpringApp should write project results"),
          actionButton("browse_new_results_root", "Browse server", class = "btn-default")
      ),
      div(class = "new-project-path-control",
          textInput("new_design_matrix_path", "Design matrix folder", value = default_design_dir, placeholder = "Optional; folder containing or receiving design_matrix.txt"),
          actionButton("browse_new_design_matrix_path", "Browse server", class = "btn-default"),
          tags$p(class = "muted", "Leave this blank to create the design matrix in the Design Matrix tab after the project is created.")
      ),
      checkboxInput("new_clear_existing_results", "Clear existing results if this project folder already exists", value = FALSE),
      actionButton("create_project_config", "Create project", class = "btn-primary"),
      textOutput("create_project_status")
    )
  })

  output$new_genome_version_ui <- renderUI({
    species <- tolower(input$new_species %||% "mouse")
    choices <- genome_reference_choices(species)
    selected <- isolate(input$new_genome_version)
    if (is.null(selected) || !selected %in% unname(choices)) selected <- unname(choices)[[1]]
    selectInput("new_genome_version", "Genome/reference version", choices = choices, selected = selected, selectize = FALSE)
  })

  observeEvent(input$use_example_dataset, {
    key <- analysis_key(input$new_project_analysis %||% input$analysis %||% "RNA-seq")
    example <- example_dataset_paths(key)
    if (is.null(example)) return()
    current_name <- trimws(input$new_project_name %||% "")
    if (!nzchar(current_name) || grepl("^example_(rnaseq|atac)$", current_name)) {
      updateTextInput(session, "new_project_name", value = example$name)
    }
    new_fastq_folders(character(0))
    updateRadioButtons(session, "new_fastq_location_mode", selected = "one")
    updateTextInput(session, "new_fastq_dir", value = example$fastq_dir)
    updateTextInput(session, "new_design_matrix_path", value = example$design_dir)
    updateSelectInput(session, "new_species", selected = "mouse")
    updateRadioButtons(session, "new_paired_end", selected = "paired")
    output$create_project_status <- renderText(paste("Loaded the bundled", analysis_label(key), "example paths. Choose a project name and click Create project."))
  })

  output$project_manage_ui <- renderUI({
    p <- filtered_projects()
    if (!length(p)) return(NULL)
    choices <- stats::setNames(vapply(p, `[[`, character(1), "id"), vapply(p, `[[`, character(1), "label"))
    tagList(
      tags$hr(),
      tags$details(class = "project-manage",
        tags$summary("Manage projects"),
        div(class = "muted small-note", "Delete saved project files from project_configs and cancel tracked active jobs. Full project folder deletion is optional and asks for confirmation."),
        checkboxGroupInput("delete_project_ids", "Projects to delete", choices = choices),
        checkboxInput("delete_project_data", "Also delete entire csl_results project folder (data, log, shiny)", value = FALSE),
        actionButton("delete_selected_projects", "Delete selected", class = "btn-danger"),
        textOutput("delete_project_status")
      )
    )
  })

  current_project <- reactive({
    selected <- input$project_id
    if (is.null(selected) || !length(selected) || !nzchar(selected) || identical(selected, "__new__")) {
      return(new_project_from_inputs(new_project_input_values()))
    }
    p <- filtered_projects()
    req(length(p) > 0)
    idx <- match(selected, names(p))
    if (!length(idx) || is.na(idx)) return(new_project_from_inputs(new_project_input_values()))
    p[[idx]]
  })

  existing_project_selected <- reactive({
    selected <- input$project_id
    !is.null(selected) && length(selected) && nzchar(selected) && !identical(selected, "__new__")
  })

  observeEvent(input$project_id, {
    selected <- input$project_id %||% "__new__"
    if (!identical(selected, "__new__")) {
      project_selection[[analysis_key(input$analysis %||% "RNA-seq")]] <- selected
      write_last_project_id(selected)
    }
    native_registered_id("")
    native_results_loaded_project("")
    p <- current_project()
    updateTextInput(session, "metadata_cols", value = paste(project_metadata_cols(p), collapse = ", "))
    if (isTRUE(existing_project_selected()) && cutadapt_outputs_available(p)) {
      updateCheckboxInput(session, "fastqc_use_trimmed", value = TRUE)
      updateCheckboxInput(session, "star_use_trimmed", value = TRUE)
      updateCheckboxInput(session, "kallisto_use_trimmed", value = TRUE)
      updateCheckboxInput(session, "cutrun_bowtie2_use_trimmed", value = TRUE)
    }
    if (isTRUE(existing_project_selected())) safe_refresh_progress_now("project switch")
  }, ignoreInit = FALSE)

  observeEvent(input$cutrun_normalization_mode, {
    choice <- selected_choice(input$cutrun_normalization_mode, c("CPM", "spikein", "none"), isolate(cutrun_normalization_choice()))
    cutrun_normalization_choice(choice)
    seacr_norm <- if (identical(tolower(choice), "spikein")) "non" else "norm"
    updateSelectInput(session, "cutrun_seacr_norm", selected = seacr_norm)
  }, ignoreInit = FALSE)

  output$project_card <- renderUI({
    p <- current_project()
    if (!isTRUE(existing_project_selected())) {
      return(div(class = "project-card",
        div(class = "project-card-top",
          div(class = "project-title-wrap",
            div(class = "eyebrow", "Selected project"),
            h3("Start a new project")
          ),
          span(class = paste("analysis-badge", p$analysis_key), p$analysis)
        ),
        div(class = "path-list compact-path-list",
          div(class = "path-item", span("Data"), code("Create or select a project to begin."))
        )
      ))
    }
    badge_class <- paste("analysis-badge", p$analysis_key)
    div(class = "project-card",
        div(class = "project-card-top",
            div(class = "project-title-wrap",
                div(class = "eyebrow", "Selected project"),
                h3(p$label)
            ),
            span(class = badge_class, p$analysis)
        ),
        div(class = "project-meta-row",
            span(class = "meta-chip", paste("Species", genome_species(p))),
            span(class = "meta-chip ref-chip", project_reference_label(p)),
            span(class = "meta-chip", if (isTRUE(p$paired_end)) "Paired-end" else "Single-end")
        ),
        div(class = "path-list compact-path-list",
            div(class = "path-item", span("Data"), code(p$data_dir))
        )
    )
  })

  output$setup_table <- renderTable({
    p <- current_project()
    if (!isTRUE(existing_project_selected())) {
      return(data.frame(
        field = c("Project", "Analysis", "Species", "Genome/reference", "Paired-end"),
        value = c("New project", p$analysis, genome_species(p), project_reference_label(p), as.character(p$paired_end)),
        stringsAsFactors = FALSE
      ))
    }
    data.frame(
      field = c("Project", "Analysis", "Species", "Genome/reference", "Reference key", "Paired-end", "Results root", "Data folder", "FASTQ folder(s)", "Design matrix"),
      value = c(p$label, p$analysis, genome_species(p), project_reference_label(p), genome_reference_key(p), as.character(p$paired_end), p$results_root, p$data_dir, paste(project_fastq_dirs(p), collapse = "\n"), p$design_matrix_path),
      stringsAsFactors = FALSE
    )
  })

  output$source_config_ui <- renderUI({
    if (!isTRUE(existing_project_selected())) return(NULL)
    p <- current_project()
    if (!nzchar(p$source_config)) return(NULL)
    div(class = "config-card",
        div(class = "eyebrow", "Imported CodeSpringLab project file"),
        code(p$source_config)
    )
  })

  output$create_project_status <- renderText("")
  output$delete_project_status <- renderText("")

  selected_projects_for_delete <- reactive({
    ids <- input$delete_project_ids %||% character(0)
    p <- projects()
    p[intersect(ids, names(p))]
  })

  observeEvent(input$delete_selected_projects, {
    to_delete <- selected_projects_for_delete()
    if (!length(to_delete)) {
      output$delete_project_status <- renderText("Select at least one project.")
      return()
    }
    if (isTRUE(input$delete_project_data)) {
      labels <- vapply(to_delete, `[[`, character(1), "label")
      result_dirs <- vapply(to_delete, project_result_dir, character(1))
      showModal(modalDialog(
        title = "Delete entire project folder?",
        tags$p("This will cancel tracked active jobs for the selected project(s), delete the project config file(s), remove old job records, and delete the entire csl_results project folder for each selected project, including data, log, and shiny folders:"),
        tags$ul(lapply(seq_along(labels), function(i) tags$li(tags$strong(labels[[i]]), tags$br(), code(result_dirs[[i]])))),
        tags$p(tags$strong("This cannot be undone.")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_delete_projects", "Yes, delete configs and project folders", class = "btn-danger")
        ),
        easyClose = TRUE
      ))
      return()
    }
    msg <- delete_projects(to_delete, delete_data = FALSE)
    projects(discover_projects())
    output$delete_project_status <- renderText(msg)
  })

  observeEvent(input$confirm_delete_projects, {
    to_delete <- selected_projects_for_delete()
    removeModal()
    msg <- delete_projects(to_delete, delete_data = TRUE)
    projects(discover_projects())
    output$delete_project_status <- renderText(msg)
  })

  observeEvent(input$create_project_config, {
    if (!nzchar(trimws(input$new_project_name %||% ""))) {
      output$create_project_status <- renderText("ERROR: Enter a project name before creating the project.")
      return()
    }
    p <- new_project_from_inputs(new_project_input_values())
    msg <- tryCatch({
      fastq_dirs <- project_fastq_dirs(p)
      if (!length(fastq_dirs)) stop("Choose at least one raw FASTQ folder before creating the project.")
      missing_fastq_dirs <- fastq_dirs[!dir.exists(fastq_dirs)]
      if (length(missing_fastq_dirs)) stop("These raw FASTQ folders do not exist: ", paste(missing_fastq_dirs, collapse = ", "))
      result_dir <- project_result_dir(p)
      if (dir_has_contents(result_dir)) {
        if (!isTRUE(input$new_clear_existing_results)) {
          stop(
            "A project results folder already exists at ", result_dir,
            ". Check 'Clear existing results if this project folder already exists' to start fresh, ",
            "or choose a different project name."
          )
        }
        cleanup <- cancel_active_project_jobs(p)
        deleted <- delete_project_results(p)
        if (!isTRUE(deleted$ok)) stop(deleted$message)
      } else {
        cleanup <- cancel_active_project_jobs(p)
        prune_project_job_history(p)
      }
      example_design_copied <- ""
      if (is_bundled_example_design(p$design_matrix_path)) {
        source_design <- p$design_matrix_path
        destination_design <- file.path(p$data_dir, "manifest", "design_matrix.txt")
        dir.create(dirname(destination_design), recursive = TRUE, showWarnings = FALSE)
        if (!isTRUE(file.copy(source_design, destination_design, overwrite = TRUE))) {
          stop("Could not copy the bundled example design matrix into the project results folder.")
        }
        p$design_matrix_path <- normalizePath(destination_design, winslash = "/", mustWork = FALSE)
        example_design_copied <- paste("Copied example design matrix:", p$design_matrix_path)
      }
      cfg <- write_project_config(p)
      refreshed <- discover_projects()
      projects(refreshed)
      new_fastq_folders(character(0))
      write_last_project_id(p$id)
      updateSelectInput(session, "project_id", choices = project_select_choices(refreshed, p$analysis), selected = p$id)
      cleanup <- cleanup %||% ""
      paste(c(
        if (nzchar(cleanup)) cleanup,
        paste("Created project:", p$name),
        if (nzchar(example_design_copied)) example_design_copied,
        paste("Saved project file:", cfg)
      ), collapse = "\n")
    }, error = function(e) paste("ERROR:", conditionMessage(e)))
    output$create_project_status <- renderText(msg)
  })

  metadata_cols_from_input <- reactive({
    parse_metadata_cols(input$metadata_cols, current_project())
  })

  observeEvent(input$scan_fastqs, {
    p <- current_project()
    is_cutrun <- is_cutrun_project(p); is_atac <- is_atac_project(p)
    scanned <- scan_fastq_dirs(project_fastq_dirs(p), p$paired_end, metadata_cols_from_input(), infer_samples = is_cutrun || is_atac)
    if (is_cutrun) scanned <- infer_cutrun_metadata(scanned)
    if (is_atac) scanned <- infer_atac_metadata(scanned)
    design_state(scanned)
  })

  observeEvent(input$add_metadata_col, {
    df <- design_state()
    design_state(sync_metadata_columns(df, metadata_cols_from_input()))
  })

  observeEvent(current_project(), {
    p <- current_project()
    df <- design_editor_from_project(p, default_metadata_cols(p))
    if (is_cutrun_project(p)) df <- infer_cutrun_metadata(df)
    design_state(df)
  }, ignoreInit = FALSE)

  observeEvent(list(input$new_design_matrix_path, input$new_project_analysis), {
    if (!identical(input$project_id, "__new__")) return()
    p <- current_project()
    metadata <- default_metadata_cols(p)
    updateTextInput(session, "metadata_cols", value = paste(metadata, collapse = ", "))
    df <- design_editor_from_project(p, metadata)
    if (is_cutrun_project(p)) df <- infer_cutrun_metadata(df)
    design_state(df)
  }, ignoreInit = TRUE)

  output$design_editor_ui <- renderUI({
    df <- design_state()
    if (!NROW(df)) {
      df <- data.frame(include = logical(), sample = character(), filename = character(), status = character())
      for (col in default_metadata_cols(current_project())) df[[col]] <- character()
      df <- df[, design_matrix_columns(df), drop = FALSE]
    }
    design_matrix_ui(df, current_project())
  })

  output$design_editor_table <- if (DT_AVAILABLE) {
    DT::renderDataTable({
      df <- design_state()
      if (!NROW(df)) return(data.frame())
      df <- df[, design_matrix_columns(df), drop = FALSE]
      df$include <- ifelse(vapply(df$include, as_design_bool, logical(1)), "TRUE", "FALSE")
      DT::datatable(
        df,
        rownames = FALSE,
        editable = list(target = "cell"),
        class = "compact stripe hover",
        options = list(
          pageLength = 25,
          lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "All")),
          scrollX = TRUE,
          scrollY = "520px",
          paging = TRUE,
          pagingType = "full_numbers",
          autoWidth = FALSE,
          dom = "lfrtip",
          columnDefs = list(
            list(width = "90px", targets = 0),
            list(width = "170px", targets = 1),
            list(width = "420px", targets = which(names(df) == "filename") - 1)
          )
        )
      )
    }, server = FALSE)
  } else {
    renderTable({
      df <- design_state()
      if (!NROW(df)) return(data.frame())
      df[, design_matrix_columns(df), drop = FALSE]
    }, striped = TRUE, bordered = TRUE, spacing = "s")
  }

  observeEvent(input$design_editor_table_cell_edit, {
    info <- input$design_editor_table_cell_edit
    df <- design_state()
    if (!NROW(df) || is.null(info$row) || is.null(info$col)) return()
    cols <- design_matrix_columns(df)
    df <- df[, cols, drop = FALSE]
    row <- as.integer(info$row)
    col <- as.integer(info$col) + 1L
    if (is.na(row) || is.na(col) || row < 1 || row > NROW(df) || col < 1 || col > NCOL(df)) return()
    col_name <- names(df)[[col]]
    value <- as.character(info$value %||% "")
    if (identical(col_name, "include")) {
      df[[col_name]][row] <- if (as_design_bool(value)) TRUE else FALSE
    } else {
      df[[col_name]][row] <- value
    }
    design_state(df)
  }, ignoreInit = TRUE)

  output$design_save_status <- renderText("")
  observeEvent(input$save_design, {
    p <- current_project()
    if (identical(input$project_id, "__new__") && !nzchar(trimws(input$new_project_name %||% ""))) {
      output$design_save_status <- renderText("ERROR: Enter a project name before saving a new project design matrix.")
      return()
    }
    df <- design_state()
    design_state(df)
    metadata <- metadata_cols_from_input()
    msg <- tryCatch({
      original_design_path <- p$design_matrix_path %||% ""
      design_path <- write_design_matrix(p, df, metadata)
      p$design_matrix_path <- design_path
      cfg <- write_project_config(p)
      refreshed <- discover_projects()
      projects(refreshed)
      write_last_project_id(p$id)
      updateSelectInput(session, "project_id", choices = project_select_choices(refreshed, p$analysis), selected = p$id)
      note <- if (nzchar(original_design_path) && normalizePath(original_design_path, winslash = "/", mustWork = FALSE) != normalizePath(design_path, winslash = "/", mustWork = FALSE)) {
        paste0("\nOriginal design matrix was not modified: ", original_design_path)
      } else {
        ""
      }
      if (identical(input$project_id, "__new__")) {
        paste("Saved edited design matrix copy:", design_path, "\nCreated project:", p$name, "\nSaved project file:", cfg, note)
      } else {
        paste("Saved edited design matrix copy:", design_path, "\nUpdated project file:", cfg, note)
      }
    }, error = function(e) paste("ERROR:", conditionMessage(e)))
    output$design_save_status <- renderText(msg)
  })

  observe({
    invalidateLater(PROGRESS_REFRESH_MS, session)
    if ((input$web_main_tabs %||% "") %in% c("Progress", "Run Pipeline")) {
      jobs <- isolate(job_history_state())
      if (length(active_job_state_map_from_jobs(jobs))) safe_refresh_progress_now("auto refresh")
    }
  })

  session$onFlushed(function() {
    isolate({
      if (isTRUE(existing_project_selected())) safe_refresh_progress_now("session restore")
    })
  }, once = TRUE)

  observeEvent(input$web_main_tabs, {
    if ((input$web_main_tabs %||% "") %in% c("Progress", "Run Pipeline")) safe_refresh_progress_now("tab refresh")
  }, ignoreInit = TRUE)

  output$progress_updated <- renderText({
    paste("Auto-refreshes active jobs every", PROGRESS_REFRESH_MS / 1000, "seconds. Last checked:", format(progress_refresh(), "%Y-%m-%d %H:%M:%S"))
  })

  progress_status <- reactive({
    progress_refresh()
    if (!isTRUE(existing_project_selected())) return(data.frame())
    df <- project_status_state()
    if (!NROW(df)) df <- project_status(current_project())
    df[order(step_order(df$step)), , drop = FALSE]
  })

  observeEvent(input$refresh_progress, {
    safe_refresh_progress_now("manual refresh")
  })

  output$pipeline_stepper <- renderUI({
    if (!isTRUE(existing_project_selected())) return(div(class = "empty-box", "Select or create a project to see pipeline progress."))
    pipeline_stepper_ui(current_project(), progress_status())
  })

  output$sample_progress_matrix_ui <- renderUI({
    if (!isTRUE(existing_project_selected())) return(div(class = "empty-box", "Select or create a project to see sample progress."))
    sample_progress_matrix_ui(sample_progress_state())
  })

  output$sample_progress_detail_table <- render_csl_table({
    sample_progress_detail_table(sample_progress_state())
  }, page_length = 20, scroll_y = "520px")

  output$run_pipeline_stepper <- renderUI({
    if (!isTRUE(existing_project_selected())) return(div(class = "empty-box", "Create or select a project before running pipeline steps."))
    progress_refresh()
    pipeline_stepper_ui(current_project(), progress_status())
  })

  output$run_resource_strip <- renderUI({
    p <- current_project()
    if (!isTRUE(existing_project_selected())) {
      return(div(class = "resource-strip",
        div(class = "resource-card",
          tags$strong("Genome resources"),
          tags$p(class = "muted status-path", project_reference_label(p))
        )
      ))
    }
    ref <- if (is_cutrun_project(p)) cutrun_reference_resources(p) else if (is_atac_project(p)) atac_reference_resources(p) else genome_resources(p)
    div(class = "resource-strip",
        div(class = "resource-card",
            tags$strong("Genome resources"),
            tags$p(class = "muted status-path", project_reference_label(p)),
            tags$p(class = "status-path", if (is_cutrun_project(p) || is_atac_project(p)) ref$bowtie2_index else ref$gtf),
            if (is_cutrun_project(p) || is_atac_project(p)) tags$p(class = "status-path", ref$chrom_sizes) else NULL
        ),
        div(class = "resource-card flowchart-card",
            if (file.exists(FLOWCHART_PATH)) tags$img(src = file.path("codespring_flowchart", basename(FLOWCHART_PATH))) else tags$p("Pipeline flowchart")
        )
    )
  })

  output$run_step_cards <- renderUI({
    run_cards_refresh()
    if (!isTRUE(existing_project_selected())) return(div(class = "empty-box", "Create or select a project to enable pipeline tools."))
    p <- current_project()
    status <- isolate(project_status_state())
    if (!NROW(status)) status <- project_status(p)
    status <- status[order(step_order(status$step)), , drop = FALSE]
    r1_choices <- adapter_choices_r1()
    r2_choices <- adapter_choices_r2()
    if (is_cutrun_project(p)) {
      normalization_choice <- isolate(cutrun_normalization_choice())
      return(div(class = "run-grid",
        tool_panel("Cutadapt", status, "Trim adapters and short reads from raw CUT&RUN FASTQs.",
          tagList(
            selectInput("cutadapt_adapter1", "R1/read1 adapter", choices = r1_choices, selected = selected_choice(input$cutadapt_adapter1, r1_choices, r1_choices[[1]]), width = "100%", selectize = FALSE),
            conditionalPanel("input.cutadapt_adapter1 == '__custom__'", textInput("cutadapt_adapter1_custom", "Custom R1/read1 adapter sequence", value = input$cutadapt_adapter1_custom %||% "", width = "100%")),
            selectInput("cutadapt_adapter2", "R2/read2 adapter", choices = r2_choices, selected = selected_choice(input$cutadapt_adapter2, r2_choices, r2_choices[[1]]), width = "100%", selectize = FALSE),
            conditionalPanel("input.cutadapt_adapter2 == '__custom__'", textInput("cutadapt_adapter2_custom", "Custom R2/read2 adapter sequence", value = input$cutadapt_adapter2_custom %||% "", width = "100%")),
            textInput("cutadapt_min_length", "Minimum read length", value = input$cutadapt_min_length %||% "20")
          ),
          "run_cutadapt", "Submit cutadapt"),
        tool_panel("FastQC", status, "Quality reports for raw or trimmed CUT&RUN reads.",
          tagList(
            checkboxInput("fastqc_use_trimmed", "Analyze trimmed reads from Cutadapt", value = trimmed_checkbox_default(p, isolate(input$fastqc_use_trimmed))),
            tags$p(class = "muted small-note", "Turn this off to inspect the original raw FASTQs instead.")
          ),
          "run_fastqc", "Submit FastQC"),
        tool_panel("Bowtie2", status, "Align CUT&RUN fragments with Bowtie2 and generate fragment bedGraph/bigWig tracks.",
          tagList(
            checkboxInput("cutrun_bowtie2_use_trimmed", "Align trimmed reads from Cutadapt", value = trimmed_checkbox_default(p, isolate(input$cutrun_bowtie2_use_trimmed))),
            tags$p(class = "read-source-note", "Checked = use FASTQs produced by Cutadapt. Unchecked = align the original raw FASTQs from the project folder."),
            textInput("cutrun_mapq", "Minimum alignment MAPQ", value = input$cutrun_mapq %||% "30"),
            textInput("cutrun_max_fragment", "Maximum fragment length", value = input$cutrun_max_fragment %||% "1000"),
            selectInput("cutrun_normalization_mode", "Signal normalization", choices = c("CPM" = "CPM", "E. coli spike-in" = "spikein", "None" = "none"), selected = normalization_choice, selectize = FALSE),
            selectInput("cutrun_spikein_genome", "Spike-in genome", choices = CUTRUN_SPIKEIN_GENOME_CHOICES, selected = CUTRUN_DEFAULT_SPIKEIN_INDEX, selectize = FALSE),
            textInput("cutrun_spikein_min_reads", "Minimum spike-in reads warning", value = input$cutrun_spikein_min_reads %||% "1000"),
            checkboxInput("cutrun_dedup_targets", "Deduplicate target reads", value = isTRUE(input$cutrun_dedup_targets)),
            checkboxInput("cutrun_dedup_controls", "Deduplicate IgG/input controls", value = if (is.null(input$cutrun_dedup_controls)) TRUE else isTRUE(input$cutrun_dedup_controls)),
            checkboxInput("cutrun_remove_mito", "Remove mitochondrial fragments from peak-calling bedGraph", value = if (is.null(input$cutrun_remove_mito)) TRUE else isTRUE(input$cutrun_remove_mito)),
            tags$p(class = "muted small-note", "Choose spikein only when E. coli DNA was intentionally added. The alignment summary will report spike-in reads and the applied scale factor.")
          ),
          "run_cutrun_bowtie2", "Submit Bowtie2"),
        tool_panel("Post-alignment Repair", status, "Resume incomplete CUT&RUN processing from existing aligned BAMs without repeating Bowtie2.",
          tagList(
            uiOutput("cutrun_postprocess_controls_ui"),
            tags$p(class = "muted small-note", "Only samples with valid aligned and duplicate-removed BAMs but missing downstream fragments or summaries are offered. Temporary sorting uses job-specific project storage instead of compute-node /tmp.")
          ),
          "run_cutrun_postprocess", "Repair selected samples", status_step = "Bowtie2", show_sample_progress = FALSE, show_job_actions = FALSE),
        {
          seacr_norm_default <- if (identical(tolower(normalization_choice), "spikein")) "non" else "norm"
          tool_panel("SEACR", status, "Recommended sparse CUT&RUN peak calling from fragment bedGraphs.",
          tagList(
            selectInput("cutrun_seacr_norm", "SEACR normalization", choices = c("norm", "non"), selected = selected_choice(input$cutrun_seacr_norm, c("norm", "non"), seacr_norm_default), selectize = FALSE),
            selectInput("cutrun_seacr_stringency", "Default SEACR stringency", choices = c("Stringent (recommended default)" = "stringent", "Relaxed" = "relaxed"), selected = selected_choice(input$cutrun_seacr_stringency, c("stringent", "relaxed"), "stringent"), selectize = FALSE),
            tags$p(class = "muted small-note", "SEACR normalization follows the Bowtie signal automatically: non for E. coli spike-in, norm for CPM or raw signal. A sample-level stringent/relaxed value in design_matrix.txt overrides this default; auto uses this setting. TF versus histone class does not automatically change SEACR stringency."),
            tags$p(class = "muted small-note", "Each normalization/stringency combination is preserved separately under data/seacr (for example, non_stringent or norm_relaxed). Selecting norm uses raw bedGraphs so SEACR performs the target/control normalization; selecting non uses the already normalized Bowtie2 tracks."),
            tags$p(class = "muted small-note", "Use stringent for the primary analysis. Relaxed is an optional sensitivity analysis and is not automatically required for histone marks."),
            tags$p(class = "muted small-note", "IgG/input rows do not receive their own SEACR peak job. Their Bowtie2 bedGraph is used as the control for matched target samples, so SEACR progress intentionally lists targets only.")
          ),
          "run_cutrun_seacr", "Submit SEACR")
        },
        tool_panel("Deduplicated-target sensitivity", status, "Diagnostic rerun that rebuilds target signal from duplicate-removed BAMs and calls SEACR against the existing matched deduplicated control.",
          tagList(
            uiOutput("cutrun_dedup_sensitivity_samples_ui"),
            tags$p(class = "muted small-note", "This does not alter Bowtie2 or primary SEACR results. It writes each normalization/stringency combination separately under data/cutrun_dedup_sensitivity and is intended to test whether target duplicate retention is driving discordant peak counts."),
            tags$p(class = "muted small-note", "Uses the SEACR normalization and stringency selected above. Do not use these sensitivity peaks for Peak QC or differential binding until you compare them with the primary run.")
          ),
          "run_cutrun_dedup_sensitivity", "Run deduplicated-target sensitivity", status_step = "SEACR", show_sample_progress = FALSE),
        tool_panel("Peak QC", status, "Build SEACR consensus peaks, a consensus peak count matrix, and FRiP summaries.",
          tags$p(class = "muted small-note", "Run after SEACR. Peak QC uses the normalization/stringency combination selected above and stores its outputs in a matching subfolder."),
          "run_cutrun_peakqc", "Submit Peak QC"),
        tool_panel("Differential Peaks", status, "Build mark-specific reproducible consensus peaks and test differential binding with DiffBind/DESeq2.",
          tagList(
            uiOutput("cutrun_diffbind_reference_ui"),
            uiOutput("cutrun_diffbind_jobs_ui"),
            tags$p(class = "muted small-note", "Differential Peaks uses only the SEACR normalization/stringency combination selected above and stores that combination separately."),
            tags$p(class = "muted small-note", "Default behavior matches the ATAC analysis: at least two biological replicates are required per condition, while the consensus includes peaks found in one or more replicates."),
            tags$details(
              tags$summary("Advanced consensus setting"),
              numericInput("cutrun_diffbind_min_replicates", "Replicates that must support a peak", value = 1, min = 1, step = 1),
              tags$p(class = "muted small-note", "Use 2 only when you want every retained condition-consensus peak to occur in at least two replicates. This can be too restrictive for shallow subset tests.")
            ),
            tags$p(class = "muted small-note", "Each selected cell type/mark comparison is submitted as its own SLURM job and writes to its own results folder. Native SEACR widths are preserved; E. coli BAMs are reused automatically when spike-in normalization was selected for Bowtie2.")
          ),
          "run_cutrun_diffbind", "Submit selected comparison jobs"),
        tool_panel("MACS2 (optional)", status, "Optional MACS2 peak calling for comparison or broad histone-mark peaks.",
          tagList(
            textInput("cutrun_macs2_qvalue", "MACS2 q-value cutoff", value = input$cutrun_macs2_qvalue %||% "0.01"),
            selectInput("cutrun_macs2_peak_type", "Peak type", choices = c("Automatic from target_class" = "auto", "Narrow for every target" = "narrow", "Broad for every target" = "broad"), selected = selected_choice(input$cutrun_macs2_peak_type, c("auto", "narrow", "broad"), "auto"), selectize = FALSE),
            tags$p(class = "muted small-note", "Automatic uses broad for histone_broad and narrow for histone_narrow or tf_or_other. This setting affects only optional MACS2; SEACR retains native enriched-region widths.")
          ),
          "run_cutrun_macs2", "Submit MACS2")
      ))
    }
    if (is_atac_project(p)) {
      nextera1 <- unname(r1_choices[["Nextera Transposase ATAC"]]); nextera2 <- unname(r2_choices[["Nextera Transposase ATAC"]])
      return(div(class = "run-grid",
        tool_panel("Cutadapt", status, "Trim ATAC-seq adapters and short reads.", tagList(
          selectInput("cutadapt_adapter1", "R1 adapter", choices = r1_choices, selected = selected_choice(input$cutadapt_adapter1, r1_choices, nextera1), selectize = FALSE),
          selectInput("cutadapt_adapter2", "R2 adapter", choices = r2_choices, selected = selected_choice(input$cutadapt_adapter2, r2_choices, nextera2), selectize = FALSE),
          textInput("cutadapt_min_length", "Minimum read length", value = input$cutadapt_min_length %||% "20")
        ), "run_cutadapt", "Submit cutadapt"),
        tool_panel("FastQC", status, "Quality reports for raw or trimmed ATAC-seq reads.", tagList(checkboxInput("fastqc_use_trimmed", "Use trimmed reads", value = trimmed_checkbox_default(p, isolate(input$fastqc_use_trimmed)))), "run_fastqc", "Submit FastQC"),
        tool_panel("Bowtie2", status, "Align paired-end ATAC fragments to the GRCm39/GENCODE M39 index, remove duplicates, and create CPM bigWigs scaled to one million mapped reads.", tagList(
          checkboxInput("atac_bowtie2_use_trimmed", "Use trimmed reads", value = trimmed_checkbox_default(p, isolate(input$atac_bowtie2_use_trimmed))),
          tags$p(class = "muted small-note", atac_reference_resources(p)$bowtie2_index)
        ), "run_atac_bowtie2", "Submit Bowtie2"),
        tool_panel("Post-alignment Repair", status, "Repair incomplete ATAC post-alignment outputs without repeating read alignment.", tagList(
          uiOutput("atac_postprocess_controls_ui"),
          tags$p(class = "muted small-note", "The app uses a lightweight 24 GB job when only the BAM index or CPM bigWig needs repair. Missing Picard, BED, or insert-size outputs trigger the full 96 GB post-alignment repair. Outputs are replaced only after validation succeeds.")
        ), "run_atac_postprocess", "Repair selected samples", status_step = "Bowtie2", show_sample_progress = FALSE, show_job_actions = FALSE),
        tool_panel("MACS2 Peaks", status, "Call shifted ATAC-seq peaks and generate TSS heatmaps and Homer annotations.", tagList(textInput("atac_macs2_qvalue", "MACS2 q-value", value = input$atac_macs2_qvalue %||% "0.05")), "run_atac_macs2", "Submit MACS2"),
        tool_panel("Differential Peaks", status, "Build the DiffBind consensus peakset and test differential accessibility.", tagList(uiOutput("atac_diffbind_controls_ui"), tags$p(class = "muted small-note", "Requires at least two biological replicates per selected condition.")), "run_atac_diffbind", "Submit DiffBind", data.frame())
      ))
    }
    div(class = "run-grid",
      tool_panel("Cutadapt", status, "Trim adapters and short reads from raw FASTQs.",
        tagList(
          selectInput("cutadapt_adapter1", "R1/read1 adapter", choices = r1_choices, selected = selected_choice(input$cutadapt_adapter1, r1_choices, r1_choices[[1]]), width = "100%", selectize = FALSE),
          conditionalPanel("input.cutadapt_adapter1 == '__custom__'", textInput("cutadapt_adapter1_custom", "Custom R1/read1 adapter sequence", value = input$cutadapt_adapter1_custom %||% "", width = "100%")),
          selectInput("cutadapt_adapter2", "R2/read2 adapter", choices = r2_choices, selected = selected_choice(input$cutadapt_adapter2, r2_choices, r2_choices[[1]]), width = "100%", selectize = FALSE),
          conditionalPanel("input.cutadapt_adapter2 == '__custom__'", textInput("cutadapt_adapter2_custom", "Custom R2/read2 adapter sequence", value = input$cutadapt_adapter2_custom %||% "", width = "100%")),
          textInput("cutadapt_min_length", "Minimum read length", value = input$cutadapt_min_length %||% "20")
        ),
        "run_cutadapt", "Submit cutadapt"),
      tool_panel("FastQC", status, "Quality reports for raw or trimmed reads.",
        tagList(checkboxInput("fastqc_use_trimmed", "Use trimmed reads", value = trimmed_checkbox_default(p, isolate(input$fastqc_use_trimmed)))),
        "run_fastqc", "Submit FastQC"),
      tool_panel("STAR", status, "Align raw or trimmed reads to the selected genome index.",
        tagList(checkboxInput("star_use_trimmed", "Use trimmed reads", value = trimmed_checkbox_default(p, isolate(input$star_use_trimmed)))),
        "run_star", "Submit STAR"),
      tool_panel("featureCounts", status, "Quantify STAR BAM files with the selected GTF attribute.",
        tagList(selectInput("feature_attr", "featureCounts attribute", choices = c("gene_name", "gene_id"), selected = selected_choice(input$feature_attr, c("gene_name", "gene_id"), "gene_name"), selectize = FALSE)),
        "run_featurecounts", "Submit featureCounts"),
      tool_panel("DESeq2", status, "Run differential expression from count_matrix.txt.",
        tagList(
          uiOutput("deseq_controls_ui"),
          uiOutput("deseq_project_summary_ui")
        ),
        "run_deseq2", "Submit DESeq2", data.frame()),
      tool_panel("GSEA", status, "Run pathway analysis from DESeq2 normalized counts.",
        tagList(uiOutput("gsea_run_controls_ui"), uiOutput("gsea_project_summary_ui")),
        "run_gsea", "Submit GSEA", data.frame()),
      tool_panel("RSEM (optional)", status, "Optional quantification from STAR BAM/transcriptome outputs.",
        tagList(selectInput("rsem_feature_attr", "RSEM feature attribute", choices = c("gene_id", "gene_name"), selected = selected_choice(input$rsem_feature_attr, c("gene_id", "gene_name"), "gene_id"), selectize = FALSE)),
        "run_rsem", "Submit RSEM"),
      tool_panel("Kallisto (optional)", status, "Optional transcript abundance quantification from raw or trimmed reads.",
        tagList(checkboxInput("kallisto_use_trimmed", "Use trimmed reads", value = trimmed_checkbox_default(p, isolate(input$kallisto_use_trimmed)))),
        "run_kallisto", "Submit Kallisto")
    )
  })

  output$cutrun_diffbind_reference_ui <- renderUI({
    p <- current_project()
    if (!is_cutrun_project(p)) return(NULL)
    conditions <- cutrun_diffbind_conditions(p)
    if (!length(conditions)) return(div(class = "empty-box", "Add condition values to the CUT&RUN design matrix first."))
    current <- input$cutrun_diffbind_reference %||% conditions[[1]]
    selectInput(
      "cutrun_diffbind_reference", "Reference condition",
      choices = conditions,
      selected = selected_choice(current, conditions, conditions[[1]]),
      selectize = FALSE
    )
  })

  output$cutrun_diffbind_jobs_ui <- renderUI({
    p <- current_project()
    if (!is_cutrun_project(p)) return(NULL)
    reference <- input$cutrun_diffbind_reference %||% ""
    support <- input$cutrun_diffbind_min_replicates %||% 1
    plan <- cutrun_diffbind_comparison_plan(p, reference, support)
    if (!NROW(plan)) return(div(class = "empty-box", "No non-reference comparisons were found for this reference condition."))
    eligible <- plan[plan$eligible, , drop = FALSE]
    unavailable <- plan[!plan$eligible, , drop = FALSE]
    if (!NROW(eligible)) return(div(class = "empty-box", paste(unique(unavailable$reason), collapse = "; ")))
    choices <- stats::setNames(eligible$id, eligible$label)
    current <- intersect(input$cutrun_diffbind_jobs %||% character(0), eligible$id)
    if (!length(current)) current <- eligible$id
    tagList(
      selectInput("cutrun_diffbind_jobs", "Comparisons (one SLURM job each)", choices = choices, selected = current, multiple = TRUE),
      if (NROW(unavailable)) tags$details(
        tags$summary(sprintf("%s under-replicated group%s not selectable", NROW(unavailable), ifelse(NROW(unavailable) == 1L, " is", "s are"))),
        tags$ul(lapply(seq_len(NROW(unavailable)), function(i) tags$li(paste(unavailable$label[[i]], "—", unavailable$reason[[i]]))))
      )
    )
  })

  output$atac_diffbind_controls_ui <- renderUI({
    p <- current_project(); if (!is_atac_project(p)) return(NULL)
    design <- project_design_df(p)
    cols <- design_compare_columns(p)
    if (!length(cols)) return(div(class = "empty-box", "Add a comparison variable such as condition or day to the ATAC-seq design matrix."))
    subset_candidates <- cols[vapply(cols, function(col) length(unique(trimws(as.character(design[[col]][nzchar(trimws(as.character(design[[col]])))])))) >= 1L, logical(1))]
    subset_default <- if ("cell_type" %in% subset_candidates) "cell_type" else ""
    subset_col <- as.character(input$atac_diffbind_subset_col %||% subset_default)
    if (!subset_col %in% c("", subset_candidates)) subset_col <- subset_default
    subset_values <- if (nzchar(subset_col)) unique(trimws(as.character(design[[subset_col]]))) else character(0)
    subset_values <- subset_values[nzchar(subset_values)]
    subset_value <- if (length(subset_values)) selected_choice(input$atac_diffbind_subset_value, subset_values, subset_values[[1]]) else ""
    scoped_design <- if (nzchar(subset_col) && nzchar(subset_value)) design[trimws(as.character(design[[subset_col]])) == subset_value, , drop = FALSE] else design
    subset_controls <- tagList(
      selectInput("atac_diffbind_subset_col", "Analyze within", choices = c("All samples" = "", stats::setNames(subset_candidates, subset_candidates)), selected = subset_col, selectize = FALSE),
      if (nzchar(subset_col)) selectInput("atac_diffbind_subset_value", "Subset", choices = subset_values, selected = subset_value, selectize = FALSE) else NULL
    )
    compare_cols <- setdiff(cols, subset_col)
    if (!length(compare_cols)) return(tagList(subset_controls, div(class = "empty-box", "Add another metadata column, such as condition, to define the comparison.")))
    compare_col <- selected_choice(input$atac_diffbind_column, compare_cols, if ("condition" %in% compare_cols) "condition" else compare_cols[[1]])
    values <- sort(unique(trimws(as.character(scoped_design[[compare_col]]))))
    values <- values[nzchar(values)]
    if (length(values) < 2L) return(tagList(
      subset_controls,
      selectInput("atac_diffbind_column", "Comparison variable", choices = compare_cols, selected = compare_col, selectize = FALSE),
      div(class = "empty-box", "The selected variable needs at least two values.")
    ))
    preferred <- values[tolower(values) %in% c("veh", "vehicle", "control", "ctrl", "untreated")]
    ref_default <- if (length(preferred)) preferred[[1]] else values[[1]]
    ref <- selected_choice(input$atac_diffbind_reference, values, ref_default)
    comp <- selected_choice(input$atac_diffbind_comparison, setdiff(values, ref), setdiff(values, ref)[[1]])
    value_labels <- function(x) if (nzchar(subset_value)) stats::setNames(x, paste(subset_value, x, sep = "_")) else x
    tagList(
      subset_controls,
      selectInput("atac_diffbind_column", "Comparison variable", choices = compare_cols, selected = compare_col, selectize = FALSE),
      selectInput("atac_diffbind_reference", "Reference condition", choices = value_labels(values), selected = ref, selectize = FALSE),
      selectInput("atac_diffbind_comparison", "Comparison condition", choices = value_labels(setdiff(values, ref)), selected = comp, selectize = FALSE),
      tags$p(class = "muted small-note", paste(NROW(scoped_design), "samples in this subset."))
    )
  })

  output$atac_postprocess_controls_ui <- renderUI({
    p <- current_project(); if (!is_atac_project(p)) return(NULL)
    checks <- atac_postprocess_status_table(p)
    if (!NROW(checks)) return(div(class = "empty-box", "No ATAC-seq Bowtie2 sample folders were found."))
    repairable <- checks[checks$status == "Repair available", , drop = FALSE]
    eligible <- checks[checks$status != "Full Bowtie2 required", , drop = FALSE]
    full_rerun <- checks[checks$status == "Full Bowtie2 required", , drop = FALSE]
    if (!NROW(eligible)) return(div(class = "empty-box", paste("Run full Bowtie2 for:", paste(full_rerun$sample, collapse = ", "))))
    choices <- stats::setNames(as.character(eligible$sample), paste0(eligible$sample, " — ", eligible$status, ifelse(eligible$status == "Complete", "", paste0(": ", eligible$issues))))
    selected <- intersect(as.character(input$atac_postprocess_samples %||% character(0)), unname(choices))
    if (!length(selected) && NROW(repairable)) selected <- as.character(repairable$sample)
    tagList(
      checkboxGroupInput("atac_postprocess_samples", "Detected repairs or manual force-repair", choices = choices, selected = selected),
      if (!NROW(repairable)) tags$p(class = "muted small-note", "All outputs pass the structural checks. Select a sample only if you want to force regeneration from its aligned BAM.") else NULL,
      if (NROW(full_rerun)) tags$p(class = "muted small-note", paste("Full Bowtie2 is required for:", paste(full_rerun$sample, collapse = ", "))) else NULL
    )
  })

  output$cutrun_postprocess_controls_ui <- renderUI({
    p <- current_project(); if (!is_cutrun_project(p)) return(NULL)
    checks <- cutrun_postprocess_status_table(p)
    if (!NROW(checks)) return(div(class = "empty-box", "No CUT&RUN samples were found."))
    repairable <- checks[checks$status == "Repair available", , drop = FALSE]
    if (!NROW(repairable)) return(tags$p(class = "muted small-note", "No selectively repairable CUT&RUN samples are currently detected."))
    choices <- stats::setNames(as.character(repairable$sample), paste0(repairable$sample, " — ", repairable$issues))
    selected <- intersect(as.character(input$cutrun_postprocess_samples %||% character(0)), unname(choices))
    if (!length(selected)) selected <- as.character(repairable$sample)
    checkboxGroupInput("cutrun_postprocess_samples", "Incomplete samples to repair", choices = choices, selected = selected)
  })

  output$cutrun_dedup_sensitivity_samples_ui <- renderUI({
    p <- current_project(); if (!is_cutrun_project(p)) return(NULL)
    design <- cutrun_target_design(p, include_controls = FALSE)
    if (!NROW(design)) return(div(class = "empty-box", "No non-control CUT&RUN target samples were found."))
    samples <- sort(unique(as.character(design$sample)))
    selected <- intersect(as.character(input$cutrun_dedup_sensitivity_samples %||% character(0)), samples)
    selectizeInput(
      "cutrun_dedup_sensitivity_samples",
      "Target samples to test",
      choices = samples,
      selected = selected,
      multiple = TRUE,
      options = list(plugins = list("remove_button"), placeholder = "Select one or more target samples")
    )
  })

  for (step in runnable_pipeline_steps()) {
    local({
      this_step <- step
      output[[tool_progress_ui_output_id(this_step)]] <- renderUI({
        sample_progress_step_ui(sample_progress_state(), this_step)
      })
      output[[tool_retry_ui_output_id(this_step)]] <- renderUI({
        sample_retry_ui(current_project(), sample_progress_state(), this_step)
      })
      output[[tool_progress_output_id(this_step)]] <- render_csl_table({
        sample_progress_step_table(sample_progress_state(), this_step)
      }, page_length = 20, scroll_y = "360px")
    })
  }

  output$deseq_controls_ui <- renderUI({
    p <- current_project()
    cols <- design_compare_columns(p)
    if (!length(cols)) return(div(class = "empty-box", "No comparison columns found between sample and filename in design_matrix.txt."))
    selected_col <- input$deseq_compare_col %||% if ("treatment" %in% cols) "treatment" else cols[[1]]
    if (!selected_col %in% cols) selected_col <- cols[[1]]
    vals <- design_compare_values(p, selected_col)
    ref <- input$deseq_reference %||% if (length(vals)) vals[[1]] else ""
    comp <- input$deseq_comparison %||% if (length(vals) > 1) vals[[2]] else ref
    ref <- selected_choice(ref, vals, if (length(vals)) vals[[1]] else "")
    comp <- selected_choice(comp, vals, if (length(vals) > 1) vals[[2]] else ref)
    tagList(
      selectInput("deseq_compare_col", "Comparison column", choices = cols, selected = selected_col, selectize = FALSE),
      selectInput("deseq_reference", "Reference/baseline", choices = vals, selected = ref, selectize = FALSE),
      selectInput("deseq_comparison", "Comparison", choices = vals, selected = comp, selectize = FALSE)
    )
  })

  output$deseq_project_summary_ui <- renderUI({
    progress_refresh()
    project_level_step_summary_ui(current_project(), job_history_state(), "DESeq2")
  })

  output$gsea_run_controls_ui <- renderUI({
    p <- current_project()
    cols <- design_compare_columns(p)
    resolved <- tryCatch(
      resolve_comparison_inputs(p, input$gsea_compare_col, input$gsea_reference, input$gsea_comparison),
      error = function(e) e
    )
    if (inherits(resolved, "error")) return(div(class = "empty-box", conditionMessage(resolved)))
    geneset <- selected_choice(input$gsea_geneset, GSEAPY_GENESET_OPTIONS, "MSigDB_Hallmark_2020")
    tagList(
      selectInput("gsea_compare_col", "Comparison column", choices = cols, selected = resolved$compare_col, selectize = FALSE),
      selectInput("gsea_reference", "Reference/baseline", choices = resolved$values, selected = resolved$reference, selectize = FALSE),
      selectInput("gsea_comparison", "Comparison", choices = resolved$values, selected = resolved$comparison, selectize = FALSE),
      selectInput("gsea_geneset", "Gene-set database", choices = GSEAPY_GENESET_OPTIONS, selected = geneset, selectize = FALSE)
    )
  })

  output$gsea_project_summary_ui <- renderUI({
    progress_refresh()
    project_level_step_summary_ui(current_project(), job_history_state(), "GSEA")
  })

  observeEvent(input$run_fastqc, {
    trimmed <- isTRUE(input$fastqc_use_trimmed)
    run_submission("FastQC", submit_fastqc_jobs(current_project(), trimmed), if (trimmed) "trimmed reads" else "raw reads")
  })
  observeEvent(input$run_cutadapt, {
    adapter1 <- selected_adapter_value(input$cutadapt_adapter1, input$cutadapt_adapter1_custom)
    adapter2 <- selected_adapter_value(input$cutadapt_adapter2, input$cutadapt_adapter2_custom)
    if (!nzchar(adapter1) || !nzchar(adapter2)) {
      msg <- "ERROR: Not submitted. Custom adapter sequences cannot be blank."
      run_message(msg)
      set_tool_message("Cutadapt", "")
      finish_submit_refresh()
    } else {
      run_submission("Cutadapt", submit_cutadapt_jobs(current_project(), adapter1, adapter2, input$cutadapt_min_length), "raw reads")
      updateCheckboxInput(session, "fastqc_use_trimmed", value = TRUE)
      updateCheckboxInput(session, "star_use_trimmed", value = TRUE)
      updateCheckboxInput(session, "kallisto_use_trimmed", value = TRUE)
      updateCheckboxInput(session, "cutrun_bowtie2_use_trimmed", value = TRUE)
    }
  })
  observeEvent(input$run_cutrun_bowtie2, {
    trimmed <- isTRUE(input$cutrun_bowtie2_use_trimmed)
    normalization_choice <- isolate(cutrun_normalization_choice())
    run_submission(
      "Bowtie2",
      submit_cutrun_bowtie2_jobs(
        current_project(),
        trimmed = trimmed,
        mapq = input$cutrun_mapq %||% "30",
        max_fragment = input$cutrun_max_fragment %||% "1000",
        dedup_target = isTRUE(input$cutrun_dedup_targets),
        dedup_control = if (is.null(input$cutrun_dedup_controls)) TRUE else isTRUE(input$cutrun_dedup_controls),
        remove_mito = if (is.null(input$cutrun_remove_mito)) TRUE else isTRUE(input$cutrun_remove_mito),
        normalization_mode = normalization_choice,
        spikein_index_path = input$cutrun_spikein_genome %||% CUTRUN_DEFAULT_SPIKEIN_INDEX,
        spikein_name = CUTRUN_DEFAULT_SPIKEIN_NAME,
        spikein_min_reads = input$cutrun_spikein_min_reads %||% "1000"
      ),
      paste(if (trimmed) "trimmed reads" else "raw reads", normalization_choice)
    )
  })
  observeEvent(input$run_cutrun_postprocess, {
    samples <- input$cutrun_postprocess_samples %||% character(0)
    trimmed <- isTRUE(input$cutrun_bowtie2_use_trimmed)
    normalization_choice <- isolate(cutrun_normalization_choice())
    run_submission(
      "Bowtie2",
      submit_cutrun_postprocess_jobs(
        current_project(), samples,
        trimmed = trimmed,
        mapq = input$cutrun_mapq %||% "30",
        max_fragment = input$cutrun_max_fragment %||% "1000",
        dedup_target = isTRUE(input$cutrun_dedup_targets),
        dedup_control = if (is.null(input$cutrun_dedup_controls)) TRUE else isTRUE(input$cutrun_dedup_controls),
        remove_mito = if (is.null(input$cutrun_remove_mito)) TRUE else isTRUE(input$cutrun_remove_mito),
        normalization_mode = normalization_choice,
        spikein_index_path = input$cutrun_spikein_genome %||% CUTRUN_DEFAULT_SPIKEIN_INDEX,
        spikein_name = CUTRUN_DEFAULT_SPIKEIN_NAME,
        spikein_min_reads = input$cutrun_spikein_min_reads %||% "1000"
      ),
      paste("CUT&RUN post-alignment repair:", paste(samples, collapse = ", ")),
      samples = samples
    )
  })
  observeEvent(input$run_atac_bowtie2, {
    trimmed <- isTRUE(input$atac_bowtie2_use_trimmed)
    run_submission("Bowtie2", submit_atac_bowtie2_jobs(current_project(), trimmed), paste(if (trimmed) "trimmed" else "raw", "ATAC reads; M39 index"))
  })
  observeEvent(input$run_atac_postprocess, {
    samples <- input$atac_postprocess_samples %||% character(0)
    run_submission("Bowtie2", submit_atac_postprocess_jobs(current_project(), samples), paste("post-alignment repair:", paste(samples, collapse = ", ")), samples = samples)
  })
  observeEvent(input$run_atac_macs2, {
    run_submission("MACS2 Peaks", submit_atac_macs2_jobs(current_project(), input$atac_macs2_qvalue %||% "0.05"), "shift -100; extsize 200")
  })
  observeEvent(input$run_atac_diffbind, {
    run_submission(
      "Differential Peaks",
      submit_atac_diffbind_job(current_project(), input$atac_diffbind_column %||% "", input$atac_diffbind_reference %||% "", input$atac_diffbind_comparison %||% "", input$atac_diffbind_subset_col %||% "", input$atac_diffbind_subset_value %||% ""),
      paste(if (nzchar(input$atac_diffbind_subset_value %||% "")) paste0(input$atac_diffbind_subset_value, ":") else "", input$atac_diffbind_comparison, "vs", input$atac_diffbind_reference)
    )
  })
  observeEvent(input$run_cutrun_seacr, {
    run_submission(
      "SEACR",
      submit_cutrun_seacr_jobs(current_project(), input$cutrun_seacr_norm %||% "norm", input$cutrun_seacr_stringency %||% "stringent"),
      paste(input$cutrun_seacr_norm %||% "norm", input$cutrun_seacr_stringency %||% "stringent")
    )
  })
  observeEvent(input$run_cutrun_dedup_sensitivity, {
    samples <- input$cutrun_dedup_sensitivity_samples %||% character(0)
    run_submission(
      "SEACR sensitivity",
      submit_cutrun_dedup_sensitivity_jobs(
        current_project(), samples, input$cutrun_seacr_norm %||% "non", input$cutrun_seacr_stringency %||% "stringent",
        input$cutrun_max_fragment %||% "1000", if (is.null(input$cutrun_remove_mito)) TRUE else isTRUE(input$cutrun_remove_mito)
      ),
      paste("deduplicated targets:", paste(samples, collapse = ", "), ";", input$cutrun_seacr_norm %||% "non", input$cutrun_seacr_stringency %||% "stringent"),
      samples = samples
    )
  })
  observeEvent(input$run_cutrun_peakqc, {
    run_submission(
      "Peak QC",
      submit_cutrun_peakqc_job(current_project(), input$cutrun_seacr_norm %||% "non", input$cutrun_seacr_stringency %||% "stringent"),
      paste("consensus peaks + FRiP", input$cutrun_seacr_norm %||% "non", input$cutrun_seacr_stringency %||% "stringent")
    )
  })
  observeEvent(input$run_cutrun_diffbind, {
    reference <- input$cutrun_diffbind_reference %||% ""
    support <- input$cutrun_diffbind_min_replicates %||% 1
    comparisons <- input$cutrun_diffbind_jobs %||% character(0)
    run_submission(
      "Differential Peaks",
      submit_cutrun_diffbind_jobs(current_project(), reference, comparisons, support, input$cutrun_seacr_norm %||% "non", input$cutrun_seacr_stringency %||% "stringent"),
      paste(length(comparisons), "comparison job(s);", input$cutrun_seacr_norm %||% "non", input$cutrun_seacr_stringency %||% "stringent", "; reference", reference, "; consensus support", support)
    )
  })
  observeEvent(input$run_cutrun_macs2, {
    run_submission(
      "MACS2",
      submit_cutrun_macs2_jobs(current_project(), input$cutrun_macs2_qvalue %||% "0.01", input$cutrun_macs2_peak_type %||% "auto"),
      paste(input$cutrun_macs2_peak_type %||% "auto", "q", input$cutrun_macs2_qvalue %||% "0.01")
    )
  })
  observeEvent(input$run_star, {
    trimmed <- isTRUE(input$star_use_trimmed)
    run_submission("STAR", submit_star_jobs(current_project(), trimmed), if (trimmed) "trimmed reads" else "raw reads")
  })
  observeEvent(input$run_rsem, {
    run_submission("RSEM", submit_rsem_jobs(current_project(), input$rsem_feature_attr), paste("STAR BAM; feature", input$rsem_feature_attr))
  })
  observeEvent(input$run_kallisto, {
    trimmed <- isTRUE(input$kallisto_use_trimmed)
    run_submission("Kallisto", submit_kallisto_jobs(current_project(), trimmed), if (trimmed) "trimmed reads" else "raw reads")
  })
  observeEvent(input$run_featurecounts, {
    featurecounts_matrix_autosubmitted(setdiff(featurecounts_matrix_autosubmitted(), current_project()$id))
    run_submission("featureCounts", submit_featurecounts_jobs(current_project(), input$feature_attr), paste("STAR BAM; feature", input$feature_attr))
  })
  observeEvent(input$run_deseq2, {
    if (identical(input$deseq_reference, input$deseq_comparison)) {
      msg <- "ERROR: Not submitted. Reference and comparison must be different."
      run_message(msg)
      set_tool_message("DESeq2", "")
      finish_submit_refresh()
    } else {
      run_submission(
        "DESeq2",
        submit_deseq2_job(current_project(), input$deseq_compare_col, input$deseq_reference, input$deseq_comparison, "NoRedundant", FALSE),
        paste(input$deseq_compare_col, input$deseq_comparison, "vs", input$deseq_reference)
      )
    }
  })
  observeEvent(input$run_gsea, {
    resolved <- tryCatch(
      resolve_comparison_inputs(current_project(), input$gsea_compare_col, input$gsea_reference, input$gsea_comparison),
      error = function(e) e
    )
    if (inherits(resolved, "error")) {
      msg <- paste("ERROR submitting GSEA:", conditionMessage(resolved))
      run_message(msg)
      set_tool_message("GSEA", "")
      finish_submit_refresh()
    } else if (identical(resolved$reference, resolved$comparison)) {
      msg <- "ERROR: Not submitted. Reference and comparison must be different."
      run_message(msg)
      set_tool_message("GSEA", "")
      finish_submit_refresh()
    } else {
      geneset <- selected_choice(input$gsea_geneset, GSEAPY_GENESET_OPTIONS, "MSigDB_Hallmark_2020")
      run_submission(
        "GSEA",
        submit_gseapy_job(current_project(), resolved$compare_col, resolved$reference, resolved$comparison, geneset),
        paste(resolved$compare_col, resolved$comparison, "vs", resolved$reference, geneset)
      )
    }
  })

  for (step in runnable_pipeline_steps()) {
    local({
      this_step <- step
      output[[tool_message_output_id(this_step)]] <- renderUI({
        tool_message_ui(this_step)
      })
      button_id <- tool_cancel_button_id(this_step)
      confirm_id <- tool_cancel_confirm_id(this_step)
      cancel_samples_id <- tool_cancel_samples_id(this_step)
      delete_button_id <- tool_delete_data_button_id(this_step)
      delete_confirm_id <- tool_delete_data_confirm_id(this_step)
      delete_samples_id <- tool_delete_data_samples_id(this_step)
      observeEvent(input[[button_id]], {
        p <- current_project()
        samples <- project_samples(p)
        sample_selector <- if (this_step %in% sample_level_pipeline_steps() && length(samples)) {
          checkboxGroupInput(cancel_samples_id, "Samples to cancel", choices = samples, selected = samples, inline = FALSE)
        } else NULL
        showModal(modalDialog(
          title = paste("Cancel active", this_step, "jobs?"),
          tags$p("This will cancel tracked active SLURM jobs for this step in the selected project only."),
          tags$ul(
            tags$li(tags$strong("Project: "), p$label %||% p$name),
            tags$li(tags$strong("Step: "), this_step)
          ),
          sample_selector,
          tags$p(tags$strong("Running jobs will stop and may leave partial outputs.")),
          footer = tagList(
            modalButton("Keep jobs running"),
            actionButton(confirm_id, "Yes, cancel active jobs", class = "btn-danger")
          ),
          easyClose = TRUE
        ))
      }, ignoreInit = TRUE)
      observeEvent(input[[confirm_id]], {
        samples <- isolate(input[[cancel_samples_id]] %||% character(0))
        if (this_step %in% sample_level_pipeline_steps() && !length(samples)) {
          removeModal()
          run_message("Select at least one sample to cancel for this step.")
          return()
        }
        removeModal()
        run_message(paste("Canceling active", this_step, "jobs..."))
        set_tool_message(this_step, "")
        msg <- tryCatch(cancel_active_step_jobs(current_project(), this_step, samples), error = function(e) paste("ERROR canceling", this_step, "jobs:", conditionMessage(e)))
        run_message(msg)
        set_tool_message(this_step, "")
        safe_refresh_progress_now("cancel")
      }, ignoreInit = TRUE)
      observeEvent(input[[delete_button_id]], {
        p <- current_project()
        samples <- project_samples(p)
        sample_selector <- if (this_step %in% sample_level_pipeline_steps() && length(samples)) {
          checkboxGroupInput(delete_samples_id, "Samples to delete", choices = samples, selected = samples, inline = FALSE)
        } else NULL
        paths <- unique(step_data_paths(p, this_step, if (this_step %in% sample_level_pipeline_steps()) samples else NULL))
        showModal(modalDialog(
          title = paste("Delete", this_step, "data outputs?"),
          tags$p("This will delete this step's output data for the selected project. It will not delete the whole project folder."),
          tags$ul(
            tags$li(tags$strong("Project: "), p$label %||% p$name),
            tags$li(tags$strong("Step: "), this_step)
          ),
          sample_selector,
          if (length(paths)) tagList(tags$p("Data paths:"), tags$ul(lapply(paths, function(path) tags$li(code(path))))) else tags$p("No expected data paths were found for this step."),
          tags$p(tags$strong("This cannot be undone.")),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(delete_confirm_id, "Yes, delete step data", class = "btn-danger")
          ),
          easyClose = TRUE
        ))
      }, ignoreInit = TRUE)
      observeEvent(input[[delete_confirm_id]], {
        samples <- isolate(input[[delete_samples_id]] %||% character(0))
        if (this_step %in% sample_level_pipeline_steps() && !length(samples)) {
          removeModal()
          run_message("Select at least one sample to delete for this step.")
          return()
        }
        removeModal()
        run_message(paste("Deleting", this_step, "data outputs..."))
        set_tool_message(this_step, "")
        msg <- tryCatch(delete_step_data(current_project(), this_step, samples), error = function(e) paste("ERROR deleting", this_step, "data outputs:", conditionMessage(e)))
        run_message(msg)
        set_tool_message(this_step, "")
        safe_refresh_progress_now("delete data")
      }, ignoreInit = TRUE)
    })
  }

  output$run_output <- renderText({
    msg <- run_message()
    if (upstream_blocking_message(msg)) "" else msg
  })

  native_results_app <- reactive({
    native_results_refresh()
    if (is_cutrun_project(current_project()) || is_atac_project(current_project())) return(NULL)
    load_native_rnaseq_viewer(current_project())
  })

  native_results_error <- reactiveVal("")

  load_native_results_once <- function() {
    p <- current_project()
    key <- as.character(p$id %||% "")
    if (!identical(native_results_loaded_project(), key)) {
      native_registered_id("")
      native_results_loaded_project(key)
      native_results_refresh(isolate(native_results_refresh()) + 1L)
    }
  }

  observeEvent(input$web_main_tabs, {
    if (identical(input$web_main_tabs %||% "", "Results Explorer")) {
      load_native_results_once()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$native_results_tab_visible, {
    load_native_results_once()
  }, ignoreInit = TRUE)

  output$native_results_ui <- renderUI({
    if (!isTRUE(existing_project_selected())) {
      return(div(class = "empty-box", "Create or select a project to open its Results Explorer."))
    }
    if (is_cutrun_project(current_project())) {
      return(cutrun_results_explorer_ui())
    }
    if (is_atac_project(current_project())) return(atac_results_explorer_ui())
    err <- native_results_error()
    if (nzchar(err)) {
      return(div(class = "empty-box", tags$h4("Results Explorer server error"), tags$pre(err)))
    }
    app <- native_results_app()
    tagList(app$ui)
  })

  observeEvent(native_results_app(), {
    app <- native_results_app()
    if (is.null(app)) return()
    if (!identical(native_registered_id(), app$id)) {
      native_results_error("")
      server_result <- tryCatch({
        app$server(input, output, session)
        NULL
      }, error = function(e) e)
      if (inherits(server_result, "error")) {
        native_results_error(conditionMessage(server_result))
      } else {
        native_registered_id(app$id)
      }
    }
  }, ignoreInit = FALSE)

  observeEvent(input$refresh_cutrun_results, {
    if (!is_cutrun_project(current_project())) return()
    safe_refresh_progress_now("CUT&RUN results refresh")
  }, ignoreInit = TRUE)
  observeEvent(input$refresh_atac_results, {
    if (is_atac_project(current_project())) safe_refresh_progress_now("ATAC-seq results refresh")
  }, ignoreInit = TRUE)

  output$results_overview <- render_csl_table(project_status(current_project()), page_length = 20)
  output$design_table <- render_csl_table(safe_read_table(current_project()$design_matrix_path), page_length = 50)
  output$atac_qc_sample_control <- renderUI({
    req(identical(input$web_main_tabs %||% "", "Results Explorer"))
    progress_refresh()
    samples <- cutrun_qc_samples(current_project())
    if (!length(samples)) return(NULL)
    selectInput("atac_qc_sample", "Sample", choices = samples,
                selected = selected_choice(input$atac_qc_sample, samples, samples[[1]]), selectize = FALSE)
  })
  output$atac_qc_mode_control <- renderUI({
    progress_refresh()
    p <- current_project()
    raw_dir <- file.path(p$data_dir, "fastqc")
    trim_dir <- file.path(p$data_dir, "fastqc_cutadapt")
    raw_available <- dir.exists(raw_dir) && length(list.files(raw_dir, pattern = "_(fastqc|screen)\\.html$", ignore.case = TRUE)) > 0
    trim_available <- dir.exists(trim_dir) && length(list.files(trim_dir, pattern = "_(fastqc|screen)\\.html$", ignore.case = TRUE)) > 0
    selected <- if (trim_available && !raw_available) TRUE else if (raw_available && !trim_available) FALSE else if (is.null(input$atac_qc_show_trimmed)) trim_available else isTRUE(input$atac_qc_show_trimmed)
    checkboxInput("atac_qc_show_trimmed", "Show cutadapt-trimmed QC", value = selected)
  })
  output$atac_qc_status_ui <- renderUI({
    progress_refresh()
    p <- current_project()
    raw_available <- dir.exists(file.path(p$data_dir, "fastqc")) && length(list.files(file.path(p$data_dir, "fastqc"), pattern = "_(fastqc|screen)\\.html$", ignore.case = TRUE)) > 0
    trim_available <- dir.exists(file.path(p$data_dir, "fastqc_cutadapt")) && length(list.files(file.path(p$data_dir, "fastqc_cutadapt"), pattern = "_(fastqc|screen)\\.html$", ignore.case = TRUE)) > 0
    if (!raw_available && !trim_available) return(div(class = "empty-box", "FastQC has not been run yet."))
    if (isTRUE(input$atac_qc_show_trimmed) && !trim_available) return(div(class = "empty-box", "QC has not been run on trimmed reads."))
    if (!isTRUE(input$atac_qc_show_trimmed) && !raw_available) return(div(class = "empty-box", "QC has not been run on raw reads."))
    NULL
  })
  output$atac_r1_fastqc_ui <- renderUI({ cutrun_qc_report_ui(current_project(), input$atac_qc_sample %||% "", "R1", "fastqc", isTRUE(input$atac_qc_show_trimmed)) })
  output$atac_r1_screen_ui <- renderUI({ cutrun_qc_report_ui(current_project(), input$atac_qc_sample %||% "", "R1", "screen", isTRUE(input$atac_qc_show_trimmed)) })
  output$atac_r2_fastqc_ui <- renderUI({
    if (!isTRUE(current_project()$paired_end)) return(div(class = "empty-box", "This is a single-end project; there is no R2 report."))
    cutrun_qc_report_ui(current_project(), input$atac_qc_sample %||% "", "R2", "fastqc", isTRUE(input$atac_qc_show_trimmed))
  })
  output$atac_r2_screen_ui <- renderUI({
    if (!isTRUE(current_project()$paired_end)) return(div(class = "empty-box", "This is a single-end project; there is no R2 report."))
    cutrun_qc_report_ui(current_project(), input$atac_qc_sample %||% "", "R2", "screen", isTRUE(input$atac_qc_show_trimmed))
  })
  output$atac_alignment_summary <- render_csl_table(atac_alignment_summary_table(current_project()), page_length = 50)
  output$atac_postprocess_status <- render_csl_table(atac_postprocess_status_table(current_project()), page_length = 50)
  output$atac_insert_size_ui <- renderUI({
    progress_refresh(); p <- current_project(); files <- if (dir.exists(file.path(p$data_dir, "bowtie2"))) list.files(file.path(p$data_dir, "bowtie2"), pattern = "_insert_size_histogram\\.jpg$", recursive = TRUE, full.names = TRUE) else character(0)
    if (!length(files)) return(div(class = "empty-box", "No insert-size histogram found yet."))
    tagList(selectInput("atac_insert_size_file", "Sample insert-size plot", choices = stats::setNames(files, basename(dirname(files))), selected = selected_choice(input$atac_insert_size_file, files, files[[1]]), selectize = FALSE), image_or_file_ui(input$atac_insert_size_file %||% files[[1]], "650px"))
  })
  output$atac_peak_file_ui <- renderUI({
    progress_refresh(); choices <- result_file_choices(current_project(), "macs2", "(narrowPeak|peaks_annotated\\.txt)$")
    if (!length(choices)) return(div(class = "empty-box", "No MACS2 peaks found yet."))
    selectInput("atac_peak_file", "Peak file", choices = choices, selected = selected_choice(input$atac_peak_file, choices, choices[[1]]), selectize = FALSE)
  })
  output$atac_peak_table <- render_csl_table({ req(input$atac_peak_file); safe_read_result_table(input$atac_peak_file, 10000) }, page_length = 50)
  output$atac_peak_heatmap_ui <- renderUI({
    req(input$atac_peak_file); dir <- dirname(input$atac_peak_file); png <- list.files(dir, pattern = "_heatmap_TSS\\.png$", full.names = TRUE)
    if (!length(png)) return(div(class = "empty-box", "No TSS heatmap found for this sample.")); image_or_file_ui(png[[1]], "700px")
  })
  output$atac_diffbind_dir_ui <- renderUI({
    progress_refresh(); root <- file.path(current_project()$data_dir, "diffbind"); dirs <- if (dir.exists(root)) list.dirs(root, recursive = FALSE, full.names = TRUE) else character(0); dirs <- dirs[lengths(lapply(dirs, function(d) list.files(d, pattern = "^DifferentialPeaks_.*\\.txt$"))) > 0]
    if (!length(dirs)) return(div(class = "empty-box", "No DiffBind comparison found yet.")); selectInput("atac_diffbind_dir", "Comparison", choices = stats::setNames(dirs, basename(dirs)), selected = selected_choice(input$atac_diffbind_dir, dirs, dirs[[1]]), selectize = FALSE)
  })
  selected_atac_diffbind_dir <- reactive({ path <- input$atac_diffbind_dir %||% ""; req(nzchar(path), dir.exists(path)); path })
  output$atac_diffbind_table <- render_csl_table({ files <- list.files(selected_atac_diffbind_dir(), pattern = "^DifferentialPeaks_.*\\.txt$", full.names = TRUE); req(length(files)); safe_read_table(files[[1]], 20000) }, page_length = 50, scroll_y = "600px")
  output$atac_diffbind_pca_ui <- renderUI({ image_or_file_ui(file.path(selected_atac_diffbind_dir(), "diffbind_pca_byNormCounts.png"), "620px") })
  output$atac_diffbind_volcano_ui <- renderUI({ image_or_file_ui(file.path(selected_atac_diffbind_dir(), "diffbind_volcano_byDiffPeaks.png"), "620px") })
  output$atac_file_ui <- renderUI({
    progress_refresh(); choices <- result_file_choices(current_project(), c("bowtie2", "macs2", "diffbind"), "\\.(bed|bedgraph|bdg|bw|bam|bai|txt|tsv|csv|png|jpg|pdf|xls|narrowPeak)$")
    if (!length(choices)) return(div(class = "empty-box", "No ATAC-seq result files found yet.")); selectInput("atac_file", "ATAC-seq result file", choices = choices, selected = selected_choice(input$atac_file, choices, choices[[1]]), selectize = FALSE)
  })
  output$atac_file_view <- renderUI({ req(input$atac_file); if (tolower(tools::file_ext(input$atac_file)) %in% c("txt","tsv","csv","bed","bdg","xls","narrowpeak")) table_output("atac_selected_table") else image_or_file_ui(input$atac_file, "850px") })
  output$atac_selected_table <- render_csl_table({ req(input$atac_file); safe_read_result_table(input$atac_file, 10000) }, page_length = 50)
  output$cutrun_summary_cards <- renderUI({
    progress_refresh()
    cutrun_summary_cards_ui(current_project())
  })
  output$cutrun_peak_qc_cards <- renderUI({
    progress_refresh()
    frip <- cutrun_seacr_frip_table(current_project())
    values <- if (NROW(frip) && "frip" %in% names(frip)) clean_metric_number(frip$frip) else numeric(0)
    values <- values[is.finite(values)]
    values <- ifelse(values <= 1, values * 100, values)
    summary <- cutrun_peak_qc_summary_table(current_project())
    summary_values <- if (NROW(summary)) stats::setNames(as.character(summary$Value), as.character(summary$Metric)) else character(0)
    summary_value <- function(metric, fallback = NA_character_) {
      if (metric %in% names(summary_values)) summary_values[[metric]] else fallback
    }
    div(class = "cutrun-metric-grid compact",
        cutrun_metric_card("Samples with FRiP", format_metric_value(length(values)), "Completed SEACR summaries", "blue"),
        cutrun_metric_card("Median FRiP", if (length(values)) format_metric_value(stats::median(values), "%") else "—", "Across target samples", "green"),
        cutrun_metric_card("Peak files", format_metric_value(clean_metric_number(summary_value("peak_files", NROW(frip)))), "Included in project QC", "gold"),
        cutrun_metric_card("Union regions", format_metric_value(clean_metric_number(summary_value("consensus_peaks"))), "Project-wide QC union", "purple")
    )
  })
  output$cutrun_alignment_summary <- render_csl_table({
    cutrun_alignment_summary_table(current_project())
  }, page_length = 50)
  output$cutrun_alignment_sample_control <- renderUI({
    progress_refresh()
    df <- cutrun_alignment_summary_table(current_project())
    samples <- if (NROW(df) && "sample" %in% names(df)) trimws(as.character(df$sample)) else character(0)
    samples <- unique(samples[nzchar(samples)])
    if (!length(samples)) return(NULL)
    selectInput("cutrun_alignment_sample", "Bowtie2 sample", choices = samples,
                selected = selected_choice(input$cutrun_alignment_sample, samples, samples[[1]]), selectize = FALSE)
  })
  output$cutrun_alignment_status_ui <- renderUI({
    progress_refresh()
    df <- cutrun_alignment_summary_table(current_project())
    if (!NROW(df)) div(class = "empty-box", "Bowtie2 alignment summaries have not been generated yet.") else NULL
  })
  output$cutrun_alignment_sample_table <- render_csl_table({
    req(input$cutrun_alignment_sample)
    df <- cutrun_alignment_summary_table(current_project())
    validate(need(NROW(df) && "sample" %in% names(df), "No Bowtie2 alignment summary is available."))
    hit <- df[trimws(as.character(df$sample)) == input$cutrun_alignment_sample, , drop = FALSE]
    validate(need(NROW(hit), "The selected sample is not present in the alignment summary."))
    data.frame(Metric = setdiff(names(hit), "sample"), Value = unlist(hit[1, setdiff(names(hit), "sample"), drop = FALSE], use.names = FALSE), check.names = FALSE)
  }, page_length = 50, scroll_y = "420px")
  output$cutrun_fragment_sample_ui <- renderUI({
    progress_refresh()
    files <- cutrun_fragment_plot_files(current_project())
    if (!length(files)) return(div(class = "empty-box", "No insert-size histogram has been generated yet."))
    labels <- stats::setNames(files, basename(dirname(files)))
    selectInput("cutrun_fragment_file", "Sample", choices = labels, selected = selected_choice(input$cutrun_fragment_file, files, files[[1]]), selectize = FALSE)
  })
  output$cutrun_fragment_size_ui <- renderUI({
    progress_refresh()
    path <- input$cutrun_fragment_file %||% ""
    if (!nzchar(path) || !file.exists(path)) return(div(class = "empty-box", "Choose a sample with a completed Picard insert-size plot."))
    tagList(
      div(class = "cutrun-section-heading", tags$h4("Insert-size distribution"), tags$p(basename(dirname(path)))),
      image_or_file_ui(path, "700px")
    )
  })
  output$cutrun_alignment_plot <- renderPlot({
    progress_refresh()
    df <- cutrun_alignment_summary_table(current_project())
    validate(need(NROW(df), "No Bowtie2 alignment summaries were found yet."))
    n <- NROW(df)
    sample <- if ("sample" %in% names(df)) trimws(as.character(df$sample)) else rep("", n)
    if (length(sample) != n) sample <- rep("", n)
    blank <- !nzchar(sample) | is.na(sample)
    sample[blank] <- paste0("sample_", seq_len(n))[blank]
    mapped <- if ("mapped_reads" %in% names(df)) clean_metric_number(df$mapped_reads) else rep(NA_real_, n)
    spikein <- if ("spikein_mapped_reads" %in% names(df)) clean_metric_number(df$spikein_mapped_reads) else rep(NA_real_, n)
    if (length(mapped) != n) mapped <- rep(NA_real_, n)
    if (length(spikein) != n) spikein <- rep(NA_real_, n)
    keep <- is.finite(mapped) | is.finite(spikein)
    validate(need(any(keep), "Alignment summaries were found, but they do not contain finite mapped-read values yet."))
    sample <- sample[keep]
    mapped <- mapped[keep]
    spikein <- spikein[keep]
    mapped[!is.finite(mapped)] <- 0
    spikein[!is.finite(spikein)] <- 0
    values <- rbind(`Genome mapped` = log10(mapped + 1), `E. coli spike-in` = log10(spikein + 1))
    validate(need(NCOL(values) > 0 && all(is.finite(values)), "No finite alignment values are available to plot."))
    old <- par(mar = c(8, 4.4, 2.5, 1), xpd = FALSE)
    on.exit(par(old), add = TRUE)
    barplot(values, beside = TRUE, names.arg = sample, las = 2, col = c("#2f6fed", "#d39116"), border = NA,
            ylim = c(0, max(1, max(values) * 1.12)), ylab = "log10(reads + 1)", main = "Bowtie2 mapped and spike-in reads")
    legend("topright", legend = rownames(values), fill = c("#2f6fed", "#d39116"), bty = "n", cex = 0.85)
  }, res = 110)
  output$cutrun_frip_summary <- render_csl_table({
    cutrun_seacr_frip_table(current_project())
  }, page_length = 50)
  output$cutrun_frip_plot <- renderPlot({
    progress_refresh()
    df <- cutrun_seacr_frip_table(current_project())
    validate(need(NROW(df) && "frip" %in% names(df), "No SEACR FRiP results were found yet."))
    sample <- if ("sample" %in% names(df)) as.character(df$sample) else paste0("sample_", seq_len(NROW(df)))
    frip <- clean_metric_number(df$frip)
    frip <- ifelse(frip <= 1, frip * 100, frip)
    keep <- is.finite(frip)
    validate(need(any(keep), "No numeric FRiP values were found yet."))
    old <- par(mar = c(8, 4.4, 2.5, 1))
    on.exit(par(old), add = TRUE)
    barplot(frip[keep], names.arg = sample[keep], las = 2, col = "#15936f", border = NA,
            ylab = "FRiP (%)", main = "Fraction of reads in SEACR peaks")
    abline(h = 1, col = "#94a3b8", lty = 3)
  }, res = 110)
  output$cutrun_peak_qc_summary <- render_csl_table({
    cutrun_peak_qc_summary_table(current_project())
  }, page_length = 50)
  output$cutrun_peak_counts <- render_csl_table({
    root <- file.path(current_project()$data_dir, "cutrun_peak_qc")
    paths <- if (dir.exists(root)) list.files(root, pattern = "^seacr_consensus_peak_counts\\.tsv$", recursive = TRUE, full.names = TRUE) else character(0)
    if (!length(paths)) return(data.frame())
    safe_read_table(paths[[which.max(file.info(paths)$mtime)]], 5000)
  }, page_length = 50)
  output$cutrun_signal_tracks <- render_csl_table({
    progress_refresh()
    cutrun_signal_track_table(current_project())
  }, page_length = 50, scroll_y = "600px")
  output$cutrun_seacr_peak_ui <- renderUI({
    req(identical(input$web_main_tabs %||% "", "Results Explorer"))
    progress_refresh()
    choices <- result_file_choices(current_project(), "seacr", "\\.bed$")
    if (!length(choices)) return(div(class = "empty-box", "No SEACR peak BED files were found yet."))
    paths <- unname(choices)
    root <- file.path(current_project()$data_dir, "seacr")
    labels <- vapply(paths, function(path) {
      rel <- sub(paste0("^", gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", normalizePath(root, winslash = "/", mustWork = FALSE), perl = TRUE), "/?"), "", normalizePath(path, winslash = "/", mustWork = FALSE))
      rel
    }, character(1))
    friendly <- stats::setNames(paths, labels)
    selectInput("cutrun_seacr_peak_file", "SEACR sample", choices = friendly, selected = selected_choice(input$cutrun_seacr_peak_file, paths, paths[[1]]), selectize = FALSE)
  })
  output$cutrun_seacr_peak_table <- render_csl_table({
    req(input$cutrun_seacr_peak_file)
    safe_read_result_table(input$cutrun_seacr_peak_file, 5000)
  }, page_length = 50)
  output$cutrun_seacr_peak_cards <- renderUI({
    progress_refresh()
    path <- input$cutrun_seacr_peak_file %||% ""
    if (!nzchar(path) || !file.exists(path)) return(NULL)
    peaks <- safe_read_result_table(path, 5000)
    widths <- if (NROW(peaks) && all(c("start", "end") %in% names(peaks))) clean_metric_number(peaks$end) - clean_metric_number(peaks$start) else numeric(0)
    widths <- widths[is.finite(widths) & widths > 0]
    div(class = "cutrun-metric-grid compact",
        cutrun_metric_card("Sample", basename(dirname(path)), basename(path), "blue"),
        cutrun_metric_card("Peaks shown", format_metric_value(NROW(peaks)), "Table preview limit: 5,000", "green"),
        cutrun_metric_card("Median width", if (length(widths)) paste0(format_metric_value(stats::median(widths)), " bp") else "—", "Native SEACR regions", "gold"),
        cutrun_metric_card("File size", human_file_size(path), "BED output", "purple")
    )
  })
  output$cutrun_macs2_peak_ui <- renderUI({
    req(identical(input$web_main_tabs %||% "", "Results Explorer"))
    progress_refresh()
    choices <- result_file_choices(current_project(), "macs2", "(narrowPeak|broadPeak|peaks\\.xls)$")
    if (!length(choices)) return(div(class = "empty-box", "No MACS2 peak files were found yet. MACS2 is optional."))
    paths <- unname(choices)
    labels <- paste(basename(dirname(paths)), basename(paths), sep = " — ")
    friendly <- stats::setNames(paths, labels)
    selectInput("cutrun_macs2_peak_file", "MACS2 sample", choices = friendly, selected = selected_choice(input$cutrun_macs2_peak_file, paths, paths[[1]]), selectize = FALSE)
  })
  output$cutrun_macs2_peak_table <- render_csl_table({
    req(input$cutrun_macs2_peak_file)
    safe_read_result_table(input$cutrun_macs2_peak_file, 5000)
  }, page_length = 50)
  output$cutrun_diffbind_comparison_ui <- renderUI({
    req(identical(input$web_main_tabs %||% "", "Results Explorer"))
    progress_refresh()
    dirs <- cutrun_diffbind_result_dirs(current_project())
    if (!length(dirs)) return(div(class = "empty-box", "No completed CUT&RUN differential comparisons were found yet."))
    labels <- gsub("__", " — ", basename(dirs), fixed = TRUE)
    choices <- stats::setNames(dirs, labels)
    selectInput(
      "cutrun_diffbind_result_dir", "Comparison",
      choices = choices,
      selected = selected_choice(input$cutrun_diffbind_result_dir, dirs, dirs[[1]]),
      selectize = FALSE
    )
  })
  selected_cutrun_diffbind_dir <- reactive({
    path <- input$cutrun_diffbind_result_dir %||% ""
    req(nzchar(path), dir.exists(path))
    path
  })
  cutrun_diffbind_all_results <- reactive({
    safe_read_table(file.path(selected_cutrun_diffbind_dir(), "all_differential_peaks.tsv"), 20000)
  })
  cutrun_diffbind_filtered_results <- reactive({
    df <- cutrun_diffbind_all_results()
    if (!NROW(df)) return(df)
    fdr_col <- intersect(c("FDR", "padj", "qvalue"), names(df))
    fold_col <- intersect(c("Fold", "log2FoldChange", "log2FC"), names(df))
    keep <- rep(TRUE, NROW(df))
    fdr_cutoff <- suppressWarnings(as.numeric(input$cutrun_diffbind_fdr %||% 0.05))
    fold_cutoff <- suppressWarnings(as.numeric(input$cutrun_diffbind_fold %||% 0))
    if (length(fdr_col) && is.finite(fdr_cutoff)) {
      values <- clean_metric_number(df[[fdr_col[[1]]]])
      keep <- keep & is.finite(values) & values <= fdr_cutoff
    }
    if (length(fold_col) && is.finite(fold_cutoff) && fold_cutoff > 0) {
      values <- clean_metric_number(df[[fold_col[[1]]]])
      keep <- keep & is.finite(values) & abs(values) >= fold_cutoff
    }
    df[keep, , drop = FALSE]
  })
  output$cutrun_diffbind_summary <- render_csl_table({
    cutrun_diffbind_summary_table(current_project())
  }, page_length = 25)
  output$cutrun_diffbind_results <- render_csl_table({
    cutrun_diffbind_filtered_results()
  }, page_length = 50, scroll_y = "620px")
  output$cutrun_diffbind_significant <- render_csl_table({
    safe_read_table(file.path(selected_cutrun_diffbind_dir(), "significant_differential_peaks.tsv"), 20000)
  }, page_length = 50, scroll_y = "620px")
  output$cutrun_diffbind_normalization <- render_csl_table({
    safe_read_table(file.path(selected_cutrun_diffbind_dir(), "normalization_factors.tsv"), 5000)
  }, page_length = 50)
  output$cutrun_diffbind_consensus <- render_csl_table({
    safe_read_table(file.path(selected_cutrun_diffbind_dir(), "consensus_peak_counts.tsv"), 20000)
  }, page_length = 50, scroll_y = "620px")
  output$cutrun_diffbind_cards <- renderUI({
    progress_refresh()
    path <- input$cutrun_diffbind_result_dir %||% ""
    if (!nzchar(path) || !dir.exists(path)) return(NULL)
    all <- cutrun_diffbind_all_results()
    significant <- safe_read_table(file.path(path, "significant_differential_peaks.tsv"), 20000)
    filtered <- cutrun_diffbind_filtered_results()
    normalization <- safe_read_table(file.path(path, "normalization_factors.tsv"), 5000)
    mode <- if (NROW(normalization) && "normalization" %in% names(normalization)) paste(unique(as.character(normalization$normalization)), collapse = ", ") else "—"
    fold_col <- intersect(c("Fold", "log2FoldChange", "log2FC"), names(significant))
    fold <- if (length(fold_col)) clean_metric_number(significant[[fold_col[[1]]]]) else numeric(0)
    div(class = "cutrun-metric-grid",
        cutrun_metric_card("Tested regions", format_metric_value(NROW(all)), basename(path), "blue"),
        cutrun_metric_card("Significant peaks", format_metric_value(NROW(significant)), "Saved at FDR ≤ 0.05", "green"),
        cutrun_metric_card("Passing filters", format_metric_value(NROW(filtered)), "Current sidebar cutoffs", "gold"),
        cutrun_metric_card("Increased / decreased", if (length(fold)) paste0(sum(fold > 0, na.rm = TRUE), " / ", sum(fold < 0, na.rm = TRUE)) else "—", "Direction in comparison", "purple"),
        cutrun_metric_card("Normalization", mode, "DiffBind factors", "blue")
    )
  })
  output$cutrun_diffbind_pca_ui <- renderUI({
    image_or_file_ui(file.path(selected_cutrun_diffbind_dir(), "pca_normalized_counts.png"), "760px")
  })
  output$cutrun_diffbind_volcano_ui <- renderUI({
    image_or_file_ui(file.path(selected_cutrun_diffbind_dir(), "volcano_differential_peaks.png"), "760px")
  })
  output$cutrun_diffbind_ma_ui <- renderUI({
    image_or_file_ui(file.path(selected_cutrun_diffbind_dir(), "ma_differential_peaks.png"), "760px")
  })
  output$cutrun_diffbind_heatmap_ui <- renderUI({
    image_or_file_ui(file.path(selected_cutrun_diffbind_dir(), "differential_peak_heatmap.png"), "760px")
  })
  output$cutrun_file_ui <- renderUI({
    req(identical(input$web_main_tabs %||% "", "Results Explorer"))
    progress_refresh()
    p <- current_project()
    choices <- cutrun_files_by_category(p, input$cutrun_file_category %||% "qc")
    if (!length(choices)) return(div(class = "empty-box", "No files are available in this category yet."))
    paths <- unname(choices)
    selectInput("cutrun_file", "Result file", choices = choices, selected = selected_choice(input$cutrun_file, paths, paths[[1]]), selectize = FALSE)
  })
  output$cutrun_file_metadata_ui <- renderUI({
    path <- input$cutrun_file %||% ""
    if (!nzchar(path) || !file.exists(path)) return(NULL)
    info <- file.info(path)
    div(class = "cutrun-file-meta",
        div(span("Type"), strong(toupper(tools::file_ext(path) %||% "file"))),
        div(span("Size"), strong(human_file_size(path))),
        div(span("Modified"), strong(format(info$mtime[[1]], "%Y-%m-%d %H:%M"))),
        tags$code(path)
    )
  })
  output$cutrun_file_view <- renderUI({
    req(input$cutrun_file)
    ext <- tolower(tools::file_ext(input$cutrun_file))
    if (ext %in% c("txt", "tsv", "csv", "bed", "bedgraph", "narrowpeak", "broadpeak", "xls")) {
      table_output("cutrun_selected_table")
    } else if (ext %in% c("bam", "bai", "bw", "zip", "rds")) {
      div(class = "empty-box", tags$h4(basename(input$cutrun_file)), tags$p("This binary result is ready for downstream use at the server path shown in the sidebar."))
    } else {
      image_or_file_ui(input$cutrun_file, "900px")
    }
  })
  output$cutrun_selected_table <- render_csl_table({
    req(input$cutrun_file)
    safe_read_result_table(input$cutrun_file, 5000)
  }, page_length = 50)
  output$cutrun_qc_sample_control <- renderUI({
    req(identical(input$web_main_tabs %||% "", "Results Explorer"))
    progress_refresh()
    samples <- cutrun_qc_samples(current_project())
    if (!length(samples)) return(NULL)
    selectInput("cutrun_qc_sample", "Sample", choices = samples,
                selected = selected_choice(input$cutrun_qc_sample, samples, samples[[1]]), selectize = FALSE)
  })
  output$cutrun_qc_mode_control <- renderUI({
    progress_refresh()
    p <- current_project()
    raw_dir <- file.path(p$data_dir, "fastqc")
    trim_dir <- file.path(p$data_dir, "fastqc_cutadapt")
    raw_available <- dir.exists(raw_dir) && length(list.files(raw_dir, pattern = "_(fastqc|screen)\\.html$", ignore.case = TRUE)) > 0
    trim_available <- dir.exists(trim_dir) && length(list.files(trim_dir, pattern = "_(fastqc|screen)\\.html$", ignore.case = TRUE)) > 0
    selected <- if (trim_available && !raw_available) TRUE else if (raw_available && !trim_available) FALSE else if (is.null(input$cutrun_qc_show_trimmed)) trim_available else isTRUE(input$cutrun_qc_show_trimmed)
    checkboxInput("cutrun_qc_show_trimmed", "Show cutadapt-trimmed QC", value = selected)
  })
  output$cutrun_qc_status_ui <- renderUI({
    progress_refresh()
    p <- current_project()
    raw_available <- dir.exists(file.path(p$data_dir, "fastqc")) && length(list.files(file.path(p$data_dir, "fastqc"), pattern = "_(fastqc|screen)\\.html$", ignore.case = TRUE)) > 0
    trim_available <- dir.exists(file.path(p$data_dir, "fastqc_cutadapt")) && length(list.files(file.path(p$data_dir, "fastqc_cutadapt"), pattern = "_(fastqc|screen)\\.html$", ignore.case = TRUE)) > 0
    if (!raw_available && !trim_available) return(div(class = "empty-box", "FastQC has not been run yet."))
    if (isTRUE(input$cutrun_qc_show_trimmed) && !trim_available) return(div(class = "empty-box", "QC has not been run on trimmed reads."))
    if (!isTRUE(input$cutrun_qc_show_trimmed) && !raw_available) return(div(class = "empty-box", "QC has not been run on raw reads."))
    NULL
  })
  output$cutrun_r1_fastqc_ui <- renderUI({ cutrun_qc_report_ui(current_project(), input$cutrun_qc_sample %||% "", "R1", "fastqc", isTRUE(input$cutrun_qc_show_trimmed)) })
  output$cutrun_r1_screen_ui <- renderUI({ cutrun_qc_report_ui(current_project(), input$cutrun_qc_sample %||% "", "R1", "screen", isTRUE(input$cutrun_qc_show_trimmed)) })
  output$cutrun_r2_fastqc_ui <- renderUI({
    if (!isTRUE(current_project()$paired_end)) return(div(class = "empty-box", "This is a single-end project; there is no R2 report."))
    cutrun_qc_report_ui(current_project(), input$cutrun_qc_sample %||% "", "R2", "fastqc", isTRUE(input$cutrun_qc_show_trimmed))
  })
  output$cutrun_r2_screen_ui <- renderUI({
    if (!isTRUE(current_project()$paired_end)) return(div(class = "empty-box", "This is a single-end project; there is no R2 report."))
    cutrun_qc_report_ui(current_project(), input$cutrun_qc_sample %||% "", "R2", "screen", isTRUE(input$cutrun_qc_show_trimmed))
  })
  output$download_cutrun_alignment <- downloadHandler(
    filename = function() paste0(clean_name(current_project()$name, "cutrun"), "_alignment_summary.tsv"),
    content = function(file) utils::write.table(cutrun_alignment_summary_table(current_project()), file, sep = "\t", row.names = FALSE, quote = FALSE, na = "")
  )
  output$download_cutrun_frip <- downloadHandler(
    filename = function() paste0(clean_name(current_project()$name, "cutrun"), "_seacr_frip.tsv"),
    content = function(file) utils::write.table(cutrun_seacr_frip_table(current_project()), file, sep = "\t", row.names = FALSE, quote = FALSE, na = "")
  )
  output$download_cutrun_seacr <- downloadHandler(
    filename = function() basename(input$cutrun_seacr_peak_file %||% "seacr_peaks.bed"),
    content = function(file) {
      path <- input$cutrun_seacr_peak_file %||% ""
      req(nzchar(path), file.exists(path))
      file.copy(path, file, overwrite = TRUE)
    }
  )
  output$download_cutrun_diffbind_results <- downloadHandler(
    filename = function() paste0(basename(input$cutrun_diffbind_result_dir %||% "cutrun_comparison"), "_filtered.tsv"),
    content = function(file) utils::write.table(cutrun_diffbind_filtered_results(), file, sep = "\t", row.names = FALSE, quote = FALSE, na = "")
  )
  output$download_cutrun_diffbind_significant <- downloadHandler(
    filename = function() paste0(basename(input$cutrun_diffbind_result_dir %||% "cutrun_comparison"), "_significant.tsv"),
    content = function(file) {
      df <- safe_read_table(file.path(selected_cutrun_diffbind_dir(), "significant_differential_peaks.tsv"), 20000)
      utils::write.table(df, file, sep = "\t", row.names = FALSE, quote = FALSE, na = "")
    }
  )
  output$star_summary <- render_csl_table(safe_read_table(file.path(current_project()$data_dir, "star_summary", "summary_matrix.txt")), page_length = 50)
  output$featurecounts_summary <- render_csl_table(safe_read_table(file.path(current_project()$data_dir, "counts", "featurecounts_summary.txt")), page_length = 50)
  output$count_matrix <- render_csl_table(safe_read_table(file.path(current_project()$data_dir, "counts", "count_matrix.txt"), 5000), page_length = 50)

  file_select <- function(id, label, dir, pattern) {
    files <- if (dir.exists(dir)) list.files(dir, pattern = pattern, recursive = TRUE, full.names = TRUE) else character(0)
    selectInput(id, label, choices = files, selected = files[1] %||% character(0), selectize = FALSE)
  }
  output$rsem_file_ui <- renderUI({ req(identical(input$web_main_tabs %||% "", "Results Explorer")); progress_refresh(); file_select("rsem_file", "RSEM table", file.path(current_project()$data_dir, "rsem"), "\\.(txt|csv|results)$") })
  output$rsem_table <- render_csl_table({ req(input$rsem_file); safe_read_table(input$rsem_file, 5000) }, page_length = 50)
  output$kallisto_file_ui <- renderUI({ req(identical(input$web_main_tabs %||% "", "Results Explorer")); progress_refresh(); file_select("kallisto_file", "Kallisto table", file.path(current_project()$data_dir, "kallisto"), "\\.(tsv|txt|csv)$") })
  output$kallisto_table <- render_csl_table({ req(input$kallisto_file); safe_read_table(input$kallisto_file, 5000) }, page_length = 50)
  output$norm_file_ui <- renderUI({ req(identical(input$web_main_tabs %||% "", "Results Explorer")); progress_refresh(); file_select("norm_file", "DESeq2 normalized counts", file.path(current_project()$data_dir, "deseq2"), "normalized.*\\.(txt|csv)$") })
  output$norm_table <- render_csl_table({ req(input$norm_file); safe_read_table(input$norm_file, 5000) }, page_length = 50)
  output$deseq_file_ui <- renderUI({ req(identical(input$web_main_tabs %||% "", "Results Explorer")); progress_refresh(); file_select("deseq_file", "DESeq2 file", file.path(current_project()$data_dir, "deseq2"), "\\.(txt|csv|png|pdf)$") })
  output$deseq_file_view <- renderUI({
    req(input$deseq_file)
    if (tolower(tools::file_ext(input$deseq_file)) %in% c("txt", "csv", "tsv")) {
      table_output("deseq_selected_table")
    } else image_or_file_ui(input$deseq_file)
  })
  output$deseq_selected_table <- render_csl_table({ req(input$deseq_file); safe_read_table(input$deseq_file, 5000) }, page_length = 50)
  output$gsea_file_ui <- renderUI({ req(identical(input$web_main_tabs %||% "", "Results Explorer")); progress_refresh(); file_select("gsea_file", "GSEA file", file.path(current_project()$data_dir, "gseapy"), "\\.(txt|csv|png|pdf)$") })
  output$gsea_file_view <- renderUI({
    req(input$gsea_file)
    if (tolower(tools::file_ext(input$gsea_file)) %in% c("txt", "csv", "tsv")) {
      table_output("gsea_selected_table")
    } else image_or_file_ui(input$gsea_file, "950px")
  })
  output$gsea_selected_table <- render_csl_table({ req(input$gsea_file); safe_read_table(input$gsea_file, 5000) }, page_length = 50)
  output$all_file_ui <- renderUI({ req(identical(input$web_main_tabs %||% "", "Results Explorer")); progress_refresh(); file_select("all_file", "Result file", current_project()$data_dir, "\\.(txt|csv|tsv|html|png|pdf)$") })
  output$all_file_view <- renderUI({ req(input$all_file); image_or_file_ui(input$all_file) })

  output$log_file_ui <- renderUI({
    req(identical(input$web_main_tabs %||% "", "Logs"))
    progress_refresh()
    project <- current_project()
    entries <- log_entries(project)
    if (!NROW(entries)) return(div(class = "empty-box", paste("No stdout/stderr log files were found in", file.path(dirname(project$data_dir), "log"))))
    tool_choices <- c("All", sort(unique(entries$tool)))
    selected_tool <- selected_choice(input$log_tool_filter, tool_choices, tool_choices[[1]])
    tool_entries <- if (identical(selected_tool, "All")) entries else entries[entries$tool == selected_tool, , drop = FALSE]
    type_choices <- c("All", sort(unique(tool_entries$log_type)))
    selected_type <- selected_choice(input$log_type_filter, type_choices, type_choices[[1]])
    type_entries <- if (identical(selected_type, "All")) tool_entries else tool_entries[tool_entries$log_type == selected_type, , drop = FALSE]
    scope_type_choices <- c("All", sort(unique(type_entries$scope_type)))
    if (selected_tool %in% c("DESeq2", "GSEA")) scope_type_choices <- intersect(scope_type_choices, c("All", "Run"))
    selected_scope_type <- selected_choice(input$log_scope_type_filter, scope_type_choices, scope_type_choices[[1]])
    scope_type_entries <- if (identical(selected_scope_type, "All")) type_entries else type_entries[type_entries$scope_type == selected_scope_type, , drop = FALSE]
    scope_choices <- c("All", sort(unique(scope_type_entries$scope)))
    selected_scope <- selected_choice(input$log_scope_filter, scope_choices, scope_choices[[1]])
    choices <- log_file_choices(project, selected_tool, selected_type, selected_scope_type, selected_scope)
    scope_label <- if (identical(selected_scope_type, "Sample")) "Sample" else if (selected_tool %in% c("DESeq2", "GSEA")) "Comparison/run" else "Run"
    controls <- fluidRow(
      column(3, selectInput("log_tool_filter", "Tool", choices = tool_choices, selected = selected_tool, selectize = FALSE)),
      column(2, selectInput("log_type_filter", "Log type", choices = type_choices, selected = selected_type, selectize = FALSE)),
      column(2, selectInput("log_scope_type_filter", "Scope", choices = scope_type_choices, selected = selected_scope_type, selectize = FALSE)),
      column(2, selectInput("log_scope_filter", scope_label, choices = scope_choices, selected = selected_scope, selectize = FALSE)),
      column(3, if (length(choices)) selectInput("selected_log_file", "Log file", choices = choices, selected = selected_choice(input$selected_log_file, choices, choices[[1]]), selectize = FALSE) else div(class = "empty-box", "No logs match this filter."))
    )
    tagList(
      controls,
      radioButtons("log_view_mode", "View", choices = c("Tail" = "tail", "Head" = "head", "Full" = "full"), selected = "tail", inline = TRUE)
    )
  })

  output$selected_log_text <- renderText({
    req(input$selected_log_file)
    read_log_excerpt(input$selected_log_file, input$log_view_mode %||% "tail")
  })

  output$methods_project_table <- render_methods_table({
    project_methods_summary(current_project())
  }, page_length = 25, scroll_y = "320px")

  output$methods_tools_table <- render_methods_table({
    tool_reference_summary(current_project())
  }, page_length = 25, scroll_y = "560px")

  output$download_methods_project <- downloadHandler(
    filename = function() paste0(clean_name(current_project()$name, "project"), "_project_reference.csv"),
    content = function(file) {
      utils::write.csv(project_methods_summary(current_project()), file, row.names = FALSE, na = "")
    }
  )

  output$download_methods_tools <- downloadHandler(
    filename = function() paste0(clean_name(current_project()$name, "project"), "_tools_reference.csv"),
    content = function(file) {
      utils::write.csv(tool_reference_summary(current_project()), file, row.names = FALSE, na = "")
    }
  )
}

MAIN_SERVER <- server
server <- function(input, output, session) {
  observeEvent(
    session$clientData$url_search,
    {
      query_string <- isolate(session$clientData$url_search %||% "")
      if (!access_token_valid(query_string)) {
        session$close()
        return(invisible(NULL))
      }
      register_authorized_session(session)
      MAIN_SERVER(input, output, session)
    },
    once = TRUE,
    ignoreNULL = TRUE
  )
}

shinyApp(ui, server)
