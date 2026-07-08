library(shiny)

DT_AVAILABLE <- requireNamespace("DT", quietly = TRUE)
BASE64_AVAILABLE <- requireNamespace("base64enc", quietly = TRUE)

table_output <- function(output_id) {
  if (DT_AVAILABLE) DT::dataTableOutput(output_id) else tableOutput(output_id)
}

render_csl_table <- function(expr, page_length = 50, editable = FALSE, scroll_y = "520px", escape = TRUE) {
  if (DT_AVAILABLE) {
    DT::renderDataTable({
      df <- expr
      if (!NROW(df)) df <- data.frame()
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
          columnDefs = list(list(width = "118px", targets = "_all"))
        )
      )
      numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      if (length(numeric_cols)) {
        pvalue_cols <- numeric_cols[grepl("(^p$|pvalue|p\\.value|padj|fdr|qvalue|q\\.value)", numeric_cols, ignore.case = TRUE)]
        integer_cols <- numeric_cols[vapply(df[numeric_cols], function(x) {
          finite <- x[is.finite(x) & !is.na(x)]
          length(finite) == 0 || all(abs(finite - round(finite)) < 1e-8)
        }, logical(1))]
        decimal_cols <- setdiff(numeric_cols, c(integer_cols, pvalue_cols))
        if (length(integer_cols)) widget <- DT::formatRound(widget, columns = integer_cols, digits = 0)
        if (length(decimal_cols)) widget <- DT::formatRound(widget, columns = decimal_cols, digits = 2)
        if (length(pvalue_cols)) widget <- DT::formatSignif(widget, columns = pvalue_cols, digits = 4)
      }
      widget
    }, server = FALSE)
  } else {
    renderTable({
      df <- expr
      if (!NROW(df)) return(data.frame())
      utils::head(df, 50)
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
LAST_PROJECT_PATH <- file.path(APP_HOME, "last_project_id.txt")
PROGRESS_REFRESH_MS <- 30000
SAMPLE_PROGRESS_NICE_LIMIT <- 30
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
    genome_version = vals$genome_version %||% vals$reference_genome %||% "",
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
      genome_version = "mouse_gencodeM29",
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
  else if (dir.exists(path.expand(design_path))) {
    design_path <- file.path(normalizePath(path.expand(design_path), winslash = "/", mustWork = FALSE), "design_matrix.txt")
  } else if (basename(design_path) != "design_matrix.txt") {
    design_path <- file.path(normalizePath(dirname(path.expand(design_path)), winslash = "/", mustWork = FALSE), "design_matrix.txt")
  }
  design_path <- normalizePath(path.expand(design_path), winslash = "/", mustWork = FALSE)
  fastq_dir <- normalizePath(path.expand(input$new_fastq_dir %||% ""), winslash = "/", mustWork = FALSE)
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
    design_matrix_path = design_path,
    source_config = "",
    source = "new project"
  )
}

project_config_dir <- function(key) {
  file.path(SCRIPTS_DIR, "project_configs", analysis_key(key))
}

project_config_roots <- function() {
  unique(normalizePath(c(file.path(SCRIPTS_DIR, "project_configs"), file.path(CSL_ROOT, "project_configs")), winslash = "/", mustWork = FALSE))
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
  if (nzchar(data_dir) && basename(data_dir) == "data") return(dirname(data_dir))
  file.path(normalizePath(project$results_root %||% "~/csl_results", winslash = "/", mustWork = FALSE), project$name %||% project$label)
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
  dir.exists(path) && length(list.files(path, all.files = TRUE, no.. = TRUE)) > 0
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
    message = paste(if (ok) "Deleted project results:" else "Could not delete project results:", result_dir, sprintf("(removed %s old job record%s)", removed_jobs, ifelse(removed_jobs == 1, "", "s")))
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
    sprintf("genome = %s", deparse(project$genome)),
    sprintf("genome_version = %s", deparse(genome_reference_key(project))),
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

results_design_matrix_path <- function(project) {
  file.path(project$data_dir, "manifest", "design_matrix.txt")
}

write_design_matrix <- function(project, df, metadata_cols) {
  if (!"include" %in% names(df)) df$include <- TRUE
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
  if (m >= 0) return(sub("job_id:[[:space:]]*", "", regmatches(x, m)))
  m <- regexpr("Job ID:[[:space:]]*[0-9]+", x)
  if (m >= 0) return(sub("Job ID:[[:space:]]*", "", regmatches(x, m)))
  m <- regexpr("Submitted batch job[[:space:]]+[0-9]+", x)
  if (m >= 0) return(sub(".*Submitted batch job[[:space:]]+", "", regmatches(x, m)))
  ""
}

job_history <- function(project) {
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
    sq <- tryCatch(system2("squeue", c("-h", "-j", paste(ids, collapse = ","), "-o", "%A|%T|%M|%j"), stdout = TRUE, stderr = FALSE), error = function(e) character(0))
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
    sac <- tryCatch(system2("sacct", c("-n", "-P", "-j", paste(ids, collapse = ","), "--format=JobIDRaw,State,Elapsed,Start,End,JobName"), stdout = TRUE, stderr = FALSE), error = function(e) character(0))
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
  keep <- intersect(c("time", "step", "sample", "job_id", "slurm_state", "elapsed", "start_time", "end_time", "input_mode", "target", "stdout", "stderr"), names(jobs))
  jobs[, keep, drop = FALSE]
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

job_filter_choices_from_jobs <- function(jobs) {
  steps <- if (NROW(jobs) && "step" %in% names(jobs)) unique(as.character(jobs$step)) else character(0)
  steps <- steps[!is.na(steps) & nzchar(steps)]
  ordered <- pipeline_order()[pipeline_order() %in% steps]
  extra <- sort(setdiff(steps, ordered))
  c("All", ordered, extra)
}

filter_jobs_by_tool <- function(jobs, tool) {
  tool <- trimws(as.character(tool %||% "All"))
  if (!NROW(jobs) || identical(tool, "All") || !nzchar(tool) || !"step" %in% names(jobs)) return(jobs)
  normalize_tool <- function(x) {
    x <- tolower(trimws(as.character(x %||% "")))
    x <- gsub("\\(optional\\)", "", x)
    gsub("[^a-z0-9]+", "", x)
  }
  step_norm <- normalize_tool(jobs$step)
  tool_norm <- normalize_tool(tool)
  jobs[step_norm == tool_norm | startsWith(step_norm, tool_norm) | startsWith(tool_norm, step_norm), , drop = FALSE]
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
      gencode_label(project),
      genome_reference_key(project),
      if (isTRUE(project$paired_end)) "Paired-end" else "Single-end",
      if (file.exists(project$design_matrix_path)) "Provided or created in app" else "Not created yet"
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

r_package_version <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) return("not detected")
  as.character(utils::packageVersion(pkg))
}

command_version <- function(command, args = "--version", pattern = NULL) {
  bin <- Sys.which(command)
  if (!nzchar(bin)) return("not detected in app environment")
  out <- tryCatch(system2(bin, args, stdout = TRUE, stderr = TRUE), error = function(e) character(0))
  out <- out[nzchar(out)]
  if (!length(out)) return("detected")
  value <- trimws(out[1])
  if (!is.null(pattern)) {
    hit <- regmatches(value, regexpr(pattern, value, perl = TRUE))
    if (length(hit) && nzchar(hit)) value <- hit
  }
  value
}

tool_reference_summary <- function(project) {
  rows <- list(
    c("Reference", "Genome annotation", gencode_label(project), paste0(genome_species(project), " / ", genome_reference_key(project)), "STAR, featureCounts, DESeq2, RSEM, Kallisto"),
    c("Tool", "FastQC", command_version("fastqc", "--version"), "Read quality control", "Raw or trimmed FASTQ"),
    c("Tool", "cutadapt", command_version("cutadapt", "--version"), "Adapter trimming", "Raw FASTQ"),
    c("Tool", "STAR", command_version("STAR", "--version"), "Spliced alignment", gencode_label(project)),
    c("Tool", "featureCounts / Subread", command_version("featureCounts", "-v"), "Gene-level counting", gencode_label(project)),
    c("Tool", "DESeq2", r_package_version("DESeq2"), "Differential expression", "featureCounts count_matrix.txt"),
    c("Tool", "R-native GSEA / fgsea", paste0("R ", getRversion(), "; fgsea ", r_package_version("fgsea"), "; ggplot2 ", r_package_version("ggplot2")), "Pathway analysis", "Selected Enrichr/MSigDB-style gene set database"),
    c("Tool", "RSEM", command_version("rsem-calculate-expression", "--version"), "Optional gene/transcript quantification", gencode_label(project)),
    c("Tool", "Kallisto", command_version("kallisto", "version"), "Optional transcript abundance quantification", gencode_label(project)),
    c("Tool", "RSeQC", "not required for core run; strand BED generated with reference", "Optional strand/QC support", gencode_label(project))
  )
  out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  colnames(out) <- c("Type", "Name", "Version/reference", "Used for", "Input/reference")
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
    "STAR" = paste0("Reads were aligned with STAR using ", gencode_label(project), ".", ref_text, mode_text),
    "featureCounts" = paste0("Gene-level counts were quantified with featureCounts using the selected GTF annotation.", ref_text, mode_text),
    "DESeq2" = paste0("Differential expression analysis was performed with DESeq2.", mode_text),
    "GSEA" = paste0("Gene set enrichment analysis was performed with the CodeSpringLab R-native GSEA runner using signal-to-noise ranking and gene-set permutations.", ref_text, mode_text),
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
  steps <- pipeline_order()[pipeline_order() %in% unique(c(completed, run_steps))]
  if (!length(steps)) steps <- completed
  lines <- c(
    paste0("Project: ", project$label),
    paste0("Analysis: ", project$analysis),
    paste0("Reference genome: ", gencode_label(project), " (", genome_reference_key(project), ")."),
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
  m <- regexec("^(output|error)_([^.]*)\\.txt$", base)
  hit <- regmatches(base, m)[[1]]
  if (length(hit) == 3) {
    log_type <- if (identical(hit[2], "output")) "output" else "error"
    tool <- sub("_[0-9]{8}_[0-9]{6}$", "", hit[3])
    return(paste(pretty_tool_name(tool), log_type, base))
  }
  paste(fallback, base)
}

log_file_choices <- function(project, tool = "All", log_type = "All") {
  vals <- character(0)
  normalize_log_tool <- function(x) {
    x <- tolower(trimws(as.character(x %||% "")))
    x <- gsub("\\(optional\\)", "", x)
    gsub("[^a-z0-9]+", "", x)
  }
  add_log_choice <- function(label, path) {
    path <- as.character(path %||% "")
    path <- path[!is.na(path) & nzchar(path)]
    if (!length(path)) return(invisible(NULL))
    for (one_path in path) {
      one_label <- label
      if (!nzchar(one_label %||% "")) one_label <- log_label_from_path(one_path)
      vals[[one_label]] <<- one_path
    }
    invisible(NULL)
  }

  project_log_dir <- file.path(dirname(project$data_dir), "log")
  if (dir.exists(project_log_dir)) {
    files <- list.files(project_log_dir, pattern = "^(output|error)_.*\\.txt$", full.names = TRUE)
    for (one_path in files) add_log_choice(log_label_from_path(one_path), one_path)
  }

  jobs <- job_history(project)
  if (NROW(jobs)) {
    for (i in seq_len(NROW(jobs))) {
      step <- jobs$step[i] %||% "Job"
      job_id <- jobs$job_id[i] %||% ""
      suffix <- if (nzchar(job_id)) paste0(" job ", job_id) else trimws(jobs$time[i] %||% "")
      if ("stdout" %in% names(jobs) && nzchar(jobs$stdout[i] %||% "")) add_log_choice(trimws(paste(step, "output", suffix)), jobs$stdout[i])
      if ("stderr" %in% names(jobs) && nzchar(jobs$stderr[i] %||% "")) add_log_choice(trimws(paste(step, "error", suffix)), jobs$stderr[i])
    }
  }

  keep <- !is.na(vals) & nzchar(vals) & file.exists(vals)
  vals <- vals[keep]
  vals <- vals[!duplicated(vals)]
  if (length(vals) && !identical(tool %||% "All", "All")) {
    label_tools <- sub("\\s+(output|error).*$", "", names(vals), ignore.case = TRUE)
    vals <- vals[normalize_log_tool(label_tools) == normalize_log_tool(tool)]
  }
  if (length(vals) && !identical(log_type %||% "All", "All")) {
    vals <- vals[grepl(paste0("\\b", log_type, "\\b"), names(vals), ignore.case = TRUE)]
  }
  vals
}

log_tool_choices <- function(project) {
  vals <- log_file_choices(project)
  if (!length(vals)) return("All")
  tools <- sub("\\s+(output|error).*$", "", names(vals), ignore.case = TRUE)
  c("All", sort(unique(tools[nzchar(tools)])))
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

server_browser_choices <- function(path, mode = "dir") {
  path <- path.expand(trimws(first_scalar_string(path, path.expand("~"))))
  if (!nzchar(path) || !dir.exists(path)) path <- path.expand("~")
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  dirs <- list.dirs(path, recursive = FALSE, full.names = TRUE)
  dirs <- dirs[dir.exists(dirs)]
  dirs <- dirs[!grepl("^\\.", basename(dirs))]
  dirs <- dirs[basename(dirs) != "__pycache__"]
  dirs <- sort(dirs)
  choices <- stats::setNames(character(0), character(0))
  if (length(dirs)) {
    choices <- stats::setNames(dirs, paste0(basename(dirs), "/"))
  }
  if (!length(choices)) {
    fallback <- first_scalar_string(path, path.expand("~"))
    choices <- stats::setNames(fallback, "(empty folder)")
  }
  choices
}

browser_start_path <- function(value, mode = "dir") {
  value <- path.expand(trimws(first_scalar_string(value, "")))
  if (nzchar(value) && file.exists(value) && !dir.exists(value)) return(dirname(value))
  if (nzchar(value) && dir.exists(value)) return(value)
  if (nzchar(value) && dir.exists(dirname(value))) return(dirname(value))
  path.expand("~")
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
  ifelse(status %in% c("Complete"), "Complete", ifelse(status %in% c("Active"), "Active", "Not started"))
}

project_status <- function(project, jobs = NULL, progress = NULL, active_states = NULL) {
  data_dir <- project$data_dir
  design <- project$design_matrix_path
  if (is.null(jobs)) jobs <- job_history(project)
  if (is.null(active_states)) active_states <- active_job_state_map_from_jobs(jobs)
  feature_count_files <- count_files(file.path(data_dir, "featurecounts"), "_counts\\.txt$")
  feature_matrix_exists <- file.exists(file.path(data_dir, "counts", "count_matrix.txt"))
  raw <- data.frame(
    step = c("Design matrix", "FastQC", "Cutadapt", "STAR", "featureCounts", "DESeq2", "GSEA", "RSEM (optional)", "Kallisto (optional)"),
    status = c(
      if (file.exists(design)) "Complete" else "Not started",
      if (count_files(file.path(data_dir, "fastqc"), "\\.html$") + count_files(file.path(data_dir, "fastqc_cutadapt"), "\\.html$") > 0) "Complete" else "Not started",
      if (count_files(file.path(data_dir, "cutadapt"), fastq_suffix_regex) > 0) "Complete" else "Not started",
      if (count_files(file.path(data_dir, "star"), "Aligned\\.sortedByCoord\\.out\\.bam$") > 0) "Complete" else "Not started",
      if (feature_matrix_exists) "Complete" else if (feature_count_files > 0) "Active" else "Not started",
      if (count_files(file.path(data_dir, "deseq2"), "DEG|normalized") > 0) "Complete" else "Not started",
      if (count_files(file.path(data_dir, "gseapy"), "\\.(csv|txt|png|pdf)$") > 0) "Complete" else "Not started",
      if (count_files(file.path(data_dir, "rsem"), "\\.genes\\.results$") > 0) "Complete" else "Not started",
      if (count_files(file.path(data_dir, "kallisto"), "abundance\\.tsv$") > 0) "Complete" else "Not started"
    ),
    path = c(
      design,
      file.path(data_dir, "fastqc"),
      file.path(data_dir, "cutadapt"),
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
    pieces <- character(0)
    if (length(running)) pieces <- c(pieces, paste("Running:", paste(running, collapse = "; ")))
    if (length(complete)) pieces <- c(pieces, paste("Complete:", paste(complete, collapse = "; ")))
    raw$detail[raw$step == step] <- paste(pieces, collapse = " | ")
  }
  if (is.null(progress)) progress <- tryCatch(sample_progress(project, active_states, data.frame(), jobs = jobs)$table, error = function(e) data.frame())
  if (NROW(progress)) {
    for (step in c("FastQC", "Cutadapt", "STAR", "featureCounts", "RSEM (optional)", "Kallisto (optional)")) {
      hit <- progress[progress$step == step, , drop = FALSE]
      if (!NROW(hit)) next
      if (all(hit$status %in% c("Completed", "Optional, not run")) && any(hit$status == "Completed")) {
        raw$status[raw$step == step] <- "Complete"
      } else if (any(hit$status %in% c("Running", "Running, no growth yet", "Waiting"))) {
        raw$status[raw$step == step] <- "Active"
      } else if (any(hit$status == "Possibly incomplete")) {
        raw$status[raw$step == step] <- "Active"
      } else {
        raw$status[raw$step == step] <- "Not started"
      }
    }
  }
  active <- names(active_states)
  raw$status[raw$step %in% active & raw$status != "Complete"] <- "Active"
  if (!feature_matrix_exists && feature_count_files > 0) raw$status[raw$step == "featureCounts"] <- "Active"
  raw$status <- normalize_pipeline_status(raw$status)
  raw
}

status_rank <- function(status) {
  match(status, c("Active", "Complete", "Not started"), nomatch = 99)
}

pipeline_order <- function() {
  c("Design matrix", "FastQC", "Cutadapt", "STAR", "featureCounts", "DESeq2", "GSEA", "RSEM (optional)", "Kallisto (optional)")
}

step_order <- function(step) {
  match(step, pipeline_order(), nomatch = length(pipeline_order()) + seq_along(step))
}

status_label <- function(status) {
  ifelse(identical(status, "Active"), "In progress", status)
}

status_pill <- function(status) {
  cls <- switch(status, "Active" = "active", "Complete" = "complete", "not-started")
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

sample_step_targets <- function(project, sample, step) {
  data_dir <- project$data_dir
  if (identical(step, "FastQC")) {
    expected_for <- function(trimmed) {
      pairs <- sample_fastq_pairs(project, trimmed)
      hit <- pairs[pairs$sample == sample, , drop = FALSE]
      if (!NROW(hit)) return(character(0))
      reads <- unique(c(hit$r1[1], if (project$paired_end) hit$r2[1] else character(0)))
      outdir <- file.path(data_dir, if (trimmed) "fastqc_cutadapt" else "fastqc")
      file.path(outdir, sub(fastq_suffix_regex, "_fastqc.html", basename(reads), ignore.case = TRUE))
    }
    raw <- expected_for(FALSE)
    trimmed <- expected_for(TRUE)
    if (length(raw) && all(file.exists(raw))) return(raw)
    if (length(trimmed) && all(file.exists(trimmed))) return(trimmed)
    return(if (length(raw)) raw else trimmed)
  }
  switch(step,
    "Cutadapt" = {
      cutadapt_dir <- file.path(data_dir, "cutadapt")
      pairs <- sample_fastq_pairs(project, FALSE)
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

sample_progress <- function(project, active_states = active_job_state_map(project), previous_cache = data.frame(), jobs = NULL) {
  design <- safe_read_table(project$design_matrix_path)
  if (!NROW(design) || !"sample" %in% names(design)) return(list(table = data.frame(), cache = previous_cache))
  sample_steps <- c("FastQC", "Cutadapt", "STAR", "featureCounts", "RSEM (optional)", "Kallisto (optional)")
  if (is.null(jobs)) jobs <- job_history(project)
  active_job_states <- c("PENDING", "CONFIGURING", "COMPLETING", "RUNNING", "SUSPENDED", "Submitted")
  completed_job_states <- c("COMPLETED", "COMPLETED+", "CD", "Finished or not in queue")
  active_jobs <- if (NROW(jobs) && "slurm_state" %in% names(jobs)) jobs[jobs$slurm_state %in% active_job_states, , drop = FALSE] else data.frame()
  rows <- list()
  cache_rows <- list()
  for (sample in as.character(design$sample)) {
    for (step in sample_steps) {
      targets <- sample_step_targets(project, sample, step)
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
      active <- NROW(active_hit) > 0 || step %in% names(active_states)
      all_step_hits <- if (NROW(jobs)) {
        step_hits <- jobs[jobs$step == step, , drop = FALSE]
        if ("sample" %in% names(step_hits) && NROW(step_hits)) {
          sample_hits <- step_hits[nzchar(step_hits$sample) & step_hits$sample == sample, , drop = FALSE]
          if (NROW(sample_hits)) sample_hits else step_hits
        } else step_hits
      } else data.frame()
      latest_hit <- if (NROW(active_hit)) tail(active_hit, 1) else if (NROW(all_step_hits)) tail(all_step_hits, 1) else data.frame()
      slurm_state <- if (NROW(latest_hit) && "slurm_state" %in% names(latest_hit)) latest_hit$slurm_state[1] else if (active && step %in% names(active_states)) active_states[[step]] else ""
      elapsed <- if (NROW(latest_hit) && "elapsed" %in% names(latest_hit)) latest_hit$elapsed[1] else ""
      min_size <- minimum_expected_bytes(step)
      complete_outputs <- length(sizes) > 0 && all(sizes >= min_size)
      growing <- active && has_previous && size > previous_size
      optional <- step %in% c("RSEM (optional)", "Kallisto (optional)")
      slurm_running <- active && slurm_state %in% c("RUNNING", "COMPLETING")
      slurm_waiting <- active && slurm_state %in% c("PENDING", "CONFIGURING", "Submitted")
      slurm_complete <- slurm_state %in% completed_job_states
      status <- if ((complete_outputs || (slurm_complete && size > 0)) && !growing && !slurm_running) {
        "Completed"
      } else if (slurm_running) {
        "Running"
      } else if (active && growing) {
        "Running"
      } else if (slurm_waiting) {
        "Waiting"
      } else if (active && size > 0) {
        "Running, no growth yet"
      } else if (active) {
        "Waiting"
      } else if (size > 0 && size < min_size) {
        "Possibly incomplete"
      } else if (optional) {
        "Optional, not run"
      } else {
        "Not started"
      }
      note <- if (status == "Possibly incomplete") {
        paste0("Output exists but is smaller than expected (<", min_size, " bytes).")
      } else if (identical(status, "Running, no growth yet")) {
        "File exists but size did not increase since the last refresh."
      } else if (identical(status, "Running") && growing) {
        "Output file size increased since the last refresh."
      } else if (identical(status, "Running") && size == 0) {
        "SLURM reports this sample is running; output has not been written yet."
      } else {
        ""
      }
      display_status <- status
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
  steps <- c("FastQC", "Cutadapt", "STAR", "featureCounts", "RSEM (optional)", "Kallisto (optional)")
  samples <- unique(progress_df$sample)
  if (length(samples) > SAMPLE_PROGRESS_NICE_LIMIT) {
    return(div(
      class = "sample-matrix-wrap",
      div(class = "adaptive-table-note", paste("Showing paginated sample progress because this project has", length(samples), "samples.")),
      table_output("sample_progress_detail_table")
    ))
  }
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
  out[order(out$Sample, step_order(out$Step)), , drop = FALSE]
}

tool_progress_output_id <- function(step) {
  paste0("tool_progress_", tolower(gsub("[^A-Za-z0-9]+", "_", step)))
}

sample_progress_step_table <- function(progress_df, step) {
  if (!NROW(progress_df) || !"step" %in% names(progress_df)) return(NULL)
  hit <- progress_df[progress_df$step == step, , drop = FALSE]
  if (!NROW(hit)) return(NULL)
  hit <- hit[order(hit$sample), , drop = FALSE]
  time_running <- if ("time_running" %in% names(hit)) as.character(hit$time_running) else rep("", NROW(hit))
  data.frame(
    Sample = hit$sample,
    Status = hit$display_status,
    `Time running` = ifelse(nzchar(time_running), time_running, "-"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

sample_progress_step_ui <- function(progress_df, step) {
  table <- sample_progress_step_table(progress_df, step)
  if (!NROW(table)) return(NULL)
  if (NROW(table) <= SAMPLE_PROGRESS_NICE_LIMIT) {
    hit <- progress_df[progress_df$step == step, , drop = FALSE]
    hit <- hit[order(hit$sample), , drop = FALSE]
    return(div(
      class = "tool-progress-wrap",
      div(class = "tool-progress-title", "Sample progress"),
      tags$table(
        class = "tool-progress-table",
        tags$thead(tags$tr(tags$th("Sample"), tags$th("Status"), tags$th("Time running"))),
        tags$tbody(lapply(seq_len(NROW(hit)), function(i) {
          title <- paste0(
            "Status: ", hit$status[i],
            "\nSLURM: ", if (nzchar(hit$slurm_state[i])) hit$slurm_state[i] else "-",
            "\nBytes: ", hit$output_bytes[i],
            "\nPath: ", hit$target[i],
            if (nzchar(hit$note[i])) paste0("\nNote: ", hit$note[i]) else ""
          )
          time_running <- if ("time_running" %in% names(hit) && nzchar(hit$time_running[i])) hit$time_running[i] else "-"
          tags$tr(
            tags$td(class = "sample-name", hit$sample[i]),
            tags$td(tags$span(class = status_class(hit$status[i]), title = title, hit$display_status[i])),
            tags$td(time_running)
          )
        }))
      )
    ))
  }
  div(
    class = "tool-progress-wrap",
    div(class = "tool-progress-title", paste("Sample progress - paginated", NROW(table), "samples")),
    table_output(tool_progress_output_id(step))
  )
}

optimistic_step_progress <- function(project, step, input_mode = "") {
  design <- safe_read_table(project$design_matrix_path)
  if (!NROW(design) || !"sample" %in% names(design)) return(data.frame())
  samples <- as.character(design$sample)
  samples <- samples[nzchar(samples)]
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
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

optimistic_status <- function(current_status, step, input_mode = "") {
  if (!NROW(current_status)) {
    current_status <- data.frame(
      step = pipeline_order(),
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

submit_sbatch <- function(project, step, script, args, log_name, input_mode = "", sample = "", target = "", reference = "") {
  log_dir <- file.path(dirname(project$data_dir), "log")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  stdout <- file.path(log_dir, paste0("output_", log_name, "_", stamp, ".txt"))
  stderr <- file.path(log_dir, paste0("error_", log_name, "_", stamp, ".txt"))
  job_name <- job_name_for(project, step, sample)
  cmd <- c("sbatch", "-J", job_name, "-e", stderr, "-o", stdout, script, args)
  if (Sys.which("sbatch") == "") {
    msg <- "sbatch was not found. Run on the server to submit jobs."
    save_job(project, step, cmd, paste(c(msg, if (nzchar(sample)) paste("sample:", sample), if (nzchar(target)) paste("target:", target)), collapse = "\n"))
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
  manifest <- append_run_manifest(project, step, sample, cmd, target, input_mode, reference, job_id)
  save_job(project, step, cmd, paste(c(out_text, if (nzchar(job_id)) paste("job_id:", job_id), if (nzchar(sample)) paste("sample:", sample), if (nzchar(target)) paste("target:", target), if (nzchar(input_mode)) paste("input_mode:", input_mode), paste("stdout:", stdout), paste("stderr:", stderr), paste("manifest:", manifest)), collapse = "\n"))
  submit_screen_message(step, sample, job_id, input_mode)
}

submit_sbatch_wrap <- function(project, step, shell_command, log_name, input_mode = "", sample = "", target = "", reference = "", dependency_ids = character(0)) {
  log_dir <- file.path(dirname(project$data_dir), "log")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  stdout <- file.path(log_dir, paste0("output_", log_name, "_", stamp, ".txt"))
  stderr <- file.path(log_dir, paste0("error_", log_name, "_", stamp, ".txt"))
  job_name <- job_name_for(project, step, sample)
  dep <- dependency_ids[nzchar(dependency_ids)]
  cmd <- c("sbatch", "-J", job_name, "-e", stderr, "-o", stdout)
  if (length(dep)) cmd <- c(cmd, paste0("--dependency=afterok:", paste(dep, collapse = ":")))
  cmd <- c(cmd, "--wrap", shell_command)
  if (Sys.which("sbatch") == "") {
    msg <- "sbatch was not found. Run on the server to submit jobs."
    save_job(project, step, cmd, paste(c(msg, if (nzchar(target)) paste("target:", target)), collapse = "\n"))
    return(msg)
  }
  out <- tryCatch(system2(cmd[1], cmd[-1], stdout = TRUE, stderr = TRUE), error = function(e) conditionMessage(e))
  out_text <- paste(out, collapse = "\n")
  job_id <- parse_sbatch_job_id(out_text)
  manifest <- append_run_manifest(project, step, sample, cmd, target, input_mode, reference, job_id)
  save_job(project, step, cmd, paste(c(out_text, if (nzchar(job_id)) paste("job_id:", job_id), if (nzchar(sample)) paste("sample:", sample), if (nzchar(target)) paste("target:", target), if (nzchar(input_mode)) paste("input_mode:", input_mode), paste("stdout:", stdout), paste("stderr:", stderr), paste("manifest:", manifest)), collapse = "\n"))
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
    "summary <- data.frame(sample=samples, total_counts=vapply(parts, function(x) sum(x$value, na.rm=TRUE), numeric(1)), stringsAsFactors=FALSE)",
    "write.table(summary, file=file.path(counts_dir, 'featurecounts_summary.txt'), sep='\\t', row.names=FALSE, quote=FALSE)"
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
  summary <- data.frame(sample = samples, total_counts = vapply(parts, function(x) sum(x$value, na.rm = TRUE), numeric(1)), stringsAsFactors = FALSE)
  utils::write.table(summary, file = file.path(counts_dir, "featurecounts_summary.txt"), sep = "\t", row.names = FALSE, quote = FALSE)
  file.path(counts_dir, "count_matrix.txt")
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
  script <- file.path(SCRIPTS_DIR, "FastQC", "qsub_fastqc.sh")
  input_mode <- if (trimmed) "trimmed reads" else "raw reads"
  commands <- character(0)
  for (i in seq_len(NROW(pairs))) {
    reads <- unique(c(pairs$r1[i], if (project$paired_end) pairs$r2[i] else character(0)))
    for (read in reads[nzchar(reads)]) {
      target <- file.path(outdir, sub(fastq_suffix_regex, "_fastqc.html", basename(read), ignore.case = TRUE))
      commands <- c(commands, submit_sbatch(project, "FastQC", script, c(read, outdir, project$name), "fastQC", input_mode, sample = pairs$sample[i], target = target))
    }
  }
  paste(commands, collapse = "\n")
}

submit_cutadapt_jobs <- function(project, adapter1, adapter2, min_length) {
  outdir <- file.path(project$data_dir, "cutadapt")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  pairs <- sample_fastq_pairs(project, FALSE)
  msg <- missing_read_message(project, pairs)
  if (nzchar(msg)) return(msg)
  script <- file.path(SCRIPTS_DIR, if (project$paired_end) "cutadapt_PE/qsub_cutadapt_PE.sh" else "cutadapt_SE/qsub_cutadapt_SE.sh")
  input_mode <- "raw reads"
  paste(apply(pairs, 1, function(row) {
    trimmed1 <- file.path(outdir, basename(row[["r1"]]))
    trimmed2 <- if (project$paired_end) file.path(outdir, basename(row[["r2"]])) else trimmed1
    read2 <- if (project$paired_end) row[["r2"]] else row[["r1"]]
    submit_sbatch(project, "Cutadapt", script, c(min_length, adapter1, adapter2, trimmed1, trimmed2, row[["r1"]], read2, project$name), "cutadapt", input_mode, sample = row[["sample"]], target = trimmed1)
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
    target <- file.path(sample_dir, paste0(row[["sample"]], "Aligned.sortedByCoord.out.bam"))
    submit_sbatch(project, "STAR", script, c(out_prefix, res$star_index, row[["r1"]], row[["r2"]], project$name), "star", input_mode, sample = row[["sample"]], target = target, reference = res$label)
  }), collapse = "\n")
}

submit_kallisto_jobs <- function(project, trimmed = FALSE) {
  res <- genome_resources(project)
  outdir <- file.path(project$data_dir, "kallisto")
  counts_dir <- file.path(project$data_dir, "counts")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  dir.create(counts_dir, recursive = TRUE, showWarnings = FALSE)
  pairs <- sample_fastq_pairs(project, trimmed)
  msg <- missing_read_message(project, pairs)
  if (nzchar(msg)) return(msg)
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
  paste(c(messages, matrix_msg), collapse = "\n")
}

submit_rsem_jobs <- function(project, feature = "gene_id") {
  res <- genome_resources(project)
  design <- safe_read_table(project$design_matrix_path)
  if (!NROW(design) || !"sample" %in% names(design)) return("No samples found in design matrix.")
  outdir <- file.path(project$data_dir, "rsem")
  counts_dir <- file.path(project$data_dir, "counts")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  dir.create(counts_dir, recursive = TRUE, showWarnings = FALSE)
  script <- file.path(SCRIPTS_DIR, "RSEM", if (project$paired_end) "qsub_RSEM_PE.sh" else "qsub_RSEM_SE.sh")
  messages <- vapply(as.character(design$sample), function(sample) {
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
  paste(c(messages, matrix_msg), collapse = "\n")
}

submit_featurecounts_jobs <- function(project, feature = "gene_id") {
  res <- genome_resources(project)
  design <- safe_read_table(project$design_matrix_path)
  if (!NROW(design) || !"sample" %in% names(design)) return("No samples found in design matrix.")
  outdir <- file.path(project$data_dir, "featurecounts")
  counts_dir <- file.path(project$data_dir, "counts")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  dir.create(counts_dir, recursive = TRUE, showWarnings = FALSE)
  script <- file.path(SCRIPTS_DIR, "featureCounts", if (project$paired_end) "qsub_featurecounts_PE.sh" else "qsub_featurecounts_SE.sh")
  messages <- vapply(as.character(design$sample), function(sample) {
    sample_dir <- file.path(outdir, sample)
    dir.create(sample_dir, recursive = TRUE, showWarnings = FALSE)
    bam <- file.path(project$data_dir, "star", sample, paste0(sample, "Aligned.sortedByCoord.out.bam"))
    count_prefix <- file.path(sample_dir, sample)
    target <- paste0(count_prefix, "_counts.txt")
    submit_sbatch(project, "featureCounts", script, c(bam, res$gtf, feature, count_prefix, res$strand_bed, project$name), "featurecounts", paste("STAR BAM; feature", feature), sample = sample, target = target, reference = res$gtf)
  }, character(1))
  ids <- vapply(messages, parse_sbatch_job_id, character(1))
  matrix_msg <- submit_featurecounts_matrix_job(project, feature, dependency_ids = ids)
  paste(c(messages, matrix_msg), collapse = "\n")
}

submit_featurecounts_matrix_job <- function(project, feature = "gene_id", dependency_ids = character(0)) {
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

featurecounts_outputs_ready <- function(project) {
  files <- expected_featurecounts_files(project)
  length(files) > 0 && all(file.exists(files)) && all(vapply(files, file_size_for, numeric(1)) > 0)
}

featurecounts_matrix_job_active <- function(jobs, matrix_path) {
  if (!NROW(jobs) || !"target" %in% names(jobs) || !"slurm_state" %in% names(jobs)) return(FALSE)
  active_states <- c("PENDING", "CONFIGURING", "COMPLETING", "RUNNING", "SUSPENDED", "Submitted")
  hit <- jobs[jobs$step == "featureCounts" & jobs$target == matrix_path, , drop = FALSE]
  NROW(hit) > 0 && any(hit$slurm_state %in% active_states)
}

submit_deseq2_job <- function(project, compare_col, reference, comparison, redundant = "NoRedundant") {
  outdir <- file.path(project$data_dir, "deseq2")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  script <- file.path(SCRIPTS_DIR, "DESeq2", "qsub_deseq2.sh")
  rscript <- file.path(SCRIPTS_DIR, "DESeq2", "DESeq2.R")
  count_matrix <- file.path(project$data_dir, "counts", "count_matrix.txt")
  design_matrix <- deseq_design_for_column(project, compare_col)
  submit_sbatch(project, "DESeq2", script, c(rscript, count_matrix, design_matrix, outdir, reference, comparison, redundant, project$name), "deseq2", format_comparison_label(compare_col, comparison, reference))
}

write_gseapy_script <- function(project) {
  log_dir <- file.path(dirname(project$data_dir), "log")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  script <- file.path(log_dir, "run_gseapy_pathway.py")
  lines <- c(
    "import os",
    "import sys",
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
    "gs, gs_res, pathways, terms, project_name_out = csl.gseapy_RunPathway(",
    "    geneset, genome, feature, design_dir, deseq_dir, outpath_pathway, refcond, compared",
    ")",
    "try:",
    "    csl.gseapy_DotPlot(outpath_pathway, pathways, geneset)",
    "except Exception as exc:",
    "    print('WARNING: GSEA completed, but dot plot creation failed: {}'.format(exc))",
    "print('GSEA completed for {} vs {} using {}'.format(compared, refcond, geneset))",
    "print('Results:', outpath_pathway)"
  )
  writeLines(lines, script)
  script
}

write_gseapy_shell_script <- function(project, python_script, script_dir, project_name, results_root, geneset, genome,
                                      feature, design_dir, deseq_dir, outpath_pathway, reference, comparison) {
  log_dir <- file.path(dirname(project$data_dir), "log")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  script <- file.path(log_dir, "run_gseapy_pathway.sh")
  lines <- c(
    "#!/usr/bin/env bash",
    "set -euo pipefail",
    "module load EBModules >/dev/null 2>&1 || true",
    "module load Python/3.7.4-GCCcore-8.3.0 >/dev/null 2>&1 || true",
    "choose_python() {",
    "  local candidates=(\"${CSL_PYTHON_BIN:-}\" \"${PYTHON_BIN:-}\" \"/grid/it/modules/bsr/software/Python/3.7.4-GCCcore-8.3.0/bin/python\" \"python3\" \"python\")",
    "  local py",
    "  for py in \"${candidates[@]}\"; do",
    "    [ -n \"$py\" ] || continue",
    "    if ! command -v \"$py\" >/dev/null 2>&1; then continue; fi",
    "    if \"$py\" - <<'PY' >/dev/null 2>&1",
    "import gseapy",
    "import pandas",
    "import matplotlib",
    "PY",
    "    then",
    "      command -v \"$py\"",
    "      return 0",
    "    fi",
    "  done",
    "  return 1",
    "}",
    "PYTHON_EXE=$(choose_python || true)",
    "if [ -z \"${PYTHON_EXE:-}\" ]; then",
    "  echo 'ERROR: Could not find a Python executable with gseapy, pandas, and matplotlib available.' >&2",
    "  echo 'Set CSL_PYTHON_BIN or PYTHON_BIN before launching CodeSpringWeb, or install gseapy in the server Python environment.' >&2",
    "  exit 1",
    "fi",
    "echo \"Using Python: $PYTHON_EXE\"",
    "\"$PYTHON_EXE\" - <<'PY'",
    "import sys, gseapy, pandas, matplotlib",
    "print('Python version:', sys.version.replace('\\n', ' '))",
    "print('gseapy version:', getattr(gseapy, '__version__', 'unknown'))",
    "PY",
    paste(
      "\"$PYTHON_EXE\"",
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
    )
  )
  writeLines(lines, script)
  Sys.chmod(script, mode = "0755")
  script
}

gsea_native_runner_path <- function() {
  candidates <- unique(c(
    file.path(getwd(), "gsea_native_runner.R"),
    file.path(dirname(normalizePath("app.R", winslash = "/", mustWork = FALSE)), "gsea_native_runner.R"),
    file.path(path.expand("~/CodeSpringWeb"), "gsea_native_runner.R")
  ))
  hit <- candidates[file.exists(candidates)]
  if (length(hit)) return(hit[[1]])
  candidates[[1]]
}

write_gsea_r_shell_script <- function(project, runner_script, script_dir, project_name, results_root, geneset, genome,
                                      compare_col, design_dir, deseq_dir, outpath_pathway, reference, comparison,
                                      gtf_path, ortholog_path) {
  log_dir <- file.path(dirname(project$data_dir), "log")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  script <- file.path(log_dir, "run_gsea_r_pathway.sh")
  lines <- c(
    "#!/usr/bin/env bash",
    "set -euo pipefail",
    "module load EBModules >/dev/null 2>&1 || true",
    "module load R >/dev/null 2>&1 || true",
    "if ! command -v Rscript >/dev/null 2>&1; then",
    "  echo 'ERROR: Rscript was not found in this server environment.' >&2",
    "  exit 1",
    "fi",
    "echo \"Using Rscript: $(command -v Rscript)\"",
    "Rscript --version || true",
    "Rscript -e 'cat(\"R library paths:\\n\", paste(.libPaths(), collapse=\"\\n\"), \"\\n\", sep=\"\"); cat(\"fgsea available: \", requireNamespace(\"fgsea\", quietly=TRUE), \"\\n\", sep=\"\"); if (!requireNamespace(\"fgsea\", quietly=TRUE)) stop(\"fgsea is not available to this SLURM job. Re-run run_codespringweb.sh or check R_LIBS_USER.\")'",
    paste(
      "Rscript",
      shQuote(runner_script),
      shQuote(script_dir),
      shQuote(project_name),
      shQuote(results_root),
      shQuote(geneset),
      shQuote(genome),
      shQuote(compare_col),
      shQuote(design_dir),
      shQuote(deseq_dir),
      shQuote(outpath_pathway),
      shQuote(reference),
      shQuote(comparison),
      shQuote(gtf_path),
      shQuote(ortholog_path)
    )
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
  if (!file.exists(normalized_file)) {
    stop("Expected DESeq2 normalized counts file was not found: ", normalized_file)
  }
  outpath_pathway <- paste0(file.path(project$data_dir, "gseapy", paste0(comparison, "_vs_", reference)), "/")
  dir.create(outpath_pathway, recursive = TRUE, showWarnings = FALSE)
  design_matrix <- deseq_design_for_column(project, compare_col)
  design_dir <- dirname(design_matrix)
  runner_script <- gsea_native_runner_path()
  if (!file.exists(runner_script)) {
    stop("The R-native GSEA runner was not found: ", runner_script)
  }
  results_root <- project$results_root %||% dirname(dirname(project$data_dir))
  res <- genome_resources(project)
  ortholog_path <- file.path(SCRIPTS_DIR, "reference", "mouse_human_orthologs_MGI.tsv")
  shell_script <- write_gsea_r_shell_script(
    project = project,
    runner_script = runner_script,
    script_dir = SCRIPTS_DIR,
    project_name = project$name,
    results_root = results_root,
    geneset = geneset,
    genome = genome_species(project),
    compare_col = compare_col,
    design_dir = design_dir,
    deseq_dir = deseq_dir,
    outpath_pathway = outpath_pathway,
    reference = reference,
    comparison = comparison,
    gtf_path = res$gtf %||% "",
    ortholog_path = ortholog_path
  )
  cmd <- paste("bash", shQuote(shell_script))
  target <- file.path(outpath_pathway, "gseapy.gene_set.gsea.report.csv")
  submit_sbatch_wrap(project, "GSEA", cmd, "gseapy", format_comparison_label(compare_col, comparison, reference, geneset), target = target, reference = geneset)
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
    order = seq_along(pipeline_order()),
    step = pipeline_order(),
    description = c(
      "Create or load design_matrix.txt.",
      "Generate per-read quality reports.",
      "Trim adapters and short reads.",
      "Align reads and write BAM files.",
      "Create gene-level count files and count_matrix.txt.",
      "Run differential expression and normalized counts.",
      "Run pathway analysis.",
      "Optional RSEM quantification from STAR BAM/transcriptome outputs.",
      "Optional Kallisto transcript quantification from raw or trimmed reads."
    ),
    stringsAsFactors = FALSE
  )
}

pipeline_stepper_ui <- function(project, status = NULL) {
  if (is.null(status) || !NROW(status)) status <- project_status(project)
  meta <- run_step_meta()
  div(class = "pipeline-stepper", lapply(seq_len(NROW(meta)), function(i) {
    hit <- match(meta$step[i], status$step)
    st <- status$status[hit] %||% "Not started"
    mode <- status$input[hit] %||% ""
    detail <- if ("detail" %in% names(status)) status$detail[hit] %||% "" else ""
    cls <- switch(st, "Complete" = "complete", "Active" = "active", "not-started")
    div(class = paste("pipeline-step", cls),
        div(class = "step-index", meta$order[i]),
        div(class = "step-main",
            tags$strong(meta$step[i]),
            tags$span(status_label(st)),
            if (nzchar(detail)) tags$em(detail) else if (nzchar(mode)) tags$em(mode) else NULL)
    )
  }))
}

tool_panel <- function(step, status, description, controls, button_id, button_label, progress_df = data.frame()) {
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
        actionButton(button_id, button_label, class = "btn-primary"),
        if (identical(st, "Active")) sample_progress_step_ui(progress_df, step) else NULL
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
  suffix <- ""
  db_split <- strsplit(x, " - ", fixed = TRUE)[[1]]
  if (length(db_split) > 1) {
    suffix <- paste(db_split[-1], collapse = " - ")
    x <- db_split[1]
  }
  parts <- strsplit(x, " ", fixed = TRUE)[[1]]
  parts <- parts[nzchar(parts)]
  if (length(parts) >= 4 && identical(parts[3], "vs")) {
    return(format_comparison_label(parts[1], parts[2], parts[4], suffix))
  }
  if (length(parts) >= 3 && identical(parts[2], "vs")) {
    return(format_comparison_label("", parts[1], parts[3], suffix))
  }
  x
}

comparison_label_from_file <- function(file, jobs = data.frame(), step = "DESeq2") {
  m <- regexec("^normalized_counts_(.*)_vs_(.*)\\(ref\\)\\.txt$", file)
  hit <- regmatches(file, m)[[1]]
  if (length(hit) != 3) return(file)
  comparison <- hit[[2]]
  reference <- hit[[3]]
  compare_col <- ""
  if (NROW(jobs) && all(c("step", "input_mode") %in% names(jobs))) {
    job_hit <- jobs[jobs$step == step & grepl(paste0("(^| )", comparison, " vs ", reference, "($| )"), jobs$input_mode), , drop = FALSE]
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
    labels <- vapply(files, function(file) {
      rel_dir <- dirname(sub(paste0("^", gsub("([\\.^$|()\\[\\]{}*+?\\\\])", "\\\\\\1", gsea_dir), "/?"), "", file))
      if (!nzchar(rel_dir) || identical(rel_dir, ".")) rel_dir <- basename(dirname(file))
      if (grepl("^report\\.gseapy\\..*\\.csv$", basename(file))) {
        db <- sub("^report\\.gseapy\\.", "", basename(file))
        db <- sub("\\.csv$", "", db)
        parse_comparison_label(paste(rel_dir, "-", db))
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
  hit <- jobs[jobs$step == step & jobs$slurm_state %in% active_slurm_states(), , drop = FALSE]
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
  if (!length(complete) && !length(running)) return(NULL)
  row_ui <- function(label, values, cls) {
    if (!length(values)) return(NULL)
    div(class = "project-step-summary-row",
        div(class = "project-step-summary-label", label),
        div(class = "project-step-chip-wrap", lapply(values, function(value) span(class = paste("project-step-chip", cls), value)))
    )
  }
  div(class = "project-step-summary",
      div(class = "project-step-summary-title", "Comparison status"),
      row_ui("Running", running, "running"),
      row_ui("Complete", complete, "complete")
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
.csl-header { background:linear-gradient(135deg,#0f2742 0%,#145f78 58%,#1f8f7a 100%); color:white; border:0; border-radius:8px; padding:34px 42px; margin-bottom:18px; min-height:168px; display:flex; align-items:center; justify-content:space-between; gap:34px; }
.brand-lockup { display:flex; align-items:center; gap:28px; }
.brand-lockup img { background:white; border-radius:8px; padding:10px; max-height:112px; max-width:285px; object-fit:contain; }
.csl-header h2 { margin:0 0 8px 0; font-weight:800; font-size:40px; color:white; }
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
.tool-progress-wrap { margin-top:16px; border:1px solid #d8dde8; border-radius:8px; overflow:hidden; background:#f8fafc; }
.tool-progress-title { padding:12px 14px; font-size:13px; font-weight:800; color:#304a66; text-transform:uppercase; letter-spacing:.04em; border-bottom:1px solid #d8dde8; background:#edf4fb; }
.tool-progress-table { width:100%; border-collapse:separate; border-spacing:0; table-layout:fixed; }
.tool-progress-table th { padding:11px 14px; font-size:12px; color:#657084; text-align:left; border-bottom:1px solid #e4eaf2; background:white; }
.tool-progress-table td { padding:11px 14px; font-size:13px; border-bottom:1px solid #edf1f6; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
.tool-progress-table tr:last-child td { border-bottom:0; }
.tool-progress-table .sample-status { min-width:118px; width:auto; max-width:100%; padding:6px 11px; font-size:12px; }
.tool-progress-table .sample-name { font-weight:800; color:#17202f; }
.project-step-summary { margin:12px 0; border:1px solid #d8dde8; border-radius:8px; background:#f8fafc; padding:12px; }
.project-step-summary-title { font-size:12px; font-weight:800; color:#304a66; text-transform:uppercase; letter-spacing:.04em; margin-bottom:8px; }
.project-step-summary-row { display:flex; align-items:flex-start; gap:10px; margin-top:8px; }
.project-step-summary-label { min-width:78px; font-size:12px; font-weight:800; color:#657084; padding-top:4px; }
.project-step-chip-wrap { display:flex; flex-wrap:wrap; gap:7px; flex:1; }
.project-step-chip { border-radius:999px; border:1px solid #cfd7e3; background:white; padding:5px 9px; font-size:12px; font-weight:800; color:#304a66; max-width:100%; overflow-wrap:anywhere; }
.project-step-chip.running { background:#fff4d6; color:#7c3d00; border-color:#f0c36d; }
.project-step-chip.complete { background:#def7e8; color:#0b6b3a; border-color:#8fd8ad; }
.adaptive-table-note { color:#657084; font-size:13px; font-weight:700; margin:0 0 10px 0; }
.resource-strip { display:grid; grid-template-columns:minmax(280px,.85fr) minmax(460px,1.45fr); gap:16px; align-items:stretch; margin:12px 0 18px 0; }
.resource-card { background:white; border:1px solid #d8dde8; border-radius:8px; padding:16px; }
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
.sample-status.waiting { background:#e8f2ff; color:#15549a; border-color:#b9d5f5; }
.sample-status.possibly-incomplete { background:#fff0ed; color:#9f2d20; border-color:#e5a397; }
.sample-status.optional-not-run { background:#f6f3fb; color:#5d4d79; border-color:#d9d0ea; }
.project-card { background:white; border:1px solid #d8dde8; border-radius:8px; padding:14px; box-shadow:0 8px 20px rgba(15,23,36,.06); }
.project-card-top { display:flex; justify-content:space-between; align-items:flex-start; gap:10px; margin-bottom:12px; }
.project-title-wrap h3 { margin:2px 0 0 0; font-size:19px; font-weight:800; color:#17202f; overflow-wrap:anywhere; }
.eyebrow { font-size:11px; text-transform:uppercase; letter-spacing:.04em; color:#657084; font-weight:800; }
.analysis-badge { border-radius:999px; padding:5px 9px; font-size:11px; font-weight:800; white-space:nowrap; background:#e8f2ff; color:#15549a; border:1px solid #b9d5f5; }
.analysis-badge.atac { background:#fff4d6; color:#7a4f00; border-color:#f0c36d; }
.analysis-badge.chip { background:#def7e8; color:#176a38; border-color:#8fd8ad; }
.project-meta-row { display:flex; flex-wrap:wrap; gap:7px; margin-bottom:12px; }
.meta-chip { background:#eef3f8; border:1px solid #d8dde8; border-radius:999px; padding:5px 8px; font-size:12px; color:#273449; font-weight:700; }
.path-list { display:flex; flex-direction:column; gap:8px; }
.path-item { border-top:1px solid #edf1f6; padding-top:8px; }
.path-item span { display:block; font-size:11px; color:#657084; text-transform:uppercase; font-weight:800; margin-bottom:3px; }
.path-item code { display:block; white-space:normal; overflow-wrap:anywhere; background:#f8fafc; color:#17202f; border:1px solid #edf1f6; border-radius:6px; padding:7px; font-size:11px; }
.config-card { margin-top:14px; background:white; border:1px solid #d8dde8; border-radius:8px; padding:14px; box-shadow:0 8px 20px rgba(15,23,36,.05); }
.config-card code { display:block; white-space:normal; overflow-wrap:anywhere; margin-top:6px; background:#f8fafc; border:1px solid #edf1f6; border-radius:6px; padding:9px; color:#17202f; }
.native-results-host { margin: 0 !important; width:100% !important; max-width:100% !important; overflow:auto !important; }
.native-results-host > .container-fluid { max-width: none !important; width: 100% !important; margin: 0 !important; padding: 6px 0 18px 0 !important; overflow-x:auto !important; }
.native-results-host .app-shell { border-radius: 10px !important; box-shadow: none !important; margin:0 !important; overflow:visible !important; }
.native-results-host .hero { padding: 18px 22px 16px 22px !important; }
.native-results-host .main-tabs { padding: 14px 14px 20px 14px !important; overflow-x:auto !important; }
.native-results-host .tab-content, .native-results-host .tab-pane, .native-results-host .main-panel { max-width:100% !important; overflow-x:auto !important; }
.dataTables_wrapper { width:100%; max-width:100%; overflow-x:auto; }
.dataTables_scroll { width:100%; max-width:100%; overflow-x:auto; }
.dataTables_scrollBody { max-height:min(62vh, 560px) !important; overflow:auto !important; }
.native-results-host .dataTables_scrollBody { max-height:min(68vh, 650px) !important; overflow:auto !important; }
.native-results-host .table, .native-results-host table { max-width:100%; }
.native-results-host .shiny-html-output { max-width:100%; overflow-x:auto !important; }


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
.native-results-host .col-sm-2 {
  width: 220px !important;
  min-width: 220px !important;
}
.native-results-host .col-sm-10 {
  width: calc(100% - 220px) !important;
  max-width: calc(100% - 220px) !important;
}
@media (max-width: 900px) {
  body > .container-fluid > .row {
    display: block;
  }
  body > .container-fluid > .row > .col-sm-2,
  body > .container-fluid > .row > .col-sm-10,
  .native-results-host .col-sm-2,
  .native-results-host .col-sm-10 {
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
  min-width: 1320px !important;
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
    "))
  ),
  div(class = "csl-header",
      div(class = "brand-lockup",
          if (file.exists(LOGO_PATH)) tags$img(src = file.path("codespring_logo", basename(LOGO_PATH))),
          div(h2("CodeSpringWeb"), div(class = "muted", "Developed by James Rouse, Rad Utama and Alex Dobin (Bioinformatics Shared Resource)"))
      ),
      if (file.exists(LOGO_CSL_PATH)) tags$img(src = file.path("csl_logo", basename(LOGO_CSL_PATH)), style = "max-height:120px;max-width:300px;background:white;border-radius:8px;padding:10px;object-fit:contain;")
  ),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      selectInput("analysis", "Analysis", choices = c("RNA-seq", "ATAC-seq", "ChIP-seq"), selected = "RNA-seq", selectize = FALSE),
      uiOutput("project_ui"),
      uiOutput("new_project_ui"),
      uiOutput("project_manage_ui"),
      tags$hr(),
      uiOutput("project_card")
    ),
    mainPanel(
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
                   column(7, textInput("metadata_cols", "Metadata columns", value = "treatment", placeholder = "treatment, batch, replicate")),
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
        tabPanel("Progress", br(),
                 div(class = "progress-header-row",
                     div(h3("Pipeline Progress"), tags$p(class = "muted", "Steps are shown in workflow order. The sample matrix below updates as outputs appear.")),
                     actionButton("refresh_progress", "Refresh now", class = "btn-primary")
                 ),
                 textOutput("progress_updated"),
                 uiOutput("pipeline_stepper"),
                 br(),
                 h4("Sample Progress"),
                 uiOutput("sample_progress_matrix_ui"),
                 div(class = "job-table-wrap", h4("Submitted Jobs"), uiOutput("progress_job_filter_ui"), table_output("active_jobs_table"))),
        tabPanel("Run Pipeline", br(), h3("Run Pipeline"),
                 tags$p(class = "muted", "Each tool has its own settings. Jobs are submitted with SLURM sbatch and keep running after this app or browser is closed."),
                 uiOutput("run_resource_strip"),
                 uiOutput("run_pipeline_stepper"),
                 uiOutput("run_step_cards"),
                 br(),
                 verbatimTextOutput("run_output")),
        tabPanel("Results Explorer", uiOutput("native_results_ui")),
        tabPanel("Logs", br(), h3("Submitted Jobs"), uiOutput("job_filter_ui"), table_output("jobs_table"), br(), uiOutput("log_file_ui"), tags$pre(class = "log-viewer", textOutput("selected_log_text"))),
        tabPanel("Methods", br(),
                 h3("Methods Documentation"),
                 tags$p(class = "muted", "Project-level methods, reference genome, tool usage, and detected versions where available."),
                 h4("Project and Reference"),
                 table_output("methods_project_table"),
                 br(),
                 h4("Tools and References"),
                 table_output("methods_tools_table"),
                 br(),
                 h4("Copyable Methods Text"),
                 tags$pre(class = "log-viewer", textOutput("methods_text")))
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
  job_history_state <- reactiveVal(data.frame())
  project_status_state <- reactiveVal(data.frame())
  progress_job_filter_state <- reactiveVal("All")
  job_filter_state <- reactiveVal("All")
  featurecounts_matrix_autosubmitted <- reactiveVal(character(0))
  sample_size_cache <- reactiveVal(data.frame(path = character(), size = numeric(), checked = character(), stringsAsFactors = FALSE))
  sample_progress_state <- reactiveVal(data.frame())
  path_browser <- reactiveValues(target = "", mode = "dir", path = path.expand("~"))

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
    p <- current_project()
    jobs <- carry_forward_job_elapsed(job_history(p), isolate(job_history_state()))
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
        submit_featurecounts_matrix_job(p, "gene_id")
        featurecounts_matrix_autosubmitted(unique(c(autosubmitted, p$id)))
        jobs <- job_history(p)
      }
    }
    active_states <- active_job_state_map_from_jobs(jobs)
    res <- sample_progress(p, active_states, isolate(sample_size_cache()), jobs = jobs)
    status <- project_status(p, jobs = jobs, progress = res$table, active_states = active_states)
    job_history_state(jobs)
    sample_size_cache(res$cache)
    sample_progress_state(res$table)
    project_status_state(status)
    progress_refresh(Sys.time())
  }

  safe_refresh_progress_now <- function(context = "refresh") {
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
      safe_refresh_progress_now("refresh")
    }, once = TRUE)
  }

  mark_submission_active <- function(label, input_mode = "") {
    p <- current_project()
    step <- switch(label,
      "RSEM" = "RSEM (optional)",
      "Kallisto" = "Kallisto (optional)",
      "GSEApy" = "GSEA",
      label
    )
    sample_level_steps <- c("FastQC", "Cutadapt", "STAR", "featureCounts", "RSEM (optional)", "Kallisto (optional)")
    if (step %in% sample_level_steps) {
      optimistic <- optimistic_step_progress(p, step, input_mode)
      if (NROW(optimistic)) {
        old <- isolate(sample_progress_state())
        if (NROW(old) && "step" %in% names(old)) old <- old[old$step != step, , drop = FALSE]
        sample_progress_state(rbind(old, optimistic))
      }
    }
    project_status_state(optimistic_status(isolate(project_status_state()), step, input_mode))
    progress_refresh(Sys.time())
  }

  run_submission <- function(label, expr, input_mode = "") {
    run_message(paste("Submitting", label, "..."))
    progress_refresh(Sys.time())
    msg <- tryCatch(force(expr), error = function(e) paste("ERROR submitting", label, ":", conditionMessage(e)))
    run_message(msg)
    if (!startsWith(msg, "ERROR")) {
      tryCatch(
        {
          mark_submission_active(label, input_mode)
          jobs_now <- job_history(current_project())
          job_history_state(carry_forward_job_elapsed(jobs_now, isolate(job_history_state())))
          progress_refresh(Sys.time())
        },
        error = function(e) run_message(paste(msg, "\nProgress display update failed:", conditionMessage(e)))
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
    paste("Current folder:", normalizePath(path_browser$path, winslash = "/", mustWork = FALSE))
  })

  output$browser_choices_ui <- renderUI({
    choices <- server_browser_choices(path_browser$path, path_browser$mode)
    if (!length(choices)) choices <- stats::setNames(first_scalar_string(path_browser$path, path.expand("~")), "(empty folder)")
    label <- "Folders"
    selected <- unname(choices[[1]])
    selectInput("browser_choice", label, choices = choices, selected = selected, selectize = FALSE, size = min(max(length(choices), 4), 18))
  })

  observeEvent(input$browse_new_fastq_dir, {
    open_server_browser("new_fastq_dir", "dir", input$new_fastq_dir %||% "")
  })

  observeEvent(input$browse_new_results_root, {
    open_server_browser("new_results_root", "dir", input$new_results_root %||% "")
  })

  observeEvent(input$browse_new_design_matrix_path, {
    open_server_browser("new_design_matrix_path", "dir", input$new_design_matrix_path %||% "")
  })

  observeEvent(input$browser_go_path, {
    candidate <- browser_start_path(input$browser_manual_path %||% path_browser$path, "dir")
    path_browser$path <- normalizePath(candidate, winslash = "/", mustWork = FALSE)
    updateTextInput(session, "browser_manual_path", value = path_browser$path)
  })

  observeEvent(input$browser_up, {
    path_browser$path <- normalizePath(dirname(path_browser$path), winslash = "/", mustWork = FALSE)
    updateTextInput(session, "browser_manual_path", value = path_browser$path)
  })

  observeEvent(input$browser_open_choice, {
    choice <- input$browser_choice %||% ""
    if (!nzchar(choice)) return()
    if (dir.exists(choice)) {
      path_browser$path <- normalizePath(choice, winslash = "/", mustWork = FALSE)
      updateTextInput(session, "browser_manual_path", value = path_browser$path)
    }
  })

  observeEvent(input$browser_use_current, {
    value <- normalizePath(path_browser$path, winslash = "/", mustWork = FALSE)
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
    labels <- if (length(p)) vapply(p, function(x) x$label, character(1)) else character(0)
    ids <- if (length(p)) names(p) else character(0)
    choices <- c("Start a new project" = "__new__", stats::setNames(ids, labels))
    selected <- isolate(input$project_id)
    if (is.null(selected) || !selected %in% unname(choices)) {
      last <- read_last_project_id()
      selected <- if (last %in% unname(choices)) last else "__new__"
    }
    selectInput("project_id", "Project Name", choices = choices, selected = selected, selectize = FALSE)
  })

  output$new_project_ui <- renderUI({
    if (!identical(input$project_id, "__new__")) return(NULL)
    tagList(
      tags$hr(),
      h4("New Project"),
      textInput("new_project_name", "Project name", value = "", placeholder = "e.g. my_project"),
      selectInput("new_project_analysis", "Analysis type", choices = c("RNA-seq", "ATAC-seq", "ChIP-seq"), selected = input$analysis, selectize = FALSE),
      selectInput("new_species", "Species", choices = c("Mouse" = "mouse", "Human" = "human"), selected = "mouse", selectize = FALSE),
      uiOutput("new_genome_version_ui"),
      radioButtons("new_paired_end", "Reads", choices = c("Paired-end" = "paired", "Single-end" = "single"), selected = "paired"),
      div(class = "new-project-path-control",
          textInput("new_fastq_dir", "Raw FASTQ folder", value = "", placeholder = "Choose with Browse or paste a server path"),
          actionButton("browse_new_fastq_dir", "Browse server", class = "btn-default")
      ),
      div(class = "new-project-path-control",
          textInput("new_results_root", "Results root", value = "~/csl_results", placeholder = "Where CodeSpringWeb should write project results"),
          actionButton("browse_new_results_root", "Browse server", class = "btn-default")
      ),
      div(class = "new-project-path-control",
          textInput("new_design_matrix_path", "Design matrix folder", value = "", placeholder = "Optional; folder containing or receiving design_matrix.txt"),
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

  output$project_manage_ui <- renderUI({
    p <- filtered_projects()
    if (!length(p)) return(NULL)
    choices <- stats::setNames(vapply(p, `[[`, character(1), "id"), vapply(p, `[[`, character(1), "label"))
    tagList(
      tags$hr(),
      tags$details(class = "project-manage",
        tags$summary("Manage projects"),
        div(class = "muted small-note", "Delete saved project files from project_configs. Data deletion is optional and asks for confirmation."),
        checkboxGroupInput("delete_project_ids", "Projects to delete", choices = choices),
        checkboxInput("delete_project_data", "Also delete associated data folders", value = FALSE),
        actionButton("delete_selected_projects", "Delete selected", class = "btn-danger"),
        textOutput("delete_project_status")
      )
    )
  })

  current_project <- reactive({
    if (identical(input$project_id, "__new__")) return(new_project_from_inputs(input))
    p <- filtered_projects()
    req(length(p) > 0)
    selected <- input$project_id
    if (!length(selected) || is.null(selected) || !nzchar(selected)) {
      idx <- 1
    } else {
      idx <- match(selected, names(p))
      if (!length(idx) || is.na(idx)) idx <- 1
    }
    p[[idx]]
  })

  observeEvent(input$project_id, {
    write_last_project_id(input$project_id %||% "__new__")
    safe_refresh_progress_now("project switch")
  }, ignoreInit = FALSE)

  output$project_card <- renderUI({
    p <- current_project()
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
            span(class = "meta-chip", gencode_label(p)),
            span(class = "meta-chip", if (isTRUE(p$paired_end)) "Paired-end" else "Single-end")
        ),
        div(class = "path-list compact-path-list",
            div(class = "path-item", span("Data"), code(p$data_dir))
        )
    )
  })

  output$setup_table <- renderTable({
    p <- current_project()
    data.frame(
      field = c("Project", "Analysis", "Species", "Genome/reference", "Reference key", "Paired-end", "Results root", "Data folder", "FASTQ folder", "Design matrix"),
      value = c(p$label, p$analysis, genome_species(p), gencode_label(p), genome_reference_key(p), as.character(p$paired_end), p$results_root, p$data_dir, p$fastq_dir, p$design_matrix_path),
      stringsAsFactors = FALSE
    )
  })

  output$source_config_ui <- renderUI({
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
        title = "Delete project results?",
        tags$p("This will delete the selected project config file(s), old job records, logs, Shiny output, and data under these project result folders:"),
        tags$ul(lapply(seq_along(labels), function(i) tags$li(tags$strong(labels[[i]]), tags$br(), code(result_dirs[[i]])))),
        tags$p(tags$strong("This cannot be undone.")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_delete_projects", "Yes, delete configs and data", class = "btn-danger")
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
    p <- new_project_from_inputs(input)
    msg <- tryCatch({
      result_dir <- project_result_dir(p)
      if (dir_has_contents(result_dir)) {
        if (!isTRUE(input$new_clear_existing_results)) {
          stop(
            "A project results folder already exists at ", result_dir,
            ". Check 'Clear existing results if this project folder already exists' to start fresh, ",
            "or choose a different project name."
          )
        }
        deleted <- delete_project_results(p)
        if (!isTRUE(deleted$ok)) stop(deleted$message)
      } else {
        prune_project_job_history(p)
      }
      cfg <- write_project_config(p)
      projects(discover_projects())
      write_last_project_id(p$id)
      updateSelectInput(session, "project_id", selected = p$id)
      paste("Created project:", p$name, "\nSaved project file:", cfg)
    }, error = function(e) paste("ERROR:", conditionMessage(e)))
    output$create_project_status <- renderText(msg)
  })

  metadata_cols_from_input <- reactive({
    cols <- clean_name(unlist(strsplit(input$metadata_cols %||% "", ",")))
    cols <- cols[nzchar(cols) & !cols %in% c("sample", "filename", "include", "status")]
    if (!length(cols)) cols <- "treatment"
    unique(cols)
  })

  observeEvent(input$scan_fastqs, {
    p <- current_project()
    design_state(scan_fastqs(p$fastq_dir, p$paired_end, metadata_cols_from_input()))
  })

  observeEvent(input$add_metadata_col, {
    df <- collect_design_inputs(input, design_state())
    design_state(sync_metadata_columns(df, metadata_cols_from_input()))
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
    } else {
      design_state(data.frame())
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
    if (identical(input$project_id, "__new__") && !nzchar(trimws(input$new_project_name %||% ""))) {
      output$design_save_status <- renderText("ERROR: Enter a project name before saving a new project design matrix.")
      return()
    }
    df <- collect_design_inputs(input, design_state())
    design_state(df)
    metadata <- setdiff(names(df), c("include", "sample", "filename", "status"))
    msg <- tryCatch({
      original_design_path <- p$design_matrix_path %||% ""
      design_path <- write_design_matrix(p, df, metadata)
      p$design_matrix_path <- design_path
      cfg <- write_project_config(p)
      projects(discover_projects())
      write_last_project_id(p$id)
      updateSelectInput(session, "project_id", selected = p$id)
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
    if ((input$web_main_tabs %||% "") %in% c("Progress", "Run Pipeline")) safe_refresh_progress_now("auto refresh")
  })

  observeEvent(input$web_main_tabs, {
    if ((input$web_main_tabs %||% "") %in% c("Progress", "Run Pipeline")) safe_refresh_progress_now("tab refresh")
  }, ignoreInit = TRUE)

  output$progress_updated <- renderText({
    paste("Auto-refreshes every", PROGRESS_REFRESH_MS / 1000, "seconds. Last checked:", format(progress_refresh(), "%Y-%m-%d %H:%M:%S"))
  })

  progress_status <- reactive({
    progress_refresh()
    df <- project_status_state()
    if (!NROW(df)) df <- project_status(current_project())
    df[order(step_order(df$step)), , drop = FALSE]
  })

  observeEvent(input$refresh_progress, {
    safe_refresh_progress_now("manual refresh")
  })

  output$pipeline_stepper <- renderUI({
    pipeline_stepper_ui(current_project(), progress_status())
  })

  output$sample_progress_matrix_ui <- renderUI({
    sample_progress_matrix_ui(sample_progress_state())
  })

  output$sample_progress_detail_table <- render_csl_table({
    sample_progress_detail_table(sample_progress_state())
  }, page_length = 20, scroll_y = "520px")

  output$active_jobs_table <- render_csl_table({
    progress_refresh()
    all_jobs <- job_history_progress_display(current_project())
    tool <- input$progress_job_tool_filter %||% progress_job_filter_state() %||% "All"
    jobs <- filter_jobs_by_tool(all_jobs, tool)
    prepare_job_table_for_display(empty_job_filter_message(jobs, all_jobs, tool))
  }, page_length = 10, escape = FALSE)

  output$progress_job_filter_ui <- renderUI({
    progress_refresh()
    jobs <- job_history_progress_display(current_project())
    choices <- job_filter_choices_from_jobs(jobs)
    selected <- isolate(input$progress_job_tool_filter %||% progress_job_filter_state())
    if (!selected %in% choices) selected <- "All"
    div(class = "button-row",
        div(style = "min-width:260px; flex:1;", selectInput("progress_job_tool_filter", "Filter submitted jobs by tool", choices = choices, selected = selected, selectize = FALSE)),
        div(style = "padding-top:25px;", actionButton("clear_progress_job_filter", "Clear", class = "btn-default"))
    )
  })

  observeEvent(input$progress_job_tool_filter, {
    progress_job_filter_state(input$progress_job_tool_filter %||% "All")
    progress_refresh(Sys.time())
  }, ignoreInit = TRUE)

  observeEvent(input$clear_progress_job_filter, {
    progress_job_filter_state("All")
    updateSelectInput(session, "progress_job_tool_filter", selected = "All")
    progress_refresh(Sys.time())
  })

  output$run_pipeline_stepper <- renderUI({
    progress_refresh()
    pipeline_stepper_ui(current_project(), progress_status())
  })

  output$run_resource_strip <- renderUI({
    p <- current_project()
    div(class = "resource-strip",
        div(class = "resource-card",
            tags$strong("Genome resources"),
            tags$p(class = "muted", gencode_label(p)),
            tags$p(class = "status-path", genome_resources(p)$gtf)
        ),
        div(class = "resource-card flowchart-card",
            if (file.exists(FLOWCHART_PATH)) tags$img(src = file.path("codespring_flowchart", basename(FLOWCHART_PATH))) else tags$p("Pipeline flowchart")
        )
    )
  })

  output$run_step_cards <- renderUI({
    progress_refresh()
    p <- current_project()
    status <- progress_status()
    progress_df <- sample_progress_state()
    div(class = "run-grid",
      tool_panel("FastQC", status, "Quality reports for raw or trimmed reads.",
        tagList(checkboxInput("fastqc_use_trimmed", "Use trimmed reads", value = FALSE)),
        "run_fastqc", "Submit FastQC", progress_df),
      tool_panel("Cutadapt", status, "Trim adapters and short reads from raw FASTQs.",
        tagList(
          selectInput("cutadapt_adapter1", "R1/read1 adapter", choices = adapter_choices_r1(), selected = adapter_choices_r1()[[1]], width = "100%", selectize = FALSE),
          conditionalPanel("input.cutadapt_adapter1 == '__custom__'", textInput("cutadapt_adapter1_custom", "Custom R1/read1 adapter sequence", value = "", width = "100%")),
          selectInput("cutadapt_adapter2", "R2/read2 adapter", choices = adapter_choices_r2(), selected = adapter_choices_r2()[[1]], width = "100%", selectize = FALSE),
          conditionalPanel("input.cutadapt_adapter2 == '__custom__'", textInput("cutadapt_adapter2_custom", "Custom R2/read2 adapter sequence", value = "", width = "100%")),
          textInput("cutadapt_min_length", "Minimum read length", value = "20")
        ),
        "run_cutadapt", "Submit cutadapt", progress_df),
      tool_panel("STAR", status, "Align raw or trimmed reads to the selected genome index.",
        tagList(checkboxInput("star_use_trimmed", "Use trimmed reads", value = TRUE)),
        "run_star", "Submit STAR", progress_df),
      tool_panel("featureCounts", status, "Quantify STAR BAM files with the selected GTF attribute.",
        tagList(selectInput("feature_attr", "featureCounts attribute", choices = c("gene_id", "gene_name"), selected = "gene_id", selectize = FALSE)),
        "run_featurecounts", "Submit featureCounts", progress_df),
      tool_panel("DESeq2", status, "Run differential expression from count_matrix.txt.",
        tagList(uiOutput("deseq_controls_ui"), uiOutput("deseq_project_summary_ui")),
        "run_deseq2", "Submit DESeq2", data.frame()),
      tool_panel("GSEA", status, "Run pathway analysis from DESeq2 normalized counts.",
        tagList(uiOutput("gsea_run_controls_ui"), uiOutput("gsea_project_summary_ui")),
        "run_gsea", "Submit GSEA", data.frame()),
      tool_panel("RSEM (optional)", status, "Optional quantification from STAR BAM/transcriptome outputs.",
        tagList(selectInput("rsem_feature_attr", "RSEM feature attribute", choices = c("gene_id", "gene_name"), selected = "gene_id", selectize = FALSE)),
        "run_rsem", "Submit RSEM", progress_df),
      tool_panel("Kallisto (optional)", status, "Optional transcript abundance quantification from raw or trimmed reads.",
        tagList(checkboxInput("kallisto_use_trimmed", "Use trimmed reads", value = TRUE)),
        "run_kallisto", "Submit Kallisto", progress_df)
    )
  })

  for (step in c("FastQC", "Cutadapt", "STAR", "featureCounts", "RSEM (optional)", "Kallisto (optional)")) {
    local({
      this_step <- step
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
    if (!length(cols)) return(div(class = "empty-box", "No comparison columns found between sample and filename in design_matrix.txt."))
    selected_col <- input$gsea_compare_col %||% if ("treatment" %in% cols) "treatment" else cols[[1]]
    if (!selected_col %in% cols) selected_col <- cols[[1]]
    vals <- design_compare_values(p, selected_col)
    ref <- input$gsea_reference %||% if (length(vals)) vals[[1]] else ""
    comp <- input$gsea_comparison %||% if (length(vals) > 1) vals[[2]] else ref
    tagList(
      selectInput("gsea_compare_col", "Comparison column", choices = cols, selected = selected_col, selectize = FALSE),
      selectInput("gsea_reference", "Reference/baseline", choices = vals, selected = ref, selectize = FALSE),
      selectInput("gsea_comparison", "Comparison", choices = vals, selected = comp, selectize = FALSE),
      selectInput("gsea_geneset", "Gene-set database", choices = GSEAPY_GENESET_OPTIONS, selected = "MSigDB_Hallmark_2020", selectize = FALSE)
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
      run_message("Custom adapter sequences cannot be blank.")
      finish_submit_refresh()
    } else {
      run_submission("Cutadapt", submit_cutadapt_jobs(current_project(), adapter1, adapter2, input$cutadapt_min_length), "raw reads")
    }
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
      run_message("Reference and comparison must be different.")
      finish_submit_refresh()
    } else {
      run_submission("DESeq2", submit_deseq2_job(current_project(), input$deseq_compare_col, input$deseq_reference, input$deseq_comparison, "NoRedundant"), paste(input$deseq_compare_col, input$deseq_comparison, "vs", input$deseq_reference))
    }
  })
  observeEvent(input$run_gsea, {
    if (identical(input$gsea_reference, input$gsea_comparison)) {
      run_message("Reference and comparison must be different.")
      finish_submit_refresh()
    } else {
      run_submission("GSEA", submit_gseapy_job(current_project(), input$gsea_compare_col, input$gsea_reference, input$gsea_comparison, input$gsea_geneset), paste(input$gsea_compare_col, input$gsea_comparison, "vs", input$gsea_reference, input$gsea_geneset))
    }
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
  output$design_table <- render_csl_table(safe_read_table(current_project()$design_matrix_path), page_length = 50)
  output$fastqc_select_ui <- renderUI({
    progress_refresh()
    p <- current_project()
    files <- c(list.files(file.path(p$data_dir, "fastqc"), pattern = "\\.html$", full.names = TRUE),
               list.files(file.path(p$data_dir, "fastqc_cutadapt"), pattern = "\\.html$", full.names = TRUE))
    selectInput("fastqc_file", "FastQC report", choices = files, selected = files[1] %||% character(0), selectize = FALSE)
  })
  output$fastqc_view <- renderUI({ req(input$fastqc_file); image_or_file_ui(input$fastqc_file, "1050px") })
  output$star_summary <- render_csl_table(safe_read_table(file.path(current_project()$data_dir, "star_summary", "summary_matrix.txt")), page_length = 50)
  output$featurecounts_summary <- render_csl_table(safe_read_table(file.path(current_project()$data_dir, "counts", "featurecounts_summary.txt")), page_length = 50)
  output$count_matrix <- render_csl_table(safe_read_table(file.path(current_project()$data_dir, "counts", "count_matrix.txt"), 5000), page_length = 50)

  file_select <- function(id, label, dir, pattern) {
    files <- if (dir.exists(dir)) list.files(dir, pattern = pattern, recursive = TRUE, full.names = TRUE) else character(0)
    selectInput(id, label, choices = files, selected = files[1] %||% character(0), selectize = FALSE)
  }
  output$rsem_file_ui <- renderUI({ progress_refresh(); file_select("rsem_file", "RSEM table", file.path(current_project()$data_dir, "rsem"), "\\.(txt|csv|results)$") })
  output$rsem_table <- render_csl_table({ req(input$rsem_file); safe_read_table(input$rsem_file, 5000) }, page_length = 50)
  output$kallisto_file_ui <- renderUI({ progress_refresh(); file_select("kallisto_file", "Kallisto table", file.path(current_project()$data_dir, "kallisto"), "\\.(tsv|txt|csv)$") })
  output$kallisto_table <- render_csl_table({ req(input$kallisto_file); safe_read_table(input$kallisto_file, 5000) }, page_length = 50)
  output$norm_file_ui <- renderUI({ progress_refresh(); file_select("norm_file", "DESeq2 normalized counts", file.path(current_project()$data_dir, "deseq2"), "normalized.*\\.(txt|csv)$") })
  output$norm_table <- render_csl_table({ req(input$norm_file); safe_read_table(input$norm_file, 5000) }, page_length = 50)
  output$deseq_file_ui <- renderUI({ progress_refresh(); file_select("deseq_file", "DESeq2 file", file.path(current_project()$data_dir, "deseq2"), "\\.(txt|csv|png|pdf)$") })
  output$deseq_file_view <- renderUI({
    req(input$deseq_file)
    if (tolower(tools::file_ext(input$deseq_file)) %in% c("txt", "csv", "tsv")) {
      table_output("deseq_selected_table")
    } else image_or_file_ui(input$deseq_file)
  })
  output$deseq_selected_table <- render_csl_table({ req(input$deseq_file); safe_read_table(input$deseq_file, 5000) }, page_length = 50)
  output$gsea_file_ui <- renderUI({ progress_refresh(); file_select("gsea_file", "GSEA file", file.path(current_project()$data_dir, "gseapy"), "\\.(txt|csv|png|pdf)$") })
  output$gsea_file_view <- renderUI({
    req(input$gsea_file)
    if (tolower(tools::file_ext(input$gsea_file)) %in% c("txt", "csv", "tsv")) {
      table_output("gsea_selected_table")
    } else image_or_file_ui(input$gsea_file, "950px")
  })
  output$gsea_selected_table <- render_csl_table({ req(input$gsea_file); safe_read_table(input$gsea_file, 5000) }, page_length = 50)
  output$all_file_ui <- renderUI({ progress_refresh(); file_select("all_file", "Result file", current_project()$data_dir, "\\.(txt|csv|tsv|html|png|pdf)$") })
  output$all_file_view <- renderUI({ req(input$all_file); image_or_file_ui(input$all_file) })
  output$jobs_table <- render_csl_table({
    progress_refresh()
    all_jobs <- job_history_display(current_project())
    tool <- input$job_tool_filter %||% job_filter_state() %||% "All"
    jobs <- filter_jobs_by_tool(all_jobs, tool)
    prepare_job_table_for_display(empty_job_filter_message(jobs, all_jobs, tool))
  }, page_length = 50, escape = FALSE)

  output$job_filter_ui <- renderUI({
    progress_refresh()
    jobs <- job_history_display(current_project())
    choices <- job_filter_choices_from_jobs(jobs)
    selected <- isolate(input$job_tool_filter %||% job_filter_state())
    if (!selected %in% choices) selected <- "All"
    div(class = "button-row",
        div(style = "min-width:260px; flex:1;", selectInput("job_tool_filter", "Filter jobs by tool", choices = choices, selected = selected, selectize = FALSE)),
        div(style = "padding-top:25px;", actionButton("clear_job_filter", "Clear", class = "btn-default"))
    )
  })

  observeEvent(input$job_tool_filter, {
    job_filter_state(input$job_tool_filter %||% "All")
    progress_refresh(Sys.time())
  }, ignoreInit = TRUE)

  observeEvent(input$clear_job_filter, {
    job_filter_state("All")
    updateSelectInput(session, "job_tool_filter", selected = "All")
    progress_refresh(Sys.time())
  })

  output$log_file_ui <- renderUI({
    progress_refresh()
    project <- current_project()
    all_choices <- log_file_choices(project)
    tools <- log_tool_choices(project)
    selected_tool <- input$log_tool_filter %||% "All"
    if (!selected_tool %in% tools) selected_tool <- "All"
    selected_type <- input$log_type_filter %||% "All"
    choices <- log_file_choices(project, selected_tool, selected_type)
    if (!length(all_choices)) return(div(class = "empty-box", paste("No stdout/stderr log files were found in", file.path(dirname(project$data_dir), "log"))))
    controls <- fluidRow(
      column(4, selectInput("log_tool_filter", "Tool", choices = tools, selected = selected_tool, selectize = FALSE)),
      column(4, selectInput("log_type_filter", "Log type", choices = c("All", "output", "error"), selected = selected_type, selectize = FALSE)),
      column(4, if (length(choices)) selectInput("selected_log_file", "Log file", choices = choices, selectize = FALSE) else div(class = "empty-box", "No logs match this filter."))
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

  output$methods_project_table <- render_csl_table({
    project_methods_summary(current_project())
  }, page_length = 25, scroll_y = "360px")

  output$methods_tools_table <- render_csl_table({
    tool_reference_summary(current_project())
  }, page_length = 25, scroll_y = "520px")

  output$methods_text <- renderText({
    project_methods_text(current_project())
  })
}

shinyApp(ui, server, onStart = cleanup_previous_shiny_processes)
