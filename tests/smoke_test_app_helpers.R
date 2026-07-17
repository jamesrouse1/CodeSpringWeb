args <- commandArgs(trailingOnly = TRUE)
repo_root <- normalizePath(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])), ".."), mustWork = TRUE)
lab_root <- if (length(args)) normalizePath(args[[1]], mustWork = TRUE) else normalizePath(file.path(repo_root, "..", "CodeSpringLab-fix"), mustWork = TRUE)
Sys.setenv(CSL_CODESPRINGLAB_ROOT = lab_root)

app_env <- new.env(parent = globalenv())
sys.source(file.path(repo_root, "app.R"), envir = app_env)

assert <- function(value, message) if (!isTRUE(value)) stop("ASSERTION FAILED: ", message, call. = FALSE)
assert(app_env$is_codespring_process_command("Rscript -e shiny::runApp('/home/user/CodeSpringWeb', port=8601)"), "CodeSpringApp process command recognized")
assert(!app_env$is_codespring_process_command("Rscript unrelated_analysis.R"), "unrelated Rscript process is not treated as CodeSpringApp")
assert(!app_env$is_codespring_process_command("Rscript -e shiny::runApp('/home/user/another_app')"), "unrelated Shiny app is not treated as CodeSpringApp")
root <- tempfile("codespring-app-smoke-")
dir.create(root, recursive = TRUE)
on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

design_path <- file.path(root, "design_matrix.txt")
design <- data.frame(
  sample = c("A1", "I1", "A2", "I2", "B1", "I3", "B2", "I4"),
  treatment = rep(c("A", "A", "B", "B"), each = 2),
  reference = rep(c("chip", "input"), 4),
  condition = rep(c("A", "A", "B", "B"), each = 2),
  replicate = rep(c(1, 1, 2, 2), each = 2),
  control_sample = c("I1", "", "I2", "", "I3", "", "I4", ""),
  filename = paste0(c("A1", "I1", "A2", "I2", "B1", "I3", "B2", "I4"), ".fastq.gz"),
  stringsAsFactors = FALSE
)
write.table(design, design_path, sep = "\t", row.names = FALSE, quote = FALSE)

chip_project <- list(
  id = "fake-chip", name = "fake-chip", analysis_key = "chip", analysis = "ChIP-seq",
  design_matrix_path = design_path, data_dir = root, results_root = dirname(root),
  fastq_dir = root, fastq_dirs = root, paired_end = FALSE, genome = "mouse"
)
assert(identical(app_env$chip_control_sample_for(chip_project, "A1"), "I1"), "explicit ChIP control resolution")
assert(nrow(app_env$chip_target_design(chip_project)) == 4L, "input rows excluded from ChIP targets")
mouse_chip_ref <- app_env$chip_reference_resources(chip_project)
human_chip_project <- chip_project
human_chip_project$genome <- "human"
human_chip_ref <- app_env$chip_reference_resources(human_chip_project)
assert(identical(mouse_chip_ref$genome_version, "mouse_gencodeM39") && grepl("mouse_gencodeM39", mouse_chip_ref$bowtie2_index), "ChIP mouse reference uses GRCm39/GENCODE M39")
assert(identical(human_chip_ref$genome_version, "human_gencode50") && grepl("human_gencode50", human_chip_ref$bowtie2_index), "ChIP human reference uses GRCh38/GENCODE v50")
assert(length(app_env$genome_reference_choices("mouse", "ChIP-seq")) == 1L, "ChIP setup offers only the current mouse reference")
assert(length(app_env$genome_reference_choices("human", "ChIP-seq")) == 1L, "ChIP setup offers only the current human reference")
assert(identical(app_env$numeric_sort_kind(c("900 KB", "1.2 GB", "14 B")), "bytes"), "human-readable file sizes receive numeric table sorting")
assert(identical(app_env$numeric_sort_kind(c("9", "100", "2.5")), "numeric"), "numeric text receives numeric table sorting")
assert(identical(app_env$numeric_sort_kind(c("00:09:00", "01:00:00")), "duration"), "elapsed times receive duration sorting")
size_defs <- app_env$smart_table_column_defs(data.frame(size = c("900 KB", "1.2 GB"), stringsAsFactors = FALSE))
assert(any(vapply(size_defs, function(def) identical(if (is.null(def$type)) "" else def$type, "num") && !is.null(def$render) && grepl("Math.pow(1024", as.character(def$render), fixed = TRUE), logical(1))), "table renderer uses byte-aware numeric sort values")
for (project_variant in list(
  RNA = within(chip_project, { analysis_key <- "rna"; analysis <- "RNA-seq" }),
  CUTRUN = within(chip_project, { analysis_key <- "cutrun"; analysis <- "CUT&RUN" }),
  ATAC = within(chip_project, { analysis_key <- "atac"; analysis <- "ATAC-seq" }),
  ChIP = chip_project
)) {
  step_meta <- app_env$run_step_meta(project_variant)
  assert(NROW(step_meta) == length(app_env$pipeline_order(project_variant)), paste(project_variant$analysis, "stepper descriptions match its pipeline"))
}
for (project_variant in list(
  within(chip_project, { analysis_key <- "cutrun"; analysis <- "CUT&RUN" }),
  within(chip_project, { analysis_key <- "atac"; analysis <- "ATAC-seq" }),
  chip_project
)) {
  assert(identical(tail(app_env$pipeline_order(project_variant), 1), "Peak Annotation"), paste(project_variant$analysis, "ends with Peak Annotation"))
}
assert(identical(app_env$canonical_job_step("peak_annotation"), "Peak Annotation"), "peak annotation job labels canonicalize")
assert(identical(app_env$step_data_paths(chip_project, "Peak Annotation"), file.path(root, "peak_annotation")), "peak annotation cleanup is confined to its project folder")

blank_editor <- app_env$blank_design_matrix_rows(c("condition", "replicate"), rows = 3)
assert(NROW(blank_editor) == 3L && all(!blank_editor$include), "blank design setup provides editable excluded rows")
blank_form <- as.character(app_env$design_form_table_ui(blank_editor))
assert(grepl("design_form_1_sample", blank_form, fixed = TRUE) && grepl("design_form_1_filename", blank_form, fixed = TRUE), "blank design setup renders visible text inputs")
provided_editor <- app_env$design_editor_from_project(chip_project)
form_values <- list()
form_values[[app_env$design_form_input_id(1, "treatment")]] <- "edited_treatment"
form_values[[app_env$design_form_input_id(1, "include")]] <- FALSE
edited_design <- app_env$apply_design_form_values(provided_editor, form_values)
assert(identical(edited_design$treatment[[1]], "edited_treatment") && !edited_design$include[[1]], "provided design matrices remain editable through visible form controls")

duplicate_design <- data.frame(
  include = TRUE, sample = c("sample-A", "sample A"), cell_type = "", condition = c("A", "B"),
  replicate = 1:2, filename = c("a.fastq.gz", "b.fastq.gz"), status = "", stringsAsFactors = FALSE
)
atac_design_project <- chip_project
atac_design_project$analysis_key <- "atac"
atac_design_project$analysis <- "ATAC-seq"
duplicate_error <- tryCatch({
  app_env$write_design_matrix(atac_design_project, duplicate_design, c("condition", "replicate"))
  ""
}, error = conditionMessage)
assert(grepl("remain unique", duplicate_error), "filesystem-safe sample collisions rejected")

blank_design <- duplicate_design[1, , drop = FALSE]
blank_design$sample <- "sample1"
blank_design$filename <- ""
blank_error <- tryCatch({ app_env$write_design_matrix(atac_design_project, blank_design, c("condition", "replicate")); "" }, error = conditionMessage)
assert(grepl("FASTQ filename", blank_error), "blank included FASTQ filenames rejected")

valid_design <- duplicate_design[1, , drop = FALSE]
valid_design$sample <- "sample1"
saved_design <- app_env$write_design_matrix(atac_design_project, valid_design, c("condition", "replicate"))
assert(file.exists(saved_design) && file.info(saved_design)$size > 0, "design matrix saved atomically")
saved_table <- app_env$safe_read_table(saved_design)
assert(all(c("cell_type", "condition", "replicate") %in% names(saved_table)), "required ATAC metadata columns preserved")

unsafe_design <- valid_design
unsafe_design$condition <- "A\tB"
unsafe_error <- tryCatch({ app_env$write_design_matrix(atac_design_project, unsafe_design, c("condition", "replicate")); "" }, error = conditionMessage)
assert(grepl("tabs or line breaks", unsafe_error), "tab characters rejected before TSV save")

for (key in c("rna", "cutrun", "atac", "chip")) {
  example <- app_env$example_dataset_paths(key)
  example_design <- file.path(example$design_dir, "design_matrix.txt")
  assert(dir.exists(example$fastq_dir) && file.exists(example_design), paste(key, "bundled example paths exist"))
  table <- app_env$safe_read_table(example_design)
  assert(NROW(table) > 0 && !anyDuplicated(table$sample), paste(key, "bundled example design has unique samples"))
  reads <- trimws(unlist(strsplit(as.character(table$filename), "[;,]")))
  read_paths <- file.path(example$fastq_dir, reads[nzchar(reads)])
  assert(all(file.exists(read_paths)), paste(key, "bundled example FASTQs match the design"))
  readable <- vapply(read_paths, function(path) {
    connection <- gzfile(path, open = "rt")
    on.exit(close(connection), add = TRUE)
    length(readLines(connection, n = 4L, warn = FALSE)) == 4L
  }, logical(1))
  assert(all(readable), paste(key, "bundled example FASTQs are readable gzip data"))
}
cutrun_example <- app_env$safe_read_table(file.path(app_env$example_dataset_paths("cutrun")$design_dir, "design_matrix.txt"))
assert(sum(cutrun_example$target_class == "control") == 2L, "CUT&RUN example has explicit matched controls")
assert(all(c("cell_type", "mark", "target_class", "condition", "replicate", "control_sample") %in% names(cutrun_example)), "CUT&RUN example contains the editable assay metadata")
cutrun_targets <- cutrun_example$target_class != "control"
assert(all(nzchar(cutrun_example$control_sample[cutrun_targets])), "CUT&RUN example assigns every target to an IgG control")

atac_project <- chip_project
atac_project$analysis_key <- "atac"
atac_project$analysis <- "ATAC-seq"
initial_progress <- app_env$sample_progress(atac_project, jobs = data.frame())$table
initial_a1_bowtie <- initial_progress$status[initial_progress$sample == "A1" & initial_progress$step == "Bowtie2"]
assert(identical(initial_a1_bowtie, "Not started"), "untouched samples start as Not started")
partial_targets <- app_env$sample_step_targets(atac_project, "A1", "Bowtie2")
dir.create(dirname(partial_targets[[1]]), recursive = TRUE, showWarnings = FALSE)
writeLines("partial", partial_targets[[1]])
partial_progress <- app_env$sample_progress(atac_project, jobs = data.frame())$table
partial_a1_bowtie <- partial_progress$status[partial_progress$sample == "A1" & partial_progress$step == "Bowtie2"]
assert(identical(partial_a1_bowtie, "Not started"), "partial files do not imply failure before a terminal job state")
unlink(partial_targets[[1]])
running_job <- data.frame(step = "Bowtie2", sample = "A1", slurm_state = "RUNNING", elapsed = "00:00:05", stderr = "", stringsAsFactors = FALSE)
running_progress <- app_env$sample_progress(atac_project, jobs = running_job)$table
assert(identical(running_progress$status[running_progress$sample == "A1" & running_progress$step == "Bowtie2"], "Running"), "active jobs are not marked failed while outputs are incomplete")
finished_job <- running_job
finished_job$slurm_state <- "COMPLETED"
finished_progress <- app_env$sample_progress(atac_project, jobs = finished_job)$table
assert(identical(finished_progress$status[finished_progress$sample == "A1" & finished_progress$step == "Bowtie2"], "Likely failed"), "missing outputs are classified only after a job completes")
retry_ui <- as.character(app_env$sample_retry_ui(atac_project, finished_progress, "Bowtie2"))
assert(grepl("atac_bowtie2_samples", retry_ui, fixed = TRUE) && grepl('wanted=[&quot;A1&quot;]', retry_ui, fixed = TRUE), "retry action selects only terminally incomplete samples before submission")
sample_dir <- file.path(root, "macs2", "A1")
dir.create(sample_dir, recursive = TRUE)
legacy_peak <- file.path(sample_dir, "A1_peaks.narrowPeak")
run_log <- file.path(sample_dir, "A1_macs2.log")
marker <- file.path(sample_dir, "A1_macs2_complete.txt")
writeLines("chr1\t1\t2", legacy_peak)
assert(identical(app_env$atac_macs2_completion_target(atac_project, "A1"), legacy_peak), "legacy ATAC peaks remain recognized")
writeLines(c("chr1\t1\t200\tpeak1", "chr1\t300\t500\tpeak2"), legacy_peak)
failed_macs_job <- data.frame(step = "MACS2 Peaks", sample = "A1", slurm_state = "FAILED", elapsed = "00:01:00", stderr = "", stringsAsFactors = FALSE)
legacy_peak_progress <- app_env$sample_progress(atac_project, jobs = failed_macs_job)$table
legacy_peak_status <- legacy_peak_progress$status[legacy_peak_progress$sample == "A1" & legacy_peak_progress$step == "MACS2 Peaks"]
assert(identical(legacy_peak_status, "Completed"), "validated legacy ATAC peaks are not hidden by a later failed retry")
writeLines(rep("validated alignment summary", 8), partial_targets[[1]])
failed_bowtie_job <- data.frame(step = "Bowtie2", sample = "A1", slurm_state = "FAILED", elapsed = "00:01:00", stderr = "", stringsAsFactors = FALSE)
failed_bowtie_progress <- app_env$sample_progress(atac_project, jobs = failed_bowtie_job)$table
assert(identical(failed_bowtie_progress$status[failed_bowtie_progress$sample == "A1" & failed_bowtie_progress$step == "Bowtie2"], "Likely failed"), "legacy-output exception is limited to ATAC MACS2 peaks")
unlink(partial_targets[[1]])
writeLines("Traceback (most recent call last):\nOSError: No space left on device", run_log)
assert(identical(app_env$atac_macs2_completion_target(atac_project, "A1"), marker), "new ATAC runs require a completion marker")
assert(app_env$cutrun_macs_fatal_error_signal(atac_project, data.frame(), "MACS2 Peaks", "A1"), "ATAC internal MACS2 exception detection")
writeLines("status\tcomplete", marker)
assert(identical(app_env$atac_macs2_completion_target(atac_project, "A1"), marker), "completed ATAC marker selected")
assert(identical(app_env$atac_macs2_peak_file(atac_project, "A1"), legacy_peak), "validated ATAC peak selected for DiffBind")

unlink(marker)
assert(identical(app_env$chip_macs2_peak_file(chip_project, "A1"), ""), "partial ChIP MACS2 output rejected")
writeLines("status\tcomplete", marker)
assert(identical(app_env$chip_macs2_peak_file(chip_project, "A1"), legacy_peak), "completed ChIP MACS2 peak accepted")
chip_peaks <- app_env$chip_peak_summary_table(chip_project)
assert(NROW(chip_peaks) == 4L && chip_peaks$status[chip_peaks$sample == "A1"] == "Completed", "ChIP matched-input peak summary reports completion")
alignment_dir <- file.path(root, "bowtie2", "A1")
dir.create(alignment_dir, recursive = TRUE, showWarnings = FALSE)
writeLines(c("sample\tA1", "mapped_reads\t100", "deduplicated_reads\t80", "bigwig_normalization\tCPM"), file.path(alignment_dir, "A1_alignment_summary.txt"))
signal_file <- file.path(alignment_dir, "A1Aligned.sortedByCoord_removeDup.out.bw")
writeBin(as.raw(seq_len(64)), signal_file)
fragment_pdf <- file.path(alignment_dir, "A1_insert_size_histogram.pdf")
grDevices::pdf(fragment_pdf, width = 8, height = 5)
graphics::plot(1:10, type = "h", main = "Synthetic insert sizes")
grDevices::dev.off()
fragment_html <- as.character(app_env$fragment_plot_ui(fragment_pdf))
assert(grepl("fragment-plot-frame", fragment_html, fixed = TRUE) && grepl("data:image/png;base64", fragment_html, fixed = TRUE) && !grepl("iframe", fragment_html, fixed = TRUE), "fragment PDFs render directly as standardized images")
assert(identical(app_env$pdf_first_page_data_uri(fragment_pdf), app_env$pdf_first_page_data_uri(fragment_pdf)), "fragment PDF rendering is cached in memory")
chip_alignment <- app_env$chip_alignment_summary_table(chip_project)
assert(NROW(chip_alignment) == 1L && all(c("role", "condition", "matched_input") %in% names(chip_alignment)) && chip_alignment$matched_input[[1]] == "I1", "ChIP alignment summary includes experimental roles")
chip_signal <- app_env$peak_signal_track_table(chip_project)
assert(NROW(chip_signal) == 1L && chip_signal$role[[1]] == "chip" && chip_signal$normalization[[1]] == "CPM", "ChIP signal table reports role and saved normalization")
igv_catalog <- app_env$genome_browser_track_catalog(chip_project)
assert(NROW(igv_catalog) >= 2L && all(c("signal", "peaks") %in% igv_catalog$kind), "embedded genome browser catalogs signal and peak tracks")
assert(identical(app_env$genome_browser_reference(chip_project), "mm39") && identical(app_env$genome_browser_reference(human_chip_project), "hg38"), "embedded genome browser follows the project reference")
shared_scale_config <- app_env$genome_browser_signal_display_config(TRUE, TRUE)
independent_scale_config <- app_env$genome_browser_signal_display_config(FALSE, TRUE)
assert(identical(shared_scale_config$autoscaleGroup, "codespring_comparison_signal"), "comparison bigWigs share an IGV autoscale group")
assert(is.null(independent_scale_config$autoscaleGroup), "manual genome-browser tracks retain independent autoscaling")
range_response <- app_env$genome_browser_range_response(
  list(path = signal_file, content_type = "application/octet-stream"),
  list(REQUEST_METHOD = "GET", HTTP_RANGE = "bytes=10-19")
)
assert(identical(range_response$status, 206) && length(range_response$content) == 10L, "genome browser serves bounded byte ranges for large tracks")
invalid_range <- app_env$genome_browser_range_response(
  list(path = signal_file, content_type = "application/octet-stream"),
  list(REQUEST_METHOD = "GET", HTTP_RANGE = "bytes=999-1000")
)
assert(identical(invalid_range$status, 416), "genome browser rejects out-of-range project track requests")
fake_file_choices <- stats::setNames(c(signal_file, file.path(root, "bowtie2", "B1", "B1_signal.bw")), c("A1 signal", "B1 signal"))
assert(identical(app_env$result_file_sample(chip_project, signal_file), "A1"), "result files resolve to their design sample")
assert(identical(unname(app_env$filter_result_files_by_sample(chip_project, fake_file_choices, "A1")), signal_file), "sample file filter excludes other samples")
assert(identical(app_env$validated_project_result_path(chip_project, signal_file), normalizePath(signal_file)), "current-project result path accepted")
outside_file <- tempfile("outside-result-")
writeLines("outside", outside_file)
assert(identical(app_env$validated_project_result_path(chip_project, outside_file), ""), "result path outside the current project is rejected")
assert(inherits(app_env$atac_summary_cards_ui(chip_project), "shiny.tag"), "ChIP summary cards render with fake results")
assert(inherits(app_env$chip_results_explorer_ui(), "shiny.tag"), "ChIP Results Explorer UI renders locally")

atac_ui_text <- as.character(app_env$atac_results_explorer_ui())
chip_ui_text <- as.character(app_env$chip_results_explorer_ui())
cutrun_ui_text <- as.character(app_env$cutrun_results_explorer_ui())
for (ui_check in list(
  ATAC = atac_ui_text,
  ChIP = chip_ui_text,
  CUTRUN = cutrun_ui_text
)) {
  assert(grepl("Developed by CSHL's Bioinformatics Shared Resource", ui_check, fixed = TRUE), paste(names(ui_check), "Results Explorer uses the shared branded header"))
  assert(grepl("Overview", ui_check, fixed = TRUE) && grepl("QC", ui_check, fixed = TRUE) && grepl("Files", ui_check, fixed = TRUE), "custom Results Explorer exposes the standard navigation")
  assert(grepl("Signal &amp; Peaks", ui_check, fixed = TRUE) || grepl("Signal & Peaks", ui_check, fixed = TRUE), "custom Results Explorer exposes standardized signal and peak navigation")
}
assert(grepl("Initial QC", chip_ui_text, fixed = TRUE) && grepl("Fragment Size", chip_ui_text, fixed = TRUE), "ChIP Results Explorer includes RNA-style QC navigation")
assert(grepl("Signal Tracks", atac_ui_text, fixed = TRUE) && grepl("Signal Tracks", chip_ui_text, fixed = TRUE), "ATAC and ChIP Results Explorers expose signal-track navigation")
assert(all(vapply(list(atac_ui_text, chip_ui_text, cutrun_ui_text), grepl, logical(1), pattern = "Genome Browser", fixed = TRUE)), "ATAC, ChIP, and CUT&RUN Results Explorers expose the embedded genome browser")
assert(all(vapply(list(atac_ui_text, chip_ui_text, cutrun_ui_text), grepl, logical(1), pattern = "Gene Annotation", fixed = TRUE)), "all peak Results Explorers expose gene annotations")
assert(grepl("cutrun_file_sample_ui", cutrun_ui_text, fixed = TRUE), "CUT&RUN file explorer exposes a sample selector")
assert(grepl("height:680px", app_env$app_css, fixed = TRUE), "fragment plots share a fixed display height")

rna_project <- chip_project
rna_project$id <- "fake-rna"
rna_project$name <- "fake-rna"
rna_project$analysis_key <- "rna"
rna_project$analysis <- "RNA-seq"
rna_project$genome <- "mouse"
rna_project$genome_version <- "mouse_gencodeM39"
old_app_home <- app_env$APP_HOME
app_env$APP_HOME <- file.path(root, "fake-app-home")
rna_config <- app_env$write_native_shiny_config(rna_project)
rna_config_text <- paste(readLines(rna_config, warn = FALSE), collapse = "\n")
assert(grepl('genome_species <- "mouse"', rna_config_text, fixed = TRUE), "RNA Results Explorer config records the analysis species")
assert(grepl('genome_version <- "mouse_gencodeM39"', rna_config_text, fixed = TRUE), "RNA Results Explorer config records the analysis reference")
assert(grepl("gencode.vM39.primary_assembly.annotation.gtf", rna_config_text, fixed = TRUE), "RNA Results Explorer config receives the analysis GTF")
rna_viewer <- app_env$load_native_rnaseq_viewer(rna_project)
app_env$APP_HOME <- old_app_home
assert(inherits(rna_viewer$ui, "shiny.tag") && is.function(rna_viewer$server), "RNA Results Explorer loads against a synthetic project")
assert(grepl("RNA-Seq Results Explorer", as.character(rna_viewer$ui), fixed = TRUE), "RNA Results Explorer branded UI is present")

fake_jobs <- data.frame(
  step = c("Bowtie2", "Bowtie2", "Bowtie2", "Bowtie2", "FastQC"),
  sample = c("A1", "A2", "A3", "A4", "A1"),
  job_id = c("101", "102", "103", "104", "105"),
  slurm_state = c("RUNNING", "PENDING", "COMPLETED", "CANCELLED", "RUNNING"),
  stringsAsFactors = FALSE
)
active_bowtie <- app_env$active_step_jobs_from_jobs(fake_jobs, "Bowtie2")
assert(identical(sort(active_bowtie$job_id), c("101", "102")), "active-job filtering excludes completed, cancelled, and other-step jobs")
assert(identical(unname(app_env$active_step_sample_choices(fake_jobs, "Bowtie2")), c("A1", "A2")), "cancellation choices contain only samples with active jobs")
assert(identical(app_env$filter_active_jobs_by_samples(active_bowtie, "A2")$job_id, "102"), "selected-sample cancellation resolves only the requested active job")
assert(inherits(app_env$active_jobs_modal_table(active_bowtie), "shiny.tag"), "active-job cancellation summary renders locally")

assay_jobs <- data.frame(
  step = c("STAR", "Bowtie2", "SEACR", "MACS2 Peaks"),
  sample = c("rna_sample", "atac_sample", "cutrun_sample", "chip_sample"),
  job_id = c("201", "202", "203", "204"),
  slurm_state = rep("RUNNING", 4),
  stringsAsFactors = FALSE
)
for (step in assay_jobs$step) {
  expected <- assay_jobs$sample[assay_jobs$step == step]
  assert(identical(unname(app_env$active_step_sample_choices(assay_jobs, step)), expected), paste(step, "supports active sample cancellation"))
}

sample_aware_submitters <- c(
  "submit_cutadapt_jobs", "submit_fastqc_jobs", "submit_star_jobs", "submit_featurecounts_jobs", "submit_rsem_jobs", "submit_kallisto_jobs",
  "submit_cutrun_bowtie2_jobs", "submit_cutrun_seacr_jobs", "submit_cutrun_macs2_jobs",
  "submit_atac_bowtie2_jobs", "submit_atac_macs2_jobs", "submit_chip_bowtie2_jobs", "submit_chip_macs2_jobs"
)
for (function_name in sample_aware_submitters) {
  assert("samples" %in% names(formals(app_env[[function_name]])), paste(function_name, "accepts explicit sample selection"))
}
assert(identical(app_env$requested_sample_subset(atac_project, c("A1", "A2"), "A2", "test step"), "A2"), "unchecked samples are excluded from submission")
cutrun_example_project <- chip_project
cutrun_example_project$analysis_key <- "cutrun"
cutrun_example_project$analysis <- "CUT&RUN"
cutrun_example_project$design_matrix_path <- file.path(app_env$example_dataset_paths("cutrun")$design_dir, "design_matrix.txt")
cutrun_targets <- app_env$pipeline_step_sample_candidates(cutrun_example_project, targets_only = TRUE)
assert(length(cutrun_targets) == 4L && !any(grepl("IgG", cutrun_targets)), "CUT&RUN peak-step selectors contain targets but not controls")

assert(system2("bash", c("-n", shQuote(file.path(repo_root, "run_codespringweb.sh")))) == 0L, "CodeSpringApp launcher shell syntax is valid")

bad_q <- app_env$submit_atac_macs2_jobs(atac_project, "not-a-number", "A1")
assert(grepl("q-value must be", bad_q), "invalid ATAC MACS2 q-value rejected before submission")
assert(grepl("two different", app_env$submit_atac_diffbind_job(atac_project, "condition", "A", "A")), "identical ATAC DiffBind conditions rejected")
assert(grepl("two different", app_env$submit_chip_diffbind_job(chip_project, "condition", "A", "A")), "identical ChIP DiffBind conditions rejected")

comparison_dir <- file.path(root, "diffbind", "B_vs_A")
dir.create(comparison_dir, recursive = TRUE)
legacy_result <- file.path(comparison_dir, "DifferentialPeaks_B_vs_A_ref.txt")
writeLines("Fold\tFDR\n1\t0.01", legacy_result)
assert(identical(app_env$peak_diffbind_status(atac_project), "Complete"), "legacy DiffBind comparison remains recognized")
writeLines(character(0), legacy_result)
assert(!app_env$diffbind_comparison_complete(comparison_dir), "empty legacy result is not accepted")
writeLines("Fold\tFDR\n1\t0.01", legacy_result)
writeLines("status\trunning", file.path(comparison_dir, "_RUN_STARTED"))
assert(identical(app_env$peak_diffbind_status(atac_project), "Likely failed"), "partial DiffBind output is not accepted")
assert(!app_env$diffbind_comparison_complete(comparison_dir), "started DiffBind comparison hidden from Results Explorer")
active_jobs <- data.frame(
  step = "Differential Peaks", slurm_state = "RUNNING", sample = basename(comparison_dir),
  target = file.path(comparison_dir, "_COMPLETE"), stringsAsFactors = FALSE
)
assert(app_env$diffbind_comparison_active(atac_project, comparison_dir, jobs = active_jobs), "active DiffBind comparison recognized")
unlink(file.path(comparison_dir, "_RUN_STARTED"))
writeLines("status\tcomplete", file.path(comparison_dir, "_COMPLETE"))
assert(app_env$diffbind_comparison_complete(comparison_dir), "final DiffBind marker accepted")
writeLines(c(
  "seqnames\tstart\tend\tFold\tp.value\tFDR",
  "chr1\t101\t220\t2.5\t0.0002\t0.004",
  "chr1\t501\t650\t-1.8\t0.003\t0.02"
), legacy_result)

differential_bed <- file.path(comparison_dir, "DifferentialPeaks_B_vs_A_ref.with_stats.bed")
writeLines(c(
  "chr1\t100\t220\tpeak_1|Fold=2.5|p.value=0.0002|FDR=0.004\t2.5",
  "chr1\t500\t650\tpeak_2|Fold=-1.8|p.value=0.003|FDR=0.02\t-1.8"
), differential_bed)
expanded_bed <- app_env$safe_read_result_table(differential_bed)
assert(all(c("Fold", "p.value", "FDR") %in% names(expanded_bed)), "ATAC with-stats BED exposes p-value and FDR columns in the Results Explorer")
comparison_annotation <- file.path(comparison_dir, "DifferentialPeaks_B_vs_A_ref_annotated_with_stats.txt")
writeLines(c(
  "PeakID\tGene Name\tAnnotation",
  "chr1:101-220|Fold=2.5|p.value=0.0002|FDR=0.004\tGeneA\tPromoter",
  "chr1:501-650|Fold=-1.8|p.value=0.003|FDR=0.02\tGeneB\tIntron"
), comparison_annotation)
navigation <- app_env$genome_browser_comparison_navigation(comparison_dir)
assert(all(c("GeneA", "GeneB") %in% unname(navigation$genes)), "genome browser offers annotated differential-peak genes")
assert(length(navigation$peaks) == 2L && grepl("chr1:101-220", names(navigation$peaks)[[1]], fixed = TRUE), "genome browser offers searchable differential-peak intervals")
differential_table <- app_env$differential_accessibility_result_table(comparison_dir)
assert(identical(differential_table[["Genomic interval"]], c("chr1:101-220", "chr1:501-650")), "differential accessibility table includes complete genomic intervals")
chip_sheet_dir <- file.path(root, "manifest", "chip_diffbind", basename(comparison_dir))
dir.create(chip_sheet_dir, recursive = TRUE, showWarnings = FALSE)
comparison_samples <- data.frame(
  SampleID = c("A1", "A2", "B1", "B2"), Condition = c("A", "A", "B", "B"), Replicate = c(1, 2, 1, 2),
  bamReads = "synthetic.bam", Peaks = "synthetic.narrowPeak", PeakCaller = "narrowpeak", stringsAsFactors = FALSE
)
write.table(comparison_samples, file.path(chip_sheet_dir, "chip_diffbind_samples.tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
for (sample in c("A2", "B1", "B2")) {
  sample_signal_dir <- file.path(root, "bowtie2", sample)
  dir.create(sample_signal_dir, recursive = TRUE, showWarnings = FALSE)
  writeBin(as.raw(seq_len(64)), file.path(sample_signal_dir, paste0(sample, "Aligned.sortedByCoord_removeDup.out.bw")))
}
comparison_catalog <- app_env$genome_browser_comparison_catalog(chip_project)
assert(NROW(comparison_catalog) == 1L, "genome browser discovers a completed differential comparison")
assert(identical(comparison_catalog$samples[[1]], c("A1", "A2", "B1", "B2")), "comparison browser retains the exact sample-sheet order")
assert(identical(comparison_catalog$differential_bed[[1]], normalizePath(differential_bed)), "comparison browser selects the differential BED annotation")
comparison_tracks <- app_env$genome_browser_preferred_signal_rows(
  chip_project, app_env$genome_browser_track_catalog(chip_project),
  comparison_catalog$samples[[1]], comparison_catalog$sample_metadata[[1]]
)
assert(NROW(comparison_tracks) == 4L && identical(comparison_tracks$sample, c("A1", "A2", "B1", "B2")), "comparison browser loads one bigWig per sample in sample-sheet order")

cutrun_comparison_dir <- file.path(root, "cutrun_diffbind", "Creb", "B_vs_A")
dir.create(cutrun_comparison_dir, recursive = TRUE, showWarnings = FALSE)
writeLines("chr1\t100\t220\tpeak_1\t2.5", file.path(cutrun_comparison_dir, "significant_differential_peaks.bed"))
comparison_samples$normalization_mode <- "spikein"
write.table(comparison_samples, file.path(cutrun_comparison_dir, "diffbind_sample_sheet.tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
cutrun_browser_project <- chip_project
cutrun_browser_project$analysis_key <- "cutrun"
cutrun_browser_project$analysis <- "CUT&RUN"
cutrun_comparisons <- app_env$genome_browser_comparison_catalog(cutrun_browser_project)
assert(NROW(cutrun_comparisons) == 1L && basename(cutrun_comparisons$id[[1]]) == "B_vs_A", "genome browser discovers nested CUT&RUN differential comparisons")

atac_browser_root <- file.path(root, "atac_browser_case")
atac_browser_samples <- paste0(rep(c("Control", "Treated"), each = 3), rep(1:3, 2))
atac_browser_design <- data.frame(
  sample = atac_browser_samples, condition = rep(c("Control", "Treated"), each = 3), replicate = rep(1:3, 2),
  filename = paste0(atac_browser_samples, "_R1.fastq.gz"), stringsAsFactors = FALSE
)
dir.create(atac_browser_root, recursive = TRUE, showWarnings = FALSE)
atac_browser_design_path <- file.path(atac_browser_root, "design_matrix.txt")
write.table(atac_browser_design, atac_browser_design_path, sep = "\t", row.names = FALSE, quote = FALSE)
atac_manifest_dir <- file.path(atac_browser_root, "manifest", "atac_diffbind", "all_samples", "condition")
dir.create(atac_manifest_dir, recursive = TRUE, showWarnings = FALSE)
write.table(atac_browser_design, file.path(atac_manifest_dir, "design_matrix.txt"), sep = "\t", row.names = FALSE, quote = FALSE)
atac_comparison_dir <- file.path(atac_browser_root, "diffbind", "Treated_vs_Control")
dir.create(atac_comparison_dir, recursive = TRUE, showWarnings = FALSE)
writeLines("chr1\t200\t320\tpeak_1\t3", file.path(atac_comparison_dir, "DifferentialPeaks_Treated_vs_Control_ref.with_stats.bed"))
for (sample in atac_browser_samples) {
  sample_signal_dir <- file.path(atac_browser_root, "bowtie2", sample)
  dir.create(sample_signal_dir, recursive = TRUE, showWarnings = FALSE)
  writeBin(as.raw(seq_len(64)), file.path(sample_signal_dir, paste0(sample, "Aligned.sortedByCoord_removeDup.out.bw")))
}
atac_browser_project <- list(
  id = "atac-browser", name = "atac-browser", analysis_key = "atac", analysis = "ATAC-seq",
  design_matrix_path = atac_browser_design_path, data_dir = atac_browser_root, results_root = root,
  fastq_dir = atac_browser_root, fastq_dirs = atac_browser_root, paired_end = TRUE, genome = "mouse"
)
atac_comparisons <- app_env$genome_browser_comparison_catalog(atac_browser_project)
assert(NROW(atac_comparisons) == 1L && identical(atac_comparisons$samples[[1]], atac_browser_samples), "existing ATAC comparisons recover all six samples from the saved run manifest")

annotation_inputs <- app_env$peak_annotation_input_files(atac_project)
assert(legacy_peak %in% annotation_inputs, "peak annotation discovers completed per-sample MACS2 peaks")
annotation_root <- file.path(root, "peak_annotation")
dir.create(annotation_root, recursive = TRUE)
annotation_jobs <- data.frame(
  step = c("Peak Annotation", "Peak Annotation"),
  slurm_state = c("RUNNING", "FAILED"), stringsAsFactors = FALSE
)
assert(identical(app_env$peak_annotation_status(atac_project, annotation_jobs), "Active"), "an active annotation job is not hidden by a newer stale job record")
writeLines("status\trunning", file.path(annotation_root, "_RUN_STARTED"))
assert(identical(app_env$peak_annotation_status(atac_project, data.frame()), "Likely failed"), "orphaned annotation run marker reports an incomplete job")
unlink(file.path(annotation_root, "_RUN_STARTED"))
annotated_peak <- file.path(sample_dir, "A1_peaks_annotated.txt")
writeLines("PeakID\tAnnotation\npeak1|Fold=2.1|p.value=0.0004|FDR=0.008\tPromoter", annotated_peak)
expanded_annotation <- app_env$safe_read_result_table(annotated_peak)
assert(all(c("Fold", "p.value", "FDR") %in% names(expanded_annotation)), "embedded peak statistics expand into explicit result columns")
assert(is.numeric(expanded_annotation$p.value) && identical(expanded_annotation$p.value[[1]], 0.0004), "expanded ATAC differential p-value remains numeric")
write.table(data.frame(
  result_type = "MACS2", sample_or_comparison = "A1", peak_count = 2,
  source_peak_file = legacy_peak, annotated_file = annotated_peak, status = "complete",
  stringsAsFactors = FALSE
), file.path(annotation_root, "peak_annotation_summary.tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
Sys.sleep(0.01)
writeLines(c("status\tcomplete", "annotated_files\t1"), file.path(annotation_root, "_COMPLETE"))
assert(app_env$peak_annotation_is_current(atac_project, legacy_peak), "current annotations are not resubmitted")
assert(identical(app_env$peak_annotation_status(atac_project, data.frame()), "Complete"), "annotation completion marker drives pipeline status")
assert(NROW(app_env$peak_annotation_summary_table(atac_project)) == 1L, "annotation summary is available to the Results Explorer")
assert(annotated_peak %in% unname(app_env$peak_annotation_result_files(atac_project)), "annotated peak tables are discoverable in Results Explorer")

cat("CodeSpringApp fake-data helper smoke tests passed.\n")
