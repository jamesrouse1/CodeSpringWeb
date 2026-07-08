args <- commandArgs(trailingOnly = TRUE)

arg <- function(i, default = "") {
  if (length(args) >= i && nzchar(args[[i]])) args[[i]] else default
}

script_dir <- arg(1)
project_name <- arg(2)
results_root <- arg(3)
geneset_name <- arg(4)
genome <- tolower(arg(5, "mouse"))
compare_col <- arg(6)
design_dir <- arg(7)
deseq_dir <- arg(8)
outpath_pathway <- arg(9)
refcond <- arg(10)
compared <- arg(11)
gtf_path <- arg(12)
ortholog_path <- arg(13)

set.seed(8)
dir.create(outpath_pathway, recursive = TRUE, showWarnings = FALSE)

message2 <- function(...) cat(paste0(..., "\n"))

safe_gsea_name <- function(value) {
  value <- trimws(as.character(value))
  expanded <- path.expand(value)
  if (file.exists(expanded)) value <- basename(expanded)
  value <- gsub("[^A-Za-z0-9_.-]+", "_", value)
  value <- gsub("^_+|_+$", "", value)
  if (nzchar(value)) value else "gene_set"
}

read_table_any <- function(path) {
  if (!file.exists(path)) stop("Missing file: ", path)
  ext <- tolower(tools::file_ext(path))
  sep <- if (identical(ext, "csv")) "," else "\t"
  utils::read.table(path, sep = sep, header = TRUE, check.names = FALSE, quote = "\"", comment.char = "", stringsAsFactors = FALSE)
}

strip_version <- function(x) sub("\\.[0-9]+$", "", as.character(x))

looks_like_ensembl_gene_ids <- function(x) {
  x <- trimws(as.character(x))
  x <- x[nzchar(x)]
  if (!length(x)) return(FALSE)
  hits <- sum(grepl("^ENS[A-Z]*G[0-9]+(\\.[0-9]+)?$", x))
  hits >= max(10, floor(0.5 * length(x)))
}

extract_attr <- function(x, key) {
  m <- regexec(paste0(key, " \"([^\"]+)\""), x)
  hit <- regmatches(x, m)
  vapply(hit, function(z) if (length(z) >= 2) z[[2]] else NA_character_, character(1))
}

read_gtf_gene_map <- function(path) {
  if (!nzchar(path) || !file.exists(path)) return(data.frame())
  message2("Using local GTF gene map: ", path)
  con <- if (grepl("\\.gz$", path)) gzfile(path, open = "rt") else file(path, open = "rt")
  on.exit(close(con), add = TRUE)
  rows <- list()
  chunk <- 1L
  repeat {
    lines <- readLines(con, n = 50000, warn = FALSE)
    if (!length(lines)) break
    lines <- lines[!startsWith(lines, "#")]
    if (!length(lines)) next
    fields <- strsplit(lines, "\t", fixed = TRUE)
    keep <- vapply(fields, function(z) length(z) >= 9 && identical(z[[3]], "gene"), logical(1))
    if (!any(keep)) next
    attrs <- vapply(fields[keep], function(z) z[[9]], character(1))
    gene_id <- strip_version(extract_attr(attrs, "gene_id"))
    gene_name <- extract_attr(attrs, "gene_name")
    ok <- !is.na(gene_id) & !is.na(gene_name) & nzchar(gene_id) & nzchar(gene_name)
    if (any(ok)) {
      rows[[chunk]] <- data.frame(ensembl_gene_id = gene_id[ok], gene_name = gene_name[ok], stringsAsFactors = FALSE)
      chunk <- chunk + 1L
    }
  }
  if (!length(rows)) return(data.frame())
  unique(do.call(rbind, rows))
}

pick_col <- function(df, candidates) {
  norm <- function(x) gsub("[^a-z0-9]+", "", tolower(x))
  keys <- norm(names(df))
  for (candidate in candidates) {
    hit <- which(keys == norm(candidate))
    if (length(hit)) return(names(df)[hit[[1]]])
  }
  NULL
}

read_ortholog_table <- function(path) {
  candidates <- unique(c(
    path,
    file.path(script_dir, "reference", "mouse_human_orthologs_MGI.tsv")
  ))
  for (candidate in candidates[nzchar(candidates)]) {
    if (!file.exists(candidate)) next
    orth <- tryCatch(read_table_any(candidate), error = function(e) data.frame())
    if (!NROW(orth)) next
    mouse_col <- pick_col(orth, c("mouse_gene_symbol", "mouse_symbol", "mgi_symbol", "marker_symbol", "external_gene_name"))
    human_col <- pick_col(orth, c("human_gene_symbol", "human_symbol", "hgnc_symbol", "human_gene_name"))
    if (is.null(mouse_col) || is.null(human_col)) next
    out <- unique(data.frame(
      mouse_gene_symbol = trimws(as.character(orth[[mouse_col]])),
      human_gene_symbol = trimws(as.character(orth[[human_col]])),
      stringsAsFactors = FALSE
    ))
    out <- out[nzchar(out$mouse_gene_symbol) & nzchar(out$human_gene_symbol), , drop = FALSE]
    mouse_counts <- table(out$mouse_gene_symbol)
    out <- out[as.integer(mouse_counts[out$mouse_gene_symbol]) == 1L, , drop = FALSE]
    message2("Using mouse-human ortholog table: ", candidate)
    message2("GSEA ortholog mapping: retained ", nrow(out), " mappings after excluding mouse-to-many mappings.")
    return(out)
  }
  data.frame()
}

collapse_by_gene <- function(expr, labels) {
  labels <- trimws(as.character(labels))
  ok <- nzchar(labels) & !is.na(labels)
  expr <- expr[ok, , drop = FALSE]
  labels <- labels[ok]
  df <- data.frame(GENE = labels, expr, check.names = FALSE)
  for (col in setdiff(names(df), "GENE")) df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  duplicate_rows <- sum(duplicated(df$GENE))
  out <- stats::aggregate(. ~ GENE, data = df, FUN = function(x) mean(x, na.rm = TRUE))
  rownames(out) <- out$GENE
  if (duplicate_rows > 0) message2("GSEA gene labels: averaged ", duplicate_rows, " duplicate rows by gene symbol.")
  out[, setdiff(names(out), "GENE"), drop = FALSE]
}

prepare_expression <- function(counts, sample_cols) {
  gene_ids <- as.character(counts[[1]])
  description <- if ("DESCRIPTION" %in% names(counts)) as.character(counts$DESCRIPTION) else rep("", length(gene_ids))
  expr <- counts[, sample_cols, drop = FALSE]
  rownames(expr) <- gene_ids
  for (col in sample_cols) expr[[col]] <- suppressWarnings(as.numeric(expr[[col]]))

  labels <- gene_ids
  index_is_ensembl <- looks_like_ensembl_gene_ids(labels)
  usable_description <- !is.na(description) & nzchar(trimws(description)) & !tolower(trimws(description)) %in% c("na", "nan", "none")
  if (index_is_ensembl && sum(usable_description) >= 10) {
    labels <- description
    message2("GSEA gene labels: using DESCRIPTION column for ", sum(usable_description), " gene labels.")
  } else if (index_is_ensembl) {
    gtf_map <- read_gtf_gene_map(gtf_path)
    if (NROW(gtf_map)) {
      key <- strip_version(labels)
      labels <- gtf_map$gene_name[match(key, gtf_map$ensembl_gene_id)]
      message2("GSEA gene labels: mapped ", sum(!is.na(labels) & nzchar(labels)), " Ensembl IDs to gene symbols using local GTF.")
    } else {
      labels <- strip_version(labels)
      message2("WARNING: GTF gene map was unavailable; using Ensembl IDs as GSEA labels.")
    }
  } else {
    message2("GSEA gene labels: using normalized-count row labels as gene symbols.")
  }

  if (identical(genome, "mouse")) {
    orth <- read_ortholog_table(ortholog_path)
    if (!NROW(orth)) stop("Mouse GSEA requires the bundled mouse-human ortholog table.")
    human <- orth$human_gene_symbol[match(labels, orth$mouse_gene_symbol)]
    mapped <- sum(!is.na(human) & nzchar(human))
    message2("GSEA gene labels: mapped ", mapped, " mouse genes to human ortholog symbols using local ortholog table.")
    if (mapped < 10) stop("Mouse-to-human ortholog mapping produced fewer than 10 mapped genes.")
    labels <- human
  } else if (identical(genome, "human")) {
    message2("GSEA gene labels: using human gene symbols for pathway analysis.")
  } else {
    message2("WARNING: Unrecognized genome '", genome, "'; using labels directly.")
  }

  collapse_by_gene(expr, labels)
}

parse_gmt_lines <- function(lines, source_name) {
  sets <- list()
  skipped <- 0L
  for (line in lines) {
    if (!nzchar(trimws(line))) next
    parts <- strsplit(line, "\t", fixed = TRUE)[[1]]
    if (length(parts) < 3) {
      skipped <- skipped + 1L
      next
    }
    term <- trimws(parts[[1]])
    genes <- unique(trimws(parts[-c(1, 2)]))
    genes <- genes[nzchar(genes)]
    if (!nzchar(term) || !length(genes)) {
      skipped <- skipped + 1L
      next
    }
    sets[[term]] <- genes
  }
  if (skipped > 0) message2("WARNING: Skipped ", skipped, " malformed gene-set lines from ", source_name, ".")
  sets
}

load_gene_sets <- function(geneset) {
  local_path <- path.expand(geneset)
  if (file.exists(local_path)) {
    con <- if (grepl("\\.gz$", local_path)) gzfile(local_path, "rt") else file(local_path, "rt")
    on.exit(close(con), add = TRUE)
    sets <- parse_gmt_lines(readLines(con, warn = FALSE), local_path)
    if (!length(sets)) stop("No usable gene sets were found in ", local_path)
    message2("Using local GMT gene set file: ", local_path, " (", length(sets), " gene sets)")
    return(sets)
  }

  cache_dir <- file.path(dirname(sub("/+$", "", outpath_pathway)), "_cache", "gene_sets")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_path <- file.path(cache_dir, paste0(safe_gsea_name(geneset), ".gmt"))
  if (file.exists(cache_path) && file.info(cache_path)$size > 0) {
    sets <- parse_gmt_lines(readLines(cache_path, warn = FALSE), cache_path)
    if (length(sets)) {
      message2("Using cached gene set library: ", cache_path, " (", length(sets), " gene sets)")
      return(sets)
    }
  }

  url <- paste0("https://maayanlab.cloud/Enrichr/geneSetLibrary?mode=text&libraryName=", utils::URLencode(geneset, reserved = TRUE))
  ok <- tryCatch({
    utils::download.file(url, cache_path, quiet = TRUE, mode = "wb")
    TRUE
  }, error = function(e) {
    message2("ERROR: Could not download gene set library '", geneset, "': ", conditionMessage(e))
    FALSE
  })
  if (!ok || !file.exists(cache_path) || file.info(cache_path)$size == 0) {
    stop("No cached or downloadable gene set library was available for ", geneset)
  }
  sets <- parse_gmt_lines(readLines(cache_path, warn = FALSE), geneset)
  if (!length(sets)) stop("No usable gene sets were downloaded for ", geneset)
  message2("Using Enrichr gene set library: ", geneset, " (", length(sets), " gene sets)")
  sets
}

signal_to_noise <- function(expr, pos_samples, neg_samples) {
  mat <- as.matrix(expr[, c(pos_samples, neg_samples), drop = FALSE])
  pos <- mat[, pos_samples, drop = FALSE]
  neg <- mat[, neg_samples, drop = FALSE]
  mean_pos <- rowMeans(pos, na.rm = TRUE)
  mean_neg <- rowMeans(neg, na.rm = TRUE)
  sd_pos <- if (ncol(pos) > 1) apply(pos, 1, stats::sd, na.rm = TRUE) else rep(0, nrow(mat))
  sd_neg <- if (ncol(neg) > 1) apply(neg, 1, stats::sd, na.rm = TRUE) else rep(0, nrow(mat))
  denom <- sd_pos + sd_neg
  denom[!is.finite(denom) | denom < 1e-9] <- 1e-9
  ranks <- (mean_pos - mean_neg) / denom
  names(ranks) <- rownames(expr)
  ranks <- ranks[is.finite(ranks) & !is.na(names(ranks)) & nzchar(names(ranks))]
  ranks <- ranks[!duplicated(names(ranks))]
  sort(ranks, decreasing = TRUE)
}

calc_es <- function(stats, genes) {
  genes <- intersect(genes, names(stats))
  n <- length(stats)
  nh <- length(genes)
  if (nh == 0 || nh == n) return(list(es = NA_real_, idx = NA_integer_))
  hits <- names(stats) %in% genes
  weights <- abs(stats)
  hit_sum <- sum(weights[hits])
  if (!is.finite(hit_sum) || hit_sum <= 0) return(list(es = NA_real_, idx = NA_integer_))
  running <- cumsum(ifelse(hits, weights / hit_sum, -1 / (n - nh)))
  idx <- which.max(abs(running))
  list(es = running[[idx]], idx = idx)
}

fallback_gsea <- function(pathways, stats, nperm = 1000L, min_size = 15L, max_size = 500L) {
  pathways <- lapply(pathways, intersect, y = names(stats))
  sizes <- vapply(pathways, length, integer(1))
  pathways <- pathways[sizes >= min_size & sizes <= max_size]
  if (!length(pathways)) stop("No gene sets passed size filtering against the ranked gene list.")
  message2("Running native R permutation GSEA fallback on ", length(pathways), " gene sets with ", nperm, " permutations.")
  rows <- vector("list", length(pathways))
  all_genes <- names(stats)
  i <- 0L
  for (term in names(pathways)) {
    i <- i + 1L
    genes <- pathways[[term]]
    obs <- calc_es(stats, genes)
    k <- length(genes)
    null_es <- replicate(nperm, calc_es(stats, sample(all_genes, k))$es)
    null_es <- null_es[is.finite(null_es)]
    if (!length(null_es) || !is.finite(obs$es)) next
    same_sign <- if (obs$es >= 0) null_es[null_es >= 0] else abs(null_es[null_es < 0])
    denom <- mean(abs(same_sign), na.rm = TRUE)
    if (!is.finite(denom) || denom <= 0) denom <- mean(abs(null_es), na.rm = TRUE)
    nes <- obs$es / denom
    pval <- if (obs$es >= 0) mean(null_es >= obs$es) else mean(null_es <= obs$es)
    leading <- genes[genes %in% names(stats)[seq_len(obs$idx)]]
    rows[[i]] <- data.frame(
      Name = "gsea",
      Term = term,
      ES = obs$es,
      NES = nes,
      `NOM p-val` = max(pval, 1 / (nperm + 1)),
      `FWER p-val` = NA_real_,
      `Tag %` = paste0(length(leading), "/", k),
      `Gene %` = sprintf("%.2f%%", 100 * obs$idx / length(stats)),
      `Lead_genes` = paste(leading, collapse = ";"),
      size = k,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  out <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  out$`FDR q-val` <- stats::p.adjust(out$`NOM p-val`, method = "BH")
  out[, c("Name", "Term", "ES", "NES", "NOM p-val", "FDR q-val", "FWER p-val", "Tag %", "Gene %", "Lead_genes", "size"), drop = FALSE]
}

run_gsea <- function(pathways, stats) {
  nperm <- as.integer(Sys.getenv("CSL_GSEA_PERMUTATIONS", "1000"))
  if (requireNamespace("fgsea", quietly = TRUE)) {
    message2("Running R GSEA with fgsea; permutations: ", nperm)
    suppressWarnings({
      fg <- fgsea::fgsea(pathways = pathways, stats = stats, nperm = nperm, minSize = 15, maxSize = 500)
    })
    if (!NROW(fg)) stop("fgsea returned no enriched pathways after filtering.")
    lead <- vapply(fg$leadingEdge, paste, character(1), collapse = ";")
    out <- data.frame(
      Name = "gsea",
      Term = fg$pathway,
      ES = fg$ES,
      NES = fg$NES,
      `NOM p-val` = fg$pval,
      `FDR q-val` = fg$padj,
      `FWER p-val` = NA_real_,
      `Tag %` = paste0(lengths(fg$leadingEdge), "/", fg$size),
      `Gene %` = NA_character_,
      `Lead_genes` = lead,
      size = fg$size,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    out[order(out$`FDR q-val`, out$`NOM p-val`, -abs(out$NES)), , drop = FALSE]
  } else if (identical(Sys.getenv("CSL_GSEA_ALLOW_BASE_R_FALLBACK", unset = "0"), "1")) {
    message2("WARNING: fgsea is not installed. Using slower base-R fallback because CSL_GSEA_ALLOW_BASE_R_FALLBACK=1.")
    out <- fallback_gsea(pathways, stats, nperm = nperm)
    out[order(out$`FDR q-val`, out$`NOM p-val`, -abs(out$NES)), , drop = FALSE]
  } else {
    stop("The fgsea R package is required for CodeSpringWeb GSEA. Start the app with run_codespringweb.sh so it can install fgsea into your user R library.")
  }
}

save_plots <- function(report, stats, pathways, collection_name) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    message2("WARNING: ggplot2 is not installed; skipping GSEA summary plots.")
    return(invisible(FALSE))
  }
  plot_report <- report[is.finite(report$NES) & is.finite(report$`FDR q-val`), , drop = FALSE]
  top <- head(plot_report[order(plot_report$`FDR q-val`, -abs(plot_report$NES)), , drop = FALSE], 10)
  if (!NROW(top)) return(invisible(FALSE))
  top$Term <- factor(top$Term, levels = rev(top$Term))
  top$neglog10_fdr <- -log10(pmax(as.numeric(top$`FDR q-val`), 1e-300))
  gp <- ggplot2::ggplot(top, ggplot2::aes(x = NES, y = Term, size = neglog10_fdr, color = `FDR q-val`)) +
    ggplot2::geom_vline(xintercept = 0, color = "grey82", linewidth = 0.5) +
    ggplot2::geom_point(alpha = 0.9) +
    ggplot2::scale_color_gradient(low = "#b2182b", high = "#2166ac", trans = "reverse") +
    ggplot2::scale_size_continuous(range = c(3.5, 8.5), name = "-log10(FDR)") +
    ggplot2::labs(x = "Normalized enrichment score", y = NULL, color = "FDR q-val", title = paste("Top GSEA pathways:", collection_name)) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), plot.title = ggplot2::element_text(face = "bold"))
  ggplot2::ggsave(file.path(outpath_pathway, paste0("DotPlot_Top10.", collection_name, ".png")), gp, width = 9.5, height = 6, dpi = 300)

  bp <- ggplot2::ggplot(top, ggplot2::aes(x = NES, y = Term, fill = NES > 0)) +
    ggplot2::geom_col(width = 0.68, alpha = 0.92) +
    ggplot2::scale_fill_manual(values = c("TRUE" = "#b2182b", "FALSE" = "#2166ac"), guide = "none") +
    ggplot2::labs(x = "Normalized enrichment score", y = NULL, title = paste("Enrichment summary:", collection_name)) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), plot.title = ggplot2::element_text(face = "bold"))
  ggplot2::ggsave(file.path(outpath_pathway, paste0("EnrichmentPlot_Top10.", collection_name, ".png")), bp, width = 9.5, height = 6, dpi = 300)

  gsea_dir <- file.path(outpath_pathway, "gsea")
  dir.create(gsea_dir, recursive = TRUE, showWarnings = FALSE)
  for (term in as.character(top$Term)) {
    genes <- pathways[[term]]
    es <- calc_es(stats, genes)
    hit_genes <- intersect(genes, names(stats))
    hits <- names(stats) %in% hit_genes
    weights <- abs(stats)
    running <- cumsum(ifelse(hits, weights / sum(weights[hits]), -1 / (length(stats) - length(hit_genes))))
    df <- data.frame(rank = seq_along(stats), running_ES = running, hit = hits)
    ep <- ggplot2::ggplot(df, ggplot2::aes(rank, running_ES)) +
      ggplot2::geom_hline(yintercept = 0, color = "grey80", linewidth = 0.4) +
      ggplot2::geom_line(color = if (is.finite(es$es) && es$es >= 0) "#b2182b" else "#2166ac", linewidth = 0.9) +
      ggplot2::geom_rug(data = df[df$hit, , drop = FALSE], sides = "b", alpha = 0.35) +
      ggplot2::labs(x = "Rank in ordered gene list", y = "Running enrichment score", title = term) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 11))
    file_term <- gsub("[^A-Za-z0-9._-]+", "_", term)
    ggplot2::ggsave(file.path(gsea_dir, paste0(file_term, ".pdf")), ep, width = 8, height = 5)
  }
  invisible(TRUE)
}

message2("R-native GSEA starting for ", compared, " vs ", refcond, " using ", geneset_name)
message2("R version: ", R.version.string)
message2("Project: ", project_name)
message2("Results root: ", results_root)

design_path <- file.path(design_dir, "design_matrix.txt")
design <- read_table_any(design_path)
if (!"sample" %in% names(design)) names(design)[[1]] <- "sample"
if (!compare_col %in% names(design)) stop("Comparison column not found in design matrix: ", compare_col)
design <- design[as.character(design[[compare_col]]) %in% c(refcond, compared), , drop = FALSE]
if (!NROW(design)) stop("No design matrix rows matched ", refcond, " or ", compared)
samples <- as.character(design$sample)
pos_samples <- samples[as.character(design[[compare_col]]) == compared]
neg_samples <- samples[as.character(design[[compare_col]]) == refcond]
if (length(pos_samples) < 1 || length(neg_samples) < 1) stop("Both comparison groups need at least one sample.")

norm_file <- file.path(deseq_dir, paste0("normalized_counts_", compared, "_vs_", refcond, "(ref).txt"))
counts <- read_table_any(norm_file)
missing_samples <- setdiff(samples, names(counts))
if (length(missing_samples)) stop("Normalized counts file is missing samples: ", paste(missing_samples, collapse = ", "))
expr <- prepare_expression(counts, samples)
expr <- expr[, samples, drop = FALSE]
stats <- signal_to_noise(expr, pos_samples, neg_samples)
if (length(stats) < 10) stop("Fewer than 10 ranked genes are available for GSEA.")
message2("GSEA ranked gene list contains ", length(stats), " genes.")

pathways <- load_gene_sets(geneset_name)
report <- run_gsea(pathways, stats)
collection_name <- safe_gsea_name(geneset_name)

main_report <- file.path(outpath_pathway, "gseapy.gene_set.gsea.report.csv")
collection_report <- file.path(outpath_pathway, paste0("report.gseapy.", collection_name, ".csv"))
utils::write.csv(report, main_report, row.names = FALSE, quote = TRUE)
utils::write.csv(report, collection_report, row.names = FALSE, quote = TRUE)
save_plots(report, stats, pathways, collection_name)

message2("GSEA completed for ", compared, " vs ", refcond, " using ", geneset_name)
message2("Results: ", outpath_pathway)
