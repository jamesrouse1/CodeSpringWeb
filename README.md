# CodeSpringApp

CodeSpringApp is a Shiny-based control center for running, monitoring, and reviewing CodeSpringLab RNA-seq, ATAC-seq, CUT&RUN, and ChIP-seq projects from one server port. It replaces notebook prompts with a button-driven interface for project setup, design-matrix editing, SLURM submission, progress tracking, logs, methods, and assay-specific Results Explorers.

It is designed for shared HPC environments where analyses should continue running after the browser or app is closed.

## Required Companion Repository

CodeSpringApp runs the web interface, but it depends on CodeSpringLab for the analysis scripts, example data, reference settings, and RNA-seq Results Explorer. Install both repositories in your home directory on the server:

```bash
cd ~
git clone https://github.com/jamesrouse1/CodeSpringLab.git
git clone https://github.com/jamesrouse1/CodeSpringApp.git
```

CodeSpringLab is mandatory for CodeSpringApp. The launcher expects to find it at `~/CodeSpringLab` unless you set `CSL_CODESPRINGLAB_ROOT` manually.
CodeSpringApp does not fall back to a developer's or another user's home directory. If the companion repository cannot be found for the current user, startup stops with an explicit path error.

The launcher derives the account and home directory from the operating system rather than inherited `USER` or `HOME` variables. It refuses to start if the CodeSpringApp checkout, CodeSpringLab checkout, or private app-state directory resolves outside that Unix user's home. Run the explicit `--check-config` diagnostic when those verified paths need to be inspected.

Each launch binds only to the server loopback interface and generates a private 64-character access token. Open the complete URL printed by the launcher, including its `?token=...` value. Tunneling to another user's port without that launch's token shows an access-denied page and does not load their projects.

After the last authorized browser session disconnects, the app process stops automatically after five minutes. Pipeline jobs already submitted to SLURM continue running. Set `CSL_WEB_IDLE_SHUTDOWN_SECONDS` before launching to change the grace period; use `0` to disable automatic shutdown.

Saved project configurations, job history, logs, and last-project selection are stored beneath the current Unix user's `~/.codespringweb` directory. They are not loaded from the cloned repositories, so one user does not inherit another user's project menu. Legacy project configs are migrated only when their exact results data path appears in that user's private job history.

## Bundled Example Datasets

The New Project panel provides a **Use Example Dataset** button for all four analyses. The examples use the small FASTQ and manifest files bundled under `CodeSpringLab/scripts_DoNotTouch/test`:

- RNA-seq: `test/fastq` and `test/manifest`
- CUT&RUN: `test/fastq` and `test/manifest_cutrun` (paired targets with explicit matched IgG controls)
- ATAC-seq: `test/fastq_atac` and `test/manifest_atac`
- ChIP-seq: `test/fastq_chip` and `test/manifest_chip`

Example FASTQs remain read-only inputs. When the project is created, CodeSpringApp copies the bundled design matrix into that user's own `~/csl_results/<project>/data/manifest` directory and writes all results beneath the user's selected results root.

## Folder Browser

The server folder browser starts from the current user's home directory and hides dotfiles by default. Folders are selectable for navigation, while visible files are listed separately for confirmation. Typed paths are validated before navigation, and empty, hidden-only, missing, and unreadable folders receive distinct messages.

## Run On The Server

Use the launcher script. It checks required packages, finds an open server port, starts Shiny, and prints the exact SSH tunnel command to run from your laptop.

The default starting port is `8601`. If that port is already taken, the launcher automatically tries `8602`, `8603`, and so on until it finds an open port.

On the server:

```bash
cd ~/CodeSpringApp
./run_codespringweb.sh --check-config
./run_codespringweb.sh
```

The optional first command prints the verified Unix user and all identity-sensitive paths without starting the app. Every printed path should belong to the logged-in user.

From your laptop, copy the SSH command printed by the launcher. It will use the port that was actually started:

```bash
ssh -N -L <PORT>:localhost:<PORT> $USER@bamdev1
```

Then open the complete private URL printed by the launcher:

```text
http://localhost:<PORT>/?token=<PRIVATE_TOKEN>
```

Example launcher output. The port in your terminal may differ if the default port is already busy:

![CodeSpringApp launcher output](docs/assets/launcher_output.png)

## What It Does

- Creates or resumes CodeSpringLab projects from saved project configs.
- Builds and edits design matrices from FASTQ folders.
- Submits real SLURM `sbatch` jobs for RNA-seq tools plus ATAC-seq, CUT&RUN, and ChIP-seq Bowtie2/peak-calling/differential workflows.
- Supports explicit sample selection for sample-level runs and cancellations across RNA-seq, ATAC-seq, CUT&RUN, and ChIP-seq, plus explicit target-to-control pairing for ChIP-seq.
- Shows checked sample lists inside every sample-level tool card; unchecking a sample excludes it from that step without changing the saved experimental design.
- Tracks per-sample and per-comparison progress with completed, running, cancelled, deleted, and likely failed states.
- Keeps untouched or partial sample outputs at `Not started` until SLURM reports a terminal job state; only a finished unsuccessful or incomplete job is classified as failed.
- Resubmits only failed, cancelled, missing, or deleted samples while skipping active and completed jobs.
- Embeds the native CodeSpringLab RNA-seq Results Explorer and provides consistently branded ATAC-seq, CUT&RUN, and ChIP-seq explorers with Overview, QC, Signal & Peaks, differential results, and Files navigation.
- Renders paired-end fragment-size PDFs as standardized in-app images instead of native browser PDF previews, and keeps fragment plots at a consistent display size across samples.
- Sorts numeric-looking table values numerically, including comma-formatted counts, percentages, elapsed times, and human-readable file sizes such as KB/MB/GB.
- Filters CUT&RUN result files by category and sample before choosing an individual file.
- Keeps every result-file view scoped to the currently selected project, including after switching between projects, and reports bigWig/bedGraph normalization in the ATAC-seq, CUT&RUN, and ChIP-seq signal-track views.
- Embeds an IGV genome browser in the ATAC-seq, CUT&RUN, and ChIP-seq Results Explorers. Users can compare up to eight samples, search by gene or genomic locus, and overlay project bigWig signal with optional BED, narrowPeak, or broadPeak calls. Track data are served through session-scoped, byte-range-aware URLs rather than exposing project directories.
- Records logs, methods, tool versions, reference genome selections, and run parameters.

### ChIP-seq workflow

ChIP-seq is a complete CodeSpringLab workflow in the app: Cutadapt and FastQC, single- or paired-end Bowtie2 alignment, duplicate-removed CPM bigWigs, matched-input MACS2 narrow or broad peaks, and target-only DiffBind/DESeq2 comparisons. Every ChIP target must identify an explicit `reference=input` sample through `control_sample`; input libraries are used during MACS2 and are not counted as DiffBind replicates.

New ChIP-seq projects use the current references only: mouse GRCm39/GENCODE M39 or human GRCh38/GENCODE v50. The Results Explorer reports target/input roles, matched controls, alignment retention, MACS2 parameters and peak counts, and completed differential-binding comparisons.

## Preview

### Project Setup

Create new projects, select species/reference builds, browse server folders, and manage project configs/results.

| Project selection | Server folder browser |
| --- | --- |
| ![Project setup](docs/assets/setup.png) | ![Server folder browser](docs/assets/setup_folder_selection.png) |

### Design Matrix

Scan FASTQ folders, include/exclude samples, rename samples, and edit metadata columns directly in the app. A provided matrix loads into the same visible input grid. Projects without a matrix receive blank editable rows, with a button to add five more rows at a time.

![Design matrix editor](docs/assets/design_matrix.png)

### Run Pipeline

Each step has its own parameters, submit button, status panel, sample progress, cancel controls, and data-delete controls. Cancellation dialogs show only active tracked jobs and allow exact sample selection when sample identities are available.

The launcher stops only CodeSpringApp processes owned by the current Unix account. Unrelated R or Shiny processes are not treated as CodeSpringApp sessions.

![Run pipeline](docs/assets/run_readme.png)

### Progress

See workflow-level status and sample-by-step status in a compact matrix.

![Pipeline progress](docs/assets/progress_readme.png)

### Results Explorer

Review assay-appropriate QC, alignment metrics, signal tracks, peaks, differential results, count matrices, PCA, volcano plots, heatmaps, and GSEA outputs without opening another port. ATAC-seq, CUT&RUN, and ChIP-seq design matrices can also be edited and saved from their Results Explorer overview.

![Results explorer QC](docs/assets/results_explorer_qc.png)

![Results explorer heatmap](docs/assets/results_explorer_heatmap.png)

### Logs And Methods

Browse project logs by tool, sample/run, and output/error type. Export project/reference and tools/reference methods tables.

![Logs](docs/assets/logs.png)

## Project Discovery

CodeSpringApp stores and discovers project configs separately for each user under:

```text
~/.codespringweb/project_configs/<analysis>/*.py
```

Project configs inside the cloned CodeSpringLab or CodeSpringApp repositories are not displayed. This prevents example, test, or another user's projects from being distributed with the application.

For a new RNA-seq project, the initial FASTQ and design-matrix fields point to CodeSpringLab's bundled RNA-seq example. The **Use Example Dataset** button loads the matching bundled paths for RNA-seq, CUT&RUN, ATAC-seq, or ChIP-seq. Replace either path when creating a real project.

For new projects, it creates project-local outputs under:

```text
<results_root>/<project_name>/
  data/
  log/
  shiny/
```

## Tabs

- `Setup`: choose analysis/project, create projects, browse server folders, select genome references, and delete configs/results.
- `Design Matrix`: scan FASTQ folders, include/exclude samples, edit metadata, and save a project-local `design_matrix.txt`.
- `Run Pipeline`: submit SLURM jobs with step-specific settings and safeguards.
- `Progress`: monitor step and sample progress, including active, cancelled, deleted, and likely failed states.
- `Results Explorer`: open the native RNA-seq viewer or the assay-specific ATAC-seq, CUT&RUN, or ChIP-seq explorer.
- `Logs`: inspect tool logs and submit logs.
- `Methods`: summarize project metadata, tools, versions, references, and parameters.

## Job Submission

Run buttons submit jobs through `sbatch`, so jobs are owned by SLURM after submission. Closing the browser or stopping Shiny does not cancel jobs already accepted by SLURM.

CodeSpringApp records submitted job metadata under:

```text
~/.codespringweb/
```

Project logs are written under:

```text
<results_root>/<project_name>/log/
```
