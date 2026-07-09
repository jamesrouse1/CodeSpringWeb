# CodeSpringApp

CodeSpringWeb is a Shiny control center for running and reviewing CodeSpringLab projects from one server port.

It provides a button-driven interface for:

- Creating or selecting RNA-seq projects
- Building/editing design matrices from FASTQ folders
- Submitting CodeSpringLab SLURM jobs
- Tracking per-sample and per-comparison progress
- Opening the native CodeSpringLab RNA-seq Results Explorer inside the same app
- Viewing logs, submitted jobs, methods, tool versions, and reference genome details

It discovers existing CodeSpringLab notebook configs from:

```text
<CodeSpringLab>/scripts_DoNotTouch/project_configs/<analysis>/*.py
<CodeSpringLab>/project_configs/<analysis>/*.py
```

## Run On The Server

Use the launcher script. It installs the required R packages into your user library if they are missing, clears stale CodeSpringWeb listeners on the chosen port, starts Shiny in the background, and prints the exact SSH tunnel command for your laptop.

```bash
cd ~/CodeSpringWeb
./run_codespringweb.sh
```

To choose a different port:

```bash
./run_codespringweb.sh 8601
```

From your laptop, run the SSH tunnel printed by the launcher. It will look like this:

```bash
ssh -N -L 8501:localhost:8501 rouse@bamdev1
```

Then open:

```text
http://localhost:8501
```

## Tabs

- `Setup`: select an analysis type and project, create new projects, browse server folders, and manage old project configs/results.
- `Design Matrix`: scan FASTQ folders, include/exclude samples, edit sample names and metadata, and save a project-local `design_matrix.txt`.
- `Progress`: color-coded step cards and sample-by-step progress for completed, active, and not-started work.
- `Run Pipeline`: submit real SLURM `sbatch` jobs for FastQC, cutadapt, STAR, featureCounts, DESeq2, GSEA, RSEM, and Kallisto. Submitted jobs keep running after the app or browser is closed.
- `Results Explorer`: loads CodeSpringLab's native `scripts_DoNotTouch/Shiny/app_server.R`, so the viewer matches the RNA-seq Shiny app while staying inside CodeSpringWeb.
- `Logs`: view submitted jobs and project logs by tool/output/error type.
- `Methods`: summarize tool versions, genome/reference selections, and methods text for completed/submitted work.

## R Packages

Required:

```r
install.packages(c("shiny", "DT", "base64enc", "ggplot2"))
```

The launcher handles these automatically:

- `DT`: editable/searchable/scrollable tables
- `base64enc`: embedded logos/images
- `ggplot2`: publication-style plot support

GSEA runs through the CodeSpringLab Python/GSEApy implementation. On bamdev1 the launcher-submitted GSEA job loads:

```bash
module load BSR
module load Python/3.7.4-GCCcore-8.3.0
```

That module currently provides `gseapy`.

On bamdev1, some R source package installs can fail if the system compiler points at a missing `gcc-annobin` plugin. The launcher avoids this for regular R package installs by using a CodeSpringWeb-specific temporary Makevars file at:

```text
~/.codespringweb/Makevars.codespringweb
```

This is only used during package checks/installs from `run_codespringweb.sh`; it does not overwrite your normal R configuration.

## Job Submission

Run buttons submit jobs through `sbatch`, so jobs are owned by SLURM after submission. Closing the browser or stopping the Shiny app does not cancel jobs already accepted by SLURM.

CodeSpringWeb records submitted job metadata under `~/.codespringweb/` and project logs under:

```text
<results_root>/<project_name>/log/
```

## GSEA

GSEA jobs are submitted as Python jobs using the BSR `Python/3.7.4-GCCcore-8.3.0` module and CodeSpringLab's existing `bulkRNAseq.gseapy_RunPathway()` function.

The app uses:

- DESeq2 normalized counts from `<project>/data/deseq2`
- The selected design-matrix comparison column
- Signal-to-noise ranking
- Gene-set permutations
- Seed `8`
- Enrichr/GMT-style gene-set databases
- The bundled local mouse-human ortholog table for mouse projects

Outputs are written under:

```text
<project>/data/gseapy/<comparison>_vs_<reference>/
```

The folder name remains `gseapy` for compatibility with the existing CodeSpringLab Results Explorer.

### Troubleshoot A Failed GSEA Job

Check the newest GSEA logs for the selected project:

```bash
cd ~/csl_results/<project_name>/log
ls -lhtr *gseapy*.txt
tail -120 "$(ls -t submit_gseapy_*.txt | head -1)"
tail -120 "$(ls -t error_gseapy_*.txt | head -1)"
tail -120 "$(ls -t output_gseapy_*.txt | head -1)"
```

The submit log is written before and after `sbatch`, so it exists even if SLURM rejects the job before creating stdout/stderr files. The output log prints the Python executable, Python version, `gseapy` version, gene-label mapping mode, ranked gene count, and which gene-set database/cache was used.

## Port Cleanup

The launcher stops stale listeners on the requested port before starting the app. The app itself also checks for older CodeSpring/R Shiny sessions on startup unless disabled:

```bash
CSL_WEB_AUTOKILL_SHINY=0 CSL_CODESPRINGLAB_ROOT=~/CodeSpringLab Rscript -e 'shiny::runApp(".", host="0.0.0.0", port=8501)'
```

## Useful Environment Variables

- `CSL_CODESPRINGLAB_ROOT`: path to the CodeSpringLab repo. Default: `~/CodeSpringLab`
- `CSL_WEB_HOST`: Shiny host binding. Default: `0.0.0.0`
- `CSL_WEB_LOG_DIR`: launcher log/pid folder. Default: `~/.codespringweb`
- `CSL_PYTHON_BIN` or `PYTHON_BIN`: optional Python executable override for GSEApy jobs. By default, the GSEA script loads BSR Python and auto-detects a Python with `gseapy`, `pandas`, and `matplotlib`.
