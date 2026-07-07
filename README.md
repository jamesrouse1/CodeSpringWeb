# CodeSpringWeb

CodeSpringWeb is now a Shiny app for running and reviewing CodeSpringLab projects from one server port.

It discovers existing CodeSpringLab notebook configs from:

```text
<CodeSpringLab>/scripts_DoNotTouch/project_configs/<analysis>/*.py
<CodeSpringLab>/project_configs/<analysis>/*.py
```

## Run On The Server

```bash
cd ~/CodeSpringWeb
CSL_CODESPRINGLAB_ROOT=~/CodeSpringLab Rscript -e 'shiny::runApp(".", host="0.0.0.0", port=8501)'
```

From your laptop:

```bash
ssh -N -L 8501:localhost:8501 rouse@bamdev1
```

Then open:

```text
http://localhost:8501
```

## Tabs

- `Setup`: selected project paths and imported config.
- `Design Matrix`: scan FASTQs, edit metadata, and save `design_matrix.txt`.
- `Progress`: color-coded `Active`, `Complete`, and `Not started` step cards plus sample-level progress.
- `Run Pipeline`: professional run cards that submit real SLURM `sbatch` jobs for FastQC, cutadapt, STAR, Kallisto, featureCounts, and DESeq2. Submitted jobs keep running after the app or browser is closed.
- `Results Explorer`: sources CodeSpringLab's native `scripts_DoNotTouch/Shiny/app_server.R`, so the viewer matches the RNA-seq Shiny app instead of maintaining a separate clone.
- `Logs`: job submissions started from this app.

## R Packages

Required:

```r
install.packages("shiny")
```

Optional but nicer:

```r
install.packages(c("DT", "base64enc"))
```

`DT` enables editable/searchable/scrollable tables. Without it, the app still launches with standard Shiny tables, matching the original CodeSpringLab Shiny fallback behavior.


## Job Submission

Run buttons call `sbatch` from the matching CodeSpringLab analysis folder, so jobs are owned by SLURM after submission. Closing the browser or stopping the Shiny app does not cancel jobs that were already accepted by `sbatch`.


## Port Cleanup

By default, CodeSpringWeb checks for older CodeSpring/R Shiny sessions and stops them before starting. To disable that behavior:

```bash
CSL_WEB_AUTOKILL_SHINY=0 CSL_CODESPRINGLAB_ROOT=~/CodeSpringLab Rscript -e 'shiny::runApp(".", host="0.0.0.0", port=8501)'
```
## Server launcher

On bamdev1, start the app with:

```bash
cd ~/CodeSpringWeb
./run_codespringweb.sh
```

The script installs the required `DT` R package into your user library if it is missing, starts Shiny in the background, writes logs to `~/.codespringweb/`, and prints the one SSH tunnel command to run on your laptop.

