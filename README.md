# CodeSpringLab Web Control Center

This is a free, server-friendly Streamlit web app for running and reviewing CodeSpringLab projects without editing notebook cells or typing repeated y/n prompts.

## Run on the server

```bash
cd ~/CodeSpringLab
streamlit run web_app/app.py --server.address 0.0.0.0 --server.port 8501
```

From your laptop, tunnel the port:

```bash
ssh -N -L 8501:localhost:8501 rouse@bamdev1
```

Then open:

```text
http://localhost:8501
```

## What it does now

- Creates a config file for each project in `~/.codespringlab_web/configs/<analysis>/<project>.json` and keeps a registry in `~/.codespringlab_web/projects.json`
- Supports three workflows: start new analysis, resume existing analysis, and visualize existing results
- Re-detects existing `design_matrix.txt`, FASTQ folders, and completed outputs from `<results_root>/<project>/data`
- Shows a detected project-state checklist and a sample-level progress table so users can see which steps are already complete
- Scans FASTQ folders, prefills filenames/inferred sample names, and builds editable design matrices with user-defined metadata columns
- Writes `design_matrix.txt` into `<results_root>/<project>/data/manifest/`
- Submits RNA-seq jobs with existing CodeSpringLab `sbatch` wrappers from individual step tabs or the Progress tab
- Tracks submitted job IDs and shows scheduler status/logs
- Displays common output tables, FastQC HTML reports, plots, and downloadable files
- Launches the existing RNA-seq Shiny Results Explorer from the Run Pipeline and Outputs tabs

The notebooks and original scripts are still available. This app wraps the existing pipeline instead of replacing it.

## Workflow modes

- `Start new analysis`: choose FASTQ folder, scan samples, build `design_matrix.txt`, then run steps from the Run Pipeline tab.
- `Resume existing analysis`: provide the old project name/results root, then click `Save setup / re-detect paths`; the app detects completed steps and recommends where to continue.
- `Visualize existing results`: provide the project name/results root/design matrix path, then use the Outputs tab or launch the RNA-seq Results Explorer.


## Project configs

The sidebar is organized as `Analysis` then `Project config`. Each saved project gets its own JSON file under:

```text
~/.codespringlab_web/configs/<analysis>/<project>.json
```

The Progress tab reads the project config and existing output folders to show a pipeline-wide status table plus a sample-by-sample table for FastQC, trimming, STAR, Kallisto, and featureCounts. For RNA-seq, the same tab can run a selected step so an older project can be resumed from the point where outputs stop.
