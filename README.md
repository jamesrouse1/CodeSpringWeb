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
- `Progress`: project and sample-level completion tables.
- `Run Pipeline`: conservative Shiny-side run controls with job logging.
- `Results Explorer`: native Shiny viewer for QC, counts, DESeq2, GSEA, plots, PDFs, and files.
- `Logs`: job submissions started from this app.

## R Packages

Required:

```r
install.packages(c("shiny", "DT"))
```

Recommended for inline PNG/PDF rendering:

```r
install.packages("base64enc")
```
