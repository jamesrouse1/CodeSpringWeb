from __future__ import annotations

import base64
import json
import os
import re
import shutil
import socket
import subprocess
import time
from datetime import datetime
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Tuple

import pandas as pd
import streamlit as st
import streamlit.components.v1 as components


REPO_ROOT = Path(__file__).resolve().parents[1]
SCRIPTS = REPO_ROOT / "scripts_DoNotTouch"
APP_HOME = Path(os.environ.get("CSL_WEB_HOME", "~/.codespringlab_web")).expanduser()
PROJECTS_PATH = APP_HOME / "projects.json"
JOBS_PATH = APP_HOME / "jobs.json"
CONFIGS_DIR = APP_HOME / "configs"
FASTQ_SUFFIXES = [".fastq.gz", ".fq.gz", ".fastq", ".fq"]

GENOME_RESOURCES = {
    "mouse": {
        "star_index": "/grid/bsr/data/data/utama/genome/GRCm39_M29_gencode/GRCm39_M29_gencode_starindex",
        "kallisto_index": "/grid/bsr/data/data/utama/genome/GRCm39_M29_gencode/gencode.vM29.transcripts.idx",
        "gtf": "/grid/bsr/data/data/utama/genome/GRCm39_M29_gencode/gencode.vM29.annotation.gtf",
        "strand_bed": "/grid/bsr/data/data/utama/genome/GRCm39_M29_gencode/gencode.vM29.annotation_forStrandDetect_geneID.bed",
    },
    "human": {
        "star_index": "/grid/bsr/data/data/utama/genome/hg38_p13_gencode/hg38_p13_gencode_rel42_all_starindex",
        "kallisto_index": "/grid/bsr/data/data/utama/genome/hg38_p13_gencode/gencode.v45.transcripts.idx",
        "gtf": "/grid/bsr/data/data/utama/genome/hg38_p13_gencode/gencode.v42.chr_patch_hapl_scaff.annotation.gtf",
        "strand_bed": "/grid/bsr/data/data/utama/genome/hg38_p13_gencode/gencode.v42.chr_patch_hapl_scaff.annotation_forStrandDetect_geneID.bed",
    },
}


def now_stamp() -> str:
    return datetime.now().strftime("%Y-%m-%d %H:%M:%S")


def ensure_home() -> None:
    APP_HOME.mkdir(parents=True, exist_ok=True)
    CONFIGS_DIR.mkdir(parents=True, exist_ok=True)


def read_json(path: Path, default):
    ensure_home()
    if not path.exists():
        return default
    try:
        return json.loads(path.read_text())
    except Exception:
        return default


def write_json(path: Path, data) -> None:
    ensure_home()
    path.write_text(json.dumps(data, indent=2, sort_keys=True))


def analysis_slug(value: str) -> str:
    raw = str(value or "RNA-seq").lower()
    if "atac" in raw:
        return "atac_seq"
    if "chip" in raw:
        return "chip_seq"
    return "rna_seq"


def project_id(project: dict) -> str:
    return f"{analysis_slug(project.get('analysis_type', 'RNA-seq'))}/{clean_name(project.get('name', 'project'), 'project')}"


def project_config_path(project: dict) -> Path:
    return CONFIGS_DIR / analysis_slug(project.get("analysis_type", "RNA-seq")) / f"{clean_name(project.get('name', 'project'), 'project')}.json"


def normalize_project(project: dict) -> dict:
    project = dict(project or {})
    project["name"] = clean_name(project.get("name", "project"), "project")
    project["analysis_type"] = project.get("analysis_type", "RNA-seq")
    project["project_id"] = project_id(project)
    return project


def load_projects() -> Dict[str, dict]:
    registry = read_json(PROJECTS_PATH, {})
    projects: Dict[str, dict] = {}
    if isinstance(registry, dict):
        for key, value in registry.items():
            if isinstance(value, dict):
                project = normalize_project(value)
                projects[project_id(project)] = project

    ensure_home()
    if CONFIGS_DIR.exists():
        for config_path in sorted(CONFIGS_DIR.glob("*/*.json")):
            try:
                project = normalize_project(json.loads(config_path.read_text()))
            except Exception:
                continue
            projects[project_id(project)] = project
    return projects


def save_project(project: dict) -> dict:
    projects = load_projects()
    project = normalize_project(project)
    project["updated_at"] = now_stamp()
    if not project.get("created_at"):
        project["created_at"] = project["updated_at"]
    projects[project_id(project)] = project
    write_json(PROJECTS_PATH, projects)
    config_path = project_config_path(project)
    config_path.parent.mkdir(parents=True, exist_ok=True)
    config_path.write_text(json.dumps(project, indent=2, sort_keys=True))
    return project


def load_jobs() -> List[dict]:
    return read_json(JOBS_PATH, [])


def save_job(job: dict) -> None:
    jobs = load_jobs()
    jobs.append(job)
    write_json(JOBS_PATH, jobs)


def project_jobs(project) -> List[dict]:
    if isinstance(project, dict):
        pid = project_id(project)
        name = project.get("name")
        analysis = project.get("analysis_type")
        return [
            j for j in load_jobs()
            if j.get("project_id") == pid
            or (j.get("project") == name and j.get("analysis_type", analysis) == analysis)
        ]
    return [j for j in load_jobs() if j.get("project") == project]


def clean_name(value: str, fallback: str = "sample") -> str:
    cleaned = re.sub(r"[^A-Za-z0-9_]+", "_", str(value).strip()).strip("_")
    return cleaned or fallback


def parse_metadata_columns(value: str, fallback: Optional[List[str]] = None) -> List[str]:
    fallback = fallback or ["treatment"]
    cols = [clean_name(x, "condition") for x in str(value or "").split(",") if x.strip()]
    cols = [c for c in cols if c not in ["sample", "filename", "include", "detected_status"]]
    deduped = []
    for col in cols:
        if col not in deduped:
            deduped.append(col)
    return deduped or fallback


def sync_design_editor_columns(df: pd.DataFrame, metadata_cols: List[str]) -> pd.DataFrame:
    if df.empty:
        df = pd.DataFrame(columns=["include", "sample", "filename", "detected_status"])
    df = df.copy()
    for col in ["include", "sample", "filename", "detected_status"]:
        if col not in df.columns:
            df[col] = True if col == "include" else ""
    for col in metadata_cols:
        if col not in df.columns:
            df[col] = ""
    ordered = ["include", "sample"] + metadata_cols + ["filename", "detected_status"]
    extra = [c for c in df.columns if c not in ordered]
    return df[ordered + extra]


def project_root(project: dict) -> Path:
    return Path(project["results_root"]).expanduser() / project["name"]


def data_dir(project: dict) -> Path:
    return project_root(project) / "data"


def log_dir(project: dict) -> Path:
    return project_root(project) / "log"


def manifest_dir(project: dict) -> Path:
    return data_dir(project) / "manifest"


def design_matrix_path(project: dict) -> Path:
    override = str(project.get("design_matrix_path", "")).strip()
    if override:
        path = Path(override).expanduser()
        return path if path.name == "design_matrix.txt" else path / "design_matrix.txt"
    return manifest_dir(project) / "design_matrix.txt"


def candidate_design_matrix_path(project: dict) -> Path:
    candidates = [
        design_matrix_path(project),
        data_dir(project) / "manifest" / "design_matrix.txt",
        data_dir(project) / "design_matrix" / "design_matrix.txt",
        data_dir(project) / "design_matrix.txt",
    ]
    for candidate in candidates:
        if candidate.exists():
            return candidate
    return candidates[0]


def infer_fastq_dir(project: dict) -> str:
    current = str(project.get("fastq_dir", "")).strip()
    if current and Path(current).expanduser().is_dir():
        return str(Path(current).expanduser())
    candidates = [
        data_dir(project) / "fastq",
        data_dir(project) / "cutadapt",
    ]
    for candidate in candidates:
        if candidate.is_dir():
            return str(candidate)
    return current


def infer_metadata_columns_from_design(project: dict) -> List[str]:
    design_path = candidate_design_matrix_path(project)
    if not design_path.exists():
        return project.get("metadata_columns", ["treatment"])
    try:
        design = pd.read_table(design_path, nrows=5)
    except Exception:
        return project.get("metadata_columns", ["treatment"])
    cols = [c for c in design.columns if c not in ["sample", "filename"]]
    return cols or project.get("metadata_columns", ["treatment"])


def apply_project_inference(project: dict) -> dict:
    project["fastq_dir"] = infer_fastq_dir(project)
    inferred_design = candidate_design_matrix_path(project)
    if inferred_design.exists():
        project["design_matrix_path"] = str(inferred_design)
    project["metadata_columns"] = infer_metadata_columns_from_design(project)
    return project


def count_files(folder: Path, patterns: Iterable[str]) -> int:
    if not folder.exists():
        return 0
    total = 0
    for pattern in patterns:
        total += len(list(folder.rglob(pattern)))
    return total


def project_step_status(project: dict) -> pd.DataFrame:
    root = data_dir(project)
    design_path = candidate_design_matrix_path(project)
    fastq_dir = Path(str(project.get("fastq_dir", ""))).expanduser() if project.get("fastq_dir") else root / "fastq"
    rows = [
        {
            "step": "Setup",
            "status": "Complete" if project.get("name") and project.get("results_root") else "Needs attention",
            "evidence": str(project_root(project)),
            "count": "",
        },
        {
            "step": "Design matrix",
            "status": "Complete" if design_path.exists() else "Missing",
            "evidence": str(design_path),
            "count": "",
        },
        {
            "step": "FASTQ reads",
            "status": "Complete" if fastq_dir.is_dir() and len(fastq_files(str(fastq_dir))) > 0 else "Optional/missing",
            "evidence": str(fastq_dir),
            "count": len(fastq_files(str(fastq_dir))) if fastq_dir.is_dir() else 0,
        },
        {
            "step": "FastQC",
            "status": "Complete" if count_files(root / "fastqc", ["*.html"]) > 0 or count_files(root / "fastqc_cutadapt", ["*.html"]) > 0 else "Not found",
            "evidence": str(root / "fastqc"),
            "count": count_files(root / "fastqc", ["*.html"]) + count_files(root / "fastqc_cutadapt", ["*.html"]),
        },
        {
            "step": "Cutadapt",
            "status": "Complete" if count_files(root / "cutadapt", ["*.fastq.gz", "*.fq.gz", "*.fastq", "*.fq"]) > 0 else "Not found",
            "evidence": str(root / "cutadapt"),
            "count": count_files(root / "cutadapt", ["*.fastq.gz", "*.fq.gz", "*.fastq", "*.fq"]),
        },
        {
            "step": "STAR",
            "status": "Complete" if count_files(root / "star", ["*Aligned.sortedByCoord.out.bam"]) > 0 else "Not found",
            "evidence": str(root / "star"),
            "count": count_files(root / "star", ["*Aligned.sortedByCoord.out.bam"]),
        },
        {
            "step": "Kallisto",
            "status": "Complete" if count_files(root / "kallisto", ["abundance.tsv"]) > 0 else "Not found",
            "evidence": str(root / "kallisto"),
            "count": count_files(root / "kallisto", ["abundance.tsv"]),
        },
        {
            "step": "featureCounts",
            "status": "Complete" if count_files(root / "featurecounts", ["*_counts.txt"]) > 0 else "Not found",
            "evidence": str(root / "featurecounts"),
            "count": count_files(root / "featurecounts", ["*_counts.txt"]),
        },
        {
            "step": "Count matrix",
            "status": "Complete" if (root / "counts" / "count_matrix.txt").exists() else "Not found",
            "evidence": str(root / "counts" / "count_matrix.txt"),
            "count": "",
        },
        {
            "step": "DESeq2",
            "status": "Complete" if count_files(root / "deseq2", ["DEG*.txt", "*normalized*.txt"]) > 0 else "Not found",
            "evidence": str(root / "deseq2"),
            "count": count_files(root / "deseq2", ["DEG*.txt", "*normalized*.txt"]),
        },
        {
            "step": "Pathway analysis",
            "status": "Complete" if count_files(root / "gseapy", ["*.csv", "*.txt", "*.png", "*.pdf"]) > 0 else "Not found",
            "evidence": str(root / "gseapy"),
            "count": count_files(root / "gseapy", ["*.csv", "*.txt", "*.png", "*.pdf"]),
        },
    ]
    df = pd.DataFrame(rows)
    df["count"] = df["count"].astype(str)
    df.insert(0, "ready", df["status"].map(lambda x: "yes" if x == "Complete" else ""))
    return df


def next_recommended_step(project: dict) -> str:
    status = project_step_status(project).set_index("step")["status"].to_dict()
    if status.get("Design matrix") != "Complete":
        return "Design Matrix"
    if status.get("STAR") != "Complete" and status.get("Kallisto") != "Complete":
        if status.get("FastQC") != "Complete" and status.get("FASTQ reads") == "Complete":
            return "FastQC"
        return "STAR"
    if status.get("featureCounts") != "Complete" and status.get("Count matrix") != "Complete":
        return "featureCounts"
    if status.get("Count matrix") != "Complete":
        return "Count matrix"
    if status.get("DESeq2") != "Complete":
        return "DESeq2"
    return "Results / Shiny Viewer"


def render_step_status(project: dict) -> None:
    status = project_step_status(project)
    st.dataframe(
        status,
        use_container_width=True,
        hide_index=True,
        column_config={
            "ready": st.column_config.TextColumn("ready", width="small"),
            "step": st.column_config.TextColumn("step", width="medium"),
            "status": st.column_config.TextColumn("status", width="small"),
            "count": st.column_config.TextColumn("count", width="small"),
            "evidence": st.column_config.TextColumn("path/evidence", width="large"),
        },
    )


def resume_step_details(project: dict, step: str) -> Tuple[str, List[str]]:
    root = data_dir(project)
    details = {
        "FastQC": (
            "Run read-level QC from raw or trimmed FASTQs.",
            [str(Path(project.get("fastq_dir", "")).expanduser()), str(root / "fastqc")],
        ),
        "Trim": (
            "Trim adapters with cutadapt and write cleaned reads.",
            [str(Path(project.get("fastq_dir", "")).expanduser()), str(root / "cutadapt")],
        ),
        "STAR": (
            "Align FASTQs or trimmed reads to the selected genome.",
            [str(candidate_design_matrix_path(project)), str(root / "star")],
        ),
        "Kallisto": (
            "Quantify transcript abundance from FASTQs or trimmed reads.",
            [str(candidate_design_matrix_path(project)), str(root / "kallisto")],
        ),
        "featureCounts": (
            "Count aligned STAR BAM files by gene_id or gene_name.",
            [str(root / "star"), str(root / "featurecounts")],
        ),
        "Count matrix": (
            "Merge featureCounts sample files into count_matrix.txt.",
            [str(root / "featurecounts"), str(root / "counts" / "count_matrix.txt")],
        ),
        "DESeq2": (
            "Run differential expression from count_matrix.txt and design_matrix.txt.",
            [str(root / "counts" / "count_matrix.txt"), str(candidate_design_matrix_path(project)), str(root / "deseq2")],
        ),
        "Shiny Viewer": (
            "Launch the existing CodeSpringLab RNA-seq Results Explorer for completed outputs.",
            [str(root), str(candidate_design_matrix_path(project))],
        ),
    }
    return details.get(step, ("Resume from this step.", [str(root)]))


def render_resume_card(project: dict, step: str) -> None:
    description, paths = resume_step_details(project, step)
    st.markdown("**Resume guidance**")
    st.caption(description)
    st.code("\n".join(paths))


def render_shiny_launcher(project: dict, key_prefix: str) -> None:
    st.markdown("**RNA-seq Results Explorer**")
    st.caption("Launch the existing Shiny-style results viewer for this project. This is the closest match to the visualization flow from the notebook.")
    data_ready = data_dir(project).exists()
    design_ready = candidate_design_matrix_path(project).exists()
    if not data_ready:
        st.warning("The project data folder was not found. Check Results root and Project name in Setup.")
    if not design_ready:
        st.warning("The design matrix was not found. Provide it in Setup so the Results Explorer can label samples correctly.")
    col1, col2 = st.columns([1, 3])
    with col1:
        port = st.number_input(
            "Viewer port",
            min_value=3838,
            max_value=3900,
            value=available_port(),
            key=key_prefix+"_viewer_port",
        )
    with col2:
        st.code("ssh -N -L {0}:localhost:{0} rouse@bamdev1".format(int(port)))
    if st.button(
        "Launch RNA-seq Results Explorer",
        type="primary",
        key=key_prefix+"_launch_viewer",
        disabled=not (data_ready and design_ready),
    ):
        job_submission_result(submit_shiny(project, int(port)))
        st.info(f"Tunnel from your laptop: ssh -N -L {int(port)}:localhost:{int(port)} rouse@bamdev1")
        st.markdown(f"Then open [http://localhost:{int(port)}](http://localhost:{int(port)})")


def split_fastq_suffix(filename: str) -> Tuple[str, str]:
    name = Path(str(filename).strip()).name
    lower = name.lower()
    for suffix in FASTQ_SUFFIXES:
        if lower.endswith(suffix):
            return name[:-len(suffix)], name[-len(suffix):]
    return name, ""


def mate_fastq_name(filename: str, mate: str) -> Optional[str]:
    stem, suffix = split_fastq_suffix(filename)
    if str(mate) == "2":
        replacements = [
            (r"([._-]R)1([._-]?\d*)$", r"\g<1>2\2"),
            (r"([._-])1$", r"\g<1>2"),
        ]
    else:
        replacements = [
            (r"([._-]R)2([._-]?\d*)$", r"\g<1>1\2"),
            (r"([._-])2$", r"\g<1>1"),
        ]
    for pattern, repl in replacements:
        new_stem, n = re.subn(pattern, repl, stem, flags=re.IGNORECASE)
        if n:
            return new_stem + suffix
    return None


def infer_sample_name(filename: str) -> str:
    stem, _suffix = split_fastq_suffix(filename)
    stem = re.sub(r"([._-]R)[12]([._-]?\d*)$", "", stem, flags=re.IGNORECASE)
    stem = re.sub(r"([._-])[12]$", "", stem)
    return clean_name(stem)


def fastq_files(folder: str) -> List[str]:
    path = Path(folder).expanduser()
    if not path.is_dir():
        return []
    return sorted([
        p.name for p in path.iterdir()
        if p.is_file() and p.name.lower().endswith(tuple(FASTQ_SUFFIXES))
    ])


def scan_fastqs(folder: str, paired: bool) -> pd.DataFrame:
    files = fastq_files(folder)
    file_set = set(files)
    rows = []
    used = set()
    if paired:
        for r1 in files:
            r2 = mate_fastq_name(r1, "2")
            if not r2:
                continue
            if r2 in file_set:
                rows.append({
                    "include": True,
                    "sample": infer_sample_name(r1),
                    "filename": f"{r1},{r2}",
                    "detected_status": "paired",
                })
                used.add(r1)
                used.add(r2)
            else:
                rows.append({
                    "include": False,
                    "sample": infer_sample_name(r1),
                    "filename": r1,
                    "detected_status": "missing R2",
                })
                used.add(r1)
    else:
        for name in files:
            mate1 = mate_fastq_name(name, "1")
            if mate1 and mate1 in file_set:
                continue
            rows.append({
                "include": True,
                "sample": infer_sample_name(name),
                "filename": name,
                "detected_status": "single",
            })
            used.add(name)
    return pd.DataFrame(rows)


def sample_fastq_pairs(project: dict, trimmed: bool = False) -> List[Tuple[str, Path, Path]]:
    design = read_design(project)
    base = data_dir(project) / "cutadapt" if trimmed else Path(project["fastq_dir"]).expanduser()
    pairs = []
    paired = bool(project.get("paired_end", True))
    for _, row in design.iterrows():
        parts = [x.strip() for x in str(row["filename"]).split(",") if x.strip()]
        if not parts:
            continue
        r1_name = Path(parts[0]).name if trimmed else parts[0]
        r1 = Path(r1_name).expanduser() if Path(r1_name).is_absolute() else base / r1_name
        if paired:
            if len(parts) > 1:
                r2_name = Path(parts[1]).name if trimmed else parts[1]
            else:
                inferred = mate_fastq_name(parts[0], "2")
                r2_name = Path(inferred).name if (trimmed and inferred) else inferred
            r2 = Path(r2_name).expanduser() if r2_name and Path(r2_name).is_absolute() else base / str(r2_name)
        else:
            r2 = r1
        pairs.append((str(row["sample"]), r1, r2))
    return pairs


def write_design(project: dict, edited: pd.DataFrame, metadata_cols: List[str]) -> Path:
    out = design_matrix_path(project)
    out.parent.mkdir(parents=True, exist_ok=True)
    keep = edited[edited["include"] == True].copy()
    if keep.empty:
        raise ValueError("No samples are included.")
    keep["sample"] = keep["sample"].map(clean_name)
    columns = ["sample"] + metadata_cols + ["filename"]
    for col in metadata_cols:
        if col not in keep.columns:
            keep[col] = "NA"
        keep[col] = keep[col].fillna("NA").astype(str).str.replace(r"\s+", "_", regex=True)
    keep[columns].to_csv(out, sep="\t", index=False)
    project["design_matrix_path"] = str(out)
    save_project(project)
    return out


def read_design(project: dict) -> pd.DataFrame:
    path = design_matrix_path(project)
    if path.exists():
        return pd.read_table(path)
    return pd.DataFrame()

def fastqc_html_name(read_name: str) -> str:
    stem, _suffix = split_fastq_suffix(Path(str(read_name)).name)
    return stem + "_fastqc.html"


def path_status(path: Path) -> str:
    return "ready" if path.exists() else "missing"


def sample_progress(project: dict) -> pd.DataFrame:
    design = read_design(project)
    if design.empty:
        return pd.DataFrame(columns=["sample", "FastQC", "Trim", "STAR", "Kallisto", "featureCounts"])
    root = data_dir(project)
    rows = []
    for _, row in design.iterrows():
        sample = str(row.get("sample", "")).strip()
        reads = [x.strip() for x in str(row.get("filename", "")).split(",") if x.strip()]
        read_names = [Path(x).name for x in reads]
        raw_fastqc = all((root / "fastqc" / fastqc_html_name(x)).exists() for x in read_names) if read_names else False
        trimmed_fastqc = all((root / "fastqc_cutadapt" / fastqc_html_name(x)).exists() for x in read_names) if read_names else False
        trimmed = all((root / "cutadapt" / Path(x).name).exists() for x in read_names) if read_names else False
        star_bam = root / "star" / sample / f"{sample}Aligned.sortedByCoord.out.bam"
        kallisto_abundance = root / "kallisto" / sample / "abundance.tsv"
        featurecounts_file = root / "featurecounts" / sample / f"{sample}_counts.txt"
        rows.append({
            "sample": sample,
            "FastQC": "ready" if raw_fastqc or trimmed_fastqc else "missing",
            "Trim": "ready" if trimmed else "missing",
            "STAR": path_status(star_bam),
            "Kallisto": path_status(kallisto_abundance),
            "featureCounts": path_status(featurecounts_file),
        })
    return pd.DataFrame(rows)


def run_selected_step(project: dict, step: str, use_trimmed: bool = False, feature: str = "gene_id", reference: str = "", comparison: str = "", redundant: str = "NoRedundant"):
    if step == "FastQC":
        return submit_fastqc(project, trimmed=use_trimmed)
    if step == "Trim":
        return submit_cutadapt(
            project,
            "AGATCGGAAGAGCACACGTCTGAACTCCAGTCA",
            "AGATCGGAAGAGCGTCGTGTAGGGAAAGAGTGT",
            "20",
        )
    if step == "STAR":
        return submit_star(project, use_trimmed=use_trimmed)
    if step == "Kallisto":
        return submit_kallisto(project, use_trimmed=use_trimmed)
    if step == "featureCounts":
        return submit_featurecounts(project, feature=feature)
    if step == "Count matrix":
        out = create_featurecounts_matrix(project)
        job = {
            "project": project["name"],
            "project_id": project_id(project),
            "analysis_type": project.get("analysis_type", "RNA-seq"),
            "step": "Count matrix",
            "command": "create_featurecounts_matrix",
            "stdout": "",
            "stderr": "",
            "submitted_at": now_stamp(),
            "job_id": None,
            "return_code": 0,
            "submit_output": f"Wrote {out}",
        }
        save_job(job)
        return job
    if step == "DESeq2":
        return submit_deseq2(project, reference, comparison, redundant)
    raise ValueError(f"Unsupported step: {step}")


def progress_tab(project: dict) -> None:
    st.subheader("Progress")
    st.caption("Select an analysis, open a project config, review completed outputs, then run or resume one step at a time.")
    c1, c2, c3 = st.columns(3)
    c1.metric("Analysis", project.get("analysis_type", "RNA-seq"))
    c2.metric("Project", project.get("name", "project"))
    c3.metric("Recommended next step", next_recommended_step(project))

    st.markdown("**Pipeline Status**")
    render_step_status(project)

    st.markdown("**Sample Progress**")
    sample_df = sample_progress(project)
    if sample_df.empty:
        st.info("No design matrix was found yet, so sample-level progress cannot be shown.")
    else:
        st.dataframe(sample_df, use_container_width=True, hide_index=True, height=360)
        st.download_button(
            "Download sample progress",
            data=sample_df.to_csv(index=False).encode(),
            file_name=f"{project.get('name', 'project')}_sample_progress.csv",
            key="download_sample_progress_"+project_id(project),
        )

    if project.get("analysis_type") != "RNA-seq":
        st.info("Run buttons are implemented for RNA-seq first. ATAC-seq and ChIP-seq configs still get status detection and output browsing.")
        return

    st.markdown("**Run One Step**")
    steps = ["FastQC", "Trim", "STAR", "Kallisto", "featureCounts", "Count matrix", "DESeq2", "Shiny Viewer"]
    recommended = next_recommended_step(project)
    default = steps.index(recommended) if recommended in steps else 0
    selected_step = st.selectbox("Step", steps, index=default, key="progress_step_"+project_id(project))
    if selected_step == "Shiny Viewer":
        render_shiny_launcher(project, "progress_tab_"+project_id(project))
        return

    use_trimmed = False
    feature = "gene_id"
    reference = ""
    comparison = ""
    redundant = "NoRedundant"
    if selected_step in ["FastQC", "STAR", "Kallisto"]:
        use_trimmed = st.toggle("Use trimmed reads", value=selected_step != "FastQC", key="progress_trimmed_"+project_id(project))
    if selected_step == "featureCounts":
        feature = st.selectbox("Feature attribute", ["gene_id", "gene_name"], key="progress_feature_"+project_id(project))
    if selected_step == "DESeq2":
        design = read_design(project)
        metadata_cols = [c for c in design.columns if c not in ["sample", "filename"]]
        if metadata_cols:
            design_col = st.selectbox("Design column", metadata_cols, key="progress_deseq_column_"+project_id(project))
            choices = sorted([x for x in design[design_col].dropna().astype(str).unique().tolist() if x])
            col1, col2, col3 = st.columns(3)
            with col1:
                reference = st.selectbox("Reference", choices or ["control"], key="progress_deseq_ref_"+project_id(project))
            with col2:
                comparison = st.selectbox("Comparison", choices or ["treated"], key="progress_deseq_comp_"+project_id(project))
            with col3:
                redundant = st.selectbox("Redundant covariate", ["NoRedundant"] + [c for c in metadata_cols if c != design_col], key="progress_deseq_redundant_"+project_id(project))
        else:
            st.warning("DESeq2 needs a design matrix with at least one metadata column.")
            return

    if st.button("Run selected step", type="primary", key="progress_run_"+project_id(project)):
        try:
            if selected_step == "DESeq2" and reference == comparison:
                st.error("Reference and comparison must be different.")
            else:
                job_submission_result(run_selected_step(project, selected_step, use_trimmed, feature, reference, comparison, redundant))
        except Exception as exc:
            st.error(str(exc))



def shell_quote(args: Iterable[object]) -> str:
    return " ".join(subprocess.list2cmdline([str(a)]) for a in args)


def command_exists(command: str) -> bool:
    return shutil.which(command) is not None


def scheduler_available() -> bool:
    return command_exists("sbatch")


def format_size(num_bytes: int) -> str:
    units = ["B", "KB", "MB", "GB", "TB"]
    value = float(num_bytes)
    for unit in units:
        if value < 1024 or unit == units[-1]:
            return f"{value:.1f} {unit}" if unit != "B" else f"{int(value)} B"
        value /= 1024
    return str(num_bytes)


def run_command(args: List[object], cwd: Optional[Path] = None) -> subprocess.CompletedProcess:
    return subprocess.run(
        [str(x) for x in args],
        cwd=str(cwd or REPO_ROOT),
        text=True,
        capture_output=True,
        check=False,
    )


def parse_job_id(output: str) -> Optional[str]:
    match = re.search(r"Submitted batch job\s+(\d+)", output or "")
    if match:
        return match.group(1)
    return None


def submit_sbatch(project: dict, step: str, script: Path, script_args: List[object], log_name: str) -> dict:
    log_dir(project).mkdir(parents=True, exist_ok=True)
    stdout = log_dir(project) / f"output_{log_name}.txt"
    stderr = log_dir(project) / f"error_{log_name}.txt"
    cmd = ["sbatch", "-e", stderr, "-o", stdout, script] + script_args
    job = {
        "project": project["name"],
        "project_id": project_id(project),
        "analysis_type": project.get("analysis_type", "RNA-seq"),
        "step": step,
        "command": shell_quote(cmd),
        "stdout": str(stdout),
        "stderr": str(stderr),
        "submitted_at": now_stamp(),
        "job_id": None,
        "return_code": None,
        "submit_output": "",
    }
    if not command_exists("sbatch"):
        job["submit_output"] = "sbatch was not found on this machine. Run this app on the server to submit jobs."
        save_job(job)
        return job
    result = run_command(cmd, cwd=REPO_ROOT)
    output = (result.stdout or "") + (result.stderr or "")
    job["return_code"] = result.returncode
    job["submit_output"] = output.strip()
    job["job_id"] = parse_job_id(output)
    save_job(job)
    return job


def scheduler_status(job_id: Optional[str]) -> str:
    if not job_id:
        return "not submitted"
    if command_exists("squeue"):
        result = run_command(["squeue", "-j", job_id, "-h", "-o", "%T"])
        status = (result.stdout or "").strip()
        if status:
            return status
    if command_exists("sacct"):
        result = run_command(["sacct", "-j", job_id, "--format=State", "-n", "-P"])
        status = (result.stdout or "").strip().splitlines()
        if status:
            return status[0].split("|")[0]
    return "completed/unknown"


def read_tail(path: str, n: int = 120) -> str:
    p = Path(path)
    if not p.exists():
        return ""
    lines = p.read_text(errors="replace").splitlines()
    return "\n".join(lines[-n:])


def genome_resources(project: dict) -> dict:
    genome = str(project.get("genome", "mouse")).lower()
    return GENOME_RESOURCES.get(genome, GENOME_RESOURCES["mouse"])


def submit_fastqc(project: dict, trimmed: bool = False) -> List[dict]:
    outdir = data_dir(project) / ("fastqc_cutadapt" if trimmed else "fastqc")
    outdir.mkdir(parents=True, exist_ok=True)
    files = []
    for _sample, r1, r2 in sample_fastq_pairs(project, trimmed=trimmed):
        files.append(r1)
        if project.get("paired_end", True):
            files.append(r2)
    jobs = []
    for read in sorted(set(files)):
        jobs.append(submit_sbatch(
            project,
            "FastQC",
            SCRIPTS / "FastQC" / "qsub_fastqc.sh",
            [read, outdir, project["name"]],
            "fastQC",
        ))
    return jobs


def submit_cutadapt(project: dict, adapter1: str, adapter2: str, min_length: str) -> List[dict]:
    outdir = data_dir(project) / "cutadapt"
    outdir.mkdir(parents=True, exist_ok=True)
    paired = bool(project.get("paired_end", True))
    script = SCRIPTS / ("cutadapt_PE/qsub_cutadapt_PE.sh" if paired else "cutadapt_SE/qsub_cutadapt_SE.sh")
    jobs = []
    for _sample, r1, r2 in sample_fastq_pairs(project, trimmed=False):
        trimmed1 = outdir / r1.name
        trimmed2 = outdir / r2.name
        jobs.append(submit_sbatch(
            project,
            "Cutadapt",
            script,
            [min_length, adapter1, adapter2, trimmed1, trimmed2, r1, r2, project["name"]],
            "cutadapt",
        ))
    return jobs


def submit_star(project: dict, use_trimmed: bool = False) -> List[dict]:
    resources = genome_resources(project)
    outdir = data_dir(project) / "star"
    outdir.mkdir(parents=True, exist_ok=True)
    paired = bool(project.get("paired_end", True))
    script = SCRIPTS / ("STAR/qsub_star_PE.sh" if paired else "STAR/qsub_star_SE.sh")
    jobs = []
    for sample, r1, r2 in sample_fastq_pairs(project, trimmed=use_trimmed):
        sample_dir = outdir / sample
        sample_dir.mkdir(parents=True, exist_ok=True)
        out_prefix = sample_dir / sample
        jobs.append(submit_sbatch(
            project,
            "STAR",
            script,
            [out_prefix, resources["star_index"], r1, r2, project["name"]],
            "star",
        ))
    return jobs


def submit_kallisto(project: dict, use_trimmed: bool = False) -> List[dict]:
    resources = genome_resources(project)
    outdir = data_dir(project) / "kallisto"
    outdir.mkdir(parents=True, exist_ok=True)
    paired = bool(project.get("paired_end", True))
    script = SCRIPTS / ("Kallisto/qsub_kallisto_PE.sh" if paired else "Kallisto/qsub_kallisto_SE.sh")
    jobs = []
    for sample, r1, r2 in sample_fastq_pairs(project, trimmed=use_trimmed):
        sample_dir = outdir / sample
        sample_dir.mkdir(parents=True, exist_ok=True)
        jobs.append(submit_sbatch(
            project,
            "Kallisto",
            script,
            [sample_dir, resources["kallisto_index"], r1, r2, project["name"]],
            "kallisto",
        ))
    return jobs


def submit_featurecounts(project: dict, feature: str = "gene_id") -> List[dict]:
    resources = genome_resources(project)
    outdir = data_dir(project) / "featurecounts"
    outdir.mkdir(parents=True, exist_ok=True)
    paired = bool(project.get("paired_end", True))
    script = SCRIPTS / ("featureCounts/qsub_featurecounts_PE.sh" if paired else "featureCounts/qsub_featurecounts_SE.sh")
    jobs = []
    for sample in read_design(project)["sample"].astype(str).tolist():
        sample_dir = outdir / sample
        sample_dir.mkdir(parents=True, exist_ok=True)
        bam = data_dir(project) / "star" / sample / f"{sample}Aligned.sortedByCoord.out.bam"
        count_prefix = sample_dir / sample
        jobs.append(submit_sbatch(
            project,
            "featureCounts",
            script,
            [bam, resources["gtf"], feature, count_prefix, resources["strand_bed"], project["name"]],
            "featurecounts",
        ))
    return jobs


def create_featurecounts_matrix(project: dict) -> Path:
    inpath = data_dir(project) / "featurecounts"
    outpath = data_dir(project) / "counts"
    outpath.mkdir(parents=True, exist_ok=True)
    matrices = []
    for sample_dir in sorted([p for p in inpath.iterdir() if p.is_dir()]):
        count_file = sample_dir / f"{sample_dir.name}_counts.txt"
        if not count_file.exists():
            continue
        df = pd.read_table(count_file, comment="#", index_col=0)
        drop_cols = [c for c in ["Chr", "Start", "End", "Strand", "Length"] if c in df.columns]
        df = df.drop(columns=drop_cols)
        if df.shape[1] > 0:
            df = df.rename(columns={df.columns[0]: sample_dir.name})
            matrices.append(df[[sample_dir.name]])
    if not matrices:
        raise FileNotFoundError("No featureCounts sample count files were found.")
    count_matrix = pd.concat(matrices, axis=1)
    out_file = outpath / "count_matrix.txt"
    count_matrix.to_csv(out_file, sep="\t")
    return out_file


def submit_deseq2(project: dict, reference: str, comparison: str, redundant: str = "NoRedundant") -> dict:
    outpath = data_dir(project) / "deseq2"
    outpath.mkdir(parents=True, exist_ok=True)
    count_matrix = data_dir(project) / "counts" / "count_matrix.txt"
    return submit_sbatch(
        project,
        "DESeq2",
        SCRIPTS / "DESeq2" / "qsub_deseq2.sh",
        [SCRIPTS / "DESeq2" / "DESeq2.R", count_matrix, design_matrix_path(project), outpath, reference, comparison, redundant or "NoRedundant", project["name"]],
        "deseq2",
    )


def write_shiny_config(project: dict, port: int) -> Path:
    outdir = project_root(project) / "shiny"
    outdir.mkdir(parents=True, exist_ok=True)
    config = outdir / "shiny_results_config.R"
    config.write_text(
        "\n".join([
            f'project_name <- "{project["name"]}"',
            f'results_root <- "{Path(project["results_root"]).expanduser().resolve()}"',
            f'data_dir <- "{data_dir(project).resolve()}"',
            f'design_matrix_path <- "{design_matrix_path(project).resolve()}"',
            'host <- "0.0.0.0"',
            f"port <- {int(port)}",
            "logo_search_dirs <- c(",
            f'  "{SCRIPTS.resolve()}"',
            ")",
            "",
        ])
    )
    return config


def submit_shiny(project: dict, port: int) -> dict:
    config = write_shiny_config(project, port)
    return submit_sbatch(
        project,
        "RNA-seq Shiny Viewer",
        SCRIPTS / "Shiny" / "sbatch_rnaseq_results_explorer.sh",
        [config, "0.0.0.0", str(port)],
        "rnaseq_shiny",
    )


def available_port(start: int = 3838, end: int = 3900) -> int:
    for port in range(start, end + 1):
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        try:
            sock.bind(("127.0.0.1", port))
            return port
        except OSError:
            continue
        finally:
            sock.close()
    return start


def style() -> None:
    st.markdown(
        """
        <style>
        :root {
            --csl-ink:#17202f;
            --csl-muted:#5f6f85;
            --csl-line:#d8dee8;
            --csl-bg:#f6f8fb;
            --csl-blue:#1d4ed8;
            --csl-green:#0f766e;
        }
        .stApp { background: var(--csl-bg); color: var(--csl-ink); }
        h1, h2, h3 { letter-spacing: 0; color: var(--csl-ink); }
        h1 { font-size: 2.4rem !important; line-height: 1.08 !important; }
        h2 { font-size: 1.7rem !important; }
        h3 { font-size: 1.35rem !important; }
        [data-testid="stSidebar"] {
            background: #ffffff;
            border-right: 1px solid var(--csl-line);
            min-width: 300px !important;
            max-width: 360px !important;
        }
        [data-testid="stSidebar"] h1 { font-size: 1.75rem !important; }
        input,
        textarea,
        [data-baseweb="input"] input,
        [data-baseweb="select"] > div,
        [data-baseweb="textarea"] textarea {
            background: #ffffff !important;
            border-color: #c7d0dd !important;
            color: var(--csl-ink) !important;
        }
        [data-baseweb="select"] span,
        [data-baseweb="select"] div {
            color: var(--csl-ink) !important;
        }
        [data-testid="stWidgetLabel"] p,
        [data-testid="stSidebar"] label,
        [data-testid="stSidebar"] p {
            color: var(--csl-ink) !important;
        }
        [data-testid="stAlert"] p,
        [data-testid="stAlert"] div {
            color: var(--csl-ink) !important;
        }
        div[data-testid="stMetric"] {
            background: #ffffff;
            border: 1px solid var(--csl-line);
            border-radius: 8px;
            padding: 14px 16px;
        }
        [data-testid="stMetricLabel"] {
            color: #5b6678 !important;
            opacity: 1 !important;
        }
        [data-testid="stMetricValue"] {
            color: var(--csl-ink) !important;
            overflow-wrap: anywhere;
        }
        .csl-header {
            background: #ffffff;
            border: 1px solid var(--csl-line);
            border-radius: 8px;
            padding: 16px 18px;
            margin-bottom: 14px;
            overflow: hidden;
        }
        .csl-header h1 {
            font-size: 2.15rem !important;
            line-height: 1.05 !important;
            white-space: normal;
        }
        .csl-subtle { color: var(--csl-muted); font-size: 0.94rem; }
        .csl-badge {
            display:inline-block;
            border:1px solid var(--csl-line);
            border-radius:999px;
            padding: 4px 10px;
            margin-right: 6px;
            background:#ffffff;
            color:var(--csl-muted);
            font-size: 0.82rem;
        }
        .stButton>button {
            border-radius: 6px;
            border: 1px solid #b9c4d4;
            background: #ffffff;
            color: var(--csl-ink);
            font-weight: 600;
        }
        .stButton>button[kind="primary"] {
            background: var(--csl-blue);
            color: white;
            border-color: var(--csl-blue);
        }
        button[data-baseweb="tab"] p {
            color: var(--csl-muted) !important;
            font-weight: 650 !important;
        }
        button[data-baseweb="tab"][aria-selected="true"] p {
            color: var(--csl-blue) !important;
        }
        div[data-testid="stDataFrame"],
        div[data-testid="stDataEditor"] {
            border: 1px solid var(--csl-line);
            border-radius: 8px;
            overflow: hidden;
            background: #ffffff;
        }
        </style>
        """,
        unsafe_allow_html=True,
    )


def header(project: Optional[dict]) -> None:
    if project:
        st.markdown(
            f"""
            <div class="csl-header">
              <h1 style="margin:0;">CodeSpringLab Control Center</h1>
              <div class="csl-subtle">Project: <b>{project["name"]}</b> · {project.get("analysis_type","RNA-seq")} · {project.get("genome","mouse")} · {"paired-end" if project.get("paired_end", True) else "single-end"}</div>
            </div>
            """,
            unsafe_allow_html=True,
        )
    else:
        st.markdown(
            """
            <div class="csl-header">
              <h1 style="margin:0;">CodeSpringLab Control Center</h1>
              <div class="csl-subtle">Create or select a project to scan reads, build design matrices, submit jobs, and inspect outputs.</div>
            </div>
            """,
            unsafe_allow_html=True,
        )


def sidebar_project_selector() -> Optional[dict]:
    st.sidebar.title("Projects")
    analysis_options = ["RNA-seq", "ATAC-seq", "ChIP-seq", "All analyses"]
    selected_analysis = st.sidebar.selectbox("Analysis", analysis_options, key="sidebar_analysis")
    projects = load_projects()
    filtered = {
        pid: project for pid, project in projects.items()
        if selected_analysis == "All analyses" or project.get("analysis_type", "RNA-seq") == selected_analysis
    }
    project_ids = sorted(
        filtered,
        key=lambda pid: filtered[pid].get("updated_at", filtered[pid].get("created_at", "")),
        reverse=True,
    )
    options = ["New project"] + project_ids
    default_project = project_ids[0] if project_ids else "New project"
    selected = st.session_state.pop("project_to_select", st.session_state.get("selected_project", default_project))
    if selected not in options:
        selected = default_project

    def label_project(option: str) -> str:
        if option == "New project":
            return "New project"
        project = filtered[option]
        return f"{project.get('name')} ({project.get('analysis_type', 'RNA-seq')})"

    choice = st.sidebar.selectbox(
        "Project config",
        options,
        index=options.index(selected),
        key="selected_project",
        format_func=label_project,
    )
    if choice != "New project":
        return filtered[choice]

    with st.sidebar.form("new_project"):
        workflow_mode = st.selectbox(
            "Workflow",
            ["Start new analysis", "Resume existing analysis", "Visualize existing results"],
        )
        name = st.text_input("Project name", value="example_dataset")
        default_analysis = selected_analysis if selected_analysis != "All analyses" else "RNA-seq"
        analysis_type = st.selectbox(
            "Analysis type",
            ["RNA-seq", "ATAC-seq", "ChIP-seq"],
            index=["RNA-seq", "ATAC-seq", "ChIP-seq"].index(default_analysis),
        )
        genome = st.selectbox("Genome", ["mouse", "human"])
        paired = st.toggle("Paired-end reads", value=True)
        results_root = st.text_input("Results root", value=str(Path("~/csl_results").expanduser()))
        fastq_dir = st.text_input("FASTQ folder", value="")
        design_path_input = st.text_input("Design matrix path or folder", value="")
        submitted = st.form_submit_button("Create / import project", type="primary")
    if submitted:
        project = {
            "name": clean_name(name, "project"),
            "analysis_type": analysis_type,
            "workflow_mode": workflow_mode,
            "genome": genome,
            "paired_end": paired,
            "results_root": str(Path(results_root).expanduser()),
            "fastq_dir": str(Path(fastq_dir).expanduser()) if fastq_dir else "",
            "design_matrix_path": str(Path(design_path_input).expanduser()) if design_path_input else "",
            "metadata_columns": ["treatment"],
        }
        project = apply_project_inference(project)
        project = save_project(project)
        st.session_state["project_to_select"] = project_id(project)
        st.sidebar.success("Project saved.")
        st.rerun()
    return None


def setup_tab(project: dict) -> dict:
    st.subheader("Project Setup")
    st.caption("Use this page to start new analyses, import older projects, or configure a visualize-only project.")
    with st.form("project_setup_form"):
        col1, col2 = st.columns(2)
        with col1:
            modes = ["Start new analysis", "Resume existing analysis", "Visualize existing results"]
            project["workflow_mode"] = st.selectbox("Workflow", modes, index=modes.index(project.get("workflow_mode", modes[0])) if project.get("workflow_mode", modes[0]) in modes else 0)
            project["analysis_type"] = st.selectbox("Analysis type", ["RNA-seq", "ATAC-seq", "ChIP-seq"], index=["RNA-seq", "ATAC-seq", "ChIP-seq"].index(project.get("analysis_type", "RNA-seq")))
            project["genome"] = st.selectbox("Genome", ["mouse", "human"], index=["mouse", "human"].index(project.get("genome", "mouse")))
            project["paired_end"] = st.toggle("Paired-end reads", value=bool(project.get("paired_end", True)))
        with col2:
            project["results_root"] = st.text_input("Results root", value=str(project.get("results_root", Path("~/csl_results").expanduser())))
            project["fastq_dir"] = st.text_input("FASTQ folder", value=str(project.get("fastq_dir", "")))
            project["design_matrix_path"] = st.text_input("Design matrix path or folder", value=str(project.get("design_matrix_path", "")))
            metadata = st.text_input("Design metadata columns", value=", ".join(project.get("metadata_columns", ["treatment"])))
        saved = st.form_submit_button("Save setup / re-detect paths", type="primary")
    if saved:
        project["metadata_columns"] = [clean_name(x, "condition") for x in metadata.split(",") if x.strip()] or ["treatment"]
        project = apply_project_inference(project)
        save_project(project)
        st.success("Project setup saved and paths re-detected.")

    c1, c2, c3, c4 = st.columns(4)
    c1.metric("Design matrix", "ready" if candidate_design_matrix_path(project).exists() else "missing")
    c2.metric("FASTQ files", len(fastq_files(project.get("fastq_dir", ""))))
    c3.metric("Submitted jobs", len(project_jobs(project)))
    c4.metric("Next step", next_recommended_step(project))

    st.markdown("**Detected Project State**")
    render_step_status(project)
    return project


def design_tab(project: dict) -> None:
    st.subheader("Design Matrix")
    st.caption("Scan the FASTQ folder to prefill filenames and inferred sample names, then type metadata directly into the table.")
    scan_key = "scan_df_"+project_id(project)
    cols_key = "metadata_cols_"+project_id(project)
    current_cols = project.get("metadata_columns", ["treatment"])

    col_a, col_b = st.columns([3, 1])
    with col_a:
        metadata_text = st.text_input(
            "Metadata columns",
            value=", ".join(st.session_state.get(cols_key, current_cols)),
            help="Comma-separated columns to add to design_matrix.txt, for example treatment, batch, replicate.",
            key="metadata_text_"+project_id(project),
        )
    with col_b:
        if st.button("Apply columns", key="apply_metadata_columns_"+project_id(project)):
            metadata_cols = parse_metadata_columns(metadata_text, current_cols)
            project["metadata_columns"] = metadata_cols
            save_project(project)
            st.session_state[cols_key] = metadata_cols
            if scan_key in st.session_state:
                st.session_state[scan_key] = sync_design_editor_columns(st.session_state[scan_key], metadata_cols)
            st.success("Metadata columns updated.")
            st.rerun()

    metadata_cols = st.session_state.get(cols_key, parse_metadata_columns(metadata_text, current_cols))
    project["metadata_columns"] = metadata_cols

    scan_col, empty_col = st.columns([1, 1])
    with scan_col:
        if st.button("Scan FASTQ folder / prefill filenames", type="primary"):
            scanned = scan_fastqs(project.get("fastq_dir", ""), bool(project.get("paired_end", True)))
            scanned = sync_design_editor_columns(scanned, metadata_cols)
            st.session_state[scan_key] = scanned
    with empty_col:
        if st.button("Start empty design table"):
            st.session_state[scan_key] = sync_design_editor_columns(pd.DataFrame(columns=["include", "sample", "filename", "detected_status"]), metadata_cols)

    existing = read_design(project)
    if scan_key not in st.session_state:
        if not existing.empty:
            display = existing.copy().fillna("")
            display.insert(0, "include", True)
            display["detected_status"] = "saved"
            st.session_state[scan_key] = sync_design_editor_columns(display, metadata_cols)
        else:
            st.session_state[scan_key] = sync_design_editor_columns(pd.DataFrame(), metadata_cols)
    else:
        st.session_state[scan_key] = sync_design_editor_columns(st.session_state[scan_key], metadata_cols)

    if st.session_state[scan_key].empty:
        st.info("Scan a FASTQ folder to prefill filenames, or start an empty design table and add rows manually.")

    display_cols = ["include", "sample"] + metadata_cols + ["filename", "detected_status"]
    column_config = {
        "include": st.column_config.CheckboxColumn("include", help="Uncheck to exclude a detected sample."),
        "sample": st.column_config.TextColumn("sample", width="medium", help="Edit sample names before saving."),
        "filename": st.column_config.TextColumn("FASTQ file(s)", width="large", help="Comma-separated R1,R2 filenames for paired-end projects."),
        "detected_status": st.column_config.TextColumn("status", width="small"),
    }
    for col in metadata_cols:
        column_config[col] = st.column_config.TextColumn(col, width="medium")

    edited = st.data_editor(
        st.session_state[scan_key],
        use_container_width=True,
        hide_index=True,
        num_rows="dynamic",
        column_order=[c for c in display_cols if c in st.session_state[scan_key].columns],
        disabled=["detected_status"],
        column_config=column_config,
    )
    st.session_state[scan_key] = sync_design_editor_columns(edited, metadata_cols)

    col1, col2 = st.columns([1, 3])
    with col1:
        if st.button("Save design_matrix.txt", type="primary"):
            try:
                project["metadata_columns"] = metadata_cols
                save_project(project)
                path = write_design(project, st.session_state[scan_key], metadata_cols)
                st.success(f"Saved {path}")
            except Exception as exc:
                st.error(str(exc))
    with col2:
        path = design_matrix_path(project)
        st.code(str(path))
    if path.exists():
        st.download_button(
            "Download design_matrix.txt",
            data=path.read_bytes(),
            file_name="design_matrix.txt",
            key="download_design_"+project_id(project),
        )


def job_submission_result(jobs) -> None:
    if not isinstance(jobs, list):
        jobs = [jobs]
    rows = []
    for job in jobs:
        rows.append({
            "step": job.get("step"),
            "job_id": job.get("job_id") or "",
            "status": scheduler_status(job.get("job_id")),
            "return_code": job.get("return_code"),
            "message": job.get("submit_output", "")[:220],
        })
    st.dataframe(pd.DataFrame(rows), use_container_width=True, hide_index=True)
    if any(not job.get("job_id") for job in jobs):
        st.caption("No scheduler job ID means the command was not submitted, usually because this is being tested away from the SLURM server.")


def run_tab(project: dict) -> None:
    st.subheader("Run Pipeline")
    if project.get("analysis_type") != "RNA-seq":
        st.info("This first web runner implements the RNA-seq run path. ATAC-seq and ChIP-seq projects can still use setup, design matrix, logs, and output browsing here.")
        return

    mode = project.get("workflow_mode", "Start new analysis")
    st.caption("Workflow: "+mode+" · recommended next step: "+next_recommended_step(project))
    with st.expander("Detected progress and resume guide", expanded=(mode != "Start new analysis")):
        render_step_status(project)
        resume_options = ["FastQC", "Trim", "STAR", "Kallisto", "featureCounts", "Count matrix", "DESeq2", "Shiny Viewer"]
        recommended = next_recommended_step(project)
        resume_default = resume_options.index(recommended) if recommended in resume_options else 0
        resume_step = st.selectbox(
            "I want to resume from",
            resume_options,
            index=resume_default,
        )
        render_resume_card(project, resume_step)
        st.info("Open the matching tab below and run only that step. Existing upstream outputs are read from the paths shown above.")

    if read_design(project).empty:
        st.warning("Save or import a design matrix before running analysis jobs. Visual outputs can still be browsed from the Outputs tab if files already exist.")
        if mode != "Visualize existing results":
            return
    if scheduler_available():
        st.success("SLURM scheduler detected. Run buttons will submit jobs with sbatch.")
    else:
        st.warning("Local preview mode: sbatch is not available here, so run buttons will record the command but will not submit jobs. Run this app on the server to execute the pipeline.")

    step_tabs = st.tabs(["FastQC", "Trim", "STAR", "Kallisto", "featureCounts", "DESeq2", "Shiny Viewer"])

    with step_tabs[0]:
        trimmed = st.toggle("Use trimmed reads", value=False, key="fastqc_trimmed")
        if st.button("Run FastQC", type="primary"):
            job_submission_result(submit_fastqc(project, trimmed=trimmed))

    with step_tabs[1]:
        a1 = st.text_input("R1 adapter", value="AGATCGGAAGAGCACACGTCTGAACTCCAGTCA")
        a2 = st.text_input("R2 adapter", value="AGATCGGAAGAGCGTCGTGTAGGGAAAGAGTGT")
        min_len = st.text_input("Minimum length", value="20")
        if st.button("Run cutadapt", type="primary"):
            job_submission_result(submit_cutadapt(project, a1, a2, min_len))

    with step_tabs[2]:
        use_trimmed = st.toggle("Use trimmed reads", value=False, key="star_trimmed")
        if st.button("Run STAR", type="primary"):
            job_submission_result(submit_star(project, use_trimmed=use_trimmed))

    with step_tabs[3]:
        use_trimmed = st.toggle("Use trimmed reads", value=False, key="kallisto_trimmed")
        if st.button("Run Kallisto", type="primary"):
            job_submission_result(submit_kallisto(project, use_trimmed=use_trimmed))

    with step_tabs[4]:
        feature = st.selectbox("Feature attribute", ["gene_id", "gene_name"])
        c1, c2 = st.columns(2)
        with c1:
            if st.button("Run featureCounts", type="primary"):
                job_submission_result(submit_featurecounts(project, feature=feature))
        with c2:
            if st.button("Build count_matrix.txt"):
                try:
                    out = create_featurecounts_matrix(project)
                    st.success(f"Wrote {out}")
                except Exception as exc:
                    st.error(str(exc))

    with step_tabs[5]:
        design = read_design(project)
        metadata_cols = [c for c in design.columns if c not in ["sample", "filename"]]
        if not metadata_cols:
            st.warning("DESeq2 needs at least one metadata column in the design matrix.")
        else:
            design_col = st.selectbox("Design column", metadata_cols, key="deseq_design_column_"+project_id(project))
            options = sorted([x for x in design[design_col].dropna().astype(str).unique().tolist() if x])
            col1, col2, col3 = st.columns(3)
            with col1:
                ref = st.selectbox("Reference", options or ["control"], key="deseq_reference_"+project_id(project))
            with col2:
                comp = st.selectbox("Comparison", options or ["treated"], key="deseq_comparison_"+project_id(project))
            with col3:
                redundant_options = ["NoRedundant"] + [c for c in metadata_cols if c != design_col]
                redundant = st.selectbox("Redundant covariate", redundant_options, key="deseq_redundant_"+project_id(project))
            if st.button("Run DESeq2", type="primary"):
                if ref == comp:
                    st.error("Reference and comparison must be different.")
                else:
                    job_submission_result(submit_deseq2(project, ref, comp, redundant))

    with step_tabs[6]:
        render_shiny_launcher(project, "run_tab_"+project_id(project))


def jobs_tab(project: dict) -> None:
    st.subheader("Jobs and Logs")
    jobs = project_jobs(project)
    if not jobs:
        st.info("No jobs submitted from the web app yet.")
        return
    rows = []
    for job in jobs:
        rows.append({
            "submitted": job.get("submitted_at"),
            "step": job.get("step"),
            "job_id": job.get("job_id") or "",
            "status": scheduler_status(job.get("job_id")),
            "return_code": job.get("return_code"),
        })
    st.dataframe(pd.DataFrame(rows), use_container_width=True, hide_index=True)
    selected = st.selectbox("Inspect job", list(range(len(jobs))), format_func=lambda i: f"{jobs[i].get('step')} {jobs[i].get('job_id') or ''} {jobs[i].get('submitted_at')}")
    job = jobs[selected]
    st.code(job.get("command", ""))
    col1, col2 = st.columns(2)
    with col1:
        st.caption(job.get("stdout", ""))
        st.text_area("stdout tail", value=read_tail(job.get("stdout", "")), height=320)
    with col2:
        st.caption(job.get("stderr", ""))
        st.text_area("stderr tail", value=read_tail(job.get("stderr", "")), height=320)


def table_preview(path: Path, label: str, preview_rows: int = 500) -> None:
    if not path.exists():
        return
    size = path.stat().st_size
    st.markdown(f"**{label}**")
    st.caption(f"{path.name} · {format_size(size)} · previewing up to {preview_rows} rows")
    try:
        df = pd.read_csv(path, sep=None, engine="python", nrows=preview_rows)
        st.dataframe(df, use_container_width=True, height=360)
        st.download_button(
            f"Download {path.name}",
            data=path.read_bytes(),
            file_name=path.name,
            key="download_table_"+str(abs(hash(str(path)))),
        )
    except Exception:
        preview = path.read_text(errors="replace")[:20000]
        st.text_area(path.name, value=preview, height=260)


def result_files(project: dict, limit: int = 1000) -> List[Path]:
    root = data_dir(project)
    if not root.exists():
        return []
    keep = []
    for path in root.rglob("*"):
        if path.is_file() and path.suffix.lower() in [".txt", ".csv", ".tsv", ".html", ".png", ".pdf"]:
            keep.append(path)
            if len(keep) >= limit:
                break
    return sorted(keep)


def results_tab(project: dict) -> None:
    st.subheader("Outputs")
    root = data_dir(project)
    col1, col2 = st.columns([3, 1])
    with col1:
        st.code(str(root))
    with col2:
        if st.button("Re-detect project paths"):
            project = apply_project_inference(project)
            save_project(project)
            st.success("Paths refreshed.")
    if not root.exists():
        st.info("No data folder exists yet.")
        return
    with st.expander("Detected project state", expanded=False):
        render_step_status(project)

    if project.get("analysis_type") == "RNA-seq":
        render_shiny_launcher(project, "outputs_tab_"+project_id(project))

    result_tabs = st.tabs(["QC", "Tables", "Plots/Files"])
    with result_tabs[0]:
        fastqc_dirs = [root / "fastqc", root / "fastqc_cutadapt"]
        htmls = []
        for d in fastqc_dirs:
            if d.exists():
                htmls.extend(sorted(d.glob("*.html")))
        if htmls:
            selected = st.selectbox("FastQC report", htmls, format_func=lambda p: p.name)
            components.html(selected.read_text(errors="replace"), height=900, scrolling=True)
        else:
            st.info("No FastQC HTML reports found.")

    with result_tabs[1]:
        candidates = [
            (root / "star_summary" / "summary_matrix.txt", "STAR summary"),
            (root / "counts" / "featurecounts_summary.txt", "featureCounts summary"),
            (root / "counts" / "count_matrix.txt", "Raw count matrix"),
        ]
        for d in [root / "deseq2", root / "gseapy"]:
            if d.exists():
                for p in sorted(d.rglob("*.txt"))[:20] + sorted(d.rglob("*.csv"))[:20]:
                    candidates.append((p, p.relative_to(root).as_posix()))
        for path, label in candidates:
            table_preview(path, label)

    with result_tabs[2]:
        files = result_files(project)
        if not files:
            st.info("No result files found.")
            return
        selected = st.selectbox("File", files, format_func=lambda p: p.relative_to(root).as_posix())
        if selected.suffix.lower() in [".png"]:
            st.image(str(selected))
        elif selected.suffix.lower() == ".html":
            components.html(selected.read_text(errors="replace"), height=900, scrolling=True)
        else:
            st.download_button(
                f"Download {selected.name}",
                data=selected.read_bytes(),
                file_name=selected.name,
                key="download_file_"+str(abs(hash(str(selected)))),
            )
            st.code(str(selected))


def main() -> None:
    st.set_page_config(page_title="CodeSpringLab Control Center", layout="wide")
    style()
    project = sidebar_project_selector()
    header(project)
    if not project:
        st.info("Create a project from the sidebar or select an existing one.")
        return

    tabs = st.tabs(["Setup", "Progress", "Design Matrix", "Run Pipeline", "Jobs & Logs", "Outputs"])
    with tabs[0]:
        project = setup_tab(project)
    with tabs[1]:
        progress_tab(project)
    with tabs[2]:
        design_tab(project)
    with tabs[3]:
        run_tab(project)
    with tabs[4]:
        jobs_tab(project)
    with tabs[5]:
        results_tab(project)


if __name__ == "__main__":
    main()
