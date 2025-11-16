#!/usr/bin/env bash
set -euo pipefail

# locate conda base
if [ -n "${CONDA_EXE:-}" ]; then
    CONDA_BASE="$(dirname "$(dirname "$CONDA_EXE")")"
elif command -v conda >/dev/null 2>&1; then
    CONDA_BASE="$(conda info --base 2>/dev/null || true)"
else
    echo "conda not found in PATH" >&2
    exit 1
fi

# source conda.sh if available so `conda activate` works in scripts
if [ -n "$CONDA_BASE" ] && [ -f "$CONDA_BASE/etc/profile.d/conda.sh" ]; then
    # shellcheck source=/dev/null
    . "$CONDA_BASE/etc/profile.d/conda.sh"
else
    # fallback: try to add conda bin to PATH
    export PATH="$CONDA_BASE/bin:$PATH"
fi

conda activate playground

# run the python script with all passed arguments
# echo "Running rankCalc.py with arguments: $*"
python rankCalc.py "$@"