#!/bin/sh
# Compare bench-v8 scores across multiple qjs binaries and print a markdown table.
# Usage: bench_compare.sh [-b <baseline_label>] <qjs1[:label]> [qjs2[:label] ...]
#
# -b <label>  Use the column with this label as the baseline (default: first column).
# Label is optional per binary; if omitted the version string embedded in the binary
# is used, falling back to the path.
#
# Example: bench_compare.sh /tmp/qjs-prev/qjs ./qjs
# Example: bench_compare.sh -b mtqjs-master /old/qjs ./qjs /path/to/mtqjs:mtqjs-master

set -e

BENCH=tests/bench-v8/combined.js

baseline=""
while [ "$#" -gt 0 ]; do
    case "$1" in
        -b|--baseline) baseline="$2"; shift 2 ;;
        *) break ;;
    esac
done

if [ "$#" -lt 1 ]; then
    echo "Usage: $0 [-b baseline_label] <qjs[:label]> [qjs2[:label] ...]" >&2
    exit 1
fi

if [ ! -f "$BENCH" ]; then echo "not found: $BENCH" >&2; exit 1; fi

# Extract QuickJS version string embedded in a binary
version_of() {
    strings "$1" 2>/dev/null \
        | grep -E 'QuickJS version [0-9]{4}-[0-9]{2}-[0-9]{2}' \
        | grep -oE '[0-9]{4}-[0-9]{2}-[0-9]{2}' \
        | head -1
}

# Run each binary, collect results into tmpfiles
tmps=""
labels=""
ncols=0
for arg in "$@"; do
    # Support optional "path:label" syntax; split on the first colon only
    qjs="${arg%%:*}"
    label="${arg#*:}"
    [ "$qjs" = "$label" ] && label=""   # no colon → clear label, use auto-detect
    if [ ! -x "$qjs" ]; then echo "not executable: $qjs" >&2; exit 1; fi
    if [ -z "$label" ]; then
        label=$(version_of "$qjs")
        [ -z "$label" ] && label="$qjs"
    fi
    tmp=$(mktemp)
    tmps="$tmps $tmp"
    labels="${labels}${labels:+,}${label}"
    printf "running [%s]... " "$label" >&2
    "$qjs" "$BENCH" > "$tmp" 2>&1
    echo "done" >&2
    ncols=$((ncols + 1))
done

# shellcheck disable=SC2064
trap "rm -f $tmps" EXIT

# Feed all tmpfiles to awk with FILE <n> markers so it knows which column each
# result belongs to. Percentages are relative to the baseline column.
{
    i=0
    for tmp in $tmps; do
        echo "FILE $i"
        cat "$tmp"
        i=$((i + 1))
    done
} | awk -v ncols="$ncols" -v labels="$labels" -v baseline="$baseline" '
BEGIN {
    split(labels, lbl, ",")
    base_col = 0
    if (baseline != "") {
        for (i = 1; i <= ncols; i++) {
            if (lbl[i] == baseline) { base_col = i - 1; break }
        }
    }
}

/^FILE [0-9]+$/ { cur = $2 + 0; next }

/^RESULT / {
    name = $2
    score[name, cur] = $3 + 0
    if (!(name in seen)) { seen[name] = 1; order[norder++] = name }
    next
}

/^SCORE [0-9]+$/ {
    score["SCORE", cur] = $2 + 0
    next
}

END {
    order[norder++] = "SCORE"

    # Header row
    printf "| 测试项"
    for (i = 0; i < ncols; i++)
        printf " | %s", lbl[i + 1]
    printf " |\n"

    # Separator row
    printf "|--------|"
    for (i = 0; i < ncols; i++)
        printf "-------:|"
    printf "\n"

    # Data rows — base_col is the baseline, the rest show score (+X%)
    for (r = 0; r < norder; r++) {
        k = order[r]
        b = (k == "SCORE") ? "**" : ""
        printf "| %s%s%s", b, k, b
        base = score[k, base_col] + 0
        for (i = 0; i < ncols; i++) {
            v = score[k, i] + 0
            if (i == base_col || base == 0) {
                printf " | %s%d%s", b, v, b
            } else {
                pct = (v - base) / base * 100
                sign = (pct >= 0) ? "+" : ""
                printf " | %s%d (%s%.1f%%)%s", b, v, sign, pct, b
            }
        }
        printf " |\n"
    }
}
'
