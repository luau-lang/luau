#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

declare -A event_map
event_map[Ir]="TotalInstructionsExecuted,executions\n"
event_map[I1mr]="L1_InstrReadCacheMisses,misses/op\n"
event_map[ILmr]="LL_InstrReadCacheMisses,misses/op\n"
event_map[Dr]="TotalMemoryReads,reads\n"
event_map[D1mr]="L1_DataReadCacheMisses,misses/op\n"
event_map[DLmr]="LL_DataReadCacheMisses,misses/op\n"
event_map[Dw]="TotalMemoryWrites,writes\n"
event_map[D1mw]="L1_DataWriteCacheMisses,misses/op\n"
event_map[DLmw]="LL_DataWriteCacheMisses,misses/op\n"
event_map[Bc]="ConditionalBranchesExecuted,executions\n"
event_map[Bcm]="ConditionalBranchMispredictions,mispredictions/op\n"
event_map[Bi]="IndirectBranchesExecuted,executions\n"
event_map[Bim]="IndirectBranchMispredictions,mispredictions/op\n"

now_ms() {
    echo -n $(date +%s%N | cut -b1-13)
}

# Run cachegrind on a given benchmark and echo the results.
ITERATION_COUNT=$4
START_TIME=$(now_ms)

ARGS=( "$@" )
REST_ARGS="${ARGS[@]:4}"

valgrind \
    --quiet \
    --tool=cachegrind \
    "$1" "$2" $REST_ARGS>/dev/null

ARGS=( "$@" )
REST_ARGS="${ARGS[@]:4}"


TIME_ELAPSED=$(bc <<< "$(now_ms) - ${START_TIME}")

# Generate report using cg_annotate and extract the header and totals of the
# recorded events valgrind was configured to record.
CG_RESULTS=$(cg_annotate $(ls -t cachegrind.out.* | head -1))
CG_HEADERS=$(grep -B2 'PROGRAM TOTALS$' <<< "$CG_RESULTS" | head -1 | sed -E 's/\s+/\n/g' | sed '/^$/d')
CG_TOTALS=$(grep 'PROGRAM TOTALS$' <<< "$CG_RESULTS" | head -1 | grep -Po '[0-9,]+\s' | tr -d ', ')

TOTALS_ARRAY=($CG_TOTALS)
HEADERS_ARRAY=($CG_HEADERS)

declare -A header_map
for i in "${!TOTALS_ARRAY[@]}"; do
    header_map[${HEADERS_ARRAY[$i]}]=$i
done

# Map the results to the format that the benchmark script expects.
for i in "${!TOTALS_ARRAY[@]}"; do
    TOTAL=${TOTALS_ARRAY[$i]}

    # Labels and unit descriptions are packed together in the map.
    EVENT_TUPLE=${event_map[${HEADERS_ARRAY[$i]}]}
    IFS=$',' read -d '\n' -ra EVENT_VALUES < <(printf "%s" "$EVENT_TUPLE")
    EVENT_NAME="${EVENT_VALUES[0]}"
    UNIT="${EVENT_VALUES[1]}"

    case ${HEADERS_ARRAY[$i]} in
        I1mr | ILmr)
            REF=${TOTALS_ARRAY[header_map["Ir"]]}
            OPS_PER_SEC=$(bc -l <<< "$TOTAL / $REF")
            ;;

        D1mr | DLmr)
            REF=${TOTALS_ARRAY[header_map["Dr"]]}
            OPS_PER_SEC=$(bc -l <<< "$TOTAL / $REF")
            ;;

        D1mw | DLmw)
            REF=${TOTALS_ARRAY[header_map["Dw"]]}
            OPS_PER_SEC=$(bc -l <<< "$TOTAL / $REF")
            ;;

        Bcm)
            REF=${TOTALS_ARRAY[header_map["Bc"]]}
            OPS_PER_SEC=$(bc -l <<< "$TOTAL / $REF")
            ;;

        Bim)
            REF=${TOTALS_ARRAY[header_map["Bi"]]}
            OPS_PER_SEC=$(bc -l <<< "$TOTAL / $REF")
            ;;

        *)
            OPS_PER_SEC=$(bc -l <<< "$TOTAL")
            ;;
        esac

    STD_DEV="0%"
    RUNS="1"

    if [[ $OPS_PER_SEC =~ ^[+-]?[0-9]*$ ]]
    then # $OPS_PER_SEC is integer
        printf "%s#%s x %.0f %s ±%s (%d runs sampled)\n" \
            "$3" "$EVENT_NAME" "$OPS_PER_SEC" "$UNIT" "$STD_DEV" "$RUNS" 
    else # $OPS_PER_SEC is float
        printf "%s#%s x %.10f %s ±%s (%d runs sampled)\n" \
            "$3" "$EVENT_NAME" "$OPS_PER_SEC" "$UNIT" "$STD_DEV" "$RUNS" 
    fi
    
done
