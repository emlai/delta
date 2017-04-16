#!/bin/bash

compile_successfully() {
    compiler_stdout=$($path_to_delta inputs/$1 "${@:2}")

    if [ $? -ne 0 ]; then
        echo "FAILED: $1"
        echo "output: $compiler_stdout"
        exit 1
    fi
}

compile_and_run_and_check_exit_status() {
    compile_successfully $1
    expected_exit_status=$2
    ./a.out
    actual_exit_status=$?
    rm a.out

    if [ $actual_exit_status -ne $expected_exit_status ]; then
        echo "FAILED: expected exit status: $expected_exit_status," \
             "actual exit status: $actual_exit_status"
        exit 1
    fi
}

validate_output() {
    if [[ "$actual_output" != "$expected_output" ]]; then
        echo "FAILED:"
        echo "expected output: \"$expected_output\""
        echo "actual output:   \"$actual_output\""
        exit 1
    fi
}

compile_and_run_and_check_output() {
    compile_successfully $1
    expected_output=$2
    actual_output="$(./a.out)"
    rm a.out
    validate_output
}

check_error() {
    source_file=inputs/$1
    line_number=$(echo $2 | sed 's/:.*//')
    column=$(echo $2 | sed 's/.*://')
    line_content=$(sed -n "${line_number}p" $source_file)
    substr=${line_content:0:column - 1}
    expected_output="$source_file:$2: error: $3"$'\n'"$line_content"$'\n'"${substr//[^	]/ }^"
    actual_output="$($path_to_delta $source_file)"
    validate_output
}

diff_output() {
    expected_output=$(cat $2)
    actual_output=$($path_to_delta -no-debug-info $1)
    output_prefix=$3
    diff <(echo "$actual_output") <(echo "$output_prefix$expected_output")
}

validate_ast() {
    source_file=inputs/$1
    ast_file=inputs/$1-ast
    diff_output "$source_file -print-ast" $ast_file

    if [ $? -ne 0 ]; then
        echo "FAILED: AST of '$source_file' doesn't match expected AST '$ast_file'"
        exit 1
    fi
}

test_all() {
    regex="// ([a-z_]+)(.*)"
    for file in $@; do
        first_line=$(head -n 1 $file)
        if [[ $first_line =~ $regex ]]; then
            eval ${BASH_REMATCH[1]} $(basename $file) ${BASH_REMATCH[2]}
        else
            echo "ERROR: No test command specified in $file."
            exit 1
        fi
    done
}
