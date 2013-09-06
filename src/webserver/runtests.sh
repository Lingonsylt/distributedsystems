#!/bin/sh

run_test () {
    erlc http.erl rudy.erl

	EXTRA_ARGS=""
	if [ -n "$1" ]; then
		CMD="erl -sname $1 -noshell"
		echo "Running ${CMD}"
		${CMD} &
		N1_PID=$!
		EXTRA_ARGS="${EXTRA_ARGS} $1@lingon"

	fi

	if [ -n "$2" ]; then
		CMD="erl -sname $2 -noshell"
		echo "Running ${CMD}"
		${CMD} &
		N2_PID=$!
		EXTRA_ARGS="${EXTRA_ARGS} $2@lingon"
	fi
	CMD="erl -run rudy run 7500 ${EXTRA_ARGS} -sname n0 -noshell"
	echo "Running ${CMD}"
    ${CMD} &
	N0_PID=$!

    sleep 1
    ab -n 1000 -c 50 http://127.0.0.1:7500/ | tee -a out.txt

	if [ -n "${N2_PID}" ]; then echo "Killing processes ${N2_PID}!" && kill -9 ${N2_PID}; fi
	if [ -n "${N1_PID}" ]; then echo "Killing processes ${N1_PID}!" && kill -9 ${N1_PID}; fi
	echo "Killing processes ${N0_PID}!" && kill -9 "${N0_PID}"
	sleep 1
	echo "=========================================" >> out.txt
}

echo "" > out.txt

run_test
run_test n2
run_test n2 n1