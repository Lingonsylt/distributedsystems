
function run_test {
	$EXTRA_ARGS = ""
	if [ -n $1 ]; then
		/usr/lib/erlang/bin/erl -pa /home/lingon/dev/erlangtest/out/production/antontest -run rudy run 7500 -sname n1 -noshell -s init stop
		$EXTRA_ARGS = "${EXTRA_ARGS} $1"
	fi
	/usr/lib/erlang/bin/erl -pa /home/lingon/dev/erlangtest/out/production/antontest -run rudy run 7500 -sname n0 -noshell -s init stop
	$n0pid = $!
}
