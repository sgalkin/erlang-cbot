-module(registrar_tests).
-include_lib("eunit/include/eunit.hrl").

stop_test() ->
    P = spawn(registrar, init, []),
    erlang:monitor(process, P),
    registrar:uninit(P),
    receive { 'DOWN', _, _, P, _ } -> ok
    after 10 -> ?assert(0)
    end.

callback_test() ->
    E = fun (R, [a, b, c]) -> R end,
    P = spawn(registrar, init, []),
    registrar:register(a, E, self(), P),
    receive { P, R } -> ?assert(ok == registrar:complete(R, [a, b, c]))
    after 10 -> ?assert(0)
    end.

% TODO: interface test
