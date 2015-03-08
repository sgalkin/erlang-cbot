-module(register_tests).
-include_lib("eunit/include/eunit.hrl").

new_test() ->
    R = register:new(),
    ?assert(register:size(R) == 0).

add_new_test() ->
    R = register:new(),
    V = x,
    NR = register:add(V, R),
    ?assert(register:size(NR) == 1).

add_existing_test() ->
    R = register:new(),
    V = x,
    NR = register:add(V, register:add(V, R)),
    ?assert(register:size(NR) == 1).

del_existing_test() ->
    R = register:new(),
    V = x,
    NR = register:del(V, register:add(V, R)),
    ?assert(register:size(NR) == 0).

del_unknonw_test() ->
    R = register:new(),
    V = x,
    NR = register:del(y, register:add(V, R)),
    ?assert(register:size(NR) == 1).

get_0th_test() ->
    R = register:add(y, register:new()),
    E = register:get(x, 0, R),
    ?assert(E == fail).

get_from_empty_test() ->
    R = register:new(),
    E = register:get(x, 1, R),
    ?assert(E == fail).

get_only_test() ->
    R = register:add(y, register:new()),
    E = register:get(y, 1, R),
    ?assert(E == fail).

get_test() ->
    R = register:add(y, register:add(x, register:new())),
    E = register:get(x, 1, R),
    ?assert(E == y).
