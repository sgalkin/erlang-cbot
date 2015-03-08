-module(registrar).
-export([init/0, uninit/1, register/4, unregister/4, lookup/4, complete/2]).

% API
register(Client, Pid, Callback, Registrar) -> 
    invoke(register, Client, Pid, Callback, Registrar).
unregister(Client, Pid, Callback, Registrar) -> 
    invoke(unregister, Client, Pid, Callback, Registrar).
lookup(Client, Pid, Callback, Registrar) -> 
    invoke(lookup, Client, Pid, Callback, Registrar).
complete({ Action, Result, Callback }, Args) ->
    Callback(Action, Result, Args).

invoke(Action, Client, Pid, Callback, Registrar) ->
    % TODO think about async
    Registrar ! { Pid, { Action, [Client], Callback } }.
    %receive
    %    { Registrar, { Action, Result } } -> Result;
    %    _ -> fail
    %end.

init() ->
    io:format("starting registrar~n"),
    random:seed(now()),
    
    API = #{ 
      register => fun do_register/2, 
      unregister => fun do_unregister/2,
      lookup => fun do_lookup/2
     },

    loop(sets:new(), API).

uninit(Registrar) ->
    io:format("stopping registrar~n"),
    Registrar ! stop.

loop(Register, API) ->
    receive
        { From, { Method, Args, Callback } } ->
            io:format("got a message from ~w - ~w(~w)~n", [From, Method, Args]),
            Callable = method(Method, API),
            { NewRegister, Reponse } = Callable(Args, Register),
            From ! { self(), { Method, Reponse, Callback } },
            loop(NewRegister, API);
        stop ->
            ok
    end.

invalid(Method, Args, R) ->
    io:format("unknown function ~w(~w)~n", [Method, Args]),
    { R, fail }.

method(Method, API) ->
    maps:get(
      Method, API, 
      fun (Args, Register) -> invalid(Method, Args, Register) end).

% API
do_register(Args, Register) -> maintain(Args, Register, fun sets:add_element/2).
do_unregister(Args, Register) -> maintain(Args, Register, fun sets:del_element/2).
do_lookup([Self | []], Register) ->
    Size = sets:size(Register),
    Index = index(Size),
    Element = registration(Self, Index, Register),
    { Register, Element };
do_lookup(_, Register) ->
    { Register, fail }.

% Private
maintain([Client | []], Register, Op) ->
    NewRegister = Op(Client, Register),
    io:format("register ~w -> ~w~n", 
              [sets:to_list(Register), sets:to_list(NewRegister)]),
    { NewRegister, ok };
maintain(_, Register, _) -> { Register, fail }.

index(0) -> 0;
index(Size) -> random:uniform(Size).
    
registration(_, 0, _) -> fail;
registration(Self, Nth, Register) ->
    {_, _, Element, _} = 
        % TODO O(n) -> O(1)
        sets:fold(
          fun (E, {DI, CI, _, PV}) 
                when DI == CI, E == Self -> { DI + 1, CI + 1, PV, PV };
              (E, {DI, CI, _, _}) 
                when DI == CI, E /= Self -> { DI, CI + 1, E, E };
              (E, {DI, CI, V, _}) 
                when DI /= CI -> { DI, CI + 1, V, E } end,
          { Nth, 1, fail, fail },
          Register),
    Element.

% Tests
-include_lib("eunit/include/eunit.hrl").

invoke_register(Args) ->
    invoke_register(sets:new(), Args).
invoke_register(S, Args) ->
    F = fun do_register/2,
    { S, F(Args, S) }.

register_no_args_test() ->
    {S, R} = invoke_register([]),
    ?assert(R == { S, fail }).
register_many_args_test() ->
    {S, R} = invoke_register([foo, bar, baz]),
    ?assert(R == { S, fail }).
register_new_test() ->
    V = foo,
    {_, {RS, ST}} = invoke_register([V]),
    ?assert(ST == ok),
    ?assert(sets:is_element(V, RS)).
register_exists_test() ->
    V = foo,
    {_, {S, ST}} = invoke_register([V]),
    {S, {RS, ST}} = invoke_register(S, [V]),
    ?assert(S == RS),
    ?assert(ST == ok),
    ?assert(sets:is_element(V, RS)).

unregister_no_args_test() ->
    {RS, ST} = do_unregister([], []),
    ?assert(ST == fail),
    ?assert(RS == []).
unregister_many_args_test() ->
    {RS, ST} = do_unregister([foo, bar, baz], []),
    ?assert(ST == fail),
    ?assert(RS == []).
unregister_unknown_test() ->
    S = sets:add_element(bar, sets:new()),
    {RS, ST} = do_unregister([foo], S),
    ?assert(ST == ok),
    ?assert(RS == S).
unregiser_known_test() ->
    S = sets:add_element(bar, sets:new()),
    {RS, ST} = do_unregister([bar], S),
    ?assert(ST == ok),
    ?assert(RS == sets:new()).

lookup_empty_test() ->
    S = sets:new(),
    ?assert(do_lookup([], S) == { S, fail }).
lookup_self_test() ->
    S = sets:add_element("1", sets:new()),
    { RS, E } = do_lookup(["1"], S),
    ?assert(RS == S),
    ?assert(E == fail).
lookup_non_empty_test() ->
    S = sets:add_element("2", sets:add_element("1", sets:new())),
    { RS, E1 } = do_lookup(["1"], S),
    { RS, E2 } = do_lookup(["2"], S),
    ?assert(RS == S),
    ?assert(sets:is_element(E1, S)),
    ?assert(sets:is_element(E2, S)),
    ?assert(E1 /= E2).

handle_invalid_test() ->
    P = spawn(registrar, init, []),
    P ! { self(), { something, [], fun (_, _) -> ok end } },
    receive { _, { M, S, C } } -> 
            ?assert(M == something),
            ?assert(S == fail)
    end,
    uninit(P).
