-module(registrar).
-export([init/0, uninit/1, register/4, unregister/4, lookup/4, complete/2]).

% API
init() ->
    io:format("starting registrar ~w~n", [self()]),
    random:seed(now()),
    loop(register:new()).

uninit(Registrar) ->
    io:format("stopping registrar ~w~n", [Registrar]),
    Registrar ! stop.

register(Client, Callback, Pid, Registrar) -> 
    invoke(fun (R) -> { register:add(Client, R), ok } end, 
           Callback, Pid, Registrar).
unregister(Client, Pid, Callback, Registrar) -> 
    invoke(fun (R) -> { register:del(Client, R), ok } end, 
           Callback, Pid, Registrar).
lookup(Client, Pid, Callback, Registrar) -> 
    invoke(fun (R) -> { R, register:get(Client, index(register:size(R)), R) } end, 
           Callback, Pid, Registrar).

complete({ Result, Callback }, Args) ->
    Callback(Result, Args).

% Private
invoke(Action, Callback, Pid, Registrar) ->
    Registrar ! { Action, { Pid, Callback } }.

loop(Register) ->
    receive
        { Action, { From, Callback } } ->
            io:format("got a message from ~w - ~w~n", [From, Action]),
            { NewRegister, Result } = Action(Register),
            From ! { self(), { Result, Callback } },
            loop(NewRegister);
        stop ->
            ok
    end.

index(0) -> 0;
index(Size) -> random:uniform(Size).
