-module(registrar_agent).
-export([init/2, uninit/2]).

init(Parent, Registrar) ->
    io:format("starting registrar agent ~w for ~w~n", [self(), Parent]),
    self() ! register,
    loop(init, Parent, Registrar).

%    State = state(registering, action(fun registrar:register/3, Parent, Registrar)),
%    loop(State, Parent, Registrar).

uninit(Parent, Pid) ->
    io:format("stopping registrar agent ~w for ~w~n", [Pid, Parent]),
    Pid ! stop.
% check 

%lookup(Parent, Pid) ->
%    Pid ! lookup

%action(F, Parent, Registrar) ->
%    F(self(), Parent, Registrar).

on_registered(_, Result, State) ->
    io:format("~w @ ~w~n", [Result, State]).

%loop(disposed, _, _) -> ok;
loop(State, Parent, Registrar) ->
    receive
        register ->
            registrar:register(Parent, self(), fun on_registered/3, Registrar),
            loop(registering, Parent, Registrar);
        stop -> ok;
        { Registrar, R } ->
            registrar:complete(R, State);
        M ->
            io:format("m? ~w~n", [M])
            %NewState = stop(Parent, Registrar), 
            %loop(NewState, Parent, Registrar)
    end.

%stop(Parent, Registrar) ->
%    state(unregistering, action(fun registrar:unregister/3, Parent, Registrar)).

%state(State, Action) ->
%    NewState = do_state(State, Action),
%    io:format("~w entered state ~w~n", [self(), NewState]).

%do_state(registering, ok) -> idle;
%do_state(unregistering, _) -> disposed;
%do_state(_, fail) -> failed.
%state(unregistered)
%    State = register_client(Parent, Registrar),
