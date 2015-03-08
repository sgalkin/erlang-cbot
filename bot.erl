-module(bot).
-export([init/0, uninit/1]).

init() ->
    io:format("starting bot ~w~n", [self()]).
%    self() ! init,
%    loop().

uninit(Bot) ->
    io:format("stopping bot ~w~n", [Bot]),
    Bot ! uninit.

on_registered(Result) ->
    io:format("on_registered ~w~n", [Result]).

on_unregistered(Result) ->
    io:format("on_unregistered ~w~n", [Result]).

loop() ->
    receive
        init ->
            registrar:register(self(), fun on_registered/1, registrar),
            loop();
        uninit ->
            registrar:unregister(self(), fun on_unregistered/1, registrar),
            loop();
        registering ->
            loop();
        registered ->
            loop();
        M ->
            io:format("invalid message `~w'~n", [M]),
            self() ! uninit
            %loop()
    end.
%
%process_response(Success, Fail) ->
%    receive
%        ok -> Success;
%        fail -> Fail
%    end.
%
%state(init) ->
%    registrar:register(self(), registrar),
%    state(registering);
%state(uninit) ->
%    registrar:unregister(self(), registrar),
%    state(unregistering);
%
%state(registering) -> state(process_response(registered, failed));
%state(registered) -> state(ready);
%state(unregistering) -> state(process_response(unregistered, failed));
%state(unregistered) -> ok;
%state(failed) -> fail;
%state(S) ->
%    io:format("Invalid state ~w~n", [S]),
%    uninit().
