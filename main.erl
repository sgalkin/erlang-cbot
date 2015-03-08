-module(main).
-export([run/0]).

%send(Method, Args) ->
%    registrar ! { self(), { Method, Args } },
%    receive 
%        Msg -> io:format('~w~n', [Msg]) 
%    end.

run() ->
    eunit:test(register),
    eunit:test(registrar),
    0.
%    register(registrar, spawn(registrar, init, [])),
%    B = spawn(bot, init, []),

%    receive Msg -> io:format('~w~n', [Msg]) end,
%    send(register, [self()]),
 
    %RA = spawn(registrar_agent, init, [self(), whereis(registrar)]),

%    timer:sleep(5000),

    %registrar_agent:uninit(self(), RA),
   
%    registrar:uninit(registrar).
