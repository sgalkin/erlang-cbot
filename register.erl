-module(register).
-export([new/0, add/2, del/2, size/1, get/3]).

new() -> sets:new().
add(V, R) -> update(fun sets:add_element/2, V, R).
del(V, R) -> update(fun sets:del_element/2, V, R).
size(R) -> sets:size(R).
get(Self, Nth, R) ->
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
          R),
    Element.

update(M, V, R) ->
    NR = M(V, R),
    io:format("register ~w -> ~w~n", [sets:to_list(R), sets:to_list(NR)]),
    NR.
