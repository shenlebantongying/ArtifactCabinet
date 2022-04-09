-module(translator).
-export([loop/0]).

% a minimum service
% c(translator)
% Trans = spawn(fun translator:loop/0).
% Trans ! "a".
% Trans ! "alsdjalskd".

loop () ->
    receive
        "a" ->
            io:format("A passed ~n"),
            loop();
        "b" ->
            io:format("B passed ~n"),
            loop();
        _ ->
            io:format("Unknown message pased ~n"),
            loop()
    end.

% TODO: spawn a process on a remote machine?