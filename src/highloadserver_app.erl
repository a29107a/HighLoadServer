-module(highloadserver_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    % io:fwrite(file:get_cwd(), []),
    Param = init:get_argument(r),
    % io:fwrite(init:get_argument(r), []),
    % io:fwrite("~n/******/~n", []),
    {ok, _} = ranch:start_listener(tcp_echo, 1,
    ranch_tcp, [{port, 80}], http_protocol, get_directory(Param)),
    highloadserver_sup:start_link().

stop(_State) ->
    ok.

get_directory({ok, [Dir]}) ->
    % io:fwrite(Dir, []),
    Dir;
    % [get_directory(""),"/",Dir];
get_directory(_) ->
    {ok, Dir} = file:get_cwd(),
    % io:fwrite(Dir, []),
    Dir.
