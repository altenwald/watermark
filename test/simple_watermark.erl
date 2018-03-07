-module(simple_watermark).
-author('manuel@altenwald.com').

-behaviour(gen_watermark).

-include_lib("eunit/include/eunit.hrl").

-export([init/1,
         handle_low/3,
         handle_high/3,
         handle_overload/3]).

init([]) ->
    {ok, []}.

handle_low(_StateName, _Val, []) ->
    % ?debugFmt("to normal from ~p (val=~p)", [_StateName, _Val]),
    {ok, []}.

handle_high(_StateName, _Val, []) ->
    % ?debugFmt("to high from ~p (val=~p)", [_StateName, _Val]),
    {ok, []}.

handle_overload(_StateName, _Val, []) ->
    % ?debugFmt("to overload from ~p (val=~p)", [_StateName, _Val]),
    {ok, []}.
