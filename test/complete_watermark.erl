-module(complete_watermark).
-author('manuel@altenwald.com').

-behaviour(gen_watermark).

-include_lib("eunit/include/eunit.hrl").

-export([init/1,
         handle_low/3,
         handle_high/3,
         handle_overload/3,

         handle_info/3,
         handle_cast/3,
         handle_call/4,
         terminate/3,
         code_change/4]).

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

handle_info(_StateName, _Info, []) ->
    {ok, []}.

handle_cast(_StateName, _Msg, []) ->
    {ok, []}.

handle_call(_StateName, ask, _From, []) ->
    {reply, give, []}.

terminate(_Reason, _StateName, []) ->
    ok.

code_change(_OldVsn, _StateName, [], _Extra) ->
    {ok, []}.
