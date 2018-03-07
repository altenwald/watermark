-module({{name}}).
-author('{{author}}').

-behaviour(gen_watermark).

-export([init/1,
         handle_low/3,
         handle_high/3,
         handle_overload/3]).

-record(state, {}).

init([]) ->
    {ok, #state{}}.

handle_low(_StateName, _Val, #state{} = State) ->
    {ok, State}.

handle_high(_StateName, _Val, #state{} = State) ->
    {ok, State}.

handle_overload(_StateName, _Val, #state{} = State) ->
    {ok, State}.
