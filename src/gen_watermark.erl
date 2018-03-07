-module(gen_watermark).
-author('manuel@altenwald.com').

-behaviour(gen_server).

-type state_data() :: term().
-type state_name() :: normal | high | overload.
-type reason() :: term().
-type from() :: {pid(), any()}.

-type watermark_ret() :: {ok, state_data()} |
                         {stop, reason(), state_data()}.

-callback init([any()]) -> {ok, state_data()} | {stop, reason()}.
-callback handle_overload(state_name(), integer(), state_data()) -> watermark_ret().
-callback handle_high(state_name(), integer(), state_data()) -> watermark_ret().
-callback handle_low(state_name(), integer(), state_data()) -> watermark_ret().

-callback handle_info(state_name(), any(), state_data()) -> watermark_ret().
-callback handle_cast(state_name(), any(), state_data()) -> watermark_ret().
-callback handle_call(state_name(), any(), from(), state_data()) ->
          {reply, any(), state_data()} |
          {ok, state_data()} |
          {stop, reason(), any(), state_data()} |
          {stop, reason(), state_data()}.

-callback code_change(term(), state_name(), state_data(), term()) ->
          {ok, term()} | {error, term()}.

-callback terminate(reason(), state_name(), state_data()) -> any().

-optional_callbacks([handle_info/3,
                     handle_cast/3,
                     handle_call/4,
                     code_change/4,
                     terminate/3]).

-export([start_link/7,
         start_link/8,
         stop/1,
         incr/1,
         incr/2,
         decr/1,
         decr/2,
         call/2,
         cast/2,
         get_state/1]).

-export([init/1,
         handle_info/2,
         handle_cast/2,
         handle_call/3,
         terminate/2,
         code_change/3]).

-record(watermark_state, {
    value = 0 :: integer(),
    low = 50 :: integer(),
    high = 75 :: integer(),
    overload = 100 :: integer(),
    mod :: module(),
    state_name :: normal | high | overload,
    substate :: term()
}).

start_link(Name, Mod, Args, Opts, InitVal, Low, High, Overload) ->
    SupArgs = [Mod, Args, InitVal, Low, High, Overload],
    gen_server:start_link(Name, ?MODULE, SupArgs, Opts).

start_link(Mod, Args, Opts, InitVal, Low, High, Overload) ->
    SupArgs = [Mod, Args, InitVal, Low, High, Overload],
    gen_server:start_link(?MODULE, SupArgs, Opts).

stop(Server) ->
    gen_server:stop(Server).

incr(Server) ->
    incr(Server, 1).

incr(Server, Value) ->
    gen_server:cast(Server, {incr, Value}).

decr(Server) ->
    decr(Server, 1).

decr(Server, Value) ->
    gen_server:cast(Server, {decr, Value}).

cast(Server, Msg) ->
    gen_server:cast(Server, {msg, Msg}).

get_state(Server) ->
    gen_server:call(Server, state).

call(Server, Msg) ->
    gen_server:call(Server, {msg, Msg}).

init([Mod, Args, InitVal, Low, High, Overload]) ->
    case Mod:init(Args) of
        {ok, SubState} ->
            {ok, #watermark_state{value = InitVal,
                                  low = Low,
                                  high = High,
                                  overload = Overload,
                                  mod = Mod,
                                  state_name = get_state(InitVal, High, Overload),
                                  substate = SubState}};
        {stop, Reason} ->
            {stop, Reason}
    end.

get_state(Val, _High, Overload) when Val >= Overload -> overload;
get_state(Val, High, _Overload) when Val >= High -> high;
get_state(_Val, _High, _Overload) -> normal.

handle_info(Info, #watermark_state{mod = Mod, substate = SubState} = WS) ->
    case erlang:function_exported(Mod, handle_info, 3) of
        true ->
            case Mod:handle_info(WS#watermark_state.state_name, Info, SubState) of
                {ok, NewSubstate} ->
                    {noreply, WS#watermark_state{substate = NewSubstate}};
                {stop, Reason, NewSubstate} ->
                    {stop, Reason, WS#watermark_state{substate = NewSubstate}}
            end;
        false ->
            {noreply, WS}
    end.

handle_cast({incr, I}, #watermark_state{value = Val, state_name = normal, mod = Mod,
                                        overload = Overload} = WS) when Val + I >= Overload ->
    case Mod:handle_overload(normal, I, WS#watermark_state.substate) of
        {ok, NewSubstate} ->
            {noreply, WS#watermark_state{value = Val + I,
                                         state_name = overload,
                                         substate = NewSubstate}};
        {stop, Reason, NewSubstate} ->
            {stop, Reason, WS#watermark_state{value = Val + I,
                                              state_name = overload,
                                              substate = NewSubstate}}
    end;
handle_cast({incr, I}, #watermark_state{value = Val, state_name = normal, mod = Mod,
                                        high = High} = WS) when Val + I >= High ->
    case Mod:handle_high(normal, I, WS#watermark_state.substate) of
        {ok, NewSubstate} ->
            {noreply, WS#watermark_state{value = Val + I,
                                         state_name = high,
                                         substate = NewSubstate}};
        {stop, Reason, NewSubstate} ->
            {stop, Reason, WS#watermark_state{value = Val + I,
                                              state_name = high,
                                              substate = NewSubstate}}
    end;
handle_cast({incr, I}, #watermark_state{value = Val, state_name = high, mod = Mod,
                                        overload = Overload} = WS) when Val + I >= Overload ->
    case Mod:handle_overload(high, I, WS#watermark_state.substate) of
        {ok, NewSubstate} ->
            {noreply, WS#watermark_state{value = Val + I,
                                         state_name = overload,
                                         substate = NewSubstate}};
        {stop, Reason, NewSubstate} ->
            {stop, Reason, WS#watermark_state{value = Val + I,
                                              state_name = overload,
                                              substate = NewSubstate}}
    end;
handle_cast({incr, _I}, #watermark_state{state_name = overload} = WS) ->
    {noreply, WS};
handle_cast({incr, I}, #watermark_state{value = Val} = WS) ->
    {noreply, WS#watermark_state{value = Val + I}};

handle_cast({decr, I}, #watermark_state{value = Val, mod = Mod, state_name = State,
                                        low = Low} = WS) when Val - I =< Low andalso
                                                              State =/= normal ->
    case Mod:handle_low(State, I, WS#watermark_state.substate) of
        {ok, NewSubstate} ->
            {noreply, WS#watermark_state{value = Val - I,
                                         state_name = normal,
                                         substate = NewSubstate}};
        {stop, Reason, NewSubstate} ->
            {stop, Reason, WS#watermark_state{value = Val - I,
                                              state_name = normal,
                                              substate = NewSubstate}}
    end;
handle_cast({decr, I}, #watermark_state{value = Val} = WS) ->
    {noreply, WS#watermark_state{value = Val - I}};

handle_cast({msg, Msg}, #watermark_state{mod = Mod, substate = SubState} = WS) ->
    case erlang:function_exported(Mod, handle_cast, 3) of
        true ->
            case Mod:handle_cast(WS#watermark_state.state_name, Msg, SubState) of
                {ok, NewSubstate} ->
                    {noreply, WS#watermark_state{substate = NewSubstate}};
                {stop, Reason, NewSubstate} ->
                    {stop, Reason, WS#watermark_state{substate = NewSubstate}}
            end;
        false ->
            {noreply, WS}
    end.

handle_call(state, _From, #watermark_state{value = Val, state_name = State} = WS) ->
    {reply, {State, Val}, WS};

handle_call({msg, Msg}, From, #watermark_state{mod = Mod, substate = SubState} = WS) ->
    case erlang:function_exported(Mod, handle_call, 4) of
        true ->
            case Mod:handle_call(WS#watermark_state.state_name, Msg, From, SubState) of
                {reply, Reply, NewSubstate} ->
                    {reply, Reply, WS#watermark_state{substate = NewSubstate}};
                {ok, NewSubstate} ->
                    {noreply, WS#watermark_state{substate = NewSubstate}};
                {stop, Reason, Reply, NewSubstate} ->
                    {stop, Reason, Reply, WS#watermark_state{substate = NewSubstate}};
                {stop, Reason, NewSubstate} ->
                    {stop, Reason, WS#watermark_state{substate = NewSubstate}}
            end;
        false ->
            {reply, {error, enoimpl}, WS}
    end.

terminate(Reason, #watermark_state{substate = SubState, mod = Mod} = WS) ->
    case erlang:function_exported(Mod, terminate, 3) of
        true ->
            Mod:terminate(Reason, WS#watermark_state.state_name, SubState);
        false ->
            ok
    end.

code_change(OldVsn, #watermark_state{substate = SubState, mod = Mod} = WS, Extra) ->
    case erlang:function_exported(Mod, code_change, 4) of
        true ->
            case Mod:code_change(OldVsn, WS#watermark_state.state_name, SubState, Extra) of
                {ok, NewSubstate} ->
                    {ok, WS#watermark_state{substate = NewSubstate}};
                {error, Error} ->
                    {error, Error}
            end;
        false ->
            {ok, WS}
    end.
