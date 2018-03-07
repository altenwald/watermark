-module(gen_watermark_tests).

-include_lib("eunit/include/eunit.hrl").

start() ->
    {ok, PID} = gen_watermark:start_link(simple_watermark, [], [], 0, 25, 75, 100),
    ?assert(erlang:is_process_alive(PID)),
    {ok, PID}.

start_com() ->
    {ok, PID} = gen_watermark:start_link(complete_watermark, [], [], 0, 25, 75, 100),
    ?assert(erlang:is_process_alive(PID)),
    {ok, PID}.

start_val(Val) ->
    {ok, PID} = gen_watermark:start_link(simple_watermark, [], [], Val, 25, 75, 100),
    ?assert(erlang:is_process_alive(PID)),
    {ok, PID}.

stop(PID) ->
    gen_watermark:stop(PID),
    ?assertNot(erlang:is_process_alive(PID)),
    ok.

start_name_test() ->
    {ok, PID} = gen_watermark:start_link({local, abc}, simple_watermark, [], [], 0, 25, 75, 100),
    ?assertEqual({normal, 0}, gen_watermark:get_state(abc)),
    stop(whereis(abc)).

incr_and_decr_test() ->
    {ok, PID} = start(),
    gen_watermark:incr(PID),
    ?assertEqual({normal, 1}, gen_watermark:get_state(PID)),
    gen_watermark:decr(PID),
    ?assertEqual({normal, 0}, gen_watermark:get_state(PID)),
    stop(PID).

to_high_test() ->
    {ok, PID} = start(),
    gen_watermark:incr(PID, 75),
    ?assertEqual({high, 75}, gen_watermark:get_state(PID)),
    gen_watermark:decr(PID, 50),
    ?assertEqual({normal, 25}, gen_watermark:get_state(PID)),
    stop(PID).

to_overload_test() ->
    {ok, PID} = start(),
    gen_watermark:incr(PID, 100),
    ?assertEqual({overload, 100}, gen_watermark:get_state(PID)),
    gen_watermark:decr(PID, 75),
    ?assertEqual({normal, 25}, gen_watermark:get_state(PID)),
    gen_watermark:incr(PID, 50),
    ?assertEqual({high, 75}, gen_watermark:get_state(PID)),
    gen_watermark:incr(PID, 25),
    ?assertEqual({overload, 100}, gen_watermark:get_state(PID)),
    gen_watermark:incr(PID, 100),
    ?assertEqual({overload, 100}, gen_watermark:get_state(PID)),
    stop(PID).

to_normal_test() ->
    {ok, PID} = start(),
    gen_watermark:incr(PID, 100),
    gen_watermark:decr(PID, 74),
    ?assertEqual({overload, 26}, gen_watermark:get_state(PID)),
    gen_watermark:decr(PID),
    ?assertEqual({normal, 25}, gen_watermark:get_state(PID)),
    stop(PID).

send_info_cast_call_test() ->
    {ok, PID} = start(),
    PID ! info,
    ?assert(erlang:is_process_alive(PID)),
    gen_watermark:cast(PID, msg),
    ?assert(erlang:is_process_alive(PID)),
    ?assertEqual({error, enoimpl}, gen_watermark:call(PID, msg)),
    stop(PID).

send_info_cast_call_complete_test() ->
    {ok, PID} = start_com(),
    PID ! info,
    ?assert(erlang:is_process_alive(PID)),
    gen_watermark:cast(PID, msg),
    ?assert(erlang:is_process_alive(PID)),
    ?assertEqual(give, gen_watermark:call(PID, ask)),
    stop(PID).

start_normal_test() ->
    {ok, PID} = start_val(25),
    ?assertEqual({normal, 25}, gen_watermark:get_state(PID)),
    stop(PID).

start_high_test() ->
    {ok, PID} = start_val(77),
    ?assertEqual({high, 77}, gen_watermark:get_state(PID)),
    stop(PID).

start_overload_test() ->
    {ok, PID} = start_val(100),
    ?assertEqual({overload, 100}, gen_watermark:get_state(PID)),
    stop(PID).

