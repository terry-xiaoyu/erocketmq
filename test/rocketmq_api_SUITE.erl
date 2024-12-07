-module(rocketmq_api_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).
-compile(nowarn_export_all).

-define(PROXY, "http://localhost:8081").
-define(NAME_SERVER, {"localhost", 9876}).
-define(TOPIC, "rwtopic").

all() -> [
    {group, clients}
].

suite() -> [{timetrap, {minutes, 3}}].

groups() ->
    [
        {clients, [sequence],
            [ query_route
            ]}
    ].

%% To run this test locally, start RocketMQ with Docker Compose:
%% ```
%% TAG='5.3.1' docker-compose -f .ci/rocketmq.yml up
%% ./rebar3 ct -v --readable true
%% ```

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(erocketmq),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(erocketmq),
    ok.

query_route(_) ->
    {ok, ClientRef} = erocketmq_api:connect(<<"channel_id1">>, ?PROXY, ?NAME_SERVER, #{}),
    query_route_with_created_topic(ClientRef, 15).

query_route_with_created_topic(ClientRef, RetryLeft) ->
    {ok, #{status := Status, message_queues := MessageQueues}, _}
        = erocketmq_api:query_route(ClientRef, ?TOPIC),
    case Status of
        #{code := 'TOPIC_NOT_FOUND'} when RetryLeft > 0 ->
            ct:sleep(2000),
            query_route_with_created_topic(ClientRef, RetryLeft - 1);
        #{code := 'TOPIC_NOT_FOUND'} ->
            ct:fail(topic_not_created);
        #{code := 'OK'} ->
            ?assertEqual(2, length(MessageQueues)),
            ?assertMatch(
                #{
                    id := _,
                    broker := #{
                        id := _,
                        name := Name,
                        endpoints := #{
                            scheme := 'IPv4',
                            addresses := [#{port := 8081, host := <<"localhost">>}]
                        }},
                    topic := #{name := <<?TOPIC>>, resource_namespace := <<>>},
                    permission := 'READ_WRITE',
                    accept_message_types := _
                } when is_binary(Name),
            hd(MessageQueues))
    end.
