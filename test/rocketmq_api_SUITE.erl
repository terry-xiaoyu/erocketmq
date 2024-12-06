-module(rocketmq_api_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).
-compile(nowarn_export_all).

-define(PROXY, "http://localhost:8081").
-define(NAME_SERVER, {"localhost", 9876}).
-define(TOPIC, "rwtopic").

all() -> [
    {group, clients},
    all_same_owner
].

groups() ->
    [
        {clients, [parallel, {repeat, 10}],
            [ query_route
            ]}
    ].

init_per_suite(Config) ->
    {ok, Config}.

end_per_suite(_Config) ->
    ok.

query_route(_) ->
    {ok, ClientRef} = erocketmq_api:connect(<<"channel_id1">>, ?PROXY, ?NAME_SERVER, #{}),
    {ok, #{status := Status, message_queues := MessageQueues}, _}
        = erocketmq_api:query_route(ClientRef, ?TOPIC),
    ?assertEqual(#{code => 'OK',message => <<"OK">>}, Status),
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
             accept_message_types := ['NORMAL']
        } when is_binary(Name),
    hd(MessageQueues)).
