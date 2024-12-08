-module(erocketmq_api).

-export([ connect/4
        , disconnect/1
        , query_route/2
        , query_route/4
        , send_message/2
        , send_message/4
        ]).

-define(service_pb, erocketmq_service_pb).
-define(service_client, apache_rocketmq_v_2_messaging_service_client).

-type name_server() :: {inet:hostname(), inet:port_number()}.
-type client_ref() ::
    #{
        grpc_channel_id := term(),
        name_server := name_server(),
        x_mq_client_id => binary()
    }.

-type message() :: #{
    topic := binary(),
    body := binary(),
    user_properties => #{iodata() := iodata()},
    system_properties => ?service_pb:system_properties()
}.
-type messages() :: message() | [message()].

-dialyzer({no_unknown, [query_route/4, send_message/4]}).

-spec connect(term(), uri_string:uri_string(), name_server(), map()) ->
    {ok, client_ref()} | {error, term()}.
connect(ChannelId, ProxyUrl, {Host, Port} = NameServer, ClientOpts) ->
    case gen_tcp:connect(Host, Port, [{active, false}]) of
        {ok, Socket} ->
            ok = gen_tcp:close(Socket),
            do_connect(ChannelId, ProxyUrl, NameServer, ClientOpts);
        {error, Reason} -> {error, Reason}
    end.

do_connect(ChannelId, ProxyUrl, NameServer, ClientOpts) ->
    case grpc_client_sup:create_channel_pool(ChannelId, ProxyUrl, ClientOpts) of
        {ok, _} ->
            {ok, #{
                grpc_channel_id => ChannelId,
                ip_family => 'IPv4',
                name_server => NameServer,
                x_mq_client_id => gen_client_id(<<"erocketmq_">>)
            }};
        {error, _} = Err -> Err
    end.

-spec disconnect(client_ref()) -> ok.
disconnect(#{grpc_channel_id := ChannelId}) ->
    grpc_client_sup:stop_channel_pool(ChannelId).

query_route(ClientRef, Topic) ->
    query_route(ClientRef, Topic, #{}, #{}).

query_route(#{grpc_channel_id := ChannelId, name_server := {Host, Port},
              ip_family := AddrFamily,
              x_mq_client_id := ClientId}, Topic, GrpcMeta, GrpcOpts) ->
    NameServer = #{scheme => AddrFamily, addresses => [#{host => Host, port => Port}]},
    ?service_client:query_route(
        #{topic => #{name => Topic}, endpoints => NameServer},
        GrpcMeta#{<<"x-mq-client-id">> => ClientId},
        GrpcOpts#{channel => ChannelId}).

-spec send_message(client_ref(), messages()) -> ok.
send_message(ClientRef, Messages) ->
    send_message(ClientRef, Messages, #{}, #{}).

send_message(#{grpc_channel_id := ChannelId,
               x_mq_client_id := ClientId}, Messages, GrpcMeta, GrpcOpts) ->
    ?service_client:send_message(
        #{messages => ensure_messages(Messages)},
        GrpcMeta#{<<"x-mq-client-id">> => ClientId},
        GrpcOpts#{channel => ChannelId}
    ).

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

gen_client_id(Prefix) ->
    RandStr = gen_random_str(8),
    <<Prefix/binary, RandStr/binary>>.

gen_random_str(Len) when Len > 0, Len =< 16 ->
    Bin = crypto:strong_rand_bytes(16),
    <<Leading:Len/binary, _/binary>> = binary:encode_hex(Bin),
    Leading.

ensure_messages(Messages) when is_list(Messages) ->
    [#{
        topic => #{name => Topic},
        body => Body,
        user_properties => maps:get(user_properties, Msg, #{}),
        system_properties =>
            maps:merge(#{
                message_id => gen_random_str(16),
                message_type => 'NORMAL'
            }, maps:get(system_properties, Msg, #{}))
    } || #{topic := Topic, body := Body} = Msg <- Messages];
ensure_messages(Message) ->
    ensure_messages([Message]).
