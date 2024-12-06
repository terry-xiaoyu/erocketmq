-module(erocketmq_api).

-export([connect/4, query_route/2]).

-type name_server() :: {inet:hostname(), inet:port_number()}.
-type client_ref() ::
    #{
        grpc_channel_id := term(),
        name_server := name_server(),
        x_mq_client_id => binary()
    }.

-define(service_client, apache_rocketmq_v_2_messaging_service_client).

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
                name_server => NameServer,
                x_mq_client_id => gen_client_id(<<"erocketmq_">>)
            }};
        {error, _} = Err -> Err
    end.

-dialyzer({no_unknown, [query_route/2]}).
query_route(#{grpc_channel_id := ChannelId, name_server := {Host, Port},
              x_mq_client_id := ClientId}, Topic) ->
    NameServer = #{scheme => 'IPv4', addresses => [#{host => Host, port => Port}]},
    ?service_client:query_route(
        #{topic => #{name => Topic}, endpoints => NameServer},
        #{<<"x-mq-client-id">> => ClientId},
        #{channel => ChannelId}).

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

