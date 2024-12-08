# RocketMQ(5.0) Client for Erlang

`erocketmq` is an Erlang implementation for the [RocketMQ](https://rocketmq.apache.org/) client. 

RocketMQ changed its protocol in version 5.0, which involves a proxy that serves as a gateway between the client and the broker. The clients talks gRPC with the proxy, see [rocketmq-apis](https://github.com/apache/rocketmq-apis) for the protocol definition.

## APIs Implemented

NOTE: this project is still under development, see `test/rocketmq_client_SUITE.erl` for the complete list of APIs implemented.

- QueryRoute
- SendMessage
- ...

## Setup RocketMQ locally for testing

1. Download the RocketMQ (Binary) > 5.3 from [Downloads](https://rocketmq.apache.org/download/)

2. Unzip the downloaded zip file:
    
    ```bash
    unzip -q rocketmq-all-5.3.1-bin-release.zip
    ```

3. Start the NameServer:

    ```bash
    nohup sh bin/mqnamesrv &

    tail -f ~/logs/rocketmqlogs/namesrv.log
    ```

4. Start the Broker:

    ```bash
    JAVA_OPT='-XX:+IgnoreUnrecognizedVMOptions' bin/mqbroker -n localhost:9876 --enable-proxy
    ```

5. Create a topic:

    ```bash
    ./bin/mqadmin updateTopic -n localhost:9876 -p 6 -t test_topic -c DefaultCluster
    ```

6. Test `erocketmq` in the Erlang shell:

    ```bash
    rebar3 shell

    1> {ok, ClientRef} = erocketmq_api:connect(<<"channel_id1">>, "http://localhost:8081", {"localhost", 9876}, #{}).
    {ok,#{grpc_channel_id => <<"channel_id1">>,ip_family => 'IPv4', name_server => {"localhost",9876},
      x_mq_client_id => <<"erocketmq_6D473C23">>}}
    
    2> erocketmq_api:send_message(ClientRef, #{topic => <<"test_topic">>, body => <<"hello rocketmq!">>}).
    {ok,#{status => #{code => 'OK',message => <<"OK">>},
          entries =>
          [#{offset => 1,
             status => #{code => 'OK',message => <<"OK">>},
             message_id => <<"6CA96C179D4D3426">>,
             transaction_id => <<"6CA96C179D4D3426">>,
             recall_handle => <<>>}]}, [{<<"grpc-status">>,<<"0">>}]}
    ```

## Usage

### Connect to the Proxy Server

The `erocketmq_api:connect/4` returns a "ClientRef" which is used in other APIs such as `send_message/2` and `query_route/2`:

```erlang
NameServer = {"localhost", 9876},
ProxyServer = "http://localhost:8081",
ClientID = <<"channel_id1">>,
{ok, ClientRef} = erocketmq_api:connect(ClientID, ProxyServer, NameServer, #{}).
```

The `ClientID` should be unique for each client.

### Send Messages

```erlang
Topic = <<"test_topic">>,
Msg = <<"hello rocketmq!">>,
{ok, _} = erocketmq_api:send_message(ClientRef, #{topic => Topic, body => Msg}).
```
