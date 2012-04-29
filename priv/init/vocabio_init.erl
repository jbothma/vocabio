-module(vocabio_init).

-export([init/0]).

init() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    ibrowse:start(),
    %% TODO: supervise openid_srv
    {ok, _Pid} = gen_server:start({local, openid_srv}, openid_srv, [], []).
