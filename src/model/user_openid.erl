%%%
%%% Model to provide a mapping between OpenID identities and users.
%%% A user can have many identities associated with their account.
%%% Each identity can only be used for one account.
%%%
-module(user_openid, [Id, UserID, OpenID::binary()]).

-export([validation_tests/0]).

validation_tests() ->
    [{fun validate_openid/0, "Invalid OpenID"}].

validate_openid() ->
    case http_uri:parse(unicode:characters_to_list(OpenID)) of
        {ok, _} -> true;
        _ -> false
    end.
