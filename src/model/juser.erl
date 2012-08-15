-module(juser, [Id, DisplayName::binary(), Email::binary()]).
-has({words, many, [{order_by, mod_datetime}, descending]}).

-export([validation_tests/0]).

validation_tests() ->
    [{fun validate_email/0, "Invalid email address"}].

validate_email() ->
    %% modified from http://www.regular-expressions.info/email.html
    RE = "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,4}$",
    Opts = [{capture, none}],
    case re:run(Email, RE, Opts) of
        match ->
            true;
        nomatch ->
            false
    end.
