-module(word_instance, [Id, WordId, Datetime::datetime(), Source::binary()]).
-belongs_to(word).

-export([validation_tests/0]).

-export_type([source_term/0]).

%%
%%--------------------------------------------------------------------
%% @doc
%% Source is a binary_to_term(source_term()).
%%
%% @end
%%--------------------------------------------------------------------
-type source_term() :: {post, post_url(), referer()}.
-type post_url() :: http:url().
-type referer() :: http:url().


validation_tests() ->
    [{fun validate_source/0, "Invalid word source"}
    ].

validate_source() ->
    validate_source(binary_to_term(Source)).

validate_source({post, POST_URL, Referer}) ->
    case
        try
            {ok, _} = http_uri:parse(unicode:characters_to_list(Referer)),
            "//localhost:8001/word/list" = unicode:characters_to_list(POST_URL),
            pass
        catch
            _ -> fail
        end
    of
        pass -> true;
        fail -> false
    end.
