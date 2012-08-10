-module(word_instance, [Id, WordId, Datetime::datetime(), Source::binary()]).
-belongs_to(word).

-export_type([source_term/0]).

%%
%%--------------------------------------------------------------------
%% @doc
%% Source is a binary_to_term(source_term).
%%
%% @end
%%--------------------------------------------------------------------
-type source_term() :: {post, post_url(), referrer()}.
-type post_url() :: http:url().
-type referrer() :: http:url().
