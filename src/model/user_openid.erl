%%%
%%% Model to provide a mapping between OpenID identities and users.
%%% A user can have many identities associated with their account.
%%% Each identity can only be used for one account.
%%%
-module(user_openid, [Id, UserID, OpenID]).

-compile(export_all).
