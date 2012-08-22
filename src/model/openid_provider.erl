%%%-------------------------------------------------------------------
%%% @author JD Bothma <jan.bothma@erlang-solutions.com>
%%% @copyright (C) 2012, Erlang Solutions Ltd.
%%% @doc CB Model for OpenID Providers.
%%%
%%% Currently intended for nice programmatic access to a provider list.
%%%
%%% @end
%%% Created : 22 Aug 2012 by JD Bothma <jan.bothma@erlang-solutions.com>
%%%-------------------------------------------------------------------

-module(openid_provider, [Id, Endpoint, FriendlyName]).

-export([load_defaults/0]).

%% Yes, hardcoding data is naughty.
%% It would be nice to be able to have logos for the providers but at
%% least for Google, it doesn't look like it's explicitly allowed.
load_defaults() ->
    Providers = [{"https://www.google.com/accounts/o8/id", "Google"}
                 ,{"http://yahoo.com", "Yahoo"}
                ],
    load_providers(Providers).

load_providers([]) ->
    ok;
load_providers([{Endpoint_, FriendlyName_}|Rest]) ->
    Provider = openid_provider:new(id, Endpoint_, FriendlyName_),
    Provider:save(),
    load_providers(Rest).
