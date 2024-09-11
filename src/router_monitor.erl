%%%-------------------------------------------------------------------
%%% @author ruine
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. mai. 2024 15:14
%%%-------------------------------------------------------------------
-module(router_monitor).
-author("ruine").

%% API
-export([start/1]).

%%
%% Starts a new router_monitor process. Started by the router.
%% @param Servers -> The available servers.
%%
start(Servers) ->
  monitor(process, whereis(router)),
  loop(Servers).

%%
%% Loops the router_monitor process to receive messages.
%% @param Servers -> The available servers.
%%
loop(Servers) ->
  receive

    %%
    %% Updates the current servers list to maintain consistency.
    %% @UpdatedServers -> Updated servers received from the router.
    %%
    {update_servers, UpdatedServers} ->
      loop(UpdatedServers);

    %%
    %% Handles router termination.
    %%
    {'DOWN', _, process, _, _} ->
      {Pid, _} = spawn_monitor(fun() -> router:restart(Servers) end),
      register(router, Pid),
      io:format("---~nRegistered new router ~p.~n---~n", [Pid]),
      loop(Servers)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helper functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%

%%