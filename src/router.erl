%%%-------------------------------------------------------------------
%%% @author ruine
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. mai. 2024 16:30
%%%-------------------------------------------------------------------
-module(router).
-author("ruine").

%% API
-export([start/0, restart/1]).

%%
%% Starts a new router process. Responsible to also start a new router_monitor and both monitors each other.
%%
start() ->
  register(router, spawn(
    fun() ->
      io:format("---~nRegistered router ~p.~n", [self()]),
      {RouterMonitorPid, _} = spawn_monitor(router_monitor, start, [maps:new()]),
      register(router_monitor, RouterMonitorPid),
      io:format("Registered router monitor ~p.~n---~n", [RouterMonitorPid]),
      loop(RouterMonitorPid, maps:new())
    end
  )).

%%
%% Restarts the router process. Called by the router_monitor when the router dies.
%% @param Servers -> The servers that were in the router before router dies.
%%
restart(Servers) ->
  RouterMonitorPid = whereis(router_monitor),
  monitor(process, RouterMonitorPid),
  UpdatedServersList = [
    {ServerMachine, {monitor(process, {server_monitor, ServerMachine}), Clients}}
    || {ServerMachine, {_, Clients}} <- maps:to_list(Servers)
  ],
  UpdatedServers = maps:from_list(UpdatedServersList),
  loop(RouterMonitorPid, UpdatedServers).

%%
%% Loops the router process to receive messages.
%% @param RouterMonitorPid -> Pid of the router_monitor. Used to check if router_monitor dies.
%% @param Servers -> The available servers.
%%
loop(RouterMonitorPid, Servers) ->
  receive

    %%
    %% Adds a new server to the servers list.
    %% @ServerMachine -> The server machine.
    %%
    {add_server, ServerMachine} ->
      case maps:is_key(ServerMachine, Servers) of
        true ->
          io:format("---~n~p already in server's list.~n---~n", [machine_name(ServerMachine)]),
          {server, ServerMachine} ! {"The server name is already registered."},
          loop(RouterMonitorPid, Servers);
        false ->
          Ref = monitor(process, {server_monitor, ServerMachine}),
          UpdatedServers = maps:put(ServerMachine, {Ref, #{}}, Servers),
          io:format("---~nServer ~p added.~nMonitoring....~n---~n", [machine_name(ServerMachine)]),
          {server, ServerMachine} ! {"Server added successfully."},
          {router_monitor, node()} ! {update_servers, UpdatedServers},
          loop(RouterMonitorPid, UpdatedServers)
      end;

    %%
    %% Sends the available servers list to a client.
    %% @ClientMachine -> Machine of the client that requested the servers.
    %%
    {get_servers, ClientMachine} ->
      io:format("---~nReturning servers...~n---~n", []),
      {client, ClientMachine} ! {maps:keys(Servers)},
      loop(RouterMonitorPid, Servers);

    %%
    %% Updates the clients in the servers list
    %% @ServerMachine -> Server machine where clients need to be updated.
    %% @UpdatedClients -> The updated clients.
    %%
    {update_clients, ServerMachine, UpdatedClients} ->
      Ref = get_ref(ServerMachine, Servers),
      UpdatedServers = maps:put(ServerMachine, {Ref, UpdatedClients}, Servers),
      {router_monitor, node()} ! {update_servers, UpdatedServers},
      loop(RouterMonitorPid, UpdatedServers);

    %%
    %% Handles router_monitor and server_monitor termination.
    %% Starts by verifying if the death process is a server_monitor (Pid =/= RouterMonitorPid) or the router_monitor.
    %% Ref is used to verify which server_monitor died.
    %%
    {'DOWN', Ref, process, Pid, _} ->

      %%
      %% Handles server_monitor termination.
      %%
      case Pid =/= RouterMonitorPid of
        true ->
          case is_ref_present(Ref, Servers) of
            {true, ServerMachine} ->
              io:format("---~nServer monitor ~p terminated.~nRestarting...~n", [machine_name(ServerMachine)]),
              demonitor(Ref),
              Clients = get_clients_by_server_machine(ServerMachine, Servers),
              UpdatedServers1 = maps:remove(ServerMachine, Servers),
              {_, NewRef} = spawn_monitor(ServerMachine, server_monitor, restart, [node(), Clients]),
              UpdatedServers2 = maps:put(ServerMachine, {NewRef, Clients}, UpdatedServers1),
              {router_monitor, node()} ! {update_servers, UpdatedServers2},
              loop(RouterMonitorPid, UpdatedServers2);
            {false, _} ->
              io:format("---~nUnexpected server monitor termination.~n---~n", []),
              loop(RouterMonitorPid, Servers)
          end;

        %%
        %% Handles router_monitor termination.
        %%
        false ->
          {NewRouterMonitorPid, _} = spawn_monitor(router_monitor, start, [Servers]),
          register(router_monitor, NewRouterMonitorPid),
          io:format("---~nRegistered new router monitor ~p.~n---~n", [NewRouterMonitorPid]),
          loop(NewRouterMonitorPid, Servers)
      end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helper functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%

get_ref(ServerMachine, Servers) ->
  case maps:get(ServerMachine, Servers) of
    {Ref, _} -> Ref;
    _ -> undefined
  end.

is_ref_present(Ref, Servers) ->
  maps:fold(
    fun(ServerMachine, {ServerRef, _}, Acc) ->
      case ServerRef =:= Ref of
        true -> {true, ServerMachine};
        false -> Acc
      end
    end, {false, undefined}, Servers).

get_clients_by_server_machine(ServerMachine, Servers) ->
  case maps:get(ServerMachine, Servers) of
    {_, Clients} when is_map(Clients) -> Clients;
    _ -> #{}
  end.

machine_name(MachineName) ->
  hd(string:split(atom_to_list(MachineName), "@", all)).