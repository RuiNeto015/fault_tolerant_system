%%%-------------------------------------------------------------------
%%% @author ruine
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. mai. 2024 00:21
%%%-------------------------------------------------------------------
-module(server_monitor).
-author("ruine").

%% API
-export([start/1, restart/2]).

%%
%% Starts a new server_monitor process. Responsible to also start a new server and monitors it.
%% @param RouterMachine -> Machine where the router is present.
%%
start(RouterMachine) ->
  register(server_monitor, spawn(
    fun() ->
      io:format("---~nRegistered server monitor...~n", []),
      {ServerPid, _} = spawn_monitor(server, start, [RouterMachine]),
      register(server, ServerPid),
      io:format("Registered server with name ~p.~n---~n", [machine_name(node())]),
      loop(machine_name(node()), RouterMachine, maps:new())
    end
  )).

%%
%% Restarts the server_monitor process. Called by the router when the server_monitor dies.
%% @param RouterMachine -> Machine where the router is present.
%% @param Clients -> The clients that were in the server before server_monitor dies.
%%
restart(RouterMachine, Clients) ->
  register(server_monitor, self()),
  io:format("---~nRecovered server monitor.~n---~n", []),
  monitor(process, server),
  loop(machine_name(node()), RouterMachine, Clients).

%%
%% Loops the server_monitor process to receive messages.
%% @param RouterMachine -> Machine where the router is present.
%% @param Clients -> The clients present in the server.
%%
loop(ServerName, RouterMachine, Clients) ->
  receive

    %%
    %% Updates the current clients list to maintain consistency. Sends the update to the router.
    %% @UpdatedClients -> Updated clients received from the server.
    %%
    {update_clients, UpdatedClients} ->
      {router, RouterMachine} ! {update_clients, node(), UpdatedClients},
      loop(ServerName, RouterMachine, UpdatedClients);

    %%
    %% Handles the server termination.
    %%
    {'DOWN', _, process, Pid, _} ->
      io:format("---~nServer ~p with name ~p died.~n---~n", [Pid, ServerName]),
      {ServerPid, _} = spawn_monitor(fun() -> server:restart(RouterMachine, Clients) end),
      register(server, ServerPid),
      io:format("---~nRecovered server: with name ~p.~n---~n", [ServerName]),
      loop(ServerName, RouterMachine, Clients)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helper functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%

machine_name(MachineName) ->
  hd(string:split(atom_to_list(MachineName), "@", all)).