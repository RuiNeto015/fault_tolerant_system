%%%-------------------------------------------------------------------
%%% @author ruine
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. mai. 2024 15:37
%%%-------------------------------------------------------------------
-module(server).
-author("ruine").

%% API
-export([start/1, restart/2]).

%%
%% Starts a new server process. Started by the server_monitor.
%% @param RouterMachine -> Machine where the router is present.
%%
start(RouterMachine) ->
  process_flag(trap_exit, true),
  loop(RouterMachine, maps:new()).

%%
%% Restarts the server process. Called by the server_monitor when the server dies.
%% @param RouterMachine -> Machine where the router is present.
%% @param Clients -> The clients that were in the server before server dies.
%%
restart(RouterMachine, Clients) ->
  process_flag(trap_exit, true),
  lists:foreach(fun(Pid) -> link(Pid) end, maps:values(Clients)),
  loop(RouterMachine, Clients).

%%
%% Loops the server process to receive messages.
%% @param RouterMachine -> Machine where the router is present.
%% @param Clients -> The clients present in the server.
%%
loop(RouterMachine, Clients) ->
  receive

    %%
    %% Asks the router to be added to the servers list.
    %%
    {add_server} ->
      case net_adm:ping(RouterMachine) of
        pang ->
          io:format("---~nRouter is down, try again later.~n---~n");
        pong ->
          {router, RouterMachine} ! {add_server, node(self())},
          receive
            {Reply} -> io:format("---~n~s~n---~n", [Reply])
          after 3000 ->
            io:format("---~nRouter is down, try again later.~n---~n")
          end
      end,
      loop(RouterMachine, Clients);

    %%
    %% Adds a new client to the clients list.
    %% @ClientPid -> The client Pid.
    %% @ClientMachine -> The client machine.
    %%
    {join_server, ClientPid, ClientMachine} ->
      case maps:is_key(ClientMachine, Clients) of
        true ->
          io:format("---~n~p tried to join the server again.~n---~n", [machine_name(ClientMachine)]),
          {client, ClientMachine} ! {"You are already in the server."},
          loop(RouterMachine, Clients);
        false ->
          io:format("---~n~s joined the server.~n---~n", [machine_name(ClientMachine)]),
          link(ClientPid),
          UpdatedClients = maps:put(ClientMachine, ClientPid, Clients),
          {client, ClientMachine} ! {"You joined the server."},
          {server_monitor, node()} ! {update_clients, UpdatedClients},
          loop(RouterMachine, UpdatedClients)
      end;

    %%
    %% Removes a client from the clients list.
    %% @ClientPid -> The client Pid.
    %% @ClientMachine -> The client machine.
    %%
    {leave_server, ClientPid, ClientMachine} ->
      case maps:is_key(ClientMachine, Clients) of
        false ->
          io:format("---~n~p tried to leave the server. Not a member, tho.~n---~n", [machine_name(ClientMachine)]),
          {client, ClientMachine} ! {"You are not a member of this server."},
          loop(RouterMachine, Clients);
        true ->
          io:format("---~n~p left the server.~n---~n", [machine_name(ClientMachine)]),
          unlink(ClientPid),
          UpdatedClients = maps:remove(ClientMachine, Clients),
          {client, ClientMachine} ! {"You left the server successfully."},
          {server_monitor, node()} ! {update_clients, UpdatedClients},
          loop(RouterMachine, UpdatedClients)
      end;

    %%
    %% Broadcast the message requested by a client.
    %% @ClientMachine -> The client machine.
    %% @Message -> The client message to broadcast.
    %%
    {send_message, ClientMachine, Message} ->
      maps:foreach(
        fun(StoredClientMachine, _) ->
          case StoredClientMachine /= ClientMachine of
            true ->
              {client, StoredClientMachine} ! {receive_message, {machine_name(ClientMachine), Message},
                machine_name(node())};
            false -> ok
          end
        end, Clients),
      io:format("---~n~s says: ~s~nBroadcasted.~n---~n", [machine_name(ClientMachine), Message]),
      loop(RouterMachine, Clients);

    %%
    %% Handles client termination.
    %%
    {'EXIT', ClientPid, _} ->
      ClientMachine = find_machine_by_client_pid(ClientPid, Clients),
      io:format("---~nClient ~p has died.~nRemoved from clients list.~n---~n", [machine_name(ClientMachine)]),
      UpdatedClients = handle_client_down(ClientPid, Clients),
      {server_monitor, node()} ! {update_clients, UpdatedClients},
      loop(RouterMachine, UpdatedClients)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helper functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%

machine_name(MachineName) ->
  hd(string:split(atom_to_list(MachineName), "@", all)).

handle_client_down(ClientPid, Clients) ->
  Keys = [Key || {Key, Value} <- maps:to_list(Clients), Value == ClientPid],
  case Keys of
    [ClientMachine | _] -> maps:remove(ClientMachine, Clients);
    [] -> Clients
  end.

find_machine_by_client_pid(ClientPid, Clients) ->
  case lists:keyfind(ClientPid, 2, maps:to_list(Clients)) of
    {Key, _} -> Key;
    false -> not_found
  end.