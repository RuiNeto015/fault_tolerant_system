%%%-------------------------------------------------------------------
%%% @author ruine
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. mai. 2024 00:41
%%%-------------------------------------------------------------------
-module(client).
-author("ruine").

%% API
-export([start/0]).

%%
%% Starts a new client process.
%%
start() ->
  Pid = spawn(
    fun() ->
      process_flag(trap_exit, true),
      loop()
    end),
  register(client, Pid),
  io:format("---~nRegistered client.~n---~n", []).

%%
%% Loops the client process to receive messages.
%%
loop() ->
  receive

    %%
    %% Asks the router to return all the available servers.
    %% @RouterMachine -> Machine where the router is present.
    %%
    {get_servers, RouterMachine} ->
      case net_adm:ping(RouterMachine) of
        pang ->
          io:format("---~nRouter is down, try again later.~n---~n");
        pong ->
          {router, RouterMachine} ! {get_servers, node()},
          receive
            {Servers} ->
              io:format("---~nAvailable servers:~n", []),
              print_servers(Servers)
          after 3000 ->
            io:format("---~nRouter is down, try again later.~n---~n")
          end
      end,
      loop();

    %%
    %% Asks a server to join it.
    %% @ServerMachine -> Machine where the server is present.
    %%
    {join_server, ServerMachine} ->
      {server, ServerMachine} ! {join_server, self(), node()},
      receive
        {Reply} ->
          io:format("---~n~s~n---~n", [Reply])
      after 3000 ->
        io:format("---~nServer is down, try again later.~n---~n")
      end,
      loop();

    %%
    %% Asks the server to leave it.
    %% @ServerMachine -> Machine where the server is present.
    %%
    {leave_server, ServerMachine} ->
      {server, ServerMachine} ! {leave_server, self(), node()},
      receive
        {Reply} -> io:format("---~n~s~n---~n", [Reply])
      after 3000 ->
        io:format("---~nServer is down, try again later.~n---~n")
      end,
      loop();

    %%
    %% Asks a server to broadcast a message.
    %% @ServerMachine -> Machine where the server is present.
    %% @Message -> The message to broadcast.
    %%
    {send_message, ServerMachine, Message} ->
      {server, ServerMachine} ! {send_message, node(), Message},
      io:format("---~nMessage Sended.~n---~n"),
      loop();

    %%
    %% Waits to receive a broadcast message from the server.
    %% @{ClientName, Message} ->
    %%      ClientName -> The name of the client who sent the message.
    %%      Message -> The message sent.
    %%
    {receive_message, {ClientName, Message}, ServerName} ->
      io:format("---~n[~s] New Message in ~s~n~s: ~p~n---~n", [get_time(), ServerName, ClientName, Message]),
      loop();

    %%
    %% Handles server termination.
    %%
    {'EXIT', _, _} ->
      io:format("---~nA server you were in has died.~nWait a few seconds before communicating again.~n---~n", []),
      loop()
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helper functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%

print_servers([]) ->
  io:format("End of the list.~n---~n");
print_servers([ServerMachine | Rest]) ->
  io:format("-> Server name: ~s. Use ~p to communicate.~n", [machine_name(ServerMachine), ServerMachine]),
  print_servers(Rest).

machine_name(MachineName) ->
  hd(string:split(atom_to_list(MachineName), "@", all)).

get_time() ->
  {{_, _, _}, {Hour, Minute, _}} = calendar:local_time(),
  lists:flatten(io_lib:format("~2..0w:~2..0w", [Hour, Minute])).