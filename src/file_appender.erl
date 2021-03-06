-module(file_appender).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

% api exports
-export([append/2, is_file_opened/0]).

% gen server exports
-export([start_link/1, init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2]).

% api

append(Path, String) ->

    % check input
    is_string(Path),
    is_string(String),

    % get file_appender_server Pir or undefined
    % to check if process is already running
    GenServerPid = whereis(file_appender_server),
    Pid = case GenServerPid =:= undefined of
        true -> 
            % start gen server if it's not running
            {ok, NewPid} = start_link(Path),
            NewPid;
        false -> GenServerPid
    end,

    % call generic append function
    gen_server:call(Pid, {append, String}).

% function to return error if passed not a string
is_string(Input) ->
    case is_list(Input) of
        true -> ok;
        false -> erlang:error(badarg)
    end.

% function to check if server is running
is_file_opened() ->
    
    % get file_appender_server Pir or undefined
    % to check if process is already running
    GenServerPid = whereis(file_appender_server),
    Reply = case GenServerPid =:= undefined of
        true -> "No";
        false -> "Yes"
    end,
    % reply to shell
    {ok, Reply}.


% gen server

% gen server start functon
% variable Path will be passed to init function
start_link(Path) ->
    % gen server internal function which runs init function
    gen_server:start_link({local, file_appender_server}, file_appender, [Path], []).

% callback function when server started
init([Path]) ->
    % open file
    {ok, IoDevice} = file:open(Path, [append]),
    % save tuple to the gen server state
    State = {Path, IoDevice, 0},
    {ok, State}.

% generic function to handle append message
handle_call({append, String}, _From, State) ->
    % get variables from current state value
    {Path, IoDevice, CurrentTimerReference} = State,
    
    % get "new line" symbol
    LineSeparator = io_lib:nl(),  
    % concatenate string and line separator
    StringToAdd = String ++ LineSeparator,
    
    % write new string to opened file
    file:write(IoDevice, StringToAdd),

    % stop current timeout
    % written with catch to handle gen server crash if Timer doesn't exist
    catch erlang:cancel_timer(CurrentTimerReference),
    
    % create new timer by using delayed info message
    NewTimerReference = erlang:send_after(10000, self(), close_file),

    % create readable reply to show in shell
    Reply = "Line '" ++ String ++ "' added to the file '" ++ Path ++ "'",

    % update gen server state with new Timer reference
    NewState = {Path, IoDevice, NewTimerReference},

    % reply to gen server
    % 2nd parameter will be rerurned from gen server to shell
    % 3rd parameter tells gen server a new state value
    {reply, {ok, Reply}, NewState};
% function to handle all other messages
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

% function to handle delayed info message
handle_info(close_file, State) ->
    {stop, normal, State}.

% callback function called when gen server shutting down
terminate(normal, State) ->
    % get file IoDevice from State
    {_, IoDevice, _} = State,

    % close the file
    file:close(IoDevice),
    ok.



% tests

append_test_() -> 
    {setup, 
        fun() -> 
            ServerStatusBefore = whereis(file_appender_server),
            File = "text.txt",
            StringToAppend = "Test string",
            AppendStatus = append_string_to_file(File, StringToAppend),
            FileStatusBeforeTimeout = is_file_opened(),
            timer:sleep(10000),
            FileStatusAfterTimeout = is_file_opened(),
            {_, StringAppendedBinary} = file:read_file(File),
            [StringAppended, _, _] = string:replace(binary_to_list(StringAppendedBinary), "\n", ""),
            {File, ServerStatusBefore, StringToAppend, AppendStatus, FileStatusBeforeTimeout, FileStatusAfterTimeout, StringAppended}
        end,
        fun (Res) -> 
            Res 
        end,
        fun (Res) -> 
            {File,ServerStatusBefore, StringToAppend, AppendStatus, FileStatusBeforeTimeout, FileStatusAfterTimeout, StringAppended} = Res,
            file:delete(File),
            ?_assertEqual(ServerStatusBefore, undefined),
            ?_assertEqual(AppendStatus, ok),
            ?_assertEqual(FileStatusBeforeTimeout, {ok, "Yes"}),
            ?_assertEqual(FileStatusAfterTimeout, {ok, "No"}),
            ?_assertEqual(StringToAppend, StringAppended)
        end
    }.

append_string_to_file(File, String) ->
    {Status, _} = append(File, String),
    Status.

    
