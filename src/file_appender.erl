-module(file_appender).

-behaviour(gen_server).

-export([append/2, is_file_opened/0]).
-export([start_link/1, init/1, handle_call/3, terminate/2, handle_info/2]).

append(Path, String) ->
    GenServerPid = whereis(file_appender_server),
    Pid = case GenServerPid =:= undefined of
        true -> 
            {ok, NewPid} = start_link(Path),
            NewPid;
        false -> GenServerPid
    end,
    gen_server:call(Pid, {append, String}).

is_file_opened() ->
    GenServerPid = whereis(file_appender_server),
    Reply = case GenServerPid =:= undefined of
        true -> "No";
        false -> "Yes"
    end,
    {ok, Reply}.

% gen server

start_link(Path) ->
  gen_server:start_link({local, file_appender_server}, file_appender, [Path], []).

init([Path]) ->
    {ok, IoDevice} = file:open(Path, [append]),
    State = {Path, IoDevice, 0},
    {ok, State}.

handle_call({append, String}, _From, State) ->
    {Path, IoDevice, CurrentTimerReference} = State,
    % get "new line" symbol
    LineSeparator = io_lib:nl(),  
    % concatenate string and line separator
    StringToAdd = String ++ LineSeparator,
    
    file:write(IoDevice, StringToAdd),
    
    catch erlang:cancel_timer(CurrentTimerReference),
    
    NewTimerReference = erlang:send_after(10000, self(), close_file),

    Reply = "Line '" ++ String ++ "' added to the file '" ++ Path ++ "'",
    NewState = {Path, IoDevice, NewTimerReference},
    {reply, {ok, Reply}, NewState};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_info(close_file, State) ->
    {stop, normal, State}.

terminate(normal, State) ->
    {_, IoDevice, _} = State,
    file:close(IoDevice),
    ok.







% % -export([append_file/2]).
% -compile(export_all).

% append(Path, String) ->
%     IoDevice = open_file(Path),
%     FileManagerPid = spawn(?MODULE, file_manager, [self(), IoDevice]),
%     append_to_file(FileManagerPid, String).

% open_file(Path) ->
%     % get IoDevice of the file by its path
%     {_, IoDevice} = file:open(Path, [append]),
%     IoDevice.

% append_to_file(FileManagerPid, String) ->
%     % get "new line" symbol
%     LineSeparator = io_lib:nl(),  
%     % concatenate string and line separator
%     StringToAdd = String ++ LineSeparator,
%     FileManagerPid ! {self(), StringToAdd},
%     receive
%         {_, Msg} -> Msg;
%         {From, {closed}} -> 
%             exit(From, normal),
%             "File was closed"
%     end.

% file_manager(From, IoDevice) ->
%     receive
%         {From, {append, String}} ->
%             file:write(IoDevice, String),
%             From ! {self(), {added}}
%     after 10000 -> 
%         file:close(IoDevice),
%         From ! {self(), {closed}}
%     end.










% file({FilePath, FilePid}) ->
%     receive

%         % receive message from shell
%         % From - Pid of the shell we sent message from 
%         % 'append/close' is atom to indicate what "file" process should do
%         % String is the string we need to append to file
        
%         {From, {append, String}} ->        
            
%             % get Pid of file by its path
%             {_, FilePid} = file:open(FilePath, [append]),
%             % get "new line" symbol
%             LineSeparator = io_lib:nl(),
%             % concatenate string and line separator
%             StringToAdd = String ++ LineSeparator,
%             % append string to the file
%             % file will be created if it doesn't exist
%             file:write(FilePid, StringToAdd),
            
%             % send message back
%             From ! {self(), "'" ++ String ++ "'" ++ " was added to the file " ++ FilePath},

%             % continue receiving messages
%             file({FilePath, FilePid});
        
%         {From, {close}} ->

%             {_, Pid} = {FilePath, FilePid},
%             file:close(Pid),
%             From ! {self(), ok};

%         terminate ->
%             ok
%     end.


% append_file(FilePath, String) ->
%     {ok, Pid} = opened_file_loop(FilePath),
%     {ok, _} = Pid ! {self(), {FilePath, String}}.

% opened_file_loop(OpenedFile) ->
%     receive
%         {From, {append, FilePath, String}} ->

%             {OpenedFilePath, OpenedFilePid} = OpenedFile,

%             FileManagerPid = if OpenedFilePid =/= 0 -> OpenedFilePid;
%                                 OpenedFilePid =:= 0 -> spawn(?MODULE, file_manager, [FilePath])
%                             end,
            
%             From ! {self(), {ok, FileManagerPid}},
%             loop({FilePath, OpenedFilePid})

%     end.

% file_manager(FilePath) ->
%     receive
%         {From, {append, String}} ->
            
%             % get Pid of file by its path
%             {_, FilePid} = file:open(FilePath, [append]),
%             % get "new line" symbol
%             LineSeparator = io_lib:nl(),
%             % concatenate string and line separator
%             StringToAdd = String ++ LineSeparator,
%             % append string to the file
%             % file will be created if it doesn't exist
%             file:write(FilePid, StringToAdd),
            
%             % send message back
%             From ! {self(), "'" ++ String ++ "'" ++ " was added to the file " ++ FilePath},

%             % continue receiving messages
%             file({FilePath, FilePid});
%     end.

% open_file(FilePath) ->
%     % get Pid of file by its path
%     {_, FilePid} = file:open(FilePath, [append]),
%     FilePid;
        

% append(Pid, String) ->
%     Pid ! {self(), {append, String}},
%     receive
%         {Pid, Msg} -> Msg
%     after 3000 ->
%         Pid ! {self(), {close}},
%         "File was closed"
%     end.

% start(File) ->
%     spawn(?MODULE, file, [File]).