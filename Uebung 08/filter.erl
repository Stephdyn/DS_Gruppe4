-module(filter).
-export([echo/0, filter_process/1, collector_process/0, pipeline/0]).


%%%%%%%%%%%%%%%%%%%%%%%%%%% Echo %%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Echo = spawn(filter, echo , []).

echo() ->
    receive
        stop -> ok ;
        Msg -> io:format( "Echo: ~p \n" , [Msg]), echo()
    end .


%%%%%%%%%%%%%%%%%%%%%%%%%%% Filter Process %%%%%%%%%%%%%%%%%%%%%%%%%%%
%% P2 = spawn(filter, filter_process, [2]).
%% P2 ! {set_sender, Echo}.

filter_process(Index) ->
    receive
        {set_sender, Pid} ->
            filter_process(Index, Pid, 0);  % Initialize with sender and count
        UnexpectedMessage ->
		    io:format("Unexpected message: ~p. You must first use {set sender, Pid} to specify the process to which the messages will be forwarded.~n", [UnexpectedMessage]),
            filter_process(Index)  % Ignore unsupported messages and continue
    end.

filter_process(Index, Sender, Count) ->
    receive
        {filter, Msg} ->
            NewCount = Count + 1,
            if
                NewCount rem Index == 0 ->
                    Sender ! {filter, Msg};  % Forward every I-th message
				true -> 
				    ok
            end,
            filter_process(Index, Sender, NewCount);
        {set_sender, NewSender} ->
            filter_process(Index, NewSender, Count);  % Update the sender process
        stop ->
		    io:format("Process with Index ~p terminated.~n", [Index]),
            ok;  % Terminate process
        UnexpectedMessage ->
            io:format("Unexpected message ' ~p ' will be ignored.~n", [UnexpectedMessage]),
			filter_process(Index, Sender, Count)  % Ignore unsupported messages
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%% Collector Process %%%%%%%%%%%%%%%%%%%%%%%%%%%
%% C = spawn(filter, collector_process, []).
%% C ! {set_sender, Echo}.

collector_process() ->
    receive
        {set_sender, Pid} ->
            collector_process(Pid, []);  % Initialize with sender and empty list
        UnexpectedMessage ->
		    io:format("Unexpected message: ~p. You must first use {set sender, Pid} to specify the process to which the collection will be forwarded.~n", [UnexpectedMessage]),
            collector_process()  % Ignore unsupported messages and continue
    end.

collector_process(Sender, List) ->
    receive
        reset ->
            collector_process(Sender, []);  % Reset the list
        {set_sender, NewSender} ->
            collector_process(NewSender, List);  % Update the sender process
        {filter, Msg} ->
            NewList = List ++ [Msg],
            case Sender of
                undefined -> ok;  % Do nothing if no sender is set
                _ -> Sender ! {filter, NewList}  % Forward the list
            end,
            collector_process(Sender, NewList);
        stop ->
		    io:format("Collector process terminated.~n"),
            ok;  % Terminate process
        _ ->
            collector_process(Sender, List)  % Ignore unsupported messages
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%% Pipeline %%%%%%%%%%%%%%%%%%%%%%%%%%%

pipeline() ->
    %% Initialization
    Echo = spawn(filter, echo, []),
    C = spawn(filter, collector_process, []),
    P2 = spawn(filter, filter_process, [2]),

    %% Configure the pipeline
    C ! {set_sender, Echo},
    P2 ! {set_sender, C},

    %% Send test messages
    P2!{filter,120},
    P2!{filter,109},
    P2!{filter,150},
    P2!{filter,101},
    P2!{filter,155},
    P2!{filter,114},
    P2!{filter,189},
    P2!{filter,114},
    P2!{filter,27},
    P2!{filter,121},
    P2!{filter,68},
    P2!{filter,32},
    P2!{filter,198},
    P2!{filter,99},
    P2!{filter,33},
    P2!{filter,104},
    P2!{filter,164},
    P2!{filter,114},
    P2!{filter,212},
    P2!{filter,105},
    P2!{filter,194},
    P2!{filter,115},
    P2!{filter,24},
    P2!{filter,116},
    P2!{filter,148},
    P2!{filter,109},
    P2!{filter,173},
    P2!{filter,97},
    P2!{filter,8},
    P2!{filter,115},
    P2!{filter,191},
    P2!{filter,33}.
