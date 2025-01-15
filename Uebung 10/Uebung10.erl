% clock.erl - Ein Modul, das einen Clock-Prozess mit konfigurierbarer Geschwindigkeit definiert.

-module(uebung10).
-export([start/1, get/1, start_with_ticker/1, start_timer/2]).

% Startet den Clock-Prozess mit der angegebenen Geschwindigkeit.
start(Speed) ->
    spawn(fun() -> init(Speed) end).

% Initialisiert den Clock-Prozess mit der gegebenen Geschwindigkeit und startet mit der Zeit 0.
init(Speed) -> 
    clock(0, Speed, false).

% Die Hauptschleife des Clock-Prozesses.
clock(Time, Speed, Paused) ->
    receive
        % Setzt die interne Zeit auf den angegebenen Wert.
        {set, Value} -> 
            clock(Value, Speed, Paused);
        % Sendet die aktuelle Zeit an den angegebenen Prozess.
        {get, Pid} -> 
            Pid ! {clock, Time},
            clock(Time, Speed, Paused);
        % Pausiert das Inkrementieren der Zeit.
        pause -> 
            clock(Time, Speed, true);
        % Setzt das Inkrementieren der Zeit fort.
        resume -> 
            clock(Time, Speed, false);
        % Beendet den Clock-Prozess.
        stop -> 
            ok;
        % Handhabt das Tick-Ereignis, das die Zeit inkrementiert, wenn nicht pausiert.
        tick -> 
            NewTime = case Paused of
                        true -> Time;  % Wenn pausiert, bleibt die Zeit unverändert.
                        false -> Time + 1  % Wenn nicht pausiert, wird die Zeit um 1 erhöht.
                      end,
            clock(NewTime, Speed, Paused)
    after Speed -> 
        % Sendet eine Tick-Nachricht an sich selbst nach der angegebenen Geschwindigkeit.
        self() ! tick,
        clock(Time, Speed, Paused)
    end.

% Kapselt das Senden einer 'get'-Nachricht an einen Clock-Prozess und das Warten auf die Antwort ein.
get(Pid) -> 
    Pid ! {get, self()},
    receive 
        {clock, Time} -> Time  % Extrahiert die Zeit aus der Antwortnachricht.
    end.

% Startet den Clock-Prozess mit einem verbundenen Ticker-Subprozess.
start_with_ticker(Speed) -> 
    spawn(fun() -> init_with_ticker(Speed) end).

% Initialisiert den Clock-Prozess mit einem Ticker-Subprozess.
init_with_ticker(Speed) -> 
    % Startet den Ticker-Subprozess, der regelmäßig Tick-Nachrichten sendet.
    TickerPid = spawn(fun() -> ticker(Speed, self()) end),
    clock_with_ticker(0, TickerPid, false).

% Erweiterter Clock-Prozess, der Nachrichten verarbeitet und mit dem Ticker-Subprozess integriert ist.
clock_with_ticker(Time, TickerPid, Paused) -> 
    receive
        % Setzt die interne Zeit auf den angegebenen Wert.
        {set, Value} -> 
            clock_with_ticker(Value, TickerPid, Paused);
        % Sendet die aktuelle Zeit an den angegebenen Prozess.
        {get, Pid} -> 
            Pid ! {clock, Time},
            clock_with_ticker(Time, TickerPid, Paused);
        % Pausiert das Inkrementieren der Zeit.
        pause -> 
            clock_with_ticker(Time, TickerPid, true);
        % Setzt das Inkrementieren der Zeit fort.
        resume -> 
            clock_with_ticker(Time, TickerPid, false);
        % Beendet den Clock- und Ticker-Prozess.
        stop -> 
            TickerPid ! stop;
        % Handhabt das Tick-Ereignis vom Ticker-Subprozess.
        {tick, TickerPid} -> 
            NewTime = case Paused of
                        true -> Time;  % Wenn pausiert, bleibt die Zeit unverändert.
                        false -> Time + 1  % Wenn nicht pausiert, wird die Zeit um 1 erhöht.
                      end,
            clock_with_ticker(NewTime, TickerPid, Paused)
    end.

% Ticker-Subprozess, der periodisch Tick-Nachrichten an den Clock-Prozess sendet.
ticker(Speed, ClockPid) -> 
    receive
        % Beendet den Ticker-Subprozess.
        stop -> 
            ok
    after Speed -> 
        % Sendet eine Tick-Nachricht an den Clock-Prozess.
        ClockPid ! {tick, self()},
        ticker(Speed, ClockPid)
    end.

% Startet einen Timer-Prozess, der nach Ablauf einer bestimmten Zeitspanne eine Funktion aufruft.
start_timer(TimeSpan, Fun) -> 
    spawn(fun() -> timer(TimeSpan, Fun) end).

% Ruft die übergebene Funktion auf, wenn der Timer 0 Ticks erreicht.
timer(0, Fun) -> 
    Fun();
% Hauptschleife des Timer-Prozesses.
timer(TimeSpan, Fun) -> 
    receive
        % Beendet den Timer-Prozess.
        stop -> 
            ok
    after 1000 ->  % Annahme: 1 Tick entspricht 1000 ms.
        % Verringert die Zeitspanne und ruft sich selbst rekursiv auf.
        timer(TimeSpan - 1, Fun)
    end.

%% Time Server Process
start_time_server() ->
    spawn(fun() -> time_server(0) end).

time_server(LocalTime) ->
    receive
        {get, Pid} ->
            T2 = erlang:timestamp(),
            CUT_C = erlang:convert_time_unit(erlang:now_to_seconds(T2), second, microsecond),
            T3 = erlang:timestamp(),
            Pid ! {CUT_C, T2, T3},
            time_server(LocalTime);
        show ->
            io:format("Time Server State: ~p~n", [LocalTime]),
            time_server(LocalTime)
    end.

%% Client Processes
start_client(Speed, Name) ->
    spawn(fun() -> client(0, Speed, Name) end).

client(LocalTime, Speed, Name) ->
    receive
        {set, Value} ->
            client(Value, Speed, Name);
        {get, Pid} ->
            Pid ! {clock, LocalTime},
            client(LocalTime, Speed, Name);
        pause ->
            client(LocalTime, Speed, Name);
        resume ->
            client(LocalTime, Speed, Name);
        stop ->
            ok;
        {adjust, TimeServerPid} ->
            T1 = erlang:timestamp(),
            TimeServerPid ! {get, self()},
            receive
                {CUT_C, T2, T3} ->
                    T4 = erlang:timestamp(),
                    Delay = erlang:convert_time_unit(erlang:timestamp_to_microseconds(T4) - erlang:timestamp_to_microseconds(T1), microsecond, second),
                    AdjustedTime = CUT_C + Delay div 2,
                    client(AdjustedTime, Speed, Name)
            end;
        show ->
            io:format("Client (~p) State: ~p~n", [Name, LocalTime]),
            client(LocalTime, Speed, Name)
    after Speed ->
        self() ! {get, self()},
        client(LocalTime + 1, Speed, Name)
    end.

%% Utility functions to display internal state
show(Pid) ->
    Pid ! show.
