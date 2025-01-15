% cristian.erl - Implementierung des Algorithmus von Cristian mit Zeit-Simulation durch den Clock-Prozess

-module(cristian).
-compile({no_auto_import, [get/1]}).  % Deaktiviert die automatische Importierung von get/1
-import(clock, [start/1, get/1, start_with_ticker/1, start_timer/2]).  % Importiere das Clock-Modul
-export([start_server/0, start_client/1, client_adjust/1, time_server/0, client/2]).

%%% Aufgabe 1: Zeitserver-Prozess %%%

% Startet den zentralen Zeitserver-Prozess.
start_server() ->
    spawn(fun() -> time_server() end).

% Die Hauptschleife des Zeitserver-Prozesses.
time_server() ->
    LocalTime = clock:start(1000),  % Startet den Clock-Prozess mit einer Geschwindigkeit von 1000ms pro Tick
    loop(LocalTime).

% Hauptschleife des Zeitservers, die auf Anfragen von Clients wartet.
loop(LocalTime) ->
    receive
        {get, Pid} -> 
            % Holt die aktuelle Zeit vom Clock-Prozess
            Time = clock:get(LocalTime),
            % Sende die Serverzeit (angebliche UTC-Zeit)
            T2 = erlang:timestamp(),
            CUTC = T2,  % Diese Zeile kann entfernt werden, wenn CUTC nicht verwendet wird
            % Die Antwort enthält CUTC, T2 und T3 (T2 und T3 als Serverzeiten)
            T3 = erlang:timestamp(),
            Pid ! {CUTC, T2, T3},
            loop(LocalTime);
        show -> 
            % Holt die aktuelle Zeit und zeigt sie an
            Time = clock:get(LocalTime),
            io:format("Zeitserver interne Zeit: ~p~n", [Time]),
            loop(LocalTime);
        stop -> 
            ok
    after 1000 -> % Optional: Warte für 1000ms
            loop(LocalTime)
    end.

%%% Aufgabe 2: Client-Prozess %%%

% Startet einen Client-Prozess mit einer bestimmten Geschwindigkeit.
start_client(Speed) ->
    ClockPid = clock:start(Speed),  % Startet den Clock-Prozess
    spawn(fun() -> client(ClockPid, Speed) end).

% Die Hauptschleife des Client-Prozesses.
client(ClockPid, Speed) ->
    receive
        % Bearbeitet die Zeitabfrage vom Client.
        {set, Value} -> 
            clock:set(ClockPid, Value),
            client(ClockPid, Speed);
        {get, Pid} -> 
            Time = clock:get(ClockPid),
            Pid ! {clock, Time},
            client(ClockPid, Speed);
        pause -> 
            clock:pause(ClockPid),
            client(ClockPid, Speed);
        resume -> 
            clock:resume(ClockPid),
            client(ClockPid, Speed);
        stop -> 
            ok;
        show -> 
            % Holt die aktuelle Zeit und zeigt sie an
            io:format("Client show message received~n"),
            Time = clock:get(ClockPid),
            io:format("Aktuelle Zeit des Clients: ~p~n", [Time]),
            client(ClockPid, Speed);   
        adjust -> 
            T1 = erlang:timestamp(),  % Lokale Zeit direkt vor der Anpassung
            Pid = self(),
            Pid ! {get, Pid},
            receive
                {CUTC, T2, T3} -> 
                    T4 = erlang:timestamp(),  % Lokale Zeit direkt nach Erhalt der Antwort
                    % Berechnung der korrigierten Zeit nach Cristian's Algorithmus
                    AdjustedTime = (T2 + T3 + T4 - T1) div 2,
                    clock:set(ClockPid, AdjustedTime),
                    io:format("Zeit angepasst: ~p~n", [AdjustedTime]),
                    client(ClockPid, Speed)
            end
    after Speed -> 
        % Selbstanpassung in regelmäßigen Abständen
        self() ! adjust,
        client(ClockPid, Speed)
    end.

%%% Aufgabe 3: Manuelle Synchronisation und Beobachtung der Nachrichten %%%

% Hilfsfunktion, um eine Zeitkorrektur bei einem bestimmten Client auszulösen.
client_adjust(ClientPid) -> 
    ClientPid ! adjust.
