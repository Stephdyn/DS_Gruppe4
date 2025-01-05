-module(ex7).
-export([convert/2, maxitem/1, diff/3]).


%%%%%%%%%%%%%%%%%%%% converter %%%%%%%%%%%%%%%%%%%%

%% 1 inch = 2.54 cm
convert(Amount, Unit) when is_number(Amount) ->
    case Unit of
        inch ->
            {cm, Amount * 2.54};  % Convert inch to cm	    
        cm ->
            {inch, Amount / 2.54}; % Convert cm to inch
        _ ->
            {error, unsupported_unit} % Return error for unsupported units
    end.


%%%%%%%%%%%%%%%%%%%% maxitem %%%%%%%%%%%%%%%%%%%%

%% Function to handle an empty list or start recursion
maxitem([]) ->
    io:format("The starting list is empty.~n"),
    0; % Return 0 for an empty list
maxitem([H|T]) ->
    io:format("Step 1 | Recursive call with list: ~p and maximum: ~p~n", [T, H]),
    maxitem(T, H, 2). % Start recursion with the first element as the current maximum

%% Helper function for recursion
maxitem([], CurrentMax, Step) ->
    io:format("         The remaining list is empty, final maximum:~n"),
    CurrentMax; % Base case: return the current maximum when the list is empty
maxitem([H|T], CurrentMax, Step) ->
    io:format("         Comparing ~p with current maximum ~p~n", [H, CurrentMax]),
    NewMax = if
        H > CurrentMax ->
            io:format("         New maximum found: ~p~n", [H]),
            H;
        true ->
            io:format("         Current maximum remains: ~p~n", [CurrentMax]),
            CurrentMax
    end,
    io:format("Step ~p | Recursive call with list: ~p and maximum: ~p~n", [Step, T, NewMax]),
    maxitem(T, NewMax, Step + 1). % Recursive call with the tail of the list and updated maximum


%%%%%%%%%%%%%%%%%%%% differentiation %%%%%%%%%%%%%%%%%%%%

%% Verification: ex7:diff(fun(X) -> 2 * X * X * X - 12 * X + 3 end, 3, 1.0e-10).
diff(F, X, H) ->
    (F(X + H) - F(X - H)) / (2 * H).