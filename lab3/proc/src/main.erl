%%%-------------------------------------------------------------------
%%% @doc Main supervisor module for process-based implementation
%%% @end
%%%-------------------------------------------------------------------

-module(main).

%% API
-export([start/0, parse_config/0, strings_to_atoms/1]).

start() ->
    process_flag(trap_exit, true),
    io:fwrite("Starting supervisor...~n"),

    {StepSize, WindowSize, InterpolationMethods} = parse_arguments(),
    io:fwrite("Booted with -- StepSize ~p, WindowSize ~p, Methods ~p~n", 
              [StepSize, WindowSize, InterpolationMethods]),
    InterpolationPid = spawn_link(interpolation, start, [{StepSize, WindowSize, InterpolationMethods}]),
    InputPid = spawn_link(io_server, start_input, [self(), InterpolationPid]),
    OutputPid = spawn_link(io_server, start_output, []),
    supervisor_main_loop([
        {interpolation, InterpolationPid}, {io_input, InputPid}, {io_output, OutputPid}
    ]).

supervisor_main_loop(ChildProcesses) ->
    receive
        {'EXIT', ExitedProcess, ExitReason} ->
            case ExitReason of
                ok ->
                    ok;
                _ ->
                    io:fwrite(
                        "Child process ~p exited with reason: ~p~n",
                        [ExitedProcess, ExitReason]
                    ),
                    UpdatedChildProcesses = restart_failed_child(ExitedProcess, ChildProcesses),
                    supervisor_main_loop(UpdatedChildProcesses)
            end;
        {exit, ShutdownReason} ->
            io:fwrite("Shutdown requested: ~p~n", [ShutdownReason]),
            stop_all_children(ChildProcesses),
            wait_for_all_children_to_exit(length(ChildProcesses));
        _Other ->
            supervisor_main_loop(ChildProcesses)
    end.

stop_all_children([]) ->
    ok;
stop_all_children([{_Tag, ProcessId} | RemainingChildren]) ->
    ProcessId ! {exit, ok},
    stop_all_children(RemainingChildren).

wait_for_all_children_to_exit(0) ->
    io:fwrite("All child processes have terminated.~n"),
    io:fwrite("Application stopped.~n"),
    exit(self(), ok);
wait_for_all_children_to_exit(RemainingCount) ->
    receive
        {'EXIT', _FromProcess, _ExitReason} ->
            wait_for_all_children_to_exit(RemainingCount - 1)
    end.

restart_failed_child(FailedProcessId, ChildProcesses) ->
    case lists:keyfind(FailedProcessId, 2, ChildProcesses) of
        {ProcessTag, _OldPid} ->
            NewProcessId =
                case ProcessTag of
                    interpolation ->
                        {StepSize, WindowSize, InterpolationMethods} = parse_config(),
                        spawn_link(
                            interpolation,
                            start,
                            [{StepSize, WindowSize, InterpolationMethods}]
                        );
                    io_input ->
                        {_, InterpolationPid} = lists:keyfind(interpolation, 1, ChildProcesses),
                        spawn_link(io_server, start_input, [self(), InterpolationPid]);
                    io_output ->
                        spawn_link(io_server, start_output, [])
                end,
            lists:keyreplace(ProcessTag, 1, ChildProcesses, {ProcessTag, NewProcessId});
        false ->
            io:fwrite("Unknown process ~p, not restarting.~n", [FailedProcessId]),
            ChildProcesses
    end.

%% Parse command line arguments
parse_config() ->
    parse_arguments().

parse_arguments() ->
    StepSize = get_command_line_argument(freq, 1),
    WindowSize = get_command_line_argument(w, 3),
    InterpolationMethods = convert_strings_to_atoms(get_multiple_arguments(methods, [])),
    {StepSize, WindowSize, InterpolationMethods}.

get_command_line_argument(ArgumentKey, DefaultValue) ->
    case init:get_argument(ArgumentKey) of
        {ok, [[ArgumentValue]]} -> parse_numeric_value(ArgumentValue);
        {ok, []} -> DefaultValue;
        _ -> DefaultValue
    end.

get_multiple_arguments(ArgumentKey, DefaultValue) ->
    case init:get_argument(ArgumentKey) of
        {ok, ArgumentValues} -> lists:append(ArgumentValues);
        _ -> DefaultValue
    end.

strings_to_atoms(StringList) ->
    convert_strings_to_atoms(StringList).

convert_strings_to_atoms(StringList) ->
    lists:map(fun erlang:list_to_atom/1, StringList).

parse_number(Num) ->
    parse_numeric_value(Num).

parse_numeric_value(NumericString) ->
    case string:to_float(NumericString) of
        {error, _} ->
            case string:to_integer(NumericString) of
                {error, _} -> io:fwrite("Parse error");
                Integer -> element(1, Integer)
            end;
        Float ->
            element(1, Float)
    end.
