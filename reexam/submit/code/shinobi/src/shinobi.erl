-module(shinobi).
-behaviour(gen_server).
% You are allowed to split your Erlang code in as many files as you
% find appropriate.
% However, you MUST have a module (this file) called shinobi.

% Export at least the API:
-export([prepare/0, register_command/3, operation/2, ambush/2, report/1]).

% You may have other exports as well
-export([]).
-export([init/1,terminate/2,handle_call/3,handle_cast/2]).

-type result() :: {success, term()}
                | {failure, term()}
                .

-type keikaku() :: {rudiment, atom(), term()}
                 | {trap, non_neg_integer()}
                 | {progression, nonempty_list(keikaku())}
                 | {race, list(keikaku())}
                 | {side_by_side, list(keikaku())}
                 | {feint, keikaku()}
                 | {relay, keikaku(), fun((result()) -> keikaku())}
                 .

-type operation_status() :: { ongoing, integer() }
                          | { success, term() }
                          | { failure, term() }
                          .

% You may change these types to something more specific, e.g. pid()
-type shinobi() :: term().
-type operation_id() :: term().

%Operation -> {ID,Status,
-spec prepare() -> {ok, Shinobi :: shinobi()} | {error, Error :: any()}.
prepare() ->
    gen_server:start_link(?MODULE, [], []).

-spec register_command(Shinobi :: shinobi(), Cmd :: atom(), Fun :: fun((term()) -> term())) -> ok | {error, already_defined}.
register_command(Shinobi, Cmd, Fun) ->
    gen_server:call(Shinobi, {register, Cmd,Fun}).

-spec operation(shinobi(), keikaku()) -> {ok, operation_id()} | {error, Error :: any()}.
operation(Shinobi, Keikakus) ->
    Reply = gen_server:call(Shinobi, {operation,Shinobi, Keikakus}),
    case Reply of 
        {ok, OperationID} ->
            gen_server:cast(Shinobi,{oper, OperationID,Keikakus}),
            Res = {ok, OperationID};
        _ ->
            Res = {error, "unknown error with operation"}
        end,
        Res.

-spec ambush(operation_id(), fun((result()) -> any())) -> any().
ambush(OperationId, Fun) ->
    case OperationId of
        {Shinobi, _} ->
            {Shinobi,_} = OperationId,
            gen_server:cast(Shinobi,{ambush, OperationId, Fun});
        _ ->
            {error, bad_operation}
        end.

-spec report(operation_id()) -> operation_status().
report(OperationId) ->
    io:format("report:start"),
    case OperationId of
        {Shinobi, _} ->
            io:format("report request: ~p~n",[[Shinobi,{report, OperationId}]]),
            gen_server:call(Shinobi,{report, OperationId});
        _ ->
            io:format("report error:"),
            {error,badarg}
        end.

%--- server API [{cmd, fun} ...]  [{{Sid,Oid},{status,value}}] [Ambush]
init([]) ->
    State =[[],[],[]],
    {ok, State}.

handle_call({register, Cmd,Fun},_From,[Func,Operation,Am]) ->
    case lists:keyfind(Cmd,1,Func) of
        false ->
            New_Func = [{Cmd,Fun}|Func],
            {reply,{ok},[New_Func,Operation,Am]};
        _ ->
            {reply,{error, already_defined},[Func,Operation,Am]}
    end;

handle_call({operation, Shinobi, Keikakus},_From,State) ->
    [Func,Operation,Ambush] = State,
    case Operation == [] of
        true -> 
            OperationID = {Shinobi,1};
        _ ->
            OperationID = {Shinobi,length(Operation)+1}
        end,
    case check_sub(Keikakus) of
        {ok,Ks} ->
            Numsub = get_totalPlan(Ks,0);
        false ->
            Numsub = 0
        end,
        New_Operation = [{OperationID,{ongoing, Numsub}}|Operation],
        {reply,{ok,OperationID},[Func,New_Operation,Ambush]};
    
handle_call({report, OperationId},_From,State)->
    [Func,Operations,Ambush] = State,
    {OperationId,{Status,Value}}=lists:keyfind(OperationId,1,Operations),
    {reply,{Status,Value},[Func,Operations,Ambush]}.

handle_cast({ambush, OperationId, Fun},[Func,Operations,Ambush])->
    {_,{Status,Value}} = lists:keyfind(OperationId,1,Operations),
    % io:format("foundvalue: ~p~n",[Value]),
    case Status == ongoing of
        false ->
            New_Ambush=lists:delete({OperationId,Fun},Ambush),
            run_ambush(Status,Value,Fun),
            {noreply,[Func,Operations,New_Ambush]};
        true -> 
            NewAmbush = [{OperationId, Fun}|Ambush],
            {noreply,[Func,Operations,NewAmbush]}
        end;

handle_cast({oper, OperationId,Keikaku},State)->
    io:format("cast operation"),
    [Func,Operations,Ambush] = State,
    case calculateK(Keikaku,State) of
        {success,R}->
            New_Operation2 = lists:keyreplace(OperationId,1,Operations,{OperationId,{success, R}}),
            check_ambush(OperationId,{OperationId,{success, R}},Ambush),
            {noreply,[Func,New_Operation2,Ambush]};
        {failure,Reason} ->
            New_Operation2 = lists:keyreplace(OperationId,1,Operations,{OperationId,{failure,Reason}}),
            check_ambush(OperationId,{OperationId,{failure,Reason}},Ambush),
            {noreply,[Func,New_Operation2,Ambush]}
    end.

terminate(_Reason, _State) ->
    ok.

%---- helper function
calculateK(Keikaku,State) -> 
    [Func,_,_] = State,
    case Keikaku of
        {rudiment, Cmd, Arg} ->
            Fun = find_func(Cmd,Func),
            % io:format("Rudiment start: ~p~n",[Fun]),
            Me = self(),
            Lid = spawn_link(fun() -> 
                listen_to_single(Me,timeout) 
            end),
            spawn(fun() -> 
                Res = Fun(Arg),
                Lid ! {success,Res},
                exit(normal)
            end),
            receive
                {success, R} ->
                    % io:format("Rudiment end: ~p~n",[R]),
                    {success, R};
                {failure, E} ->
                    % io:format("Rudiment end: ~p~n",[E]),
                    {failure, E}
            end;
        {trap, Limit} ->
            io:format("trap2: ~p~n",[Limit]),
            timer:sleep(Limit*10),
            {failure, timeout};
        {progression, SubKeikakus}->
            case SubKeikakus == [] of
                true -> 
                    {failure, "empty plan"};
                _ ->
                    sequencek(SubKeikakus,State,null)
            end;
        {race, SubKeikakus}->
            dealRace(SubKeikakus,State);
        {side_by_side, SubKeikakus} ->
            dealSide(SubKeikakus,State);
        {feint, SubKeikaku}->
            case calculateK(SubKeikaku,State) of
                {success, R} ->
                    {failure, R};
                {failure, E} ->
                    {success, E}
            end;
        {relay, SubKeikaku, FunOperation}->
            case calculateK(SubKeikaku,State) of
                {success, R} ->
                    Res = {succes,R},
                    io:format("relay first operation: ~p~n",[FunOperation]);
                {failure, E} ->
                    Res = {failure, E},
                    io:format("relay first result: ~p~n",[Res])
            end,
            try
                TempResult = FunOperation(Res),
                Result = calculateK(TempResult,State),
                Result
            catch
                _:_ ->
                    {failure, not_according_to_keikaku}
            end
        end.
                
%helper funtion to find the function {cmd, function}
find_func(Cmd,Func)->
    case lists:keyfind(Cmd,1,Func) of
        {Cmd,Function} ->
            Function;
        _ ->
            error
        end.

%function for solve progression
sequencek([],_,R)->  
    {success, R};          
sequencek([Keikakus|Total],State,_)->
    Me = self(),
    Lid = spawn_link(fun() -> 
        listen_to_single(Me,timeout) 
    end),
    spawn(fun() -> 
        Res = calculateK(Keikakus,State),
        Lid ! Res,
        exit(normal)
    end),
    receive
        {success, R1} -> 
            sequencek(Total,State,R1);
        {failure, Reason} -> 
            {failure, Reason}
    end.

check_sub(Keikaku)->
    case Keikaku of
        {side_by_side,Ks}->
            {ok,Ks};
        {race, Ks}->
            {ok,Ks};
        {progression, Ks}->
            {ok,Ks};
        _ -> false
    end.

%get total plan number
get_totalPlan([],Sum)->Sum;
get_totalPlan([K|Keikakus],Sum)->
    case (K) of 
        {rudiment, _,_} -> Sum2 = get_totalPlan(Keikakus,Sum+1);
        {trap,_} -> Sum2 = get_totalPlan(Keikakus,Sum+1);
        {progression, SubKeikakus}->
            Sum2 = get_totalPlan(Keikakus,Sum+1+get_totalPlan(SubKeikakus,0));               
        {race, SubKeikakus}  ->
            Sum2 = get_totalPlan(Keikakus,Sum+1+get_totalPlan(SubKeikakus,0));     
        {side_by_side, SubKeikakus}  ->
            Sum2 = get_totalPlan(Keikakus,Sum+1+get_totalPlan(SubKeikakus,0));     
        {feint, SubKeikaku}  -> Sum2 = get_totalPlan(SubKeikaku,Sum+1);
        {relay, SubKeikaku, _} -> Sum2 = get_totalPlan(SubKeikaku,Sum+1)
        end,
    Sum2.

%main function to deal race
dealRace(SubKeikakus,State) ->
    Me = self(),
    Pid = spawn(fun() ->
            wait_for_race(Me)
        end),
    lists:map(fun(El) ->
                    spawn(fun() ->
                                race_calculateK(El,State,Pid),
                                exit(normal)
                            end)
                    end,
                    SubKeikakus),
    % wait_for_race(Me),
    receive 
        {_,{success, R}} ->
            Res = {success, R};
        {_,{failure, _}} ->
            Res = {failure, []}
    end,
    Res.


wait_for_race(S)->
    receive
        {success, R}->
            Res = {1,{success, R}},
            S ! Res;
        {failure, _} ->
            wait_for_race(S);
        {Pid,{success, R}}->
            S ! {Pid,{success, R}};
        {_,{failure, []}}->
            wait_for_race(S)
    after 
        1000 ->
            % 1 just for no meanings
            Res = {1,{failure, []}},
            S ! Res
        end.

dealSide(SubKeikakus,State) ->
    Me = self(),
    NumSub = get_totalPlan(SubKeikakus,0),
    io:format("Side by side NumSub: ~p~n",[NumSub]),
    Pid = spawn(fun() ->
            wait_for_side(Me)
        end),
    Plist = lists:map(fun(Son) ->
                    spawn(fun() ->
                                Result = race_calculateK(Son,State,Pid),
                                Pid ! {Pid,Result},
                                exit(normal)
                            end)
                    end,
                    SubKeikakus),
    io:format("Plist main: ~p~n",[Plist]),
    solvePlist(Plist,Me,[]),
    receive
        {success, ResultList} -> 
            io:format("Receive son's ResultList: ~p~n",[ResultList]),
            {success,get_origin_result(Plist,ResultList)};
        {failure, E} -> {failure, E}
    end.

%wait for side by side answer
wait_for_side(Me)->
    receive
        {_,{failure, E}} ->

            Me ! {failure, E};
        {Pid,{success, R}} ->
            io:format("Pid: ~p~n",[Pid]),
            Me ! {Pid, R},
            wait_for_side(Me)
    end.

listen_to_single(Me,Res) ->
    receive
        {success, R} ->
            % io:format("R1: ~p~n",[R]),
            Me ! {success, R};
        {failure, E} ->
            % io:format("E: ~p~n",[E]),
            Me ! {failure, E}
        after 
            1000 ->
                Me ! {failure, Res}
        end.

run_ambush(Status,Value,Fun) ->
     spawn(fun() -> 
                Fun({Status,Value}),
                exit(normal)
            end).

check_ambush(OperationId,{OperationId,{Status, Value}},Ambush) ->
    case lists:keyfind(OperationId,1,Ambush) of
        {OperationId, Fun} ->
            run_ambush(Status,Value,Fun),
            New_Ambush = lists:delete({OperationId, Fun},Ambush),
            check_ambush(OperationId,{OperationId,{Status, Value}},New_Ambush);
        false ->
            Ambush
        end.

%helper function to deal with son nodes
race_calculateK(Keikaku,State,Pid) -> 
    Mid = self(),
    [Func,_,_] = State,
    case Keikaku of
        {rudiment, Cmd, Arg} ->
            Fun = find_func(Cmd,Func),
            Lid = spawn_link(fun() -> 
                listen_to_single(Mid,timeout) 
            end),
            spawn(fun() -> 
                Res = Fun(Arg),
                Lid ! {success,Res}
            end),
            receive
                {success, R} ->
                    Result = {success, R};
                {failure, E} ->
                    Result = {failure, E}
            end,
            Pid ! {Mid,Result};
        {trap, Limit} ->
            io:format("trap2: ~p~n",[Limit]),
            timer:sleep(Limit),
            Pid ! {Mid,{failure, timeout}};
        {progression, SubKeikakus}->
            case SubKeikakus == [] of
                true -> 
                    Result = {failure, "empty plan"};
                _ ->
                    Null = "null",
                    Result = sequencek(SubKeikakus,State,Null)
            end,
            Pid ! {Mid,Result};
        {race, SubKeikakus}->
            subdealRace(SubKeikakus,State,Pid);
        {side_by_side, SubKeikakus} ->
            subdealSide(SubKeikakus,State,Pid);
        {feint, SubKeikaku}->
            case calculateK(SubKeikaku,State) of
                {success, R} ->
                    Pid ! {Mid,{failure, R}};
                {failure, E} ->
                    Pid ! {Mid,{success, E}}
            end;
        {relay, SubKeikaku, FunOperation}->
            case calculateK(SubKeikaku,State) of
                {success, R} ->
                    Res = {succes,R},
                    io:format("relay first operation: ~p~n",[FunOperation]);
                {failure, E} ->
                    Res = {failure, E},
                    io:format("relay first result: ~p~n",[Res])
            end,
            try
                TempResult = FunOperation(Res),
                Result = calculateK(TempResult,State),
                Pid ! {Mid,Result}
            catch
                _:_ ->
                    Pid ! {Mid,{failure, not_according_to_keikaku}}
            end
        end.

% son method for race
subdealRace(SubKeikakus,State,Pid) ->
    Me = self(),
    Pid2 = spawn(fun() ->
            wait_for_race(Me)
        end),
    io:format("Race Pid2: ~p~n",[Pid2]),
    lists:map(fun(El) ->
                    spawn(fun() ->
                                race_calculateK(El,State,Pid2)
                            end)
                    end,
                    SubKeikakus),
    receive 
        {_,{success, R}} ->
           Pid ! {1,{success, R}};
        {Pid2,{failure, _}} ->
           Pid ! {1,{failure, []}}
    end.

%main function to solve list
solvePlist(Plist,Me,ResultList) ->
        receive
            {failure, ER} ->
                Me ! {failure, ER};
            {Pid,R} ->
                NewResultList = [{Pid, R}|ResultList],
                io:format("NewResultList: ~p~n",[NewResultList]),
                io:format("Plist: ~p~n",[Plist]),
                case is_list(Plist) of
                    true ->
                        case length(NewResultList) == length(Plist) of
                            true ->
                                Me ! {success, NewResultList};
                            false ->
                                solvePlist(Plist,Me,NewResultList)
                            end;
                    false ->
                        case length(NewResultList) == 1 of
                            true ->
                                Me ! {success, NewResultList};
                            false ->
                                solvePlist(Plist,Me,NewResultList)
                            end
                        end
                    after 1000 ->
                        Me ! {failure, timeout}               
    end.

% resultlist = [{Pid, R}]
get_origin_result([],ResultList) ->
    io:format("Final ResultList: ~p~n",[ResultList]),
    Final = lists:reverse(ResultList),
    Final;
get_origin_result([Pid|Plist],ResultList) ->
    Value = lists:keyfind(Pid,1,ResultList),
    % io:format("Original Value found: ~p~n",[Value]),
    case Value == false of
        true ->
            get_origin_result(Plist,[{failure, timeout}|ResultList]);
        false ->
            {Pid, Value1} = Value,
            ResultList2 = lists:delete({Pid, Value1},ResultList),
            get_origin_result(Plist,[Value1|ResultList2])
    end.

% son process of side_by_side
subdealSide(SubKeikakus,State,Pid) ->
    Mid = self(),
    Pid2 = spawn(fun() ->
            wait_for_side(Mid)
        end),
    Plist = lists:map(fun(Son) ->
                    spawn(fun() ->
                                Result = race_calculateK(Son,State,Pid2),
                                % io:format("Son result: ~p~n",[[Result]]),
                                Pid2 ! {Mid ,Result},
                                exit(normal)
                            end)
                    end,
                    SubKeikakus),
    io:format("Plist sub: ~p~n",[Plist]),
    subsolvePlist(Plist,Mid,[]),
    receive
        {success, ResultList} -> 
            ResultList2 = get_origin_result(Plist,ResultList),
            io:format("ResultList2: ~p~n",[ResultList2]),
            Pid ! {Mid,{success, ResultList2}};
        {failure, E} -> Pid ! {failure, E}
    end.

%son function to solve list
subsolvePlist(Plist,Me,ResultList) ->
        receive
            {failure, ER} ->
                Me ! {failure, ER};
            {Pid,R} ->
                NewResultList = [{Pid, R}|ResultList],
                io:format("sub_NewResultList: ~p~n",[NewResultList]),
                case is_list(Plist) of
                    true ->
                        case length(NewResultList) == length(Plist) of
                            true ->
                                Me ! {success, NewResultList};
                            false ->
                                subsolvePlist(Plist,Me,NewResultList)
                            end;
                    false ->
                        io:format("single process: ~n"),
                        case length(NewResultList) == 1 of
                            true ->
                                Me ! {success, NewResultList};
                            false ->
                                subsolvePlist(Plist,Me,NewResultList)
                            end
                        end
                    after 1000 ->
                        Me ! {failure, timeout}
    end.