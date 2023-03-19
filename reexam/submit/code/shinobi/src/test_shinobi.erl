-module(test_shinobi).

% You are allowed to split your test code in as many files as you
% think is appropriate, just remember that they should all start with
% 'test_'.
% But you MUST have a module (this file) called test_shinobi.

-export([test_all/0, test_everything/0]).
-export([test/0]). % You may have other exports as well

-include_lib("eunit/include/eunit.hrl").
test_all() ->
  eunit:test(tests(),[verbose]).

tests() ->
  [{"Basic",spawn,
    [test_launch(),
    test_rudiment(),
    test_trap(),
    test_progression(),
    test_race(),
    test_side_by_side(),
    test_feint(),
    test_relay()
    ]}].

test_everything() ->
  test_all().


test_launch() ->
  [{"launch1", fun() -> 
    ?assertMatch({ok, _},shinobi:prepare())
  end}].
  

test_rudiment() ->
  [{"rudiment1", fun() -> 
    {ok, A} = shinobi:prepare(),
    shinobi:register_command(A, forever, fun Loop(X) -> Loop(X) end),
    Res = shinobi:operation(A, {rudiment, forever, 1}),
    ?assertEqual(Res, {ok, {A,1}})
  end},
  {"rudient100",fun()->
    {ok, A} = shinobi:prepare(),
    shinobi:register_command(A, forever, fun Loop(X) -> Loop(X) end),
    shinobi:operation(A, {rudiment, forever, 1}),
    Result = shinobi:report({A,1}),
    ?assertEqual(Result, {failure, timeout})
    end},
  {"rudiment100",fun()->
    {ok, A} = shinobi:prepare(),
    shinobi:register_command(A, ret, fun return2/1),
    shinobi:operation(A, {rudiment, ret, 1}),
    Result = shinobi:report({A,1}),
    ?assertEqual(Result, {success, 1})
    end},
  {"rudimentdouble",fun()->
    {ok, A} = shinobi:prepare(),
    {ok, B} = shinobi:prepare(),
    shinobi:register_command(A, double, fun return3/1),
    shinobi:register_command(B, double, fun return3/1),
    shinobi:operation(A, {rudiment, double, 4}),
    shinobi:operation(A, {rudiment, double, 8}),
    shinobi:operation(B, {rudiment, double, 32}),
    R1 = shinobi:report({A,1}),
    R2 = shinobi:report({A,2}),
    R3 = shinobi:report({B,1}),
    ?assertEqual(R1, {success, 8}),
    ?assertEqual(R2, {success, 16}),
    ?assertEqual(R3, {success, 64})
    end}].

test_trap() ->
  [{"small trap",fun()->
    {ok, A} = shinobi:prepare(),
    shinobi:register_command(A, forever, fun Loop(X) -> Loop(X) end),
    shinobi:operation(A, {trap, 1}),
    Res = shinobi:report({A,1}),
    ?assertEqual(Res, {failure, timeout})
    end}].

test_progression() ->
  [{"simple progress",
    fun()->
    {ok, A} = shinobi:prepare(),
    shinobi:register_command(A, forever, fun Loop(X) -> Loop(X) end),
    shinobi:register_command(A, ret, fun return2/1),
    Operation = {progression, [{rudiment, ret, 1}, {rudiment, ret, 1}]},
    {ok, OperationID} = shinobi:operation(A, Operation),
    ?assertEqual(shinobi:report(OperationID), {success, 1})
  end},
  {"simple timeout",fun()->
    {ok, A} = shinobi:prepare(),
    shinobi:register_command(A, forever, fun Loop(X) -> Loop(X) end),
    shinobi:register_command(A, ret, fun return2/1),
    Operation =  {progression, [{rudiment, forever, 1}, {rudiment, ret, "one"}]},
    {ok, OperationID} = shinobi:operation(A, Operation),
    ?assertEqual(shinobi:report(OperationID),{failure, timeout})
  end},
  {"two process",fun()->
    {ok, A} = shinobi:prepare(),
    {ok, B} = shinobi:prepare(),
    shinobi:register_command(A, ret, fun return2/1),
    shinobi:register_command(B, ret, fun return2/1),
    Operation =  {progression, [{rudiment, ret, 1000}, {rudiment, ret, "one"}]},
    Operation2 =  {progression, [{rudiment, ret, 1000}, {rudiment, ret, 1}]},
    {ok, OperationID} = shinobi:operation(A, Operation),
    {ok, OperationID2} = shinobi:operation(B, Operation2),
    ?assertEqual(shinobi:report(OperationID),{success,"one"}),
    ?assertEqual(shinobi:report(OperationID2),{success, 1})
  end}].


test_race() ->
  [{"simple 1",
    fun() ->
    {ok, A} = shinobi:prepare(),
    shinobi:register_command(A, forever, fun Loop(X) -> Loop(X) end),
    shinobi:register_command(A, ret, fun return2/1),
    Operation = {race, [{trap, 999}, {rudiment, ret, 1}]},
    {ok, OperationID} = shinobi:operation(A, Operation),
    Result = shinobi:report(OperationID),
    ?assertEqual(Result, {success, 1})
  end},
  {"simple 2",
    fun() ->
    {ok, A} = shinobi:prepare(),
    shinobi:register_command(A, forever, fun Loop(X) -> Loop(X) end),
    shinobi:register_command(A, ret, fun return2/1),
    Operation = {race, [{trap, 999}, {race, [{rudiment, ret, 1}, {trap, 999}]}]},
    {ok, OperationID} = shinobi:operation(A, Operation),
    Result = shinobi:report(OperationID),
    ?assertEqual(Result, {success, 1})
  end},
  {"2 mix",
    fun() ->
    {ok, A} = shinobi:prepare(),
    shinobi:register_command(A, forever, fun Loop(X) -> Loop(X) end),
    shinobi:register_command(A, ret, fun return2/1),
    Operation = {race, [{trap, 999}, {side_by_side, [{rudiment, ret, 1}, {trap, 999}]}]},
    {ok, OperationID} = shinobi:operation(A, Operation),
    Result = shinobi:report(OperationID),
    ?assertEqual(Result, {failure, []})
  end},
  {"race*3",
    fun() ->
    {ok, A} = shinobi:prepare(),
    shinobi:register_command(A, forever, fun Loop(X) -> Loop(X) end),
    shinobi:register_command(A, ret, fun return2/1),
    Operation = {race, [{trap, 999}, {race, [{race, [{rudiment, ret, 100}, {trap, 999}]}, {trap, 999}]}]},
    {ok, OperationID} = shinobi:operation(A, Operation),
    Result = shinobi:report(OperationID),
    ?assertEqual(Result, {success, 100})
  end}].


test_side_by_side() ->
  [{"simple_fail",
    fun() ->
      {ok, Shinobi} = shinobi:prepare(),
      shinobi:register_command(Shinobi, forever, fun Loop(X) -> Loop(X) end),
      shinobi:register_command(Shinobi, ret, fun return2/1),
      {ok, OperationID} = shinobi:operation(Shinobi, mkoperation(6)),
      ?assertEqual(shinobi:report(OperationID),{failure, timeout})
    end},
    {"double_fail",
    fun() ->
      {ok, Shinobi} = shinobi:prepare(),
      shinobi:register_command(Shinobi, forever, fun Loop(X) -> Loop(X) end),
      shinobi:register_command(Shinobi, ret, fun return2/1),
      Operation = {side_by_side, [{side_by_side, [{rudiment, ret,1}, {rudiment, forever,2}]}, {rudiment, forever,3}]},
      {_, OperationID} = shinobi:operation(Shinobi, Operation),
      ?assertEqual(shinobi:report(OperationID),{failure, timeout})
    end},
    {"simple_succ",
      fun()->
      {ok, Shinobi} = shinobi:prepare(),
      shinobi:register_command(Shinobi, forever, fun Loop(X) -> Loop(X) end),
      shinobi:register_command(Shinobi, ret, fun return2/1),
      Operation = {side_by_side, [{side_by_side, [{rudiment, ret,1}, {rudiment, ret,2}]}, {rudiment, ret,3}]},
      {ok, OperationID} = shinobi:operation(Shinobi, Operation),
      ?assertEqual(shinobi:report(OperationID) , {success, [[1,2], 3]})
      end},
    {"extra_side1",
      fun()->
      {ok, Shinobi} = shinobi:prepare(),
      shinobi:register_command(Shinobi, forever, fun Loop(X) -> Loop(X) end),
      shinobi:register_command(Shinobi, ret, fun return2/1),
      Operation = {side_by_side, [{side_by_side, [{rudiment, ret,1}, {rudiment, ret,2}]}, {side_by_side, [{rudiment, ret,3}, {rudiment, ret,4}]}]},
      {ok, OperationID} = shinobi:operation(Shinobi, Operation),
      ?assertEqual(shinobi:report(OperationID) , {success, [[1,2], [3,4]]})
      end},
    {"comlicated side2",
      fun()->
      {ok, Shinobi} = shinobi:prepare(),
      shinobi:register_command(Shinobi, forever, fun Loop(X) -> Loop(X) end),
      shinobi:register_command(Shinobi, ret, fun return2/1),
      Operation = {side_by_side, [{side_by_side, [{rudiment, ret,1}, {rudiment, ret,2}]}, {side_by_side, [{rudiment, ret,3}, {rudiment, ret,4},{rudiment, ret,5}]}]},
      {ok, OperationID} = shinobi:operation(Shinobi, Operation),
      ?assertEqual(shinobi:report(OperationID) , {success, [[1,2], [3,4,5]]})
      end},
    {"complicated side3",
      fun()->
      {ok, Shinobi} = shinobi:prepare(),
      shinobi:register_command(Shinobi, forever, fun Loop(X) -> Loop(X) end),
      shinobi:register_command(Shinobi, ret, fun return2/1),
      Operation = {side_by_side, [{side_by_side, [{rudiment, ret,1}, {rudiment, ret,2}]}, 
                      {side_by_side, [{side_by_side, [{rudiment, ret,1}, {rudiment, ret,2}]}, {rudiment, ret,4},{rudiment, ret,5}]}]},
      {ok, OperationID} = shinobi:operation(Shinobi, Operation),
      ?assertEqual(shinobi:report(OperationID) , {success, [[1,2], [[1,2],4,5]]})
      end},{
  "function with sleep longer than the timeout, the result should be timeout",
  fun() ->
    {ok,Shinobi} = shinobi:prepare(),
    Add_1_sleep = fun(X) -> timer:sleep(2100), X + 1 end,
    Add_5_sleep = fun(X) -> timer:sleep(2100), X + 5 end,
    Add_7 = fun(X) -> X + 7 end,
    Cmd_Add_1_sleep = add_with_sleep1,
    Cmd_Add_6_sleep = add_with_sleep2,
    Cmd_Add_7 = add_seven,
    shinobi:register_command(Shinobi, Cmd_Add_1_sleep, Add_1_sleep),
    shinobi:register_command(Shinobi, Cmd_Add_6_sleep, Add_5_sleep),
    shinobi:register_command(Shinobi, Cmd_Add_7, Add_7),
    Keikaku = {side_by_side, [{rudiment, Cmd_Add_1_sleep, 15}, {rudiment, Cmd_Add_6_sleep, 20}, {rudiment, Cmd_Add_7, 31}]},
    {ok, OperationId} = shinobi:operation(Shinobi, Keikaku),
    timer:sleep(500),
    ?assertMatch({failure,timeout}, shinobi:report(OperationId))
  end
}].
      

test_feint() ->
  [{"simple_timeout",
    fun() ->
    {ok, Shinobi} = shinobi:prepare(),
    shinobi:register_command(Shinobi, forever, fun Loop(X) -> Loop(X) end),
    shinobi:register_command(Shinobi, ret, fun return2/1),
    {ok, OperationID} = shinobi:operation(Shinobi, mkoperation(7)),
    ?assertEqual(shinobi:report(OperationID),{success, timeout})
  end},
  {"feint_failed",
  fun()-> 
    {ok, Shinobi} = shinobi:prepare(),
    shinobi:register_command(Shinobi, forever, fun Loop(X) -> Loop(X) end),
    shinobi:register_command(Shinobi, ret, fun return2/1),
    Operation = {feint, {rudiment, ret,1}},
    {ok, OperationID} = shinobi:operation(Shinobi, Operation),
    ?assertEqual(shinobi:report(OperationID),{failure, 1})
  end},
  {"race_feint",
  fun()-> 
    {ok, Shinobi} = shinobi:prepare(),
    shinobi:register_command(Shinobi, forever, fun Loop(X) -> Loop(X) end),
    shinobi:register_command(Shinobi, ret, fun return2/1),
    Operation = {feint, {race, [{trap, 999}, {rudiment, ret, 3}]}},
    {ok, OperationID} = shinobi:operation(Shinobi, Operation),
    ?assertEqual(shinobi:report(OperationID),{failure, 3})
  end}].
  

test_relay() ->
  [{"simple_succ",
    fun()->
      {ok, Shinobi} = shinobi:prepare(),
    shinobi:register_command(Shinobi, forever, fun Loop(X) -> Loop(X) end),
    shinobi:register_command(Shinobi, ret, fun return2/1),
    {ok, OperationID} = shinobi:operation(Shinobi, mkoperation(4)),
    ?assertEqual(shinobi:report(OperationID),{success, 2})
  end},
  {"simple_fail",
    fun()->
      {ok, Shinobi} = shinobi:prepare(),
    shinobi:register_command(Shinobi, forever, fun Loop(X) -> Loop(X) end),
    shinobi:register_command(Shinobi, ret, fun return2/1),
    Operation = {relay,{rudiment, ret,1},fun (_,_) ->{rudiment, ret,2}end},
    {ok, OperationID} = shinobi:operation(Shinobi, Operation),
    ?assertEqual(shinobi:report(OperationID), {failure, not_according_to_keikaku})
    end}].


return2(Arg) -> Arg.
return3(Arg) -> Arg+Arg.

%%minimal ret and forever functions
mkoperation(Opr) ->
  case Opr of
    1 ->
      {progression, [{rudiment, forever, 1}, {rudiment, ret, "one"}]};
    2 ->
      {rudiment, forever, 1};
    3 ->
      {trap, 99};
    4 ->
      {relay,{rudiment, ret,1},fun (_) ->{rudiment, ret,2}end};
    5 ->
      {race, [{rudiment, ret, 1}, {trap, 99}]};
    6 -> 
      {side_by_side, [ {trap, 99}, {rudiment, forever, 2}, {rudiment, forever, 3}, {rudiment, forever, 4}]};
    7 -> 
      {feint, {rudiment, forever, 1}}
    end.

test() ->
    {ok, A} = shinobi:prepare(),
    shinobi:register_command(A, forever, fun Loop(X) -> Loop(X) end),
    shinobi:register_command(A, ret, fun return2/1),
    Operation = {race, [{trap, 999}, {race, [{rudiment, ret, 1}, {trap, 999}]}]},
    io:format("Operation~p~n",[Operation]),
    {ok, OperationID} = shinobi:operation(A, Operation),
    Result = shinobi:report(OperationID),
    io:format("Result~p~n",[Result]).
