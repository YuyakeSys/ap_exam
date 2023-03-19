-module(minimal).
-export([sneaky/0]).

return(Arg) -> Arg.

sneaky() ->
  {ok, S1} = shinobi:prepare(),
  shinobi:register_command(S1, ret, fun return/1),
  {ok, S2} = shinobi:prepare(),
  shinobi:register_command(S2, ret, fun return/1),

  shinobi:register_command(S2, forever, fun Loop(X) -> Loop(X) end),
  shinobi:register_command(S1, forever, fun Loop(X) -> Loop(X) end),

  Keikaku = {progression, [ {race, [ {rudiment, forever, 1}
                                   , {rudiment, ret, "one"}]}
                          , {race, [ {rudiment, ret, 2}
                                   , {rudiment, ret, "two"}]}
               ]},

  Me = self(),

  Report = fun (O) -> fun (Result) ->
             Me ! {O, case Result of
                         {success, "two"} -> true;
                         {success, 2}     -> true;
                         R -> io:format("~w~n", [R]), false
                       end}
           end end,

  {ok, O1} = shinobi:operation(S1, Keikaku),
  {ok, O2} = shinobi:operation(S2, Keikaku),
  shinobi:ambush(O1, Report(O1)),
  shinobi:ambush(O2, Report(O2)),

  R1 = receive {O1, Res} -> Res end,
  R2 = receive {O2, Res2} -> Res2 end,

  R1 andalso R2.
