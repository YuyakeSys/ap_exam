-module(example_keikakus).
-export([simple/0, involved/0]).

simple() ->
  {progression, [ {rudiment, write_to_file, ["hello.txt", "Can we fix it?\n"]}
                , {rudiment, write_to_file, ["hello.txt", "Yes, we can!\n"]}
                ]}.

involved() ->
  {side_by_side, [ {rudiment, print, "Hurry up!"}
                 , {race,
                    [{feint, {trap, 600}},
                     {relay,
                      {rudiment, read_file, "hello.txt"},
                      fun ({success, Content}) ->
                          {rudiment, write_to_file,
                           ["olleh.txt",
                            lists:reverse(Content)]}
                      end}
                    ]
                   }
                 ]}.
