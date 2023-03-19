-module(shinobi).

% You are allowed to split your Erlang code in as many files as you
% find appropriate.
% However, you MUST have a module (this file) called shinobi.

% Export at least the API:
-export([prepare/0, register_command/3, operation/2, ambush/2, report/1]).

% You may have other exports as well
-export([]).

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

-spec prepare() -> {ok, Shinobi :: shinobi()} | {error, Error :: any()}.
prepare() ->
    undefined.

-spec register_command(Shinobi :: shinobi(), Cmd :: atom(), Fun :: fun((term()) -> term())) -> ok | {error, already_defined}.
register_command(_, _, _) ->
    undefined.

-spec operation(shinobi(), keikaku()) -> {ok, operation_id()} | {error, Error :: any()}.
operation(_, _) ->
    undefined.

-spec ambush(operation_id(), fun((result()) -> any())) -> any().
ambush(_, _) ->
    undefined.

-spec report(operation_id()) -> operation_status().
report(_) ->
    undefined.
    
