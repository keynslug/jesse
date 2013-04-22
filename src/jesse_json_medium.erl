%% Copyright 2013 Andrew Majorov
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @copyright 2013 Andrew Majorov
%% @author Andrew Majorov <encube.ul@gmail.com>

-module(jesse_json_medium).

-export([
    parse/2,
    path/2,
    path/3,
    value/2,
    value/3,
    size/1,
    is_object/1,
    is_array/1,
    fold/3
]).

-export([
    names/1,
    values/1,
    all/2,
    any/2,
    foreach/2
]).

%%

%% Only allowed key is a binary string.
-type object_key()   :: binary().
-type object_path()  :: [object_key()].

%% Allowed values are null, true and false, numbers, arrays and binary strings.
-type object_value() :: null | boolean() | number() | list(object_value()) | binary().

-export_type([object_key/0, object_path/0, object_value/0]).

-type object() :: any().

-callback(
    parse(binary()) -> object_value() | object()
).

-callback(
    value(object_key(), object()) -> object_value() | object() | undefined
).

-callback(
    value(object_key(), object(), Default) -> object_value() | object() | Default when
        Default :: any()
).

-callback(
    is_object(any()) -> boolean()
).

-callback(
    is_array(any()) -> boolean()
).

-callback(
    fold(fun ((object_key(), object_value() | object(), any()) -> any()), any(), object()) -> any()
).

%%

-spec parse(module(), binary()) -> object_value() | object().

parse(Medium, Binary) ->
    Result = Medium:parse(Binary),
    retag(Medium, Result).

-spec path(binary() | object_path(), object()) -> object_value() | object() | undefined.

path(Path, Object) ->
    path(Path, Object, undefined).

-spec path(binary() | object_path(), object(), Default) -> object_value() | object() | Default when
    Default :: any().

path([], Result, _Default) ->
    Result;

path([Key | Rest], Object, Default) ->
    case is_object(Object) of
        true ->
            path(Rest, value(Key, Object), Default);
        false ->
            Default
    end;

path(Path, Object, Default) when is_binary(Path) ->
    path(splitpath(Path), Object, Default);

path(_, _, _) ->
    error(badarg).

-spec value(object_key(), object()) -> object_value() | object() | undefined.

value(Path, {Medium, Object}) ->
    Result = Medium:value(Path, Object),
    retag(Medium, Result).

-spec value(object_key(), object(), Default) -> object_value() | object() | Default when
    Default :: any().

value(Path, {Medium, Object}, Default) ->
    Result = Medium:value(Path, Object, Default),
    retag(Medium, Result).

-spec is_object(any()) -> boolean().

is_object({Medium, Object}) ->
    Medium:is_object(Object);

is_object(_) ->
    false.

-spec is_array(any()) -> boolean().

is_array({Medium, Object}) ->
    Medium:is_array(Object);

is_array(_) ->
    false.

-spec size(any()) -> non_neg_integer().

size({Medium, Object}) ->
    Medium:size(Object).

-type object_i() :: object_key() | non_neg_integer().

-spec fold(fun ((object_i(), object_value() | object(), Acc) -> Acc), Acc, object()) -> Acc when
    Acc :: any().

fold(FoldFun, Acc0, {Medium, Object}) ->
    Medium:fold(fun (Key, Value, Acc) ->
        FoldFun(Key, retag(Medium, Value), Acc) end, Acc0, Object).

-spec any(fun ((object_i(), object_value() | object()) -> boolean()), object()) -> boolean().

any(CheckFun, Object) ->
    catch fold(fun
        (_, _, true) -> throw(true);
        (Key, Value, false) -> CheckFun(Key, Value);
        (_, _, _) -> error(badarg)
    end, false, Object).

-spec all(fun ((object_i(), object_value() | object()) -> boolean()), object()) -> boolean().

all(CheckFun, Object) ->
    catch fold(fun
        (_, _, false) -> throw(false);
        (Key, Value, true) -> CheckFun(Key, Value);
        (_, _, _) -> error(badarg)
    end, true, Object).

-spec foreach(fun ((object_i(), object_value() | object()) -> any()), object()) -> ok.

foreach(WalkFun, Object) ->
    fold(fun (Key, Value, Ok) -> _ = WalkFun(Key, Value), Ok end, ok, Object).

-spec names(object()) -> [object_key()].

names(Object) ->
    lists:reverse(fold(fun (Name, _Value, Acc) -> [Name | Acc] end, [], Object)).

-spec values(object()) -> [object_value()].

values(Object) ->
    lists:reverse(fold(fun (_N, Value, Acc) -> [Value | Acc] end, [], Object)).

%%

-spec splitpath(binary()) -> object_path().

splitpath(<<>>) ->
    [];

splitpath(<<$., Bin/binary>>) ->
    splitpath(Bin, <<>>, []);

splitpath(Bin) ->
    splitpath(Bin, <<>>, []).

splitpath(<<>>, Buffer, Acc) ->
    lists:reverse([Buffer | Acc]);

splitpath(<<$., Rest/binary>>, Buffer, Acc) ->
    splitpath(Rest, <<>>, [Buffer | Acc]);

splitpath(<<$\\, C, Rest/binary>>, Buffer, Acc) ->
    splitpath(Rest, <<Buffer/binary, C>>, Acc);

splitpath(<<$\\>>, _Buffer, _Acc) ->
    error(badarg);

splitpath(<<C, Rest/binary>>, Buffer, Acc) ->
    splitpath(Rest, <<Buffer/binary, C>>, Acc).

%%

retag(Medium, Object) ->
    retag(Medium:is_object(Object) or Medium:is_array(Object), Medium, Object).

retag(true, Medium, Object) ->
    {Medium, Object};

retag(false, _, Value) ->
    Value.
