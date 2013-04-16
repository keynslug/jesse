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

-module(jesse_json_medium_jiffy).
-behaviour(jesse_json_medium).

-export([
    parse/1,
    path/2,
    path/3,
    value/2,
    value/3,
    is_object/1,
    is_array/1,
    size/1,
    fold/3
]).

-type object_key()    :: jesse_json_medium:object_key().
-type object_path()   :: jesse_json_medium:object_path().
-type object_value()  :: jesse_json_medium:object_value().
-type object_prop()   :: {object_key(), object_value() | object()}.
-type object()        :: {[object_prop()]} | object_value().

-spec parse(binary()) -> object_value() | object().

parse(Bin) ->
    jiffy:decode(Bin).

-spec path(binary() | object_path(), object()) -> object_value() | object() | undefined.

path(Path, Object) ->
    path(Path, Object, undefined).

-spec path(binary() | object_path(), object(), Default) -> object_value() | object() | Default when
    Default :: any().

path([], Result, _) ->
    Result;

path([Key | Rest], {Object = [{_, _} | _]}, Default) when is_binary(Key) ->
    case lists:keyfind(Key, 1, Object) of
        {Key, Result} ->
            path(Rest, Result, Default);
        _False ->
            Default
    end;

path([_ | _], _Value, Default) ->
    Default;

path(Path, Object, Default) when is_binary(Path) ->
    path(jesse_json_medium:splitpath(Path), Object, Default);

path(_, _, _) ->
    error(badarg).

-spec value(object_key(), object()) -> object_value() | object() | undefined.

value(Key, Object) ->
    value(Key, Object, undefined).

-spec value(object_key(), object(), Default) -> object_value() | object() | Default when
    Default :: any().

value(Key, {Object = [{_, _}]}, Default) when is_binary(Key) ->
    case lists:keyfind(Key, 1, Object) of
        {Key, Value} ->
            Value;
        _False ->
            Default
    end;

value(Key, _Value, Default) when is_binary(Key) ->
    Default;

value(_, _, _) ->
    error(badarg).

-spec is_object(any()) -> boolean().

is_object({[]}) ->
    true;

is_object({[{_, _} | _]}) ->
    true;

is_object(_) ->
    false.

-spec is_array(any()) -> boolean().

is_array(L) ->
    is_list(L).

-spec size(any()) -> non_neg_integer().

size({Object}) ->
    length(Object);

size(Array) when is_list(Array) ->
    length(Array).

-spec fold(fun ((object_key() | non_neg_integer(), object_value() | object(), Acc) -> Acc), Acc, object()) -> Acc when
    Acc :: any().

fold(FoldFun, Acc, {Props}) ->
    fold_props(FoldFun, Acc, Props);

fold(FoldFun, Acc, Elements) when is_list(Elements) ->
    fold_elems(FoldFun, Acc, 0, Elements);

fold(_, _, _) ->
    error(badarg).

fold_props(_FoldFun, Acc, []) ->
    Acc;

fold_props(FoldFun, Acc, [{Key, Value} | Rest]) ->
    fold_props(FoldFun, FoldFun(Key, Value, Acc), Rest).

fold_elems(_FoldFun, Acc, _N, []) ->
    Acc;

fold_elems(FoldFun, Acc, N, [Value | Rest]) ->
    fold_elems(FoldFun, FoldFun(N, Value, Acc), N + 1, Rest).
