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

-module(jesse_json_pointer).

-export([
    resolve/2,
    resolve/3,
    urldecode/1
]).

%%

-spec resolve(binary(), jesse:json_term()) -> jesse:json_term() | undefined.

resolve(Ref, Root) ->
    resolve(Ref, Root, jesse_database).

-spec resolve(binary(), jesse:json_term(), module()) -> jesse:json_term() | undefined.

resolve(Ref, Root, Resolver) when is_binary(Ref) ->
    resolve_ptr(urldecode(Ref), Root, Resolver).

%%

resolve_ptr(<<>>, Root, _Resolver) ->
    Root;

resolve_ptr(<<$#, Rest/binary>>, Root, _Resolver) ->
    jesse_json_medium:path(splitref(Rest), Root);

resolve_ptr(Ref, _Root, Resolver) ->
    case binary:split(Ref, [<<$#>>]) of
        [Ref] ->
            resolve_resource(Ref, Resolver);
        [Ref, Rest] ->
            jesse_json_medium:path(splitref(Rest), resolve_resource(Ref, Resolver))
    end.

%%

resolve_resource(Ref, jesse_database) ->
    try
        jesse_database:read(Ref)
    catch
        throw:{database_error, _, _} ->
            error(unresolved)
    end;

resolve_resource(Ref, Resolver) ->
    Resolver:resolve(Ref).

%%

-spec splitref(binary()) -> [binary()].

splitref(<<>>) ->
    [];

splitref(<<$/, Bin/binary>>) ->
    splitref(Bin, <<>>, []).

splitref(<<>>, Buffer, Acc) ->
    lists:reverse([Buffer | Acc]);

splitref(<<$/, Rest/binary>>, Buffer, Acc) ->
    splitref(Rest, <<>>, [Buffer | Acc]);

splitref(<<$~, $0, Rest/binary>>, Buffer, Acc) ->
    splitref(Rest, <<Buffer/binary, $~>>, Acc);

splitref(<<$~, $1, Rest/binary>>, Buffer, Acc) ->
    splitref(Rest, <<Buffer/binary, $/>>, Acc);

splitref(<<$~, _/binary>>, _Buffer, _Acc) ->
    error(badarg);

splitref(<<C, Rest/binary>>, Buffer, Acc) ->
    splitref(Rest, <<Buffer/binary, C>>, Acc).

%%

-spec urldecode(binary()) -> binary().

urldecode(Binary) ->
    urldecode(Binary, <<>>).

urldecode(<<>>, Acc) ->
    Acc;

urldecode(<<$%, H, L, Rest/binary>>, Acc) ->
    urldecode(Rest, <<Acc/binary, (unpercent(H, L))>>);

urldecode(<<$%, _/binary>>, _Acc) ->
    error(badarg);

urldecode(<<C, Rest/binary>>, Acc) ->
    urldecode(Rest, <<Acc/binary, C>>).

unpercent(H, L) ->
    fromhex(H) * 16 + fromhex(L).

fromhex(D) when D >= $0, D =< $9 ->
    D - $0;

fromhex(D) when D >= $a, D =< $f ->
    D - $a + 10;

fromhex(D) when D >= $A, D =< $F ->
    D - $A + 10;

fromhex(_) ->
    error(badarg).
