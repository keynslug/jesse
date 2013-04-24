%%%=============================================================================
%% Copyright 2013 Klarna AB
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
%% @copyright 2013 Klarna AB
%% @author Alexander Dergachev <alexander.dergachev@klarna.com>
%%
%% @doc Json schema validation module.
%%
%% This module is the core of jesse, it implements the validation functionality
%% according to the standard.
%% @end
%%%=============================================================================

-module(jesse_schema_validator).

%% API
-export([ validate/2
        , is_schema_ok/1
        , get_schema_id/1
        , is_json_object/1
        ]).

%% Constant definitions for Json schema keywords
-define(TYPE,                 <<"type">>).
-define(PROPERTIES,           <<"properties">>).
-define(PATTERNPROPERTIES,    <<"patternProperties">>).
-define(ADDITIONALPROPERTIES, <<"additionalProperties">>).
-define(ITEMS,                <<"items">>).
-define(ADDITIONALITEMS,      <<"additionalItems">>).
-define(REQUIRED,             <<"required">>).
-define(DEPENDENCIES,         <<"dependencies">>).
-define(MINIMUM,              <<"minimum">>).
-define(MAXIMUM,              <<"maximum">>).
-define(EXCLUSIVEMINIMUM,     <<"exclusiveMinimum">>).
-define(EXCLUSIVEMAXIMUM,     <<"exclusiveMaximum">>).
-define(MINITEMS,             <<"minItems">>).
-define(MAXITEMS,             <<"maxItems">>).
-define(UNIQUEITEMS,          <<"uniqueItems">>).
-define(PATTERN,              <<"pattern">>).
-define(MINLENGTH,            <<"minLength">>).
-define(MAXLENGTH,            <<"maxLength">>).
-define(ENUM,                 <<"enum">>).
-define(FORMAT,               <<"format">>).               % NOT IMPLEMENTED YET
-define(DIVISIBLEBY,          <<"divisibleBy">>).
-define(DISALLOW,             <<"disallow">>).
-define(EXTENDS,              <<"extends">>).
-define(ID,                   <<"id">>).
-define(_REF,                 <<"$ref">>).

%% Constant definitions for Json types
-define(ANY,                  <<"any">>).
-define(ARRAY,                <<"array">>).
-define(BOOLEAN,              <<"boolean">>).
-define(INTEGER,              <<"integer">>).
-define(NULL,                 <<"null">>).
-define(NUMBER,               <<"number">>).
-define(OBJECT,               <<"object">>).
-define(STRING,               <<"string">>).

%%% API
%% @doc Validates json `Data' against `Schema'. If the given json is valid,
%% then it is returned to the caller as is, otherwise an exception
%% will be thrown.
-spec validate( JsonSchema :: jesse:json_term()
              , Data       :: jesse:json_term()
              ) -> {ok, jesse:json_term()}
                 | no_return().
validate(JsonSchema, Value) ->
  {check_value(Value, JsonSchema, JsonSchema, JsonSchema), Value}.

%% @doc Checks if schema is valid to be used by jesse. Currently verifies
%% that schema has id, no subschemas specify its own ids, and schemas which contain
%% $ref do not contain anything else.
-spec is_schema_ok(Schema :: jesse:json_term()) -> boolean().
is_schema_ok(Schema) ->
  try
    get_schema_id(Schema) =/= undefined andalso
      jesse_json_medium:all(fun have_no_nested_id/2, Schema)
  catch
    _:_ -> false
  end.

have_no_nested_ids(Schema) ->
  jesse_json_medium:all(fun have_no_nested_id/2, Schema).

have_no_nested_id(?ENUM, _Value) ->
  true;

have_no_nested_id(Name, Value) when Name =:= ?PROPERTIES; Name =:= ?PATTERNPROPERTIES ->
  jesse_json_medium:all(fun (_, Schema) ->
    case is_json_object(Schema) of
      true -> have_no_nested_ids(Schema);
      false -> true
    end end, Value);

have_no_nested_id(Name, Value) when
  Name =:= ?TYPE
  ; Name =:= ?ADDITIONALPROPERTIES
  ; Name =:= ?ITEMS
  ; Name =:= ?ADDITIONALITEMS
  ; Name =:= ?DEPENDENCIES
  ; Name =:= ?DISALLOW
  ; Name =:= ?EXTENDS ->
  case is_json_object(Value) of
    true -> jesse_json_medium:all(fun have_no_id/2, Value);
    false ->
      case is_array(Value) of
        true -> have_no_nested_ids(Value);
        false -> true
      end
  end;

have_no_nested_id(_, _) ->
  true.

have_no_id(?ID, _) ->
  false;
have_no_id(_, _) ->
  true.

%% @doc Returns value of "id" field from json object `Schema', assuming that
%% the given json object has such a field, otherwise an exception
%% will be thrown.
-spec get_schema_id(Schema :: jesse:json_term()) -> string().
get_schema_id(Schema) ->
  get_path(?ID, Schema).

%% @doc A naive check if the given data is a json object.
%% Returns `true' if the given data is an object, otherwise `false' is returned.
-spec is_json_object(any()) -> boolean().
is_json_object(Object) ->
  jesse_json_medium:is_object(Object).

-spec is_array(any()) -> boolean().
is_array(Object) ->
  jesse_json_medium:is_array(Object).

json_size(Object) ->
  jesse_json_medium:size(Object).

%%% Internal functions
%% @doc Goes through attributes of the given schema `JsonSchema' and
%% validates the value `Value' against them.
%% @private
check_value(Value, JsonSchema, OwnerSchema, RootSchema) ->
  jesse_json_medium:foreach(fun (What, Which) ->
    check_value(Value, What, Which, OwnerSchema, RootSchema) end, JsonSchema).

check_value(Value, ?TYPE, Type, JsonSchema, RootSchema) ->
  check_type(Value, Type, JsonSchema, RootSchema);
check_value(Value, ?PROPERTIES, Properties, _JsonSchema, RootSchema) ->
  is_json_object(Value) andalso
    check_properties(Value, Properties, RootSchema);
check_value(Value, ?PATTERNPROPERTIES, PatternProperties, _JsonSchema, RootSchema) ->
  is_json_object(Value) andalso
    check_pattern_properties(Value, PatternProperties, RootSchema);
check_value(Value, ?ADDITIONALPROPERTIES, AdditionalProperties, JsonSchema, RootSchema) ->
  is_json_object(Value) andalso
    check_additional_properties(Value, AdditionalProperties, JsonSchema, RootSchema);
check_value(Value, ?ITEMS, Items, JsonSchema, RootSchema) ->
  is_array(Value) andalso
    check_items(Value, Items, JsonSchema, RootSchema);
%% doesn't really do anything, since this attribute will be handled
%% by the previous function clause if it's presented in the schema
check_value(_Value, ?ADDITIONALITEMS, _AdditionalItems, _JsonSchema, _RootSchema) ->
  true;
%% doesn't really do anything, since this attribute will be handled
%% by the previous function clause if it's presented in the schema
check_value(_Value, ?REQUIRED, _Required, _JsonSchema, _RootSchema) ->
  true;
check_value(Value, ?DEPENDENCIES, Dependencies, _JsonSchema, RootSchema) ->
  is_json_object(Value) andalso
    check_dependencies(Value, Dependencies, RootSchema);
check_value(Value, ?MINIMUM, Minimum, JsonSchema, _RootSchema) ->
  case is_number(Value) of
    true  ->
      ExclusiveMinimum = get_path(?EXCLUSIVEMINIMUM, JsonSchema),
      check_minimum(Value, Minimum, ExclusiveMinimum);
    false ->
      true
  end;
check_value(Value, ?MAXIMUM, Maximum, JsonSchema, _RootSchema) ->
  case is_number(Value) of
    true  ->
      ExclusiveMaximum = get_path(?EXCLUSIVEMAXIMUM, JsonSchema),
      check_maximum(Value, Maximum, ExclusiveMaximum);
    false ->
      true
  end;
%% doesn't really do anything, since this attribute will be handled
%% by the previous function clause if it's presented in the schema
check_value(_Value, ?EXCLUSIVEMINIMUM, _ExclusiveMinimum, _JsonSchema, _RootSchema) ->
  true;
%% doesn't really do anything, since this attribute will be handled
%% by the previous function clause if it's presented in the schema
check_value(_Value, ?EXCLUSIVEMAXIMUM, _ExclusiveMaximum, _JsonSchema, _RootSchema) ->
  true;
check_value(Value, ?MINITEMS, MinItems, _JsonSchema, _RootSchema) ->
  is_array(Value) andalso check_min_items(Value, MinItems);
check_value(Value, ?MAXITEMS, MaxItems, _JsonSchema, _RootSchema) ->
  is_array(Value) andalso check_max_items(Value, MaxItems);
check_value(Value, ?UNIQUEITEMS, Uniqueitems, _JsonSchema, _RootSchema) ->
  is_array(Value) andalso check_unique_items(Value, Uniqueitems);
check_value(Value, ?PATTERN, Pattern, _JsonSchema, _RootSchema) ->
  is_binary(Value) andalso check_pattern(Value, Pattern);
check_value(Value, ?MINLENGTH, MinLength, _JsonSchema, _RootSchema) ->
  is_binary(Value) andalso check_min_length(Value, MinLength);
check_value(Value, ?MAXLENGTH, MaxLength, _JsonSchema, _RootSchema) ->
  is_binary(Value) andalso check_max_length(Value, MaxLength);
check_value(Value, ?ENUM, Enum, _JsonSchema, _RootSchema) ->
  check_enum(Value, Enum);
check_value(Value, ?FORMAT, Format, _JsonSchema, _RootSchema) ->
  check_format(Value, Format);
check_value(Value, ?DIVISIBLEBY, DivisibleBy, JsonSchema, _RootSchema) ->
  is_number(Value) andalso check_divisible_by(Value, DivisibleBy, JsonSchema);
check_value(Value, ?DISALLOW, Disallow, JsonSchema, RootSchema) ->
  check_disallow(Value, Disallow, JsonSchema, RootSchema);
check_value(Value, ?EXTENDS, Extends, _JsonSchema, RootSchema) ->
  check_extends(Value, Extends, RootSchema);
check_value(Value, ?_REF, Ref, _JsonSchema, RootSchema) ->
  {DerefSchema, DerefRootSchema} = deref_schema(Ref, RootSchema),
  check_value(Value, DerefSchema, DerefSchema, DerefRootSchema);
%% schema actually may contain arbitrary properties not defined in
%% specification, since spec itself does not forbid them.
check_value(_Value, _Any, _, _Schema, _RootSchema) ->
  true.

%% @doc 5.1.  type
%%
%% This attribute defines what the primitive type or the schema of the
%% instance MUST be in order to validate.  This attribute can take one
%% of two forms:
%% <dl>
%% <dt>Simple Types</dt>
%%  <dd>A string indicating a primitive or simple type. The
%%    following are acceptable string values:
%%    <dl>
%%    <dt>string</dt>  <dd>Value MUST be a string.</dd>
%%
%%    <dt>number</dt>  <dd>Value MUST be a number, floating point numbers are
%%       allowed.</dd>
%%
%%    <dt>integer</dt>  <dd>Value MUST be an integer, no floating point numbers
%%       are allowed.  This is a subset of the number type.</dd>
%%
%%    <dt>boolean</dt>  <dd>Value MUST be a boolean.</dd>
%%
%%    <dt>object</dt>  <dd>Value MUST be an object.</dd>
%%
%%    <dt>array</dt>  <dd>Value MUST be an array.</dd>
%%
%%    <dt>null</dt>  <dd>Value MUST be null.  Note this is mainly for purpose of
%%       being able use union types to define nullability.  If this type
%%       is not included in a union, null values are not allowed (the
%%       primitives listed above do not allow nulls on their own).</dd>
%%
%%    <dt>any</dt>  <dd>Value MAY be of any type including null.</dd>
%%
%%    If the property is not defined or is not in this list,
%%    then any type of value is acceptable.  Other type values MAY be used for
%%    custom purposes, but minimal validators of the specification
%%    implementation can allow any instance value on unknown type
%%    values.
%%    </dl>
%%  </dd>
%% <dt>Union Types</dt>
%%  <dd>An array of two or more simple type definitions.  Each
%%     item in the array MUST be a simple type definition or a schema.
%%     The instance value is valid if it is of the same type as one of
%%     the simple type definitions, or valid by one of the schemas, in
%%     the array.</dd>
%% </dl>
%%  For example, a schema that defines if an instance can be a string or
%%  a number would be:
%%
%%  {"type":["string","number"]}
%% @private
check_type(Value, ?STRING, JsonSchema, _RootSchema) ->
  is_binary(Value) orelse throw({data_invalid, Value, not_string, JsonSchema});
check_type(Value, ?NUMBER, JsonSchema, _RootSchema) ->
  is_number(Value) orelse throw({data_invalid, Value, not_number, JsonSchema});
check_type(Value, ?INTEGER, JsonSchema, _RootSchema) ->
  is_integer(Value) orelse throw({data_invalid, Value, not_integer, JsonSchema});
check_type(Value, ?BOOLEAN, JsonSchema, _RootSchema) ->
  is_boolean(Value) orelse throw({data_invalid, Value, not_boolean, JsonSchema});
check_type(Value, ?OBJECT, JsonSchema, _RootSchema) ->
  is_json_object(Value) orelse throw({data_invalid, Value, not_object, JsonSchema});
check_type(Value, ?ARRAY, JsonSchema, _RootSchema) ->
  is_array(Value) orelse throw({data_invalid, Value, not_array, JsonSchema});
check_type(null, ?NULL, _JsonSchema, _RootSchema) ->
  ok;
check_type(Value, ?NULL, JsonSchema, _RootSchema) ->
  throw({data_invalid, Value, not_null, JsonSchema});
check_type(_Value, ?ANY, _JsonSchema, _RootSchema) ->
  ok;
check_type(Value, UnionType, JsonSchema, RootSchema) ->
  is_array(UnionType) andalso check_union_type(Value, UnionType, JsonSchema, RootSchema).

%% @private
check_union_type(Value, UnionType, JsonSchema, RootSchema) ->
  jesse_json_medium:any( fun(_, Type) ->
                 try
                   case is_json_object(Type) of
                     true ->
                       %% case when there's a schema in the array,
                       %% then we need to validate against
                       %% that schema
                       check_value(Value, Type, Type, RootSchema),
                       true;
                     false ->
                       check_type(Value, Type, JsonSchema, RootSchema),
                       true
                   end
                 catch
                   throw:{data_invalid, _, _, _} -> false;
                   throw:{schema_invalid, _, _}  -> false
                 end
             end
           , UnionType
           ) orelse
              throw({data_invalid, Value, not_correct_type, JsonSchema}).

%% @doc 5.2.  properties
%%
%% This attribute is an object with property definitions that define the
%% valid values of instance object property values.  When the instance
%% value is an object, the property values of the instance object MUST
%% conform to the property definitions in this object.  In this object,
%% each property definition's value MUST be a schema, and the property's
%% name MUST be the name of the instance property that it defines.  The
%% instance property value MUST be valid according to the schema from
%% the property definition.  Properties are considered unordered, the
%% order of the instance properties MAY be in any order.
%% @private
check_properties(Value, Properties, RootSchema) ->
  jesse_json_medium:foreach(fun (Name, Schema) ->
    check_property(Value, Name, Schema, RootSchema) end, Properties).

check_property(Value, Name, Schema, RootSchema) ->
  case get_path(Name, Value) of
    undefined ->
%% @doc 5.7.  required
%%
%% This attribute indicates if the instance must have a value, and not
%% be undefined.  This is false by default, making the instance
%% optional.
%% @end
      case get_path(?REQUIRED, Schema) of
        true  -> throw({data_invalid, Value, missing_required_property, Name});
        _     -> ok
      end;
    Property ->
      check_value(Property, Schema, Schema, RootSchema)
  end.

%% @doc 5.3.  patternProperties
%%
%% This attribute is an object that defines the schema for a set of
%% property names of an object instance.  The name of each property of
%% this attribute's object is a regular expression pattern in the ECMA
%% 262/Perl 5 format, while the value is a schema.  If the pattern
%% matches the name of a property on the instance object, the value of
%% the instance's property MUST be valid against the pattern name's
%% schema value.
%% @private
check_pattern_properties(Value, PatternProperties, RootSchema) ->
  jesse_json_medium:foreach(fun (PropertyName, PropertyValue) ->
    jesse_json_medium:foreach(fun (Pattern, Schema) ->
      check_match(PropertyName, PropertyValue, Pattern, Schema, RootSchema)
    end, PatternProperties) end, Value).

%% @private
check_match(PropertyName, PropertyValue, Pattern, Schema, RootSchema) ->
  case re:run(PropertyName, Pattern, [{capture, none}]) of
    match   -> check_value(PropertyValue, Schema, Schema, RootSchema);
    nomatch -> ok
  end.

%% @doc 5.4.  additionalProperties
%%
%% This attribute defines a schema for all properties that are not
%% explicitly defined in an object type definition.  If specified,
%% the value MUST be a schema or a boolean.  If false is provided,
%% no additional properties are allowed beyond the properties defined in
%% the schema.  The default value is an empty schema which allows any
%% value for additional properties.
%% @private
check_additional_properties(Value, false, JsonSchema, _RootSchema) ->
  Properties        = get_path(?PROPERTIES, JsonSchema),
  PatternProperties = get_path(?PATTERNPROPERTIES, JsonSchema),
  case get_additional_properties(Value, Properties, PatternProperties) of
    []      -> ok;
    _Extras -> throw({ data_invalid
                     , {Value, _Extras}
                     , no_extra_properties_allowed
                     , JsonSchema
                     })
  end;
check_additional_properties(_Value, true, _JsonSchema, _RootSchema) ->
  ok;
check_additional_properties(Value, AdditionalProperties, JsonSchema, RootSchema) ->
  Properties        = get_path(?PROPERTIES, JsonSchema),
  PatternProperties = get_path(?PATTERNPROPERTIES, JsonSchema),
  case get_additional_properties(Value, Properties, PatternProperties) of
    []     -> ok;
    Extras -> lists:foreach( fun(Extra) ->
                                 check_value( Extra
                                            , AdditionalProperties
                                            , AdditionalProperties
                                            , RootSchema
                                            )
                             end
                           , Extras
                           )
  end.

%% @private
get_additional_properties(Value, Properties, PatternProperties) ->
  ValuePropertiesNames  = names_opt(Value),
  SchemaPropertiesNames = names_opt(Properties),
  Patterns    = names_opt(PatternProperties),
  ExtraNames0 = lists:subtract(ValuePropertiesNames, SchemaPropertiesNames),
  ExtraNames  = lists:foldl( fun(Pattern, ExtraAcc) ->
                                 filter_extra_names(Pattern, ExtraAcc)
                             end
                           , ExtraNames0
                           , Patterns
                           ),
  lists:map(fun(Name) -> get_path(Name, Value) end, ExtraNames).

names_opt(undefined) ->
  [];
names_opt(Object) ->
  jesse_json_medium:names(Object).

%% @private
filter_extra_names(Pattern, ExtraNames) ->
  Filter = fun(ExtraName) ->
               case re:run(ExtraName, Pattern, [{capture, none}]) of
                 match   -> false;
                 nomatch -> true
               end
           end,
  lists:filter(Filter, ExtraNames).

%% @doc 5.5.  items
%%
%% This attribute defines the allowed items in an instance array,
%% and MUST be a schema or an array of schemas.  The default value is an
%% empty schema which allows any value for items in the instance array.
%%
%% When this attribute value is a schema and the instance value is an
%% array, then all the items in the array MUST be valid according to the
%% schema.
%%
%% When this attribute value is an array of schemas and the instance
%% value is an array, each position in the instance array MUST conform
%% to the schema in the corresponding position for this array.  This
%% called tuple typing.  When tuple typing is used, additional items are
%% allowed, disallowed, or constrained by the "additionalItems"
%% (Section 5.6) attribute using the same rules as
%% "additionalProperties" (Section 5.4) for objects.
%% @private
check_items(Value, Items, JsonSchema, RootSchema) ->
  case is_json_object(Items) of
    true  ->
      jesse_json_medium:foreach(fun (_, Item) -> check_value(Item, Items, Items, RootSchema) end, Value);
    false ->
      case is_array(Items) of
        true  -> check_items_array(Value, Items, JsonSchema, RootSchema);
        false -> throw({schema_invalid, Items, wrong_type_items})
      end
  end.

%% @private
check_items_array(Value, Items, JsonSchema, RootSchema) ->
  Tuples = case json_size(Value) - json_size(Items) of
             0 ->
               lists:zip(
                jesse_json_medium:values(Value),
                jesse_json_medium:values(Items));
             NExtra when NExtra > 0 ->
%% @doc 5.6.  additionalItems
%%
%% This provides a definition for additional items in an array instance
%% when tuple definitions of the items is provided.  This can be false
%% to indicate additional items in the array are not allowed, or it can
%% be a schema that defines the schema of the additional items.
%% @end
               case get_path(?ADDITIONALITEMS, JsonSchema) of
                 undefined ->
                   [];
                 true ->
                   [];
                 false ->
                   throw({ data_invalid
                         , Value
                         , no_extra_items_allowed
                         , JsonSchema
                         });
                 AdditionalItems ->
                   ExtraSchemas = lists:duplicate(NExtra, AdditionalItems),
                   lists:zip(
                    jesse_json_medium:values(Value),
                    jesse_json_medium:values(Items) ++ ExtraSchemas)
               end;
             NExtra when NExtra < 0 ->
               throw({ data_invalid
                     , Value
                     , not_enought_items
                     , JsonSchema
                     })
           end,
  lists:foreach(fun({Item, Schema}) ->
    check_value(Item, Schema, Schema, RootSchema) end, Tuples).

%% @doc 5.8.  dependencies
%%
%% This attribute is an object that defines the requirements of a
%% property on an instance object.  If an object instance has a property
%% with the same name as a property in this attribute's object, then the
%% instance must be valid against the attribute's property value
%% (hereafter referred to as the "dependency value").
%%
%% The dependency value can take one of two forms:
%% <dl>
%% <dt>Simple Dependency</dt>  <dd>If the dependency value is a string,
%%    then the instance object MUST have a property with the same name as the
%%    dependency value.  If the dependency value is an array of strings,
%%    then the instance object MUST have a property with the same name
%%    as each string in the dependency value's array.</dd>
%%
%% <dt>Schema Dependency</dt>  <dd>If the dependency value is a schema, then the
%%    instance object MUST be valid against the schema.</dd>
%% </dl>
%% @private
check_dependencies(Value, Dependencies, RootSchema) ->
  jesse_json_medium:foreach(fun(DependencyName, DependencyValue) ->
                     case get_path(DependencyName, Value) of
                       undefined -> ok;
                       _         -> check_dependency_value(Value, DependencyValue, RootSchema)
                     end
                 end
               , Dependencies).

%% @private
check_dependency_value(Value, Dependency, _RootSchema) when is_binary(Dependency) ->
  case get_path(Dependency, Value) of
    undefined -> throw({ data_invalid
                , Value
                , missing_dependency
                , Dependency
                });
    _         -> ok
  end;
check_dependency_value(Value, Dependency, RootSchema) ->
  case is_json_object(Dependency) of
    true  -> check_value(Value, Dependency, Dependency, RootSchema);
    false ->
      case is_array(Dependency) of
        true  -> check_dependency_array(Value, Dependency, RootSchema);
        false -> throw({ schema_invalid
                       , Dependency
                       , wrong_type_dependency
                       })
      end
  end.

%% @private
check_dependency_array(Value, Dependency, RootSchema) ->
  jesse_json_medium:foreach( fun(_, PropertyName) ->
                     check_dependency_value(Value, PropertyName, RootSchema)
                 end
               , Dependency
               ).

%% @doc 5.9.  minimum
%%
%% This attribute defines the minimum value of the instance property
%% when the type of the instance value is a number.
%% @private
check_minimum(Value, Minimum, ExclusiveMinimum) ->
%% @doc 5.11.  exclusiveMinimum
%%
%% This attribute indicates if the value of the instance (if the
%% instance is a number) can not equal the number defined by the
%% "minimum" attribute.  This is false by default, meaning the instance
%% value can be greater then or equal to the minimum value.
%% @end
  Result = case ExclusiveMinimum of
             true -> Value > Minimum;
             _    -> Value >= Minimum
           end,
  Result orelse
    throw({ data_invalid
          , Value
          , not_in_range
          , {{minimum, Minimum}, {exclusive, ExclusiveMinimum}}
          }).

%%% @doc 5.10.  maximum
%%
%% This attribute defines the maximum value of the instance property
%% when the type of the instance value is a number.
%% @private
check_maximum(Value, Maximum, ExclusiveMaximum) ->
%% @doc 5.12.  exclusiveMaximum
%%
%% This attribute indicates if the value of the instance (if the
%% instance is a number) can not equal the number defined by the
%% "maximum" attribute.  This is false by default, meaning the instance
%% value can be less then or equal to the maximum value.
%% @end
  Result = case ExclusiveMaximum of
             true -> Value < Maximum;
             _    -> Value =< Maximum
           end,
  Result orelse
    throw({ data_invalid
          , Value
          , not_in_range
          , {{maximum, Maximum}, {exclusive, ExclusiveMaximum}}
          }).

%% @doc 5.13.  minItems
%%
%% This attribute defines the minimum number of values in an array when
%% the array is the instance value.
%% @private
check_min_items(Value, MinItems) ->
  jesse_json_medium:size(Value) >= MinItems orelse
    throw({ data_invalid
          , Value
          , not_correct_size
          , {min_items, MinItems}
          }).

%% @doc 5.14.  maxItems
%%
%% This attribute defines the maximum number of values in an array when
%% the array is the instance value.
%% @private
check_max_items(Value, MaxItems) ->
  jesse_json_medium:size(Value) =< MaxItems orelse
    throw({ data_invalid
          , Value
          , not_correct_size
          , {max_items, MaxItems}
          }).

%% @doc 5.15.  uniqueItems
%%
%% This attribute indicates that all items in an array instance MUST be
%% unique (contains no two identical values).
%%
%% Two instance are consider equal if they are both of the same type
%% and:
%% <ul>
%%   <li>are null; or</li>
%%
%%   <li>are booleans/numbers/strings and have the same value; or</li>
%%
%%   <li>are arrays, contains the same number of items, and each item in
%%       the array is equal to the corresponding item in the other array;
%%       or</li>
%%
%%   <li>are objects, contains the same property names, and each property
%%       in the object is equal to the corresponding property in the other
%%       object.</li>
%% </ul>
%% @private
check_unique_items(Value, true) ->
  Elems = jesse_json_medium:values(Value),
  find_unique_items(Elems, Elems).

find_unique_items(_Elems, []) ->
  true;

find_unique_items(Value, [Item | Rest]) ->
  lists:any(fun (NextItem) -> is_equal(Item, NextItem) end, Rest) andalso
    throw({data_invalid, Value, {Item, not_unique}, {uniqueItems, true}}),
  find_unique_items(Value, Rest).

%% @doc 5.16.  pattern
%% When the instance value is a string, this provides a regular
%% expression that a string instance MUST match in order to be valid.
%% Regular expressions SHOULD follow the regular expression
%% specification from ECMA 262/Perl 5
%% @private
check_pattern(Value, Pattern) ->
  case re:run(Value, Pattern, [{capture, none}]) of
    match   -> ok;
    nomatch -> throw({data_invalid, Value, no_match, Pattern})
  end.

%% @doc 5.17.  minLength
%%
%% When the instance value is a string, this defines the minimum length
%% of the string.
%% @private
check_min_length(Value, MinLength) ->
  case length(unicode:characters_to_list(Value)) >= MinLength of
    true  -> ok;
    false -> throw({ data_invalid
                   , Value
                   , not_correct_length
                   , {min_length, MinLength}
                   })
  end.

%% @doc 5.18.  maxLength
%%
%% When the instance value is a string, this defines the maximum length
%% of the string.
%% @private
check_max_length(Value, MaxLength) ->
  case length(unicode:characters_to_list(Value)) =< MaxLength of
    true  -> ok;
    false -> throw({ data_invalid
                   , Value
                   , not_correct_length
                   , {max_length, MaxLength}
                   })
  end.

%% @doc 5.19.  enum
%%
%% This provides an enumeration of all possible values that are valid
%% for the instance property.  This MUST be an array, and each item in
%% the array represents a possible value for the instance value.  If
%% this attribute is defined, the instance value MUST be one of the
%% values in the array in order for the schema to be valid.  Comparison
%% of enum values uses the same algorithm as defined in "uniqueItems"
%% (Section 5.15).
%% @private
check_enum(Value, Enum) ->
  jesse_json_medium:any( fun(_, ExpectedValue) ->
                 is_equal(Value, ExpectedValue)
             end
           , Enum) orelse
              throw({data_invalid, Value, not_in_enum, Enum}).

%% TODO:
check_format(_Value, _Format) -> ok.

%% @doc 5.24.  divisibleBy
%%
%% This attribute defines what value the number instance must be
%% divisible by with no remainder (the result of the division must be an
%% integer.)  The value of this attribute SHOULD NOT be 0.
%% @private
check_divisible_by(_Value, 0, JsonSchema) ->
  throw({schema_invalid, JsonSchema, {divide_by, 0}});
check_divisible_by(Value, DivisibleBy, _JsonSchema) ->
  Result = (Value / DivisibleBy - trunc(Value / DivisibleBy)) * DivisibleBy,
  case Result of
    0.0 -> ok;
    _   -> throw({data_invalid, Value, not_divisible_by, DivisibleBy})
  end.

%% @doc 5.25.  disallow
%%
%% This attribute takes the same values as the "type" attribute, however
%% if the instance matches the type or if this value is an array and the
%% instance matches any type or schema in the array, then this instance
%% is not valid.
%% @private
check_disallow(Value, Disallow, JsonSchema, RootSchema) ->
  try check_type(Value, Disallow, Disallow, RootSchema) of
      _ -> throw({data_invalid, Value, disallowed, JsonSchema})
  catch
    throw:{data_invalid, _, _, _} -> ok
  end.

%% @doc 5.26.  extends
%%
%% The value of this property MUST be another schema which will provide
%% a base schema which the current schema will inherit from.  The
%% inheritance rules are such that any instance that is valid according
%% to the current schema MUST be valid according to the referenced
%% schema.  This MAY also be an array, in which case, the instance MUST
%% be valid for all the schemas in the array.  A schema that extends
%% another schema MAY define additional attributes, constrain existing
%% attributes, or add other constraints.
%% @private
check_extends(Value, Extends, RootSchema) when is_binary(Extends) ->
  {DerefSchema, DerefRootSchema} = deref_schema(Extends, RootSchema),
  check_value(Value, DerefSchema, DerefSchema, DerefRootSchema);

check_extends(Value, Extends, RootSchema) ->
  case is_json_object(Extends) of
    true  ->
      check_value(Value, Extends, Extends, RootSchema);
    false ->
      check_extends_array(Value, Extends, RootSchema)
  end.

%% @private
check_extends_array(Value, Extends, RootSchema) ->
  jesse_json_medium:foreach(fun (_, Schema) ->
    check_extends(Value, Schema, RootSchema) end, Extends).

%%=============================================================================
%% @doc Returns `true' if given values (instance) are equal, otherwise `false'
%% is returned.
%%
%% Two instance are consider equal if they are both of the same type
%% and:
%% <ul>
%%   <li>are null; or</li>
%%
%%   <li>are booleans/numbers/strings and have the same value; or</li>
%%
%%   <li>are arrays, contains the same number of items, and each item in
%%       the array is equal to the corresponding item in the other array;
%%       or</li>
%%
%%   <li>are objects, contains the same property names, and each property
%%       in the object is equal to the corresponding property in the other
%%       object.</li>
%% </ul>
%% @private
is_equal(Value1, Value2) ->
  case is_json_object(Value1) andalso is_json_object(Value2) of
    true  -> compare_objects(Value1, Value2);
    false -> case is_array(Value1) andalso is_array(Value2) of
               true  -> compare_lists(
                jesse_json_medium:values(Value1),
                jesse_json_medium:values(Value2));
               false ->
                Value1 =:= Value2
             end
  end.

%% @private
compare_lists(Value1, Value2) ->
  length(Value1) =:= length(Value2) andalso
    compare_elements(Value1, Value2).

%% @private
compare_elements(Value1, Value2) ->
  lists:all( fun({Element1, Element2}) ->
                 is_equal(Element1, Element2)
             end
           , lists:zip(Value1, Value2)
           ).

%% @private
compare_objects(Value1, Value2) ->
  Length1 = jesse_json_medium:fold(fun (_, _, Count) ->
    Count + 1 end, 0, Value1),
  case jesse_json_medium:fold(fun
    (_Name, _PropertyValue, false) ->
      false;
    (Name, PropertyValue, Count) ->
      is_equal(get_path(Name, Value1), PropertyValue) andalso
        Count + 1
    end, 0, Value2) of
      false ->
        false;
      Length2 ->
        Length1 =:= Length2
  end.

%%=============================================================================
%% @private
get_path(Key, Schema) ->
  jesse_json_medium:path(Key, Schema).

deref_schema(Ref, RootSchema) ->
  try jesse_json_pointer:resolve(Ref, RootSchema) of
    {undefined, _} ->
      throw({schema_invalid, RootSchema, invalid_ref, Ref});
    Schemas ->
      Schemas
  catch
    error:unresolved ->
      throw({schema_invalid, RootSchema, unresolved_ref, Ref});
    _:Reason ->
      throw({schema_invalid, RootSchema, resolve_failed, Reason})
  end.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
