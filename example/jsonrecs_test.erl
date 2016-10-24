-module(jsonrecs_test).
-compile({parse_transform, border_control}).

-record(subrecord, {
	fielda = false :: boolean()
}).

-record(jsonrecs_test, {
	field1 :: [apple],
	field2 :: [#subrecord{}]
}).

-border_control(jsonrecs_test).

