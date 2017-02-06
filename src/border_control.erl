-module(border_control).

-export([parse_transform/2, format_error/1]).

-type form()    :: any().
-type forms()   :: [form()].
-type options() :: [{atom(), any()}].

-spec parse_transform(forms(), options()) -> forms().
parse_transform(Forms, _) ->
	Attributes   = get_forms(attribute, Forms),
	Functions    = get_forms(function,  Forms),
	[{eof, EOF}] = get_forms(eof,       Forms),
	EOExports    = get_eoexports(Forms),
	RecordInfo   = get_record_info(Forms),

	State0 = #{
		attributes  => Attributes,
		functions   => Functions,
		record_info => RecordInfo,
		eoexports   => EOExports,
		eof         => EOF,
		generated   => []
	},

	JsonRecs = get_attributes(border_control, Forms),
	case length(JsonRecs) of
		0 -> Forms;
		1 ->
			State1 = do_parse_transform(JsonRecs, State0),
			forms_from_state(State1);
		X -> report_error(invalid_num_jsonrec, {parse_transform, 2}, [{count, X}]) %% may support more in future. Only 1 for now
	end.

do_parse_transform([], State) ->
	State;
do_parse_transform(
	[{_,_,_,JsonRec}|JsonRecs],
	#{
		record_info := RecordInfo,
		eoexports   := EOExports,
		eof         := EOF,
		generated   := Generated
	} = State0
) ->
	Generated1 = generate(JsonRec, EOExports, EOF + length(Generated) + 1, RecordInfo),
	State1 = State0#{
		generated => [Generated|Generated1]
	},
	do_parse_transform(JsonRecs, State1).

-define(generate(A,B,C), {fun A/3, B, C}).
generate(Record, EOExports, Line, RecordInfo) ->
	Generate = [
		?generate(generate_get_record_infos0, get_record_infos, 0),
		?generate(generate_new0, new, 0),
		?generate(generate_new1, new, 1),
		?generate(generate_get2, get, 2),
		?generate(generate_set3, set, 3),
		?generate(generate_from_json1,     from_json,     1),
		?generate(generate_from_map1,      from_map,      1),
		?generate(generate_from_proplist1, from_proplist, 1),
		?generate(generate_to_json1,       to_json,       1),
		?generate(generate_to_map1,        to_map,        1),
		?generate(generate_to_proplist1,   to_proplist,   1),
		?generate(generate_to_src1,        to_src,        1)
	],
	do_generate(Record, EOExports, Line, RecordInfo, Generate, []).

do_generate(_, _, _, _, [], Acc) ->
	Acc;
do_generate(
	Record,
	EOExports,
	Line,
	RecordInfo,
	[Generator| Generate],
	Acc0
) ->
	Acc1 = case Generator of 
		{no_export, {Fun, _, _}} ->
			[
				Fun(Record, Line, RecordInfo)|
				Acc0
			];
		{Fun, Name, Arity} ->
			[
				{attribute, EOExports, export, [{Name, Arity}]},
				Fun(Record, Line, RecordInfo)|
				Acc0
			]
	end,	
	do_generate(Record, EOExports, Line+1, RecordInfo, Generate, Acc1).

generate_get_record_infos0(_, Line, RecordInfos) ->
	{function,Line,get_record_infos,0,[
		{clause,Line,[],[],[
			erl_parse:abstract(RecordInfos, [{line, Line}])
		]}
	]}.

generate_new0(Record, Line, _) ->
	{function,Line,new,0,[
		{clause,Line,[],[],[
			{record,Line,Record,[]}
		]}
	]}.

generate_new1(_, Line, _) ->
	 Clauses = [
	 	{
			clause,
			Line,
			[{var,Line,'Value'}],
			[
				[
					{
						call,
						Line,
						{atom,Line,is_binary},
						[
							{var,Line,'Value'}
						]
					}
				]
			],
			[
				{
					call,
					Line,
					{atom,Line,from_json},
					[
						{var,Line,'Value'}
					]
				}
			]
		},
		{
			clause,
			Line,
			[{var,Line,'Value'}],
			[
				[
					{
						call,
						Line,
						{atom,Line,is_map},
						[
							{var,Line,'Value'}
						]
					}
				]
			],
			[
				{
					call,
					Line,
					{atom,Line,from_map},					
					[
						{var,Line,'Value'}
					]
				}
			]
		},
		{
			clause,
			Line,
			[{var,Line,'Value'}],
			[
				[
					{
						call,
						Line,
						{atom,Line,is_list},
						[
							{var,Line,'Value'}
						]
					}
				]
			],
			[
				{
					call,
					Line,
					{atom,Line,from_proplist},
					[
						{var,Line,'Value'}
					]
				}
			]
		}
	 ],
	 {function, Line, new, 1, Clauses}.

generate_get2(Record, Line, RecordInfos) ->
	RecordInfo = maps:get(Record, RecordInfos),
	Clauses = lists:map(
		fun({_,Field,_}) ->
			generate_get2_clause(Line, Record, Field)
		end,
		RecordInfo
	),
	{function, Line, get, 2, Clauses}.
generate_get2_clause(Line, Record, Field) ->
	{
		clause,
		Line,
		[{atom,Line,Field},{var,Line,'Record'}],
		[],
		[
			{record_field,Line,{var,Line,'Record'},Record,{atom, Line, Field}}
		]
	}.

generate_set3(Record, Line, RecordInfos) ->
	RecordInfo = maps:get(Record, RecordInfos),
	Clauses = lists:foldl(
		fun({_, Field, Type}, Acc) ->
			[generate_set3_clause(Line, Record, Field, Type, RecordInfos) | Acc]
		end,
		[],
		RecordInfo
	),
	{function,Line,set,3,Clauses}.

generate_set3_clause(Line, Record, Field, undefined, _) ->
	{
		clause,
		Line,
		[
			{atom,Line,Field},
			{var,Line,'Value'},
			{var,Line,'Record'}
		],
		[],
		[
			{
				record,
				Line,
				{var,Line,'Record'},
				Record,
				[
					{record_field,Line,{atom,Line,Field},{var,Line,'Value'}}
				]
			}
		]
	};
generate_set3_clause(Line, Record, Field, Type, _) ->
	{
		clause,
		Line,
		[
			{atom,Line,Field},
			{var,Line,'Value'},
			{var,Line,'Record'}
		],
		[],
		[
			{
				match,
				Line,
				{atom,Line,ok},
				{
					call,
					Line,
					{
						remote,
						Line,
						{atom,Line,border_control_util},
						{atom,Line,validate}
					},
					[
						{var,Line,'Value'},
						erl_parse:abstract(Type, [{line, Line}]),
						{call,Line,{atom,Line,get_record_infos},[]}
					]
				}
			},
			{
				record,
				Line,
				{var,Line,'Record'},
				Record,
				[
					{record_field,Line,{atom,Line,Field},{var,Line,'Value'}}
				]
			}
		]
	}.

generate_from_json1(Record, Line, _) ->
	Clauses = [
		{
			clause,
			Line,
			[{var,Line,'Value'}],
			[
				[
					{
						call,
						Line,
						{atom,Line,is_binary},
						[
							{var,Line,'Value'}
						]
					}
				]
			],
			[
				{
					call,
					Line,
					{
						remote,
						Line,
						{atom,Line,border_control_util},
						{atom,Line,from_json}
					},
					[
						erl_parse:abstract({record, {Record, []}}, [{line, Line}]),
						{call,Line,{atom,Line,get_record_infos},[]},
						{var,Line,'Value'}						
					]
				}
			]
		}
	],
	{function,Line,from_json,1,Clauses}.

generate_from_map1(Record, Line, _) ->
	Clauses = [
		{
			clause,
			Line,
			[{var,Line,'Value'}],
			[
				[
					{
						call,
						Line,
						{atom,Line,is_map},
						[
							{var,Line,'Value'}
						]
					}
				]
			],
			[
				{
					call,
					Line,
					{
						remote,
						Line,
						{atom,Line,border_control_util},
						{atom,Line,from_map}
					},
					[
						{var,Line,'Value'},
						{call,Line,{atom,Line,get_record_infos},[]},
						{record,Line,Record,[]}
					]
				}
			]
		}
	],
	{function,Line,from_map,1,Clauses}.

generate_from_proplist1(Record, Line, _) ->
	Clauses = [
		{
			clause,
			Line,
			[{var,Line,'Value'}],
			[
				[
					{
						call,
						Line,
						{atom,Line,is_list},
						[
							{var,Line,'Value'}
						]
					}
				]
			],
			[
				{
					call,
					Line,
					{
						remote,
						Line,
						{atom,Line,border_control_util},
						{atom,Line,from_proplist}
					},
					[
						{var,Line,'Value'},
						{call,Line,{atom,Line,get_record_infos},[]},
						{record,Line,Record,[]}
					]
				}
			]
		}
	],
	{function,Line,from_proplist,1,Clauses}.

generate_to_json1(Record, Line, RecordInfos) ->
	TupleLength = length(maps:get(Record, RecordInfos)) + 1,
	Clauses = [
		{
			clause,
			Line,
			[{var,Line,'Value'}],
			[
				[
					{
						call,
						Line,
						{atom,Line,is_record},
						[
							{var,Line,'Value'},
							{atom,Line,Record},
							{integer,Line,TupleLength}
						]
					}
				]
			],
			[
				{
					call,
					Line,
					{
						remote,
						Line,
						{atom,Line,border_control_util},
						{atom,Line,to_json}
					},
					[
						{call,Line,{atom,Line,get_record_infos},[]},
						{var,Line,'Value'}
					]
				}
			]
		}
	],
	{function,Line,to_json,1,Clauses}.

generate_to_map1(Record, Line, RecordInfos) ->
	TupleLength = length(maps:get(Record, RecordInfos)) + 1,
	Clauses = [
		{
			clause,
			Line,
			[{var,Line,'Value'}],
			[
				[
					{
						call,
						Line,
						{atom,Line,is_record},
						[
							{var,Line,'Value'},
							{atom,Line,Record},
							{integer,Line,TupleLength}
						]
					}
				]
			],
			[
				{
					call,
					Line,
					{
						remote,
						Line,
						{atom,Line,border_control_util},
						{atom,Line,to_map}
					},
					[
						{call,Line,{atom,Line,get_record_infos},[]},
						{var,Line,'Value'}
					]
				}
			]
		}
	],
	{function,Line,to_map,1,Clauses}.

generate_to_proplist1(Record, Line, RecordInfos) ->
	TupleLength = length(maps:get(Record, RecordInfos)) + 1,
	Clauses = [
		{
			clause,
			Line,
			[{var,Line,'Value'}],
			[
				[
					{
						call,
						Line,
						{atom,Line,is_record},
						[
							{var,Line,'Value'},
							{atom,Line,Record},
							{integer,Line,TupleLength}
						]
					}
				]
			],
			[
				{
					call,
					Line,
					{
						remote,
						Line,
						{atom,Line,border_control_util},
						{atom,Line,to_proplist}
					},
					[
						{call,Line,{atom,Line,get_record_infos},[]},
						{var,Line,'Value'}
					]
				}
			]
		}
	],
	{function,Line,to_proplist,1,Clauses}.

generate_to_src1(Record, Line, _) ->
	{
		function,
		Line,
		to_src,
		1,
		[
			{
				clause,
				Line,
				[{var,Line,'File'}],
				[],
				[
					{
						match,
						Line,
						{tuple,Line,[{atom,Line,ok},{var,Line,'F'}]},
						{
							call,
							Line,
							{remote,Line,{atom,Line,file},{atom,Line,open}},
							[
								{var,Line,'File'},
								{cons,Line,{atom,Line,write},{nil,Line}}
							]
						}
					},
					{
						call,
						Line,
						{remote,Line,{atom,Line,file},{atom,Line,write}},
						[
							{var,Line,'F'},
							{
								call,
								Line,
								{remote,Line,{atom,Line,forms},{atom,Line,from_abstract}},
								[
									{
										call,
										Line,
										{remote,Line,{atom,Line,forms},{atom,Line,read}},
										[{atom,Line,Record}]
									}
								]
							}
						]
					},
					{call,Line,{remote,Line,{atom,Line,file},{atom,Line,close}},[{var,Line,'F'}]}
				]
			}
		]
	}.

forms_from_state(#{
	attributes := Attributes,
	functions  := Functions,
	eof        := EOF,
	generated  := Generated
}) ->
	lists:keysort(
		2,
		lists:flatten(
			[lists:reverse(Attributes), Functions, Generated, {eof, EOF}]
		)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Accessor functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Get forms
get_forms(eof, Forms) ->
	[lists:last(Forms)];
get_forms(Type, Forms) ->
	get_forms(Type, Forms, []).

get_forms(_, [], Acc) -> Acc;	
get_forms(Type, [Form|Forms], Acc) when element(1, Form) =:= Type ->
	get_forms(Type, Forms, [Form | Acc]);
get_forms(Type, [_|Forms], Acc) ->
	get_forms(Type, Forms, Acc).

%% Get attributes
get_attributes(Type, Forms) ->
	get_attributes(Type, Forms, []).
get_attributes(_, [], Acc) -> lists:reverse(Acc);
get_attributes(Type, [Form|Forms], Acc) when size(Form) < 3 ->
	get_attributes(Type, Forms, Acc);
get_attributes(Type, [Form|Forms], Acc) when element(1, Form) =:= attribute andalso element(3, Form) =:= Type ->
	get_attributes(Type, Forms, [Form|Acc]);
get_attributes(Type, [_|Forms], Acc) ->
	get_attributes(Type, Forms, Acc).	

%% Get the line for end of exports
get_eoexports(Forms) ->
	Attributes   = get_forms(attribute,   Forms),
	Functions    = get_forms(function,    Forms),
	Exports      = get_attributes(export, Attributes),

	EOAtt = element(2, lists:last(Attributes)),
	EOExp = case Exports of
		[] -> EOAtt;
		_  -> element(2, lists:last(Exports))
	end,
	SOFun = case Functions of
		[] ->
			undefined;
		_ ->
			element(2, hd(Functions))
	end,

	case SOFun > EOAtt of
		true  ->
			EOExp;
		false ->
			EOAtt
	end.

%% Get record info from forms
get_record_info(Forms) ->
	lists:foldl(
		fun({_,_,_,RecordInfo0}, Acc) ->
			{Record, RecordInfo1} = parse_rec_spec(RecordInfo0),
			Acc#{
				Record => RecordInfo1
			}
		end,
		#{},
		get_attributes(record, Forms)
	).

parse_rec_spec({Record, FieldInfo}) ->
	{_, Properties} = lists:foldl(
		fun(FieldSpec, {Index, Acc}) ->
			io:format("Parse - ~p\n", [FieldSpec]),
			{Field, Types} = parse_record_field(FieldSpec),
			{Index+1, [{Index, Field, Types}|Acc]}
		end,
		{2, []},
		FieldInfo
	),
	{Record, lists:reverse(Properties)}.

parse_record_field({record_field,_,{_,_,Field}}) ->
	{Field, undefined};
parse_record_field({record_field,_,{_,_,Field},_}) ->
	{Field, undefined};
parse_record_field({typed_record_field,{record_field,_,{_,_,Field}},Types}) ->
	{Field, parse_type(Types)};
parse_record_field({typed_record_field,{record_field,_,{_,_,Field},_},Types}) ->
	{Field, parse_type(Types)}.

parse_type({Type, _, Value}) ->
	{Type, [Value]};
parse_type({_, _, record, [{atom,_,Record}]}) ->
	{record, {Record, []}};
parse_type({_, _, Type, Parameters}) ->
	{Type, [parse_type(P) || P <- Parameters]}.

report_error(Reason, Fun, Info) ->
	Fmt = lists:flatten([
		"*** ERROR in parse_transform function:~n"
		"*** Reason     = ~p~n",
		"*** Location: ~p~n",
		["*** ~10w = ~p~n" || _ <- Info]
	]),
	Args = [
		Reason,
		Fun |
		lists:foldr(
			fun({K,V}, Acc) ->
				[K, V | Acc]
			end,
			[],
			Info
		)
	],
	io:format(Fmt, Args).

format_error({_Cat, Error}) ->
	Error.