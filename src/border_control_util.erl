-module(border_control_util).

-export([
	validate/3,
	from_json/3,
	to_json/2
]).

-spec validate(term(), {record,{atom(),list()}} | {atom(), list()}, map()) -> ok.
validate(_,_,_) -> ok.

from_json(Type, RecordInfos, Term) ->
	from_json_erl(Type, RecordInfos, jiffy:decode(Term, [return_maps])).

from_json_erl({record, {RecordType, _}}, RecordInfos, Term) when is_map(Term) ->
	RecordInfo = maps:get(RecordType, RecordInfos),
	{_, Fields, _} = lists:unzip3(RecordInfo),
	
	UseTupleInvocation = case code:which(RecordType) of
		non_existing -> false;
		_ ->
			Exports = proplists:get_value(exports, RecordType:module_info()),
			lists:member({new, 0}, Exports)
	end,

	NewRecord = case UseTupleInvocation of
		true ->
			RecordType:new();
		false ->
			list_to_tuple([RecordType | lists:duplicate(length(Fields), undefined)])
	end,

	lists:foldl(
		fun(KAtom, Acc0) ->
			KBin = list_to_binary(atom_to_list(KAtom)),
			{Ordinal, _, Type} = lists:keyfind(KAtom, 2, RecordInfo),
			case maps:get(KBin, Term, undefined) of
				undefined ->
					Acc0;
				V0 when UseTupleInvocation ->
					V1 = from_json_erl(Type, RecordInfos, V0),
					Acc0:set(KAtom, V1);
				V0 ->
					V1 = from_json_erl(Type, RecordInfos, V0),
					ok = validate(Type, RecordInfos, V1),
					setelement(Ordinal, Acc0, V1)
			end
		end,
		NewRecord,
		Fields
	);
from_json_erl({list, [Type]}, RecordInfos, Term) when is_list(Term) ->
	[from_json_erl(Type, RecordInfos, X) || X <- Term];
from_json_erl({atom, _}, _, Term) when is_binary(Term) ->
	list_to_existing_atom(binary_to_list(Term));
from_json_erl(Type, RecordInfos, Term) ->
	case application:get_env(border_control, converter, undefined) of
		undefined ->
			Term;
		{Mod, Fun} ->
			Mod:Fun(Type, RecordInfos, Term)
	end.

to_json(RecordInfos, Term) ->
	jiffy:encode(from_erl_json(RecordInfos, Term)).

from_erl_json(RecordInfos, Term) when is_tuple(Term) ->
	RecordInfo     = maps:get(element(1, Term), RecordInfos),
	{_, Fields, _} = lists:unzip3(RecordInfo),

	lists:foldl(
		fun
			({_, undefined}, Acc) ->
				Acc;
			({K, V}, Acc) ->
				Acc#{K => from_erl_json(RecordInfos, V)}
		end,
		#{},
		lists:zip(
			Fields,
			tl(tuple_to_list(Term))
		)
	);
from_erl_json(RecordInfos, List) when is_list(List) ->
	[from_erl_json(RecordInfos, X) || X <- List];
from_erl_json(_, Term) ->
	Term.
	

