-module(hist).
-export([new/1, update/3]).


new(Name) -> 
	[{Name, inf}].


update(Node, N, History) -> 
	Message = lists:keyfind(Node, 1, History),
	case Message of 
		false ->
			{new, History ++ [{Node, N}]};
		_ ->
			{_, Num} = Message,
			if 
				N < Num ->
					IntermediaryList = lists:keydelete(Node, 1, History),
					{new, IntermediaryList ++ [{Node, N}]};
				true ->
					old
			end
	end.
