-module(day19).

-export([accepted_rating_number_total/1
        ,distinct_accepted_combinations_count/1
        ]).

-define(ACCEPTED, <<"A">>).
-define(REJECTED, <<"R">>).

accepted_rating_number_total(Input) ->
    {Workflows, Parts} = parse_input(Input),
    AcceptedPartRatings =
        [rating(Part)|| Part <- Parts, is_accepted(Part, Workflows)],
    lists:sum(AcceptedPartRatings).

distinct_accepted_combinations_count(Input) ->
    {Workflows, _Parts} = parse_input(Input),
    PartRange = #{'x' => {1, 4000}
                 ,'m' => {1, 4000}
                 ,'a' => {1, 4000}
                 ,'s' => {1, 4000}
                 },
    traverse(PartRange, Workflows).

is_accepted(Part, Workflows) -> is_accepted(<<"in">>, Part, Workflows).

is_accepted(?ACCEPTED, _, _) -> 'true';
is_accepted(?REJECTED, _, _) -> 'false';
is_accepted(CurrentWFName, Part, Workflows) ->
    #{CurrentWFName := Rules} = Workflows,
    is_accepted(next_workflow(Rules, Part), Part, Workflows).

next_workflow([{'goto', Target}], _) -> Target;
next_workflow([{_, _, _, Target}=Rule | Rules], Part) ->
    case rule_matches_part(Rule, Part) of
        'true' -> Target;
        'false' -> next_workflow(Rules, Part)
    end.

traverse(PartRange, Workflows) -> traverse(<<"in">>, PartRange, Workflows).

traverse(?ACCEPTED, PartRange, _) -> combinations(PartRange);
traverse(?REJECTED, _, _) -> 0;
traverse(CurrentWFName, PartRange, Workflows) ->
    #{CurrentWFName := Rules} = Workflows,
    lists:sum([traverse(WFName, TrimmedRange, Workflows)
               || {WFName, TrimmedRange} <- trimmed_ranges(Rules, PartRange)
              ]).

trimmed_ranges(Rules, PartRange) -> trimmed_ranges(Rules, PartRange, []).

trimmed_ranges([], _, Acc) -> Acc;
trimmed_ranges([{'goto', Target} | Rules], PartRange, Acc) ->
    Acc1 = [{Target, PartRange} | Acc],
    trimmed_ranges(Rules, PartRange, Acc1);
trimmed_ranges([{_, _, _, Target}=Rule | Rules], PartRange, Acc) ->
    case trimmed_range(Rule, PartRange) of
        'undefined' -> trimmed_ranges(Rules, PartRange, Acc);
        {Trimmed, 'undefined'} -> [{Target, Trimmed} | Acc];
        {Trimmed, Untrimmed} ->
            trimmed_ranges(Rules, Untrimmed, [{Target, Trimmed} | Acc])
    end.

rule_matches_part({Category, '>', Cmp, _}, Part) ->
    maps:get(Category, Part) > Cmp;
rule_matches_part({Category, '<', Cmp, _}, Part) ->
    maps:get(Category, Part) < Cmp.

trimmed_range({Category, '>', Cmp, _}, Part) ->
    #{Category := {_Min, Max}=PartRange} = Part,
    case range:partition_overlap(PartRange, {Cmp + 1, max(Max, Cmp + 1)}) of
        {{PartRange, _}, 'undefined', _} ->
            %% No overlap - rule applied to no parts
            'undefined';
        {{'undefined', _}, PartRange, _} ->
            %% Full overlap - rule applied to all parts
            {Part, 'undefined'};
        {{Remaining, _}, Overlap, _} ->
            %% Partial overlap - rule applied to some parts, split ranges
            {Part#{Category => Overlap}
            ,Part#{Category => Remaining}
            }
    end;
trimmed_range({Category, '<', Cmp, _}, Part) ->
    #{Category := {Min, _Max}=PartRange} = Part,
    case range:partition_overlap(PartRange, {min(Min, Cmp - 1), Cmp - 1}) of
        {_, 'undefined', {PartRange, _}} ->
            %% No overlap - rule applied to no parts
            'undefined';
        {_, PartRange, {'undefined', _}} ->
            %% Full overlap - rule applied to all parts
            {Part, 'undefined'};
        {_, Overlap, {Remaining, _}} ->
            %% Partial overlap - rule applied to some parts, split ranges
            {Part#{Category => Overlap}
            ,Part#{Category => Remaining}
            }
    end.

rating(Part) -> lists:sum(maps:values(Part)).

combinations(PartRange) ->
    Combinations = [Max - Min + 1 || {_, {Min, Max}} <- maps:to_list(PartRange)],
    lists:foldl(fun 'erlang':'*'/2, 1, Combinations).

parse_input(Input) ->
    [Workflows, Parts] = binary:split(Input, <<"\n\n">>),
    {parse_workflows(Workflows), parse_parts(Parts)}.

parse_workflows(Workflows) ->
    Workflows1 = binary:split(Workflows, <<"\n">>, ['global']),
    lists:foldl(fun add_workflow/2, #{}, Workflows1).

add_workflow(Workflow, Workflows) ->
    {Name, Rules} = parse_workflow(Workflow),
    Workflows#{Name => Rules}.

parse_workflow(Workflow) ->
    [Name, Rules] = binary:split(Workflow, [<<"{">>, <<"}">>], ['global', 'trim_all']),
    {Name, parse_rules(Rules)}.

parse_rules(Rules) ->
    [parse_rule(Rule) || Rule <- binary:split(Rules, <<",">>, ['global'])].

parse_rule(Rule) ->
    case binary:split(Rule, [<<":">>], ['global']) of
        [Target] -> {'goto', Target};
        [<<Category:8, Operator:8, Cmp/binary>>, Target] ->
            {parse_category(Category)
            ,parse_operator(Operator)
            ,binary_to_integer(Cmp)
            ,Target
            }
    end.

parse_parts(Parts) ->
    [parse_part(Part) || Part <- binary:split(Parts, <<"\n">>, ['global'])].

parse_part(Part) ->
    Categories = binary:split(Part, [<<"{">>, <<",">>, <<"}">>], ['global', 'trim_all']),
    lists:foldl(fun add_category/2, #{}, Categories).

add_category(<<Category:8, "=", Rating/binary>>, Part) ->
    Part#{parse_category(Category) => binary_to_integer(Rating)}.

parse_category($x) -> 'x';
parse_category($m) -> 'm';
parse_category($a) -> 'a';
parse_category($s) -> 's'.

parse_operator($>) -> '>';
parse_operator($<) -> '<'.
