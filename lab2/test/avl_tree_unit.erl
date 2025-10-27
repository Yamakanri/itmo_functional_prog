-module(avl_tree_unit).
-include_lib("eunit/include/eunit.hrl").


insert_find_test() ->
  Tree = avl_dict:insert(avl_dict:new(), 1, "one"),
  ?assertEqual({ok, "one"}, avl_dict:find(Tree, 1)),
  ?assertEqual(not_found, avl_dict:find(Tree, 99)).

delete_test() ->
  Tree = avl_dict:from_list([{1, "one"}, {2, "two"}, {3, "three"}]),
  Tree2 = avl_dict:delete(Tree, 2),
  ?assertEqual(not_found, avl_dict:find(Tree2, 2)),
  ?assertEqual(2, avl_dict:size_custom(Tree2)).


map_test() ->
  Tree = avl_dict:from_list([{1, 10}, {2, 20}]),
  Tree2 = avl_dict:map(fun(_K, V) -> V * 2 end, Tree),
  ?assertEqual({ok, 20}, avl_dict:find(Tree2, 1)),
  ?assertEqual({ok, 40}, avl_dict:find(Tree2, 2)).

filter_test() ->
  Tree = avl_dict:from_list([{1, 10}, {2, 20}, {3, 30}]),
  Tree2 = avl_dict:filter(fun(_K, V) -> V > 15 end, Tree),
  ?assertEqual(2, avl_dict:size_custom(Tree2)),
  ?assertEqual(not_found, avl_dict:find(Tree2, 1)).


fold_left_test() ->
  Tree = avl_dict:from_list([{1, 10}, {2, 20}, {3, 30}]),
  Sum = avl_dict:fold_left(fun(_K, V, Acc) -> Acc + V end, 0, Tree),
  ?assertEqual(60, Sum).

fold_right_test() ->
  Tree = avl_dict:from_list([{1, "a"}, {2, "b"}, {3, "c"}]),
  Keys = avl_dict:fold_right(fun(K, _V, Acc) -> [K | Acc] end, [], Tree),
  ?assertEqual([1, 2, 3], Keys).

%%% Моноид
monoid_identity_test() ->
  Tree = avl_dict:from_list([{1, "one"}]),
  ?assert(avl_dict:equal(avl_dict:concat(avl_dict:empty(), Tree), Tree)),
  ?assert(avl_dict:equal(avl_dict:concat(Tree, avl_dict:empty()), Tree)).

monoid_associativity_test() ->
  A = avl_dict:from_list([{1, "a"}]),
  B = avl_dict:from_list([{2, "b"}]),
  C = avl_dict:from_list([{3, "c"}]),
  Left = avl_dict:concat(avl_dict:concat(A, B), C),
  Right = avl_dict:concat(A, avl_dict:concat(B, C)),
  ?assert(avl_dict:equal(Left, Right)).


immutability_test() ->
  Tree1 = avl_dict:from_list([{1, "a"}]),
  Tree2 = avl_dict:insert(Tree1, 2, "b"),
  ?assertEqual(1, avl_dict:size_custom(Tree1)),
  ?assertEqual(2, avl_dict:size_custom(Tree2)).


polymorphic_test() ->
  TreeInt = avl_dict:from_list([{1, 100}]),
  TreeAtom = avl_dict:from_list([{a, "alpha"}]),
  ?assertEqual({ok, 100}, avl_dict:find(TreeInt, 1)),
  ?assertEqual({ok, "alpha"}, avl_dict:find(TreeAtom, a)).


to_list_sorted_test() ->
  Tree = avl_dict:from_list([{3, "c"}, {1, "a"}, {2, "b"}]),
  ?assertEqual([{1, "a"}, {2, "b"}, {3, "c"}], avl_dict:to_list(Tree)).


equal_test() ->
  Tree1 = avl_dict:from_list([{1, "a"}, {2, "b"}]),
  Tree2 = avl_dict:from_list([{1, "a"}, {2, "b"}]),
  Tree3 = avl_dict:from_list([{1, "a"}, {2, "c"}]),
  ?assert(avl_dict:equal(Tree1, Tree2)),
  ?assertNot(avl_dict:equal(Tree1, Tree3)).
