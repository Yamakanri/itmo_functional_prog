-module(avl_tree_unit).
-include_lib("eunit/include/eunit.hrl").


insert_find_test() ->
  Tree = avl_tree:insert(avl_tree:new(), 1, "one"),
  ?assertEqual({ok, "one"}, avl_tree:find(Tree, 1)),
  ?assertEqual(not_found, avl_tree:find(Tree, 99)).

delete_test() ->
  Tree = avl_tree:from_list([{1, "one"}, {2, "two"}, {3, "three"}]),
  Tree2 = avl_tree:delete(Tree, 2),
  ?assertEqual(not_found, avl_tree:find(Tree2, 2)),
  ?assertEqual(2, avl_tree:size_custom(Tree2)).


map_test() ->
  Tree = avl_tree:from_list([{1, 10}, {2, 20}]),
  Tree2 = avl_tree:map(fun(_K, V) -> V * 2 end, Tree),
  ?assertEqual({ok, 20}, avl_tree:find(Tree2, 1)),
  ?assertEqual({ok, 40}, avl_tree:find(Tree2, 2)).

filter_test() ->
  Tree = avl_tree:from_list([{1, 10}, {2, 20}, {3, 30}]),
  Tree2 = avl_tree:filter(fun(_K, V) -> V > 15 end, Tree),
  ?assertEqual(2, avl_tree:size_custom(Tree2)),
  ?assertEqual(not_found, avl_tree:find(Tree2, 1)).


fold_left_test() ->
  Tree = avl_tree:from_list([{1, 10}, {2, 20}, {3, 30}]),
  Sum = avl_tree:fold_left(fun(_K, V, Acc) -> Acc + V end, 0, Tree),
  ?assertEqual(60, Sum).

fold_right_test() ->
  Tree = avl_tree:from_list([{1, "a"}, {2, "b"}, {3, "c"}]),
  Keys = avl_tree:fold_right(fun(K, _V, Acc) -> [K | Acc] end, [], Tree),
  ?assertEqual([1, 2, 3], Keys).

%%% Моноид
monoid_identity_test() ->
  Tree = avl_tree:from_list([{1, "one"}]),
  ?assert(avl_tree:equal(avl_tree:concat(avl_tree:empty(), Tree), Tree)),
  ?assert(avl_tree:equal(avl_tree:concat(Tree, avl_tree:empty()), Tree)).

monoid_associativity_test() ->
  A = avl_tree:from_list([{1, "a"}]),
  B = avl_tree:from_list([{2, "b"}]),
  C = avl_tree:from_list([{3, "c"}]),
  Left = avl_tree:concat(avl_tree:concat(A, B), C),
  Right = avl_tree:concat(A, avl_tree:concat(B, C)),
  ?assert(avl_tree:equal(Left, Right)).


immutability_test() ->
  Tree1 = avl_tree:from_list([{1, "a"}]),
  Tree2 = avl_tree:insert(Tree1, 2, "b"),
  ?assertEqual(1, avl_tree:size_custom(Tree1)),
  ?assertEqual(2, avl_tree:size_custom(Tree2)).


polymorphic_test() ->
  TreeInt = avl_tree:from_list([{1, 100}]),
  TreeAtom = avl_tree:from_list([{a, "alpha"}]),
  ?assertEqual({ok, 100}, avl_tree:find(TreeInt, 1)),
  ?assertEqual({ok, "alpha"}, avl_tree:find(TreeAtom, a)).


to_list_sorted_test() ->
  Tree = avl_tree:from_list([{3, "c"}, {1, "a"}, {2, "b"}]),
  ?assertEqual([{1, "a"}, {2, "b"}, {3, "c"}], avl_tree:to_list(Tree)).


equal_test() ->
  Tree1 = avl_tree:from_list([{1, "a"}, {2, "b"}]),
  Tree2 = avl_tree:from_list([{1, "a"}, {2, "b"}]),
  Tree3 = avl_tree:from_list([{1, "a"}, {2, "c"}]),
  ?assert(avl_tree:equal(Tree1, Tree2)),
  ?assertNot(avl_tree:equal(Tree1, Tree3)).
