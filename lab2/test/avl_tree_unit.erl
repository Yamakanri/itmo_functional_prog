-module(avl_tree_unit).
-include_lib("eunit/include/eunit.hrl").


insert_find_test() ->
  Tree = tree_cur:insert(tree_cur:new(), 1, "one"),
  ?assertEqual({ok, "one"}, tree_cur:find(Tree, 1)),
  ?assertEqual(not_found, tree_cur:find(Tree, 99)).

delete_test() ->
  Tree = tree_cur:from_list([{1, "one"}, {2, "two"}, {3, "three"}]),
  Tree2 = tree_cur:delete(Tree, 2),
  ?assertEqual(not_found, tree_cur:find(Tree2, 2)),
  ?assertEqual(2, tree_cur:size_custom(Tree2)).


map_test() ->
  Tree = tree_cur:from_list([{1, 10}, {2, 20}]),
  Tree2 = tree_cur:map(fun(_K, V) -> V * 2 end, Tree),
  ?assertEqual({ok, 20}, tree_cur:find(Tree2, 1)),
  ?assertEqual({ok, 40}, tree_cur:find(Tree2, 2)).

filter_test() ->
  Tree = tree_cur:from_list([{1, 10}, {2, 20}, {3, 30}]),
  Tree2 = tree_cur:filter(fun(_K, V) -> V > 15 end, Tree),
  ?assertEqual(2, tree_cur:size_custom(Tree2)),
  ?assertEqual(not_found, tree_cur:find(Tree2, 1)).


fold_left_test() ->
  Tree = tree_cur:from_list([{1, 10}, {2, 20}, {3, 30}]),
  Sum = tree_cur:fold_left(fun(_K, V, Acc) -> Acc + V end, 0, Tree),
  ?assertEqual(60, Sum).

fold_right_test() ->
  Tree = tree_cur:from_list([{1, "a"}, {2, "b"}, {3, "c"}]),
  Keys = tree_cur:fold_right(fun(K, _V, Acc) -> [K | Acc] end, [], Tree),
  ?assertEqual([1, 2, 3], Keys).

%%% Моноид
monoid_identity_test() ->
  Tree = tree_cur:from_list([{1, "one"}]),
  ?assert(tree_cur:equal(tree_cur:concat(tree_cur:empty(), Tree), Tree)),
  ?assert(tree_cur:equal(tree_cur:concat(Tree, tree_cur:empty()), Tree)).

monoid_associativity_test() ->
  A = tree_cur:from_list([{1, "a"}]),
  B = tree_cur:from_list([{2, "b"}]),
  C = tree_cur:from_list([{3, "c"}]),
  Left = tree_cur:concat(tree_cur:concat(A, B), C),
  Right = tree_cur:concat(A, tree_cur:concat(B, C)),
  ?assert(tree_cur:equal(Left, Right)).


immutability_test() ->
  Tree1 = tree_cur:from_list([{1, "a"}]),
  Tree2 = tree_cur:insert(Tree1, 2, "b"),
  ?assertEqual(1, tree_cur:size_custom(Tree1)),
  ?assertEqual(2, tree_cur:size_custom(Tree2)).


polymorphic_test() ->
  TreeInt = tree_cur:from_list([{1, 100}]),
  TreeAtom = tree_cur:from_list([{a, "alpha"}]),
  ?assertEqual({ok, 100}, tree_cur:find(TreeInt, 1)),
  ?assertEqual({ok, "alpha"}, tree_cur:find(TreeAtom, a)).


to_list_sorted_test() ->
  Tree = tree_cur:from_list([{3, "c"}, {1, "a"}, {2, "b"}]),
  ?assertEqual([{1, "a"}, {2, "b"}, {3, "c"}], tree_cur:to_list(Tree)).


equal_test() ->
  Tree1 = tree_cur:from_list([{1, "a"}, {2, "b"}]),
  Tree2 = tree_cur:from_list([{1, "a"}, {2, "b"}]),
  Tree3 = tree_cur:from_list([{1, "a"}, {2, "c"}]),
  ?assert(tree_cur:equal(Tree1, Tree2)),
  ?assertNot(tree_cur:equal(Tree1, Tree3)).
